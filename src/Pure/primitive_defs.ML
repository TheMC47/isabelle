(*  Title:      Pure/primitive_defs.ML
    Author:     Makarius

Primitive definition forms.
*)

signature PRIMITIVE_DEFS =
sig
  val dest_def: Proof.context ->
    {check_head: term -> bool,
     check_free_lhs: string -> bool,
     check_free_rhs: string -> bool,
     check_tfree: string -> bool} ->
    term -> (term * term) * term list * term
  val abs_def: term -> term * term
end;

structure Primitive_Defs: PRIMITIVE_DEFS =
struct

fun term_kind (Const _) = "existing constant "
  | term_kind (Free _) = "free variable "
  | term_kind (Bound _) = "bound variable "
  | term_kind _ = "";

(*c x \<equiv> t[x] to \<And>x. c x \<equiv> t[x]*)
fun dest_def ctxt {check_head, check_free_lhs, check_free_rhs, check_tfree} eq =
  let
    fun err msg = raise TERM (msg, [eq]);
    val eq_vars = Term.strip_all_vars eq;
    val eq_body = Term.strip_all_body eq;

    val display_terms =
      commas_quote o map (Syntax.string_of_term ctxt o Syntax_Trans.bound_vars ctxt eq_vars);
    val display_types = commas_quote o map (Syntax.string_of_typ ctxt);

    val (raw_lhs, rhs) = Logic.dest_equals eq_body handle TERM _ => err "Not a meta-equality (\<equiv>)";
    val lhs = Envir.beta_eta_contract raw_lhs;
    val (head, args) = Term.strip_comb lhs;
    val head_tfrees = TFrees.build (TFrees.add_tfrees head);

    fun check_arg (Bound _) = true
      | check_arg (Free (x, _)) = check_free_lhs x
      | check_arg (Const ("Pure.type", Type ("itself", [TFree _]))) = true
      | check_arg _ = false;
    fun close_arg (Bound _) t = t
      | close_arg x t = Logic.all x t;

    val lhs_bads = filter_out check_arg args;
    val lhs_dups = duplicates (op aconv) args;
    val rhs_extras = Term.fold_aterms (fn v as Free (x, _) =>
      if check_free_rhs x orelse member (op aconv) args v then I
      else insert (op aconv) v | _ => I) rhs [];
    val rhs_extrasT =
      TFrees.build (rhs |> TFrees.add_tfrees_unless
        (fn (a, S) => check_tfree a orelse TFrees.defined head_tfrees (a, S)))
      |> TFrees.keys |> map TFree;
  in
    if not (check_head head) then
      err ("Bad head of lhs: " ^ term_kind head ^ display_terms [head])
    else if not (null lhs_bads) then
      err ("Bad arguments on lhs: " ^ display_terms lhs_bads)
    else if not (null lhs_dups) then
      err ("Duplicate arguments on lhs: " ^ display_terms lhs_dups)
    else if not (null rhs_extras) then
      err ("Extra variables on rhs: " ^ display_terms rhs_extras)
    else if not (null rhs_extrasT) then
      err ("Extra type variables on rhs: " ^ display_types rhs_extrasT)
    else if exists_subterm (fn t => t aconv head) rhs then
      err "Entity to be defined occurs on rhs"
    else
      ((lhs, rhs), args,
        fold_rev close_arg args (Logic.list_all (eq_vars, (Logic.mk_equals (lhs, rhs)))))
  end;

(*\<And>x. c x \<equiv> t[x] to c \<equiv> \<lambda>x. t[x]*)
fun abs_def eq =
  let
    val body = Term.strip_all_body eq;
    val vars = map Free (Term.variant_bounds body (Term.strip_all_vars eq));
    val (lhs, rhs) = Logic.dest_equals (Term.subst_bounds (rev vars, body));
    val (lhs', args) = Term.strip_comb lhs;
    val rhs' = fold_rev (absfree o dest_Free) args rhs;
  in (lhs', rhs') end;

end;
