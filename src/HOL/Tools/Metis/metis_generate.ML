(*  Title:      HOL/Tools/Metis/metis_generate.ML
    Author:     Jia Meng, Cambridge University Computer Laboratory and NICTA
    Author:     Kong W. Susanto, Cambridge University Computer Laboratory
    Author:     Lawrence C. Paulson, Cambridge University Computer Laboratory
    Author:     Jasmin Blanchette, TU Muenchen

Translation of HOL to FOL for Metis.
*)

signature METIS_GENERATE =
sig
  type type_enc = ATP_Problem_Generate.type_enc

  datatype isa_thm =
    Isa_Reflexive_or_Trivial |
    Isa_Lambda_Lifted |
    Isa_Raw of thm

  val metis_equal : string
  val metis_predicator : string
  val metis_app_op : string
  val metis_systematic_type_tag : string
  val metis_ad_hoc_type_tag : string
  val metis_generated_var_prefix : string
  val trace : bool Config.T
  val verbose : bool Config.T
  val trace_msg : Proof.context -> (unit -> string) -> unit
  val verbose_warning : Proof.context -> string -> unit
  val metis_name_table : ((string * int) * ((type_enc -> string) * bool)) list
  val reveal_old_skolem_terms : (string * term) list -> term -> term
  val reveal_lam_lifted : (string * term) list -> term -> term
  val eliminate_lam_wrappers : term -> term
  val have_common_thm : Proof.context -> thm list -> thm list -> bool
  val generate_metis_problem : Proof.context -> type_enc -> string -> thm list -> thm list ->
    int Symtab.table * (Metis_Thm.thm * isa_thm) list * (unit -> (string * int) list)
    * ((string * term) list * (string * term) list)
end

structure Metis_Generate : METIS_GENERATE =
struct

open ATP_Problem
open ATP_Problem_Generate

val metis_equal = "="
val metis_predicator = "{}"
val metis_app_op = Metis_Name.toString Metis_Term.appName
val metis_systematic_type_tag =
  Metis_Name.toString Metis_Term.hasTypeFunctionName
val metis_ad_hoc_type_tag = "**"
val metis_generated_var_prefix = "_"

val trace = Attrib.setup_config_bool \<^binding>\<open>metis_trace\<close> (K false)
val verbose = Attrib.setup_config_bool \<^binding>\<open>metis_verbose\<close> (K true)

fun trace_msg ctxt msg = if Config.get ctxt trace then tracing (msg ()) else ()
fun verbose_warning ctxt msg =
  if Config.get ctxt verbose then warning ("Metis: " ^ msg) else ()

val metis_name_table =
  [((tptp_equal, 2), (K metis_equal, false)),
   ((tptp_old_equal, 2), (K metis_equal, false)),
   ((prefixed_predicator_name, 1), (K metis_predicator, false)),
   ((prefixed_app_op_name, 2), (K metis_app_op, false)),
   ((prefixed_type_tag_name, 2),
    (fn type_enc =>
        if level_of_type_enc type_enc = All_Types then metis_systematic_type_tag
        else metis_ad_hoc_type_tag, true))]

fun old_skolem_const_name i j num_T_args =
  Long_Name.implode (old_skolem_const_prefix :: map string_of_int [i, j, num_T_args])

fun conceal_old_skolem_terms i old_skolems t =
  if exists_Const (curry (op =) \<^const_name>\<open>Meson.skolem\<close> o fst) t then
    let
      fun aux old_skolems (t as \<^Const_>\<open>Meson.skolem T for _\<close>) =
          let
            val (old_skolems, s) =
              if i = ~1 then
                (old_skolems, \<^const_name>\<open>undefined\<close>)
              else
                (case AList.find (op aconv) old_skolems t of
                  s :: _ => (old_skolems, s)
                | [] =>
                  let
                    val s =
                      old_skolem_const_name i (length old_skolems) (length (Term.add_tvarsT T []))
                  in ((s, t) :: old_skolems, s) end)
          in (old_skolems, Const (s, T)) end
        | aux old_skolems (t1 $ t2) =
          let
            val (old_skolems, t1) = aux old_skolems t1
            val (old_skolems, t2) = aux old_skolems t2
          in (old_skolems, t1 $ t2) end
        | aux old_skolems (Abs (s, T, t')) =
          let val (old_skolems, t') = aux old_skolems t' in
            (old_skolems, Abs (s, T, t'))
          end
        | aux old_skolems t = (old_skolems, t)
    in aux old_skolems t end
  else
    (old_skolems, t)

fun reveal_old_skolem_terms old_skolems =
  map_aterms (fn t as Const (s, _) =>
      if String.isPrefix old_skolem_const_prefix s then
        AList.lookup (op =) old_skolems s |> the
        |> map_types (map_type_tvar (K dummyT))
      else
        t
    | t => t)

fun reveal_lam_lifted lifted =
  map_aterms (fn t as Const (s, _) =>
      if String.isPrefix lam_lifted_prefix s then
        (case AList.lookup (op =) lifted s of
          SOME t =>
            \<^Const>\<open>Metis.lambda dummyT\<close> $
              map_types (map_type_tvar (K dummyT)) (reveal_lam_lifted lifted t)
        | NONE => t)
      else
        t
    | t => t)


(* ------------------------------------------------------------------------- *)
(* Logic maps manage the interface between HOL and first-order logic.        *)
(* ------------------------------------------------------------------------- *)

datatype isa_thm =
  Isa_Reflexive_or_Trivial |
  Isa_Lambda_Lifted |
  Isa_Raw of thm

val proxy_defs = map (fst o snd o snd) proxy_table
fun prepare_helper ctxt =
  Meson.make_meta_clause ctxt #> rewrite_rule ctxt (map safe_mk_meta_eq proxy_defs)

fun metis_term_of_atp type_enc (ATerm ((s, []), tms)) =
  if is_tptp_variable s then
    Metis_Term.Var (Metis_Name.fromString s)
  else
    (case AList.lookup (op =) metis_name_table (s, length tms) of
       SOME (f, swap) => (f type_enc, swap)
     | NONE => (s, false))
    |> (fn (s, swap) =>
           Metis_Term.Fn (Metis_Name.fromString s,
                          tms |> map (metis_term_of_atp type_enc)
                              |> swap ? rev))
fun metis_atom_of_atp type_enc (AAtom tm) =
    (case metis_term_of_atp type_enc tm of
       Metis_Term.Fn x => x
     | _ => raise Fail "non CNF -- expected function")
  | metis_atom_of_atp _ _ = raise Fail "not CNF -- expected atom"
fun metis_literal_of_atp type_enc (AConn (ANot, [phi])) =
    (false, metis_atom_of_atp type_enc phi)
  | metis_literal_of_atp type_enc phi = (true, metis_atom_of_atp type_enc phi)
fun metis_literals_of_atp type_enc (AConn (AOr, phis)) =
    maps (metis_literals_of_atp type_enc) phis
  | metis_literals_of_atp type_enc phi = [metis_literal_of_atp type_enc phi]
fun metis_axiom_of_atp ctxt type_enc clauses (Formula ((ident, _), _, phi, _, _)) =
    let
      fun some isa =
        SOME (phi |> metis_literals_of_atp type_enc
                  |> Metis_LiteralSet.fromList
                  |> Metis_Thm.axiom, isa)
    in
      if String.isPrefix tags_sym_formula_prefix ident then
        Isa_Reflexive_or_Trivial |> some
      else if String.isPrefix conjecture_prefix ident then
        NONE
      else if String.isPrefix helper_prefix ident then
        (case (String.isSuffix typed_helper_suffix ident, space_explode "_" ident) of
          (needs_fairly_sound, _ :: const :: j :: _) =>
          nth (AList.lookup (op =) (helper_table true) (const, needs_fairly_sound) |> the)
            (the (Int.fromString j) - 1)
          |> snd |> prepare_helper ctxt
          |> Isa_Raw |> some
        | _ => raise Fail ("malformed helper identifier " ^ quote ident))
      else
        (case try (unprefix fact_prefix) ident of
          SOME s =>
          let val s = s |> space_explode "_" |> tl |> space_implode "_" in
            (case Int.fromString s of
              SOME j => Meson.make_meta_clause ctxt (snd (nth clauses j)) |> Isa_Raw |> some
            | NONE =>
              if String.isPrefix lam_fact_prefix (unascii_of s) then Isa_Lambda_Lifted |> some
              else raise Fail ("malformed fact identifier " ^ quote ident))
          end
        | NONE => some (Isa_Raw TrueI))
    end
  | metis_axiom_of_atp _ _ _ _ = raise Fail "not CNF -- expected formula"

fun eliminate_lam_wrappers \<^Const_>\<open>Metis.lambda _ for t\<close> = eliminate_lam_wrappers t
  | eliminate_lam_wrappers (t $ u) = eliminate_lam_wrappers t $ eliminate_lam_wrappers u
  | eliminate_lam_wrappers (Abs (s, T, t)) = Abs (s, T, eliminate_lam_wrappers t)
  | eliminate_lam_wrappers t = t

(* Designed to work also with monomorphic instances of polymorphic theorems. *)
fun have_common_thm ctxt ths1 ths2 =
  exists (member (Term.aconv_untyped o apply2 Thm.prop_of) ths1)
    (map (Meson.make_meta_clause ctxt) ths2)

(* Function to generate metis clauses, including comb and type clauses *)
fun generate_metis_problem ctxt type_enc lam_trans conj_clauses fact_clauses =
  let
    val (conj_clauses, fact_clauses) =
      if is_type_enc_polymorphic type_enc then
        (conj_clauses, fact_clauses)
      else
        conj_clauses @ fact_clauses
        |> map (pair 0)
        |> Monomorph.monomorph atp_schematic_consts_of ctxt
        |> chop (length conj_clauses)
        |> apply2 (maps (map (zero_var_indexes o snd)))
    (* Pretend every clause is a "simp" rule, to guide the term ordering. *)
    val clauses =
      map_index (apfst (fn j => (Int.toString j, (Local, Simp)))) (conj_clauses @ fact_clauses)
    val (old_skolems, props) =
      fold_rev (fn (name, th) => fn (old_skolems, props) =>
           th |> Thm.prop_of |> Logic.strip_imp_concl
              |> conceal_old_skolem_terms (length clauses) old_skolems
              ||> lam_trans = liftingN ? eliminate_lam_wrappers
              ||> (fn prop => (name, prop) :: props))
         clauses ([], [])
    (*
    val _ =
      tracing ("PROPS:\n" ^
               cat_lines (map (Syntax.string_of_term ctxt o snd) props))
    *)
    val lam_trans = if lam_trans = combsN then no_lamsN else lam_trans
    val (atp_problem, _, lifted, sym_tab) =
      generate_atp_problem ctxt true CNF Hypothesis type_enc Metis lam_trans false false false []
        \<^prop>\<open>False\<close> props

    val _ = trace_msg ctxt (fn () => cat_lines ("OLD SKOLEM TERMS" ::
        (old_skolems |> map (fn (s, t) => s ^ ": " ^ Syntax.string_of_term ctxt t))))
    val _ = trace_msg ctxt (fn () => cat_lines ("LIFTED LAMBDAS" ::
        (lifted |> map (fn (s, t) => s ^ ": " ^ Syntax.string_of_term ctxt t))))

    (*
    val _ = tracing ("ATP PROBLEM: " ^
                     cat_lines (lines_of_atp_problem CNF atp_problem))
    *)
    (* "rev" is for compatibility with existing proof scripts. *)
    val axioms = atp_problem
      |> maps (map_filter (metis_axiom_of_atp ctxt type_enc clauses) o snd) |> rev
    fun ord_info () = atp_problem_term_order_info atp_problem
  in
    (sym_tab, axioms, ord_info, (lifted, old_skolems))
  end

end;
