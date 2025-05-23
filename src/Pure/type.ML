(*  Title:      Pure/type.ML
    Author:     Tobias Nipkow, Lawrence C Paulson, and Markus Wenzel

Type signatures and certified types, special treatment of type vars,
matching and unification of types, extend and merge type signatures.
*)

signature TYPE =
sig
  (*constraints*)
  val mark_polymorphic: typ -> typ
  val constraint: typ -> term -> term
  val constraint_type: Proof.context -> typ -> typ
  val strip_constraints: term -> term
  val appl_error: Proof.context -> term -> typ -> term -> typ -> string
  (*type signatures and certified types*)
  datatype decl =
    Logical_Type of int |
    Abbreviation of string list * typ * bool |
    Nonterminal
  val decl_args: decl -> int
  val decl_logical: decl -> bool
  type tsig
  val eq_tsig: tsig * tsig -> bool
  val rep_tsig: tsig ->
   {classes: Name_Space.T * Sorts.algebra,
    default: sort,
    types: decl Name_Space.table,
    log_types: string list}
  val change_base: bool -> tsig -> tsig
  val change_ignore: tsig -> tsig
  val empty_tsig: tsig
  val class_space: tsig -> Name_Space.T
  val defaultS: tsig -> sort
  val logical_types: tsig -> string list
  val eq_sort: tsig -> sort * sort -> bool
  val subsort: tsig -> sort * sort -> bool
  val of_sort: tsig -> typ * sort -> bool
  val inter_sort: tsig -> sort * sort -> sort
  val cert_class: tsig -> class -> class
  val cert_sort: tsig -> sort -> sort
  val minimize_sort: tsig -> sort -> sort
  val witness_sorts: tsig ->
    (typ * sort) list -> sort Ord_List.T ->
    (typ * sort) list * sort Ord_List.T
  type mode
  val mode_default: mode
  val mode_syntax: mode
  val mode_abbrev: mode
  val get_mode: Proof.context -> mode
  val set_mode: mode -> Proof.context -> Proof.context
  val restore_mode: Proof.context -> Proof.context -> Proof.context
  val type_space: tsig -> Name_Space.T
  val type_alias: Name_Space.naming -> binding -> string -> tsig -> tsig
  val check_decl: Context.generic -> tsig ->
    xstring * Position.T -> (string * Position.report list) * decl
  val the_decl: tsig -> string * Position.T -> decl
  val certify_typ_same: mode -> tsig -> typ Same.operation
  val certify_typ: mode -> tsig -> typ -> typ
  val certify_types: mode -> tsig -> term -> term
  val arity_number: tsig -> string -> int
  val arity_sorts: Context.generic -> tsig -> string -> sort -> sort list

  (*special treatment of type vars*)
  val sort_of_atyp: typ -> sort
  val no_tvars: typ -> typ
  val varify_global_names: TFrees.set -> term -> ((string * sort) * (indexname * sort)) list
  val varify_global: TFrees.set -> term -> ((string * sort) * (indexname * sort)) list * term
  val legacy_freezeT: term -> ((string * int) * sort -> typ option) option
  val legacy_freeze_thaw_type: typ -> typ * (typ -> typ)
  val legacy_freeze_type: typ -> typ
  val legacy_freeze_thaw: term -> term * (term -> term)
  val legacy_freeze: term -> term

  (*matching and unification*)
  exception TYPE_MATCH
  type tyenv = (sort * typ) Vartab.table
  val lookup: tyenv -> indexname * sort -> typ option
  val devar: tyenv -> typ -> typ
  val typ_match: tsig -> typ * typ -> tyenv -> tyenv
  val typ_instance: tsig -> typ * typ -> bool
  val raw_match: typ * typ -> tyenv -> tyenv
  val raw_matches: typ list * typ list -> tyenv -> tyenv
  val could_match: typ * typ -> bool
  val could_matches: typ list * typ list -> bool
  val raw_instance: typ * typ -> bool
  val raw_equiv: typ * typ -> bool
  exception TUNIFY
  val unify: tsig -> typ * typ -> tyenv * int -> tyenv * int
  val raw_unify: typ * typ -> tyenv -> tyenv
  val raw_unifys: typ list * typ list -> tyenv -> tyenv
  val could_unify: typ * typ -> bool
  val could_unifys: typ list * typ list -> bool
  val unified: tyenv -> typ * typ -> bool

  (*extend and merge type signatures*)
  val add_class: Context.generic -> binding * class list -> tsig -> tsig
  val hide_class: bool -> string -> tsig -> tsig
  val set_defsort: sort -> tsig -> tsig
  val add_type: Context.generic -> binding * int -> tsig -> tsig
  val add_abbrev: Context.generic -> binding * string list * typ -> tsig -> tsig
  val add_nonterminal: Context.generic -> binding -> tsig -> tsig
  val hide_type: bool -> string -> tsig -> tsig
  val add_arity: Context.generic -> arity -> tsig -> tsig
  val add_classrel: Context.generic -> class * class -> tsig -> tsig
  val merge_tsig: Context.generic -> tsig * tsig -> tsig
end;

structure Type: TYPE =
struct

(** constraints **)

(*indicate polymorphic Vars*)
fun mark_polymorphic T = Type ("_polymorphic_", [T]);

fun constraint T t =
  if T = dummyT then t
  else Const ("_type_constraint_", T --> T) $ t;

fun constraint_type ctxt T =
  let fun err () = error ("Malformed internal type constraint: " ^ Syntax.string_of_typ ctxt T);
  in (case T of Type ("fun", [A, B]) => if A = B then A else err () | _ => err ()) end;

fun strip_constraints (Const ("_type_constraint_", _) $ t) = strip_constraints t
  | strip_constraints (t $ u) = strip_constraints t $ strip_constraints u
  | strip_constraints (Abs (x, T, t)) = Abs (x, T, strip_constraints t)
  | strip_constraints a = a;

fun appl_error ctxt (Const ("_type_constraint_", Type ("fun", [T, _]))) _ u U =
      cat_lines
       ["Failed to meet type constraint:", "",
        Pretty.string_of (Pretty.block
         [Pretty.str "Term:", Pretty.brk 2, Syntax.pretty_term ctxt u,
          Pretty.str " ::", Pretty.brk 1, Syntax.pretty_typ ctxt U]),
        Pretty.string_of (Pretty.block
         [Pretty.str "Type:", Pretty.brk 2, Syntax.pretty_typ ctxt T])]
  | appl_error ctxt t T u U =
      cat_lines
       ["Type error in application: " ^
          (case T of
            Type ("fun", _) => "incompatible operand type"
          | _ => "operator not of function type"),
        "",
        Pretty.string_of (Pretty.block
          [Pretty.str "Operator:", Pretty.brk 2, Syntax.pretty_term ctxt t,
            Pretty.str " ::", Pretty.brk 1, Syntax.pretty_typ ctxt T]),
        Pretty.string_of (Pretty.block
          [Pretty.str "Operand:", Pretty.brk 3, Syntax.pretty_term ctxt u,
            Pretty.str " ::", Pretty.brk 1, Syntax.pretty_typ ctxt U])];



(** type signatures and certified types **)

(* type declarations *)

datatype decl =
  Logical_Type of int |
  Abbreviation of string list * typ * bool |
  Nonterminal;

fun decl_args (Logical_Type n) = n
  | decl_args (Abbreviation (vs, _, _)) = length vs
  | decl_args Nonterminal = 0;

fun decl_logical (Logical_Type _) = true
  | decl_logical _ = false;


(* type tsig *)

datatype tsig =
  TSig of {
    classes: Name_Space.T * Sorts.algebra,  (*order-sorted algebra of type classes*)
    default: sort,                          (*default sort on input*)
    types: decl Name_Space.table,           (*declared types*)
    log_types: string list};                (*logical types sorted by number of arguments*)

fun eq_tsig
   (TSig {classes = classes1, default = default1, types = types1, log_types = _},
    TSig {classes = classes2, default = default2, types = types2, log_types = _}) =
  pointer_eq (classes1, classes2) andalso
  default1 = default2 andalso
  pointer_eq (types1, types2);

fun rep_tsig (TSig comps) = comps;

fun make_tsig (classes, default, types, log_types) =
  TSig {classes = classes, default = default, types = types, log_types = log_types};

fun change_base begin (TSig {classes, default, types, log_types}) =
  make_tsig (classes, default, Name_Space.change_base begin types, log_types);

fun change_ignore (TSig {classes, default, types, log_types}) =
  make_tsig (classes, default, Name_Space.change_ignore types, log_types);

fun build_tsig (classes, default, types) =
  let
    val log_types =
      Name_Space.fold_table (fn (c, Logical_Type n) => cons (c, n) | _ => I) types []
      |> Library.sort (int_ord o apply2 snd) |> map fst;
  in make_tsig (classes, default, types, log_types) end;

fun map_tsig f (TSig {classes, default, types, log_types = _}) =
  build_tsig (f (classes, default, types));

val empty_tsig =
  build_tsig ((Name_Space.empty Markup.classN, Sorts.empty_algebra), [],
    Name_Space.empty_table Markup.type_nameN);


(* classes and sorts *)

val class_space = #1 o #classes o rep_tsig;

fun defaultS (TSig {default, ...}) = default;
fun logical_types (TSig {log_types, ...}) = log_types;

fun eq_sort (TSig {classes, ...}) = Sorts.sort_eq (#2 classes);
fun subsort (TSig {classes, ...}) = Sorts.sort_le (#2 classes);
fun of_sort (TSig {classes, ...}) = Sorts.of_sort (#2 classes);
fun inter_sort (TSig {classes, ...}) = Sorts.inter_sort (#2 classes);

fun cert_class (TSig {classes, ...}) = Sorts.cert_class (#2 classes);
fun cert_sort (TSig {classes, ...}) = Sorts.cert_sort (#2 classes);

fun minimize_sort (TSig {classes, ...}) = Sorts.minimize_sort (#2 classes);

fun witness_sorts (TSig {classes, log_types, ...}) =
  Sorts.witness_sorts (#2 classes) log_types;


(* certification mode *)

datatype mode = Mode of {normalize: bool, logical: bool};

val mode_default = Mode {normalize = true, logical = true};
val mode_syntax = Mode {normalize = true, logical = false};
val mode_abbrev = Mode {normalize = false, logical = false};

structure Mode = Proof_Data
(
  type T = mode;
  fun init _ = mode_default;
);

val get_mode = Mode.get;
fun set_mode mode = Mode.map (K mode);
fun restore_mode ctxt = set_mode (get_mode ctxt);


(* types *)

val type_space = Name_Space.space_of_table o #types o rep_tsig;

fun type_alias naming binding name = map_tsig (fn (classes, default, types) =>
  (classes, default, (Name_Space.alias_table naming binding name types)));


fun undecl_type c = "Undeclared type constructor: " ^ quote c;

fun lookup_type (TSig {types, ...}) = Name_Space.lookup types;

fun check_decl context (TSig {types, ...}) (c, pos) =
  Name_Space.check_reports context types (c, [pos]);

fun the_decl tsig (c, pos) =
  (case lookup_type tsig c of
    NONE => error (undecl_type c ^ Position.here pos)
  | SOME decl => decl);


(* certify types *)

fun bad_nargs t = "Bad number of arguments for type constructor: " ^ quote t;

fun certify_typ_same (Mode {normalize, logical}) tsig =
  let
    fun err T msg = raise TYPE (msg, [T], []);
    fun err_syntactic T c = err T ("Illegal occurrence of syntactic type: " ^ quote c);

    fun sort S = (cert_sort tsig S; raise Same.SAME);
    fun typ (T as Type (c, args)) =
          let val decl = the_decl tsig (c, Position.none) in
            if length args <> decl_args decl then err T (bad_nargs c)
            else
              (case decl of
                Logical_Type _ => Type (c, Same.map typ args)
              | Abbreviation (vs, U, syntactic) =>
                  if syntactic andalso logical then err_syntactic T c
                  else if normalize then inst_typ vs args U
                  else Type (c, Same.map typ args)
              | Nonterminal => if logical then err_syntactic T c else raise Same.SAME)
          end
      | typ (TFree (_, S)) = sort S
      | typ (T as TVar ((x, i), S)) =
          if i < 0 then err T ("Malformed type variable: " ^ quote (Term.string_of_vname (x, i)))
          else sort S
    and inst_typ vs args =
      Term_Subst.instantiateT_frees
        (TFrees.build (fold2 (fn v => fn T => TFrees.add ((v, []), Same.commit typ T)) vs args));
  in typ end;

val certify_typ = Same.commit oo certify_typ_same;
val certify_types = Term.map_types oo certify_typ_same;


(* type arities *)

fun arity_number tsig a =
  (case lookup_type tsig a of
    SOME (Logical_Type n) => n
  | _ => error (undecl_type a));

fun arity_sorts _ tsig a [] = replicate (arity_number tsig a) []
  | arity_sorts context (TSig {classes, ...}) a S =
      Sorts.mg_domain (#2 classes) a S
        handle Sorts.CLASS_ERROR err => error (Sorts.class_error context err);



(** special treatment of type vars **)

(* sort_of_atyp *)

fun sort_of_atyp (TFree (_, S)) = S
  | sort_of_atyp (TVar (_, S)) = S
  | sort_of_atyp T = raise TYPE ("sort_of_atyp", [T], []);


(* no_tvars *)

fun no_tvars T =
  (case Term.add_tvarsT T [] of
    [] => T
  | vs =>
      raise TYPE ("Illegal schematic type variable(s): " ^
        commas_quote (map (Term.string_of_vname o #1) (rev vs)), [T], []));


(* varify_global *)

fun varify_global_names fixed t =
  let
    val xs =
      build (t |> (Term.fold_types o Term.fold_atyps)
        (fn TFree v => if TFrees.defined fixed v then I else insert (op =) v | _ => I));
    val used = Name.build_context (t |> Term.declare_tvar_names (K true));
    val ys = Name.variants used (map #1 xs);
  in map2 (fn (a, S) => fn b => ((a, S), ((b, 0), S))) xs ys end;

fun varify_global fixed t =
  let
    val names = varify_global_names fixed t;
    val tab = TFrees.make names;
    fun get v =
      (case TFrees.lookup tab v of
        NONE => raise Same.SAME
      | SOME w => TVar w);
  in (names, (map_types o map_type_tfree) get t) end;


(* legacy_freezeT *)

local

fun new_name ix (pairs, used) =
  let val v = singleton (Name.variant_list used) (string_of_indexname ix)
  in ((ix, v) :: pairs, v :: used) end;

fun freeze_one alist (ix, S) =
  AList.lookup (op =) alist ix |> Option.map (fn a => TFree (a, S));

in

fun legacy_freezeT t =
  let
    val used = Term.add_tfree_names t [];
    val (alist, _) = fold_rev new_name (map #1 (Term.add_tvars t [])) ([], used);
  in if null alist then NONE else SOME (freeze_one alist) end;

end;


(* freeze_thaw: freeze TVars in a term; return the "thaw" inverse *)

local

fun new_name ix (pairs, used) =
  let val v = singleton (Name.variant_list used) (string_of_indexname ix)
  in ((ix, v) :: pairs, v :: used) end;

fun freeze_one alist (ix, sort) =
  TFree (the (AList.lookup (op =) alist ix), sort)
    handle Option.Option =>
      raise TYPE ("Failure during freezing of ?" ^ string_of_indexname ix, [], []);

fun thaw_one alist (a, sort) = TVar (the (AList.lookup (op =) alist a), sort)
  handle Option.Option => raise Same.SAME;

in

fun legacy_freeze_thaw_type T =
  let
    val used = Term.add_tfree_namesT T [];
    val (alist, _) = fold_rev new_name (map #1 (Term.add_tvarsT T [])) ([], used);
  in (map_type_tvar (freeze_one alist) T, map_type_tfree (thaw_one (map swap alist))) end;

val legacy_freeze_type = #1 o legacy_freeze_thaw_type;

fun legacy_freeze_thaw t =
  let
    val used = Term.add_tfree_names t [];
    val (alist, _) = fold_rev new_name (map #1 (Term.add_tvars t [])) ([], used);
  in
    (case alist of
      [] => (t, fn x => x) (*nothing to do!*)
    | _ => (map_types (map_type_tvar (freeze_one alist)) t,
      map_types (map_type_tfree (thaw_one (map swap alist)))))
  end;

val legacy_freeze = #1 o legacy_freeze_thaw;

end;



(** matching and unification of types **)

type tyenv = (sort * typ) Vartab.table;

fun tvar_clash ixn S S' =
  raise TYPE ("Type variable has two distinct sorts", [TVar (ixn, S), TVar (ixn, S')], []);

fun lookup tye (ixn, S) =
  (case Vartab.lookup tye ixn of
    NONE => NONE
  | SOME (S', T) => if S = S' then SOME T else tvar_clash ixn S S');


(* matching *)

exception TYPE_MATCH;

fun typ_match tsig =
  let
    fun match (TVar (v, S), T) subs =
          (case lookup subs (v, S) of
            NONE =>
              if of_sort tsig (T, S) then Vartab.update_new (v, (S, T)) subs
              else raise TYPE_MATCH
          | SOME U => if U = T then subs else raise TYPE_MATCH)
      | match (Type (a, Ts), Type (b, Us)) subs =
          if a <> b then raise TYPE_MATCH
          else matches (Ts, Us) subs
      | match (TFree x, TFree y) subs =
          if x = y then subs else raise TYPE_MATCH
      | match _ _ = raise TYPE_MATCH
    and matches (T :: Ts, U :: Us) subs = matches (Ts, Us) (match (T, U) subs)
      | matches _ subs = subs;
  in match end;

fun typ_instance tsig (T, U) =
  (Vartab.build (typ_match tsig (U, T)); true) handle TYPE_MATCH => false;

(*purely structural matching*)
fun raw_match (TVar (v, S), T) subs =
      (case lookup subs (v, S) of
        NONE => Vartab.update_new (v, (S, T)) subs
      | SOME U => if U = T then subs else raise TYPE_MATCH)
  | raw_match (Type (a, Ts), Type (b, Us)) subs =
      if a <> b then raise TYPE_MATCH
      else raw_matches (Ts, Us) subs
  | raw_match (TFree x, TFree y) subs =
      if x = y then subs else raise TYPE_MATCH
  | raw_match _ _ = raise TYPE_MATCH
and raw_matches (T :: Ts, U :: Us) subs = raw_matches (Ts, Us) (raw_match (T, U) subs)
  | raw_matches ([], []) subs = subs
  | raw_matches _ _ = raise TYPE_MATCH;

(*fast matching filter*)
fun could_match (Type (a, Ts), Type (b, Us)) = a = b andalso could_matches (Ts, Us)
  | could_match (TFree (a, _), TFree (b, _)) = a = b
  | could_match (TVar _, _) = true
  | could_match _ = false
and could_matches (T :: Ts, U :: Us) = could_match (T, U) andalso could_matches (Ts, Us)
  | could_matches ([], []) = true
  | could_matches _ = false;

fun can_raw_match arg =
  (Vartab.build (raw_match arg); true) handle TYPE_MATCH => false;

fun raw_instance (T, U) = could_match (U, T) andalso can_raw_match (U, T);
fun raw_equiv (T, U) = could_match (U, T) andalso can_raw_match (U, T) andalso can_raw_match (T, U);


(* unification *)

exception TUNIFY;

(*occurs check*)
fun occurs v tye =
  let
    fun occ (Type (_, Ts)) = exists occ Ts
      | occ (TFree _) = false
      | occ (TVar (w, S)) =
          Term.eq_ix (v, w) orelse
            (case lookup tye (w, S) of
              NONE => false
            | SOME U => occ U);
  in occ end;

(*chase variable assignments; if devar returns a type var then it must be unassigned*)
fun devar tye (T as TVar v) =
      (case lookup tye v of
        SOME U => devar tye U
      | NONE => T)
  | devar _ T = T;

(*order-sorted unification*)
fun unify (TSig {classes = (_, classes), ...}) TU (tyenv, maxidx) =
  let
    val tyvar_count = Unsynchronized.ref maxidx;
    fun gen_tyvar S = TVar ((Name.aT, Unsynchronized.inc tyvar_count), S);

    fun mg_domain a S = Sorts.mg_domain classes a S
      handle Sorts.CLASS_ERROR _ => raise TUNIFY;

    fun meet (_, []) tye = tye
      | meet (TVar (xi, S'), S) tye =
          if Sorts.sort_le classes (S', S) then tye
          else Vartab.update_new
            (xi, (S', gen_tyvar (Sorts.inter_sort classes (S', S)))) tye
      | meet (TFree (_, S'), S) tye =
          if Sorts.sort_le classes (S', S) then tye
          else raise TUNIFY
      | meet (Type (a, Ts), S) tye = meets (Ts, mg_domain a S) tye
    and meets (T :: Ts, S :: Ss) tye = meets (Ts, Ss) (meet (devar tye T, S) tye)
      | meets _ tye = tye;

    fun unif (ty1, ty2) tye =
      (case (devar tye ty1, devar tye ty2) of
        (T as TVar (v, S1), U as TVar (w, S2)) =>
          if Term.eq_ix (v, w) then
            if S1 = S2 then tye else tvar_clash v S1 S2
          else if Sorts.sort_le classes (S1, S2) then
            Vartab.update_new (w, (S2, T)) tye
          else if Sorts.sort_le classes (S2, S1) then
            Vartab.update_new (v, (S1, U)) tye
          else
            let val S = gen_tyvar (Sorts.inter_sort classes (S1, S2)) in
              Vartab.update_new (v, (S1, S)) (Vartab.update_new (w, (S2, S)) tye)
            end
      | (TVar (v, S), T) =>
          if occurs v tye T then raise TUNIFY
          else meet (T, S) (Vartab.update_new (v, (S, T)) tye)
      | (T, TVar (v, S)) =>
          if occurs v tye T then raise TUNIFY
          else meet (T, S) (Vartab.update_new (v, (S, T)) tye)
      | (Type (a, Ts), Type (b, Us)) =>
          if a <> b then raise TUNIFY
          else unifs (Ts, Us) tye
      | (T, U) => if T = U then tye else raise TUNIFY)
    and unifs (T :: Ts, U :: Us) tye = unifs (Ts, Us) (unif (T, U) tye)
      | unifs _ tye = tye;
  in (unif TU tyenv, ! tyvar_count) end;

(*purely structural unification*)
fun raw_unify (ty1, ty2) tye =
  (case (devar tye ty1, devar tye ty2) of
    (T as TVar (v, S1), TVar (w, S2)) =>
      if Term.eq_ix (v, w) then
        if S1 = S2 then tye else tvar_clash v S1 S2
      else Vartab.update_new (w, (S2, T)) tye
  | (TVar (v, S), T) =>
      if occurs v tye T then raise TUNIFY
      else Vartab.update_new (v, (S, T)) tye
  | (T, TVar (v, S)) =>
      if occurs v tye T then raise TUNIFY
      else Vartab.update_new (v, (S, T)) tye
  | (Type (a, Ts), Type (b, Us)) =>
      if a <> b then raise TUNIFY
      else raw_unifys (Ts, Us) tye
  | (T, U) => if T = U then tye else raise TUNIFY)
and raw_unifys (T :: Ts, U :: Us) tye = raw_unifys (Ts, Us) (raw_unify (T, U) tye)
  | raw_unifys ([], []) tye = tye
  | raw_unifys _ _ = raise TUNIFY;

(*fast unification filter*)
fun could_unify (Type (a, Ts), Type (b, Us)) = a = b andalso could_unifys (Ts, Us)
  | could_unify (TFree (a, _), TFree (b, _)) = a = b
  | could_unify (TVar _, _) = true
  | could_unify (_, TVar _) = true
  | could_unify _ = false
and could_unifys (T :: Ts, U :: Us) = could_unify (T, U) andalso could_unifys (Ts, Us)
  | could_unifys ([], []) = true
  | could_unifys _ = false;

(*equality with respect to a type environment*)
fun unified tye =
  let
    fun unif (T, T') =
      (case (devar tye T, devar tye T') of
         (Type (s, Ts), Type (s', Ts')) => s = s' andalso unifs (Ts, Ts')
       | (U, U') => U = U')
    and unifs ([], []) = true
      | unifs (T :: Ts, T' :: Ts') = unif (T', T') andalso unifs (Ts, Ts')
      | unifs _ = false;
  in if Vartab.is_empty tye then op = else unif end;



(** extend and merge type signatures **)

(* classes *)

fun add_class context (c, cs) tsig =
  tsig |> map_tsig (fn ((space, classes), default, types) =>
    let
      val cs' = cert_sort tsig cs handle TYPE (msg, _, _) => error msg;
      val _ = Binding.check c;
      val (c', space') = space |> Name_Space.declare context true c;
      val classes' = classes |> Sorts.add_class context (c', cs');
    in ((space', classes'), default, types) end);

fun hide_class fully c = map_tsig (fn ((space, classes), default, types) =>
  ((Name_Space.hide fully c space, classes), default, types));


(* arities *)

fun add_arity context (t, Ss, S) tsig = tsig |> map_tsig (fn ((space, classes), default, types) =>
  let
    val _ =
      (case lookup_type tsig t of
        SOME (Logical_Type n) => if length Ss <> n then error (bad_nargs t) else ()
      | SOME _ => error ("Logical type constructor expected: " ^ quote t)
      | NONE => error (undecl_type t));
    val (Ss', S') = (map (cert_sort tsig) Ss, cert_sort tsig S)
      handle TYPE (msg, _, _) => error msg;
    val classes' = classes |> Sorts.add_arities context ((t, map (fn c' => (c', Ss')) S'));
  in ((space, classes'), default, types) end);


(* classrel *)

fun add_classrel context rel tsig =
  tsig |> map_tsig (fn ((space, classes), default, types) =>
    let
      val rel' = apply2 (cert_class tsig) rel
        handle TYPE (msg, _, _) => error msg;
      val classes' = classes |> Sorts.add_classrel context rel';
    in ((space, classes'), default, types) end);


(* default sort *)

fun set_defsort S tsig = tsig |> map_tsig (fn (classes, _, types) =>
  (classes, cert_sort tsig S handle TYPE (msg, _, _) => error msg, types));


(* types *)

local

fun new_decl context (c, decl) types =
  (Binding.check c; #2 (Name_Space.define context true (c, decl) types));

fun map_types f = map_tsig (fn (classes, default, types) =>
  let
    val types' = f types;
    val _ =
      not (Name_Space.defined types' "dummy") orelse
      Name_Space.intern (Name_Space.space_of_table types') "dummy" = "dummy" orelse
      error "Illegal declaration of dummy type";
  in (classes, default, types') end);

fun syntactic tsig (Type (c, Ts)) =
      (case lookup_type tsig c of SOME Nonterminal => true | _ => false)
        orelse exists (syntactic tsig) Ts
  | syntactic _ _ = false;

in

fun add_type context (c, n) =
  if n < 0 then error ("Bad type constructor declaration " ^ Binding.print c)
  else map_types (new_decl context (c, Logical_Type n));

fun add_abbrev context (a, vs, rhs) tsig = tsig |> map_types (fn types =>
  let
    fun err msg =
      cat_error msg ("The error(s) above occurred in type abbreviation " ^ Binding.print a);
    val rhs' = Term.strip_sortsT (no_tvars (certify_typ mode_syntax tsig rhs))
      handle TYPE (msg, _, _) => err msg;
    val _ =
      (case duplicates (op =) vs of
        [] => []
      | dups => err ("Duplicate variables on lhs: " ^ commas_quote dups));
    val _ =
      (case subtract (op =) vs (map #1 (Term.add_tfreesT rhs' [])) of
        [] => []
      | extras => err ("Extra variables on rhs: " ^ commas_quote extras));
  in types |> new_decl context (a, Abbreviation (vs, rhs', syntactic tsig rhs')) end);

fun add_nonterminal context = map_types o new_decl context o rpair Nonterminal;

end;

fun hide_type fully c = map_tsig (fn (classes, default, types) =>
  (classes, default, Name_Space.hide_table fully c types));


(* merge type signatures *)

fun merge_tsig context (tsig1, tsig2) =
  let
    val (TSig {classes = (space1, classes1), default = default1, types = types1,
      log_types = _}) = tsig1;
    val (TSig {classes = (space2, classes2), default = default2, types = types2,
      log_types = _}) = tsig2;

    val space' = Name_Space.merge (space1, space2);
    val classes' = Sorts.merge_algebra context (classes1, classes2);
    val default' = Sorts.inter_sort classes' (default1, default2);
    val types' = Name_Space.merge_tables (types1, types2);
  in build_tsig ((space', classes'), default', types') end;

end;
