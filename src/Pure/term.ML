(*  Title:      Pure/term.ML
    Author:     Lawrence C Paulson, Cambridge University Computer Laboratory
    Author:     Makarius

Simply typed lambda-calculus: types, terms, and basic operations.
*)

infix 9 $;
infixr 5 -->;
infixr --->;
infix aconv;

signature BASIC_TERM =
sig
  type indexname = string * int
  type class = string
  type sort = class list
  type arity = string * sort list * sort
  datatype typ =
    Type  of string * typ list |
    TFree of string * sort |
    TVar  of indexname * sort
  datatype term =
    Const of string * typ |
    Free of string * typ |
    Var of indexname * typ |
    Bound of int |
    Abs of string * typ * term |
    $ of term * term
  exception TYPE of string * typ list * term list
  exception TERM of string * term list
  val dummyS: sort
  val dummyT: typ
  val no_dummyT: typ -> typ
  val --> : typ * typ -> typ
  val ---> : typ list * typ -> typ
  val is_Type: typ -> bool
  val is_TFree: typ -> bool
  val is_TVar: typ -> bool
  val eq_Type_name: typ * typ -> bool
  val dest_Type: typ -> string * typ list
  val dest_Type_name: typ -> string
  val dest_Type_args: typ -> typ list
  val dest_TFree: typ -> string * sort
  val dest_TVar: typ -> indexname * sort
  val is_Bound: term -> bool
  val is_Const: term -> bool
  val is_Free: term -> bool
  val is_Var: term -> bool
  val eq_Const_name: term * term -> bool
  val dest_Const: term -> string * typ
  val dest_Const_name: term -> string
  val dest_Const_type: term -> typ
  val dest_Free: term -> string * typ
  val dest_Var: term -> indexname * typ
  val dest_comb: term -> term * term
  val domain_type: typ -> typ
  val range_type: typ -> typ
  val dest_funT: typ -> typ * typ
  val binder_types: typ -> typ list
  val body_type: typ -> typ
  val strip_type: typ -> typ list * typ
  val type_of1: typ list * term -> typ
  val type_of: term -> typ
  val fastype_of1: typ list * term -> typ
  val fastype_of: term -> typ
  val strip_abs: term -> (string * typ) list * term
  val strip_abs_body: term -> term
  val strip_abs_vars: term -> (string * typ) list
  val strip_qnt_body: string -> term -> term
  val strip_qnt_vars: string -> term -> (string * typ) list
  val list_comb: term * term list -> term
  val strip_comb: term -> term * term list
  val head_of: term -> term
  val size_of_term: term -> int
  val size_of_typ: typ -> int
  val map_atyps: typ Same.operation -> typ -> typ
  val map_aterms: term Same.operation -> term -> term
  val map_types: typ Same.operation -> term -> term
  val map_type_tvar: (indexname * sort, typ) Same.function -> typ -> typ
  val map_type_tfree: (string * sort, typ) Same.function -> typ -> typ
  val fold_atyps: (typ -> 'a -> 'a) -> typ -> 'a -> 'a
  val fold_atyps_sorts: (typ * sort -> 'a -> 'a) -> typ -> 'a -> 'a
  val fold_aterms: (term -> 'a -> 'a) -> term -> 'a -> 'a
  val fold_term_types: (term -> typ -> 'a -> 'a) -> term -> 'a -> 'a
  val fold_types: (typ -> 'a -> 'a) -> term -> 'a -> 'a
  val burrow_types: (typ list -> typ list) -> term list -> term list
  val aconv: term * term -> bool
  val propT: typ
  val strip_all_body: term -> term
  val strip_all_vars: term -> (string * typ) list
  val incr_bv_same: int -> int -> term Same.operation
  val incr_bv: int -> int -> term -> term
  val incr_boundvars: int -> term -> term
  val add_loose_bnos: term * int * int list -> int list
  val loose_bnos: term -> int list
  val loose_bvar: term * int -> bool
  val loose_bvar1: term * int -> bool
  val subst_bounds_same: term list -> int -> term Same.operation
  val subst_bounds: term list * term -> term
  val subst_bound: term * term -> term
  val betapply: term * term -> term
  val betapplys: term * term list -> term
  val subst_free: (term * term) list -> term -> term
  val abstract_over: term * term -> term
  val lambda: term -> term -> term
  val absfree: string * typ -> term -> term
  val absdummy: typ -> term -> term
  val subst_atomic: (term * term) list -> term -> term
  val typ_subst_atomic: (typ * typ) list -> typ -> typ
  val subst_atomic_types: (typ * typ) list -> term -> term
  val typ_subst_TVars: (indexname * typ) list -> typ -> typ
  val subst_TVars: (indexname * typ) list -> term -> term
  val subst_Vars: (indexname * term) list -> term -> term
  val subst_vars: (indexname * typ) list * (indexname * term) list -> term -> term
  val is_first_order: string list -> term -> bool
  val maxidx_of_typ: typ -> int
  val maxidx_of_typs: typ list -> int
  val maxidx_of_term: term -> int
  val fold_subtypes: (typ -> 'a -> 'a) -> typ -> 'a -> 'a
  val exists_subtype: (typ -> bool) -> typ -> bool
  val exists_type: (typ -> bool) -> term -> bool
  val exists_subterm: (term -> bool) -> term -> bool
  val exists_Const: (string * typ -> bool) -> term -> bool
  val is_schematic: term -> bool
end;

signature TERM =
sig
  include BASIC_TERM
  val map_atyps_same: typ Same.operation -> typ Same.operation
  val map_aterms_same: term Same.operation -> term Same.operation
  val map_types_same: typ Same.operation -> term Same.operation
  val aT: sort -> typ
  val itselfT: typ -> typ
  val a_itselfT: typ
  val argument_type_of: term -> int -> typ
  val abs: string * typ -> term -> term
  val args_of: term -> term list
  val add_tvar_namesT: typ -> indexname list -> indexname list
  val add_tvar_names: term -> indexname list -> indexname list
  val add_tvarsT: typ -> (indexname * sort) list -> (indexname * sort) list
  val add_tvars: term -> (indexname * sort) list -> (indexname * sort) list
  val add_var_names: term -> indexname list -> indexname list
  val add_vars: term -> (indexname * typ) list -> (indexname * typ) list
  val add_tfree_namesT: typ -> string list -> string list
  val add_tfree_names: term -> string list -> string list
  val add_tfreesT: typ -> (string * sort) list -> (string * sort) list
  val add_tfrees: term -> (string * sort) list -> (string * sort) list
  val add_free_names: term -> string list -> string list
  val add_frees: term -> (string * typ) list -> (string * typ) list
  val add_const_names: term -> string list -> string list
  val add_consts: term -> (string * typ) list -> (string * typ) list
  val declare_tfree_namesT: typ -> Name.context -> Name.context
  val declare_tfree_names: term -> Name.context -> Name.context
  val declare_tvar_namesT: (int -> bool) -> typ -> Name.context -> Name.context
  val declare_tvar_names: (int -> bool) -> term -> Name.context -> Name.context
  val declare_free_names: term -> Name.context -> Name.context
  val declare_var_names: (int -> bool) -> term -> Name.context -> Name.context
  val variant_bounds: term -> (string * 'a) list -> (string * 'a) list
  val hidden_polymorphism: term -> (indexname * sort) list
  val smash_sortsT_same: sort -> typ Same.operation
  val smash_sortsT: sort -> typ -> typ
  val smash_sorts: sort -> term -> term
  val strip_sortsT_same: typ Same.operation
  val strip_sortsT: typ -> typ
  val strip_sorts: term -> term
  val eq_ix: indexname * indexname -> bool
  val eq_tvar: (indexname * sort) * (indexname * sort) -> bool
  val eq_var: (indexname * typ) * (indexname * typ) -> bool
  val aconv_untyped: term * term -> bool
  val could_unify: term * term -> bool
  val strip_abs_eta: int -> term -> (string * typ) list * term
  val match_bvars: (term * term) -> (string * string) list -> (string * string) list
  val map_abs_vars: (string -> string) -> term -> term
  val rename_abs: term -> term -> term -> term option
  val is_open: term -> bool
  val is_dependent: term -> bool
  val term_name: term -> string
  val dependent_lambda_name: string * term -> term -> term
  val lambda_name: string * term -> term -> term
  val close_schematic_term: term -> term
  val maxidx_typ: typ -> int -> int
  val maxidx_typs: typ list -> int -> int
  val maxidx_term: term -> int -> int
  val could_beta_contract: term -> bool
  val could_eta_contract: term -> bool
  val used_free: string -> term -> bool
  exception USED_FREE of string * term
  val dest_abs_fresh: string -> term -> (string * typ) * term
  val dest_abs_global: term -> (string * typ) * term
  val dummy_pattern: typ -> term
  val dummy: term
  val dummy_prop: term
  val is_dummy_pattern: term -> bool
  val free_dummy_patterns: term -> Name.context -> term * Name.context
  val no_dummy_patterns: term -> term
  val replace_dummy_patterns: term -> int -> term * int
  val show_dummy_patterns: term -> term
  val string_of_vname: indexname -> string
  val string_of_vname': indexname -> string
end;

structure Term: TERM =
struct

(*Indexnames can be quickly renamed by adding an offset to the integer part,
  for resolution.*)
type indexname = string * int;

(*Types are classified by sorts.*)
type class = string;
type sort  = class list;
type arity = string * sort list * sort;

(*The sorts attached to TFrees and TVars specify the sort of that variable.*)
datatype typ = Type  of string * typ list
             | TFree of string * sort
             | TVar  of indexname * sort;

(*Terms.  Bound variables are indicated by depth number.
  Free variables, (scheme) variables and constants have names.
  An term is "closed" if every bound variable of level "lev"
  is enclosed by at least "lev" abstractions.

  It is possible to create meaningless terms containing loose bound vars
  or type mismatches.  But such terms are not allowed in rules. *)

datatype term =
    Const of string * typ
  | Free  of string * typ
  | Var   of indexname * typ
  | Bound of int
  | Abs   of string*typ*term
  | op $  of term*term;

(*Errors involving type mismatches*)
exception TYPE of string * typ list * term list;

(*Errors errors involving terms*)
exception TERM of string * term list;

(*Note variable naming conventions!
    a,b,c: string
    f,g,h: functions (including terms of function type)
    i,j,m,n: int
    t,u: term
    v,w: indexnames
    x,y: any
    A,B,C: term (denoting formulae)
    T,U: typ
*)


(** Types **)

(*dummies for type-inference etc.*)
val dummyS = [""];
val dummyT = Type ("dummy", []);

fun no_dummyT typ =
  let
    fun check (T as Type ("dummy", _)) =
          raise TYPE ("Illegal occurrence of '_' dummy type", [T], [])
      | check (Type (_, Ts)) = List.app check Ts
      | check _ = ();
  in check typ; typ end;

fun S --> T = Type("fun",[S,T]);

(*handy for multiple args: [T1,...,Tn]--->T  gives  T1-->(T2--> ... -->T)*)
val op ---> = Library.foldr (op -->);


(** Discriminators **)

fun is_Type (Type _) = true
  | is_Type _ = false;

fun is_TFree (TFree _) = true
  | is_TFree _ = false;

fun is_TVar (TVar _) = true
  | is_TVar _ = false;

fun eq_Type_name (Type (a, _), Type (b, _)) = a = b
  | eq_Type_name _ = false;


(** Destructors **)

fun dest_Type (Type x) = x
  | dest_Type T = raise TYPE ("dest_Type", [T], []);

val dest_Type_name = #1 o dest_Type;
val dest_Type_args = #2 o dest_Type;

fun dest_TFree (TFree x) = x
  | dest_TFree T = raise TYPE ("dest_TFree", [T], []);

fun dest_TVar (TVar x) = x
  | dest_TVar T = raise TYPE ("dest_TVar", [T], []);


(** Discriminators **)

fun is_Bound (Bound _) = true
  | is_Bound _         = false;

fun is_Const (Const _) = true
  | is_Const _ = false;

fun is_Free (Free _) = true
  | is_Free _ = false;

fun is_Var (Var _) = true
  | is_Var _ = false;

fun eq_Const_name (Const (a, _), Const (b, _)) = a = b
  | eq_Const_name _ = false;


(** Destructors **)

fun dest_Const (Const x) =  x
  | dest_Const t = raise TERM("dest_Const", [t]);

val dest_Const_name = #1 o dest_Const;
val dest_Const_type = #2 o dest_Const;

fun dest_Free (Free x) =  x
  | dest_Free t = raise TERM("dest_Free", [t]);

fun dest_Var (Var x) =  x
  | dest_Var t = raise TERM("dest_Var", [t]);

fun dest_comb (t1 $ t2) = (t1, t2)
  | dest_comb t = raise TERM("dest_comb", [t]);


fun domain_type (Type ("fun", [T, _])) = T;

fun range_type (Type ("fun", [_, U])) = U;

fun dest_funT (Type ("fun", [T, U])) = (T, U)
  | dest_funT T = raise TYPE ("dest_funT", [T], []);


(*maps  [T1,...,Tn]--->T  to the list  [T1,T2,...,Tn]*)
fun binder_types (Type ("fun", [T, U])) = T :: binder_types U
  | binder_types _ = [];

(*maps  [T1,...,Tn]--->T  to T*)
fun body_type (Type ("fun", [_, U])) = body_type U
  | body_type T = T;

(*maps  [T1,...,Tn]--->T  to   ([T1,T2,...,Tn], T)*)
fun strip_type T = (binder_types T, body_type T);


(*Compute the type of the term, checking that combinations are well-typed
  Ts = [T0,T1,...] holds types of bound variables 0, 1, ...*)
fun type_of1 (Ts, Const (_,T)) = T
  | type_of1 (Ts, Free  (_,T)) = T
  | type_of1 (Ts, Bound i) = (nth Ts i
        handle General.Subscript => raise TYPE("type_of: bound variable", [], [Bound i]))
  | type_of1 (Ts, Var (_,T)) = T
  | type_of1 (Ts, Abs (_,T,body)) = T --> type_of1(T::Ts, body)
  | type_of1 (Ts, f$u) =
      let val U = type_of1(Ts,u)
          and T = type_of1(Ts,f)
      in case T of
            Type("fun",[T1,T2]) =>
              if T1=U then T2  else raise TYPE
                    ("type_of: type mismatch in application", [T1,U], [f$u])
          | _ => raise TYPE
                    ("type_of: function type is expected in application",
                     [T,U], [f$u])
      end;

fun type_of t : typ = type_of1 ([],t);

(*Determine the type of a term, with minimal checking*)
local

fun fastype_of_term Ts (Abs (_, T, t)) = T --> fastype_of_term (T :: Ts) t
  | fastype_of_term Ts (t $ _) = range_type_of Ts t
  | fastype_of_term Ts a = fastype_of_atom Ts a
and fastype_of_atom _ (Const (_, T)) = T
  | fastype_of_atom _ (Free (_, T)) = T
  | fastype_of_atom _ (Var (_, T)) = T
  | fastype_of_atom Ts (Bound i) = fastype_of_bound Ts i
and fastype_of_bound (T :: Ts) i = if i = 0 then T else fastype_of_bound Ts (i - 1)
  | fastype_of_bound [] i = raise TERM ("fastype_of: Bound", [Bound i])
and range_type_of Ts (Abs (_, T, u)) = fastype_of_term (T :: Ts) u
  | range_type_of Ts (t $ u) = range_type_ofT (t $ u) (range_type_of Ts t)
  | range_type_of Ts a = range_type_ofT a (fastype_of_atom Ts a)
and range_type_ofT _ (Type ("fun", [_, T])) = T
  | range_type_ofT t _ = raise TERM ("fastype_of: expected function type", [t]);

in

val fastype_of1 = uncurry fastype_of_term;
val fastype_of = fastype_of_term [];

end;

(*Determine the argument type of a function*)
fun argument_type_of tm k =
  let
    fun argT i (Type ("fun", [T, U])) = if i = 0 then T else argT (i - 1) U
      | argT _ T = raise TYPE ("argument_type_of", [T], []);

    fun arg 0 _ (Abs (_, T, _)) = T
      | arg i Ts (Abs (_, T, t)) = arg (i - 1) (T :: Ts) t
      | arg i Ts (t $ _) = arg (i + 1) Ts t
      | arg i Ts a = argT i (fastype_of1 (Ts, a));
  in arg k [] tm end;


fun abs (x, T) t = Abs (x, T, t);

fun strip_abs (Abs (a, T, t)) = strip_abs t |>> cons (a, T)
  | strip_abs t = ([], t);

fun strip_abs_body (Abs (_, _, t)) = strip_abs_body t
  | strip_abs_body t = t;

fun strip_abs_vars (Abs (a, T, t)) = strip_abs_vars t |> cons (a, T)
  | strip_abs_vars _ = [];


fun strip_qnt_body qnt =
  let
    fun strip (tm as Const (c, _) $ Abs (_, _, t)) = if c = qnt then strip t else tm
      | strip t = t;
  in strip end;

fun strip_qnt_vars qnt =
  let
    fun strip (Const (c, _) $ Abs (a, T, t)) = if c = qnt then (a, T) :: strip t else []
      | strip _ = [];
  in strip end;


(*maps   (f, [t1,...,tn])  to  f(t1,...,tn)*)
val list_comb : term * term list -> term = Library.foldl (op $);


(*maps   f(t1,...,tn)  to  (f, [t1,...,tn]) ; naturally tail-recursive*)
fun strip_comb tm =
  let
    fun strip (f $ t, ts) = strip (f, t :: ts)
      | strip res = res;
  in strip (tm, []) end;

val args_of =
  let
    fun args (f $ t) ts = args f (t :: ts)
      | args _ ts = ts;
  in build o args end;

(*maps   f(t1,...,tn)  to  f , which is never a combination*)
fun head_of (f $ _) = head_of f
  | head_of t = t;

(*number of atoms and abstractions in a term*)
fun size_of_term tm =
  let
    fun add_size (t $ u) n = add_size t (add_size u n)
      | add_size (Abs (_ ,_, t)) n = add_size t (n + 1)
      | add_size _ n = n + 1;
  in add_size tm 0 end;

(*number of atoms and constructors in a type*)
fun size_of_typ ty =
  let
    fun add_size (Type (_, tys)) n = fold add_size tys (n + 1)
      | add_size _ n = n + 1;
  in add_size ty 0 end;


(* map types and terms *)

fun map_atyps_same f =
  let
    fun typ (Type (a, Ts)) = Type (a, Same.map typ Ts)
      | typ T = f T;
  in typ end;

fun map_aterms_same f =
  let
    fun term (Abs (x, T, t)) = Abs (x, T, term t)
      | term (t $ u) =
          (term t $ Same.commit term u
            handle Same.SAME => t $ term u)
      | term a = f a;
  in term end;

fun map_types_same f =
  let
    fun term (Const (c, T)) = Const (c, f T)
      | term (Free (x, T)) = Free (x, f T)
      | term (Var (xi, T)) = Var (xi, f T)
      | term (Bound _) = raise Same.SAME
      | term (Abs (x, T, t)) =
          (Abs (x, f T, Same.commit term t)
            handle Same.SAME => Abs (x, T, term t))
      | term (t $ u) =
          (term t $ Same.commit term u
            handle Same.SAME => t $ term u);
  in term end;

val map_atyps = Same.commit o map_atyps_same;
val map_aterms = Same.commit o map_aterms_same;
val map_types = Same.commit o map_types_same;

fun map_type_tvar f = map_atyps (fn TVar x => f x | _ => raise Same.SAME);
fun map_type_tfree f = map_atyps (fn TFree x => f x | _ => raise Same.SAME);


(* fold types and terms *)

fun fold_atyps f (Type (_, Ts)) = fold (fold_atyps f) Ts
  | fold_atyps f T = f T;

fun fold_atyps_sorts f =
  fold_atyps (fn T as TFree (_, S) => f (T, S) | T as TVar (_, S) => f (T, S));

fun fold_aterms f (t $ u) = fold_aterms f t #> fold_aterms f u
  | fold_aterms f (Abs (_, _, t)) = fold_aterms f t
  | fold_aterms f a = f a;

fun fold_term_types f (t as Const (_, T)) = f t T
  | fold_term_types f (t as Free (_, T)) = f t T
  | fold_term_types f (t as Var (_, T)) = f t T
  | fold_term_types f (Bound _) = I
  | fold_term_types f (t as Abs (_, T, b)) = f t T #> fold_term_types f b
  | fold_term_types f (t $ u) = fold_term_types f t #> fold_term_types f u;

fun fold_types f = fold_term_types (K f);

fun replace_types (Const (c, _)) (T :: Ts) = (Const (c, T), Ts)
  | replace_types (Free (x, _)) (T :: Ts) = (Free (x, T), Ts)
  | replace_types (Var (xi, _)) (T :: Ts) = (Var (xi, T), Ts)
  | replace_types (Bound i) Ts = (Bound i, Ts)
  | replace_types (Abs (x, _, b)) (T :: Ts) =
      let val (b', Ts') = replace_types b Ts
      in (Abs (x, T, b'), Ts') end
  | replace_types (t $ u) Ts =
      let
        val (t', Ts') = replace_types t Ts;
        val (u', Ts'') = replace_types u Ts';
      in (t' $ u', Ts'') end;

fun burrow_types f ts =
  let
    val Ts = rev ((fold o fold_types) cons ts []);
    val Ts' = f Ts;
    val (ts', []) = fold_map replace_types ts Ts';
  in ts' end;

(*collect variables*)
val add_tvar_namesT = fold_atyps (fn TVar (xi, _) => insert (op =) xi | _ => I);
val add_tvar_names = fold_types add_tvar_namesT;
val add_tvarsT = fold_atyps (fn TVar v => insert (op =) v | _ => I);
val add_tvars = fold_types add_tvarsT;
val add_var_names = fold_aterms (fn Var (xi, _) => insert (op =) xi | _ => I);
val add_vars = fold_aterms (fn Var v => insert (op =) v | _ => I);
val add_tfree_namesT = fold_atyps (fn TFree (a, _) => insert (op =) a | _ => I);
val add_tfree_names = fold_types add_tfree_namesT;
val add_tfreesT = fold_atyps (fn TFree v => insert (op =) v | _ => I);
val add_tfrees = fold_types add_tfreesT;
val add_free_names = fold_aterms (fn Free (x, _) => insert (op =) x | _ => I);
val add_frees = fold_aterms (fn Free v => insert (op =) v | _ => I);
val add_const_names = fold_aterms (fn Const (c, _) => insert (op =) c | _ => I);
val add_consts = fold_aterms (fn Const c => insert (op =) c | _ => I);

(*renaming variables*)
val declare_tfree_namesT = fold_atyps (fn TFree (a, _) => Name.declare a | _ => I);
val declare_tfree_names = fold_types declare_tfree_namesT;
fun declare_tvar_namesT pred = fold_atyps (fn TVar ((a, i), _) => pred i ? Name.declare a | _ => I);
val declare_tvar_names = fold_types o declare_tvar_namesT;
val declare_free_names = fold_aterms (fn Free (x, _) => Name.declare x | _ => I);
fun declare_var_names pred = fold_aterms (fn Var ((x, i), _) => pred i ? Name.declare x | _ => I);
fun variant_bounds t = Name.variant_names_build (declare_free_names t);

(*extra type variables in a term, not covered by its type*)
fun hidden_polymorphism t =
  let
    val T = fastype_of t;
    val tvarsT = add_tvarsT T [];
    val extra_tvars = fold_types (fold_atyps
      (fn TVar v => if member (op =) tvarsT v then I else insert (op =) v | _ => I)) t [];
  in extra_tvars end;


(* sorts *)

fun smash_sortsT_same S' =
  map_atyps_same
    (fn TFree (x, S) => if S = S' then raise Same.SAME else TFree (x, S')
      | TVar (xi, S) => if S = S' then raise Same.SAME else TVar (xi, S'));

val smash_sortsT = Same.commit o smash_sortsT_same;
val smash_sorts = map_types o smash_sortsT_same;

val strip_sortsT_same = smash_sortsT_same [];
val strip_sortsT = Same.commit strip_sortsT_same;
val strip_sorts = map_types strip_sortsT_same;



(** Comparing terms **)

(* variables *)

fun eq_ix ((x, i): indexname, (y, j)) = i = j andalso x = y;

fun eq_tvar ((xi, S: sort), (xi', S')) = eq_ix (xi, xi') andalso S = S';
fun eq_var ((xi, T: typ), (xi', T')) = eq_ix (xi, xi') andalso T = T';


(* alpha equivalence *)

fun tm1 aconv tm2 =
  pointer_eq (tm1, tm2) orelse
    (case (tm1, tm2) of
      (t1 $ u1, t2 $ u2) => t1 aconv t2 andalso u1 aconv u2
    | (Abs (_, T1, t1), Abs (_, T2, t2)) => t1 aconv t2 andalso T1 = T2
    | (a1, a2) => a1 = a2);

fun aconv_untyped (tm1, tm2) =
  pointer_eq (tm1, tm2) orelse
    (case (tm1, tm2) of
      (t1 $ u1, t2 $ u2) => aconv_untyped (t1, t2) andalso aconv_untyped (u1, u2)
    | (Abs (_, _, t1), Abs (_, _, t2)) => aconv_untyped (t1, t2)
    | (Const (a, _), Const (b, _)) => a = b
    | (Free (x, _), Free (y, _)) => x = y
    | (Var (xi, _), Var (yj, _)) => xi = yj
    | (Bound i, Bound j) => i = j
    | _ => false);


(*A fast unification filter: true unless the two terms cannot be unified.
  Terms must be NORMAL.  Treats all Vars as distinct. *)
fun could_unify (t, u) =
  let
    fun matchrands (f $ t) (g $ u) = could_unify (t, u) andalso matchrands f g
      | matchrands _ _ = true;
  in
    case (head_of t, head_of u) of
      (_, Var _) => true
    | (Var _, _) => true
    | (Const (a, _), Const (b, _)) => a = b andalso matchrands t u
    | (Free (a, _), Free (b, _)) => a = b andalso matchrands t u
    | (Bound i, Bound j) => i = j andalso matchrands t u
    | (Abs _, _) => true   (*because of possible eta equality*)
    | (_, Abs _) => true
    | _ => false
  end;



(** Connectives of higher order logic **)

fun aT S = TFree (Name.aT, S);

fun itselfT ty = Type ("itself", [ty]);
val a_itselfT = itselfT (TFree (Name.aT, []));

val propT : typ = Type ("prop",[]);

(*maps  \<And>x1...xn. t   to   t*)
fun strip_all_body (Const ("Pure.all", _) $ Abs (_, _, t)) = strip_all_body t
  | strip_all_body t = t;

(*maps  \<And>x1...xn. t   to   [x1, ..., xn]*)
fun strip_all_vars (Const ("Pure.all", _) $ Abs (a, T, t)) = (a, T) :: strip_all_vars t
  | strip_all_vars t = [];

(*increments a term's non-local bound variables
  required when moving a term within abstractions
     inc is  increment for bound variables
     lev is  level at which a bound variable is considered 'loose'*)
fun incr_bv_same inc =
  if inc = 0 then fn _ => Same.same
  else
    let
      fun term lev (Bound i) =
            if i >= lev then Bound (i + inc) else raise Same.SAME
        | term lev (Abs (a, T, t)) = Abs (a, T, term (lev + 1) t)
        | term lev (t $ u) =
            (term lev t $ Same.commit (term lev) u
              handle Same.SAME => t $ term lev u)
        | term _ _ = raise Same.SAME;
    in term end;

fun incr_bv inc lev = Same.commit (incr_bv_same inc lev);

fun incr_boundvars inc = incr_bv inc 0;

(*Scan a pair of terms; while they are similar, accumulate corresponding bound vars*)
fun match_bvs (Abs (x, _, s), Abs (y, _, t)) =
      (x <> "" andalso y <> "") ? cons (x, y) #> match_bvs (s, t)
  | match_bvs (f $ s, g $ t) = match_bvs (s, t) #> match_bvs (f, g)
  | match_bvs _ = I;

(* strip abstractions created by parameters *)
val match_bvars = apply2 strip_abs_body #> match_bvs;

fun map_abs_vars_same rename =
  let
    fun term (Abs (x, T, t)) =
          let val y = rename x
          in if x = y then Abs (x, T, term t) else Abs (y, T, Same.commit term t) end
      | term (t $ u) = (term t $ Same.commit term u handle Same.SAME => t $ term u)
      | term _ = raise Same.SAME;
  in term end;

fun map_abs_vars rename = Same.commit (map_abs_vars_same rename);

fun rename_abs pat obj t =
  let
    val renaming = Symtab.make_distinct (match_bvs (pat, obj) []);
    fun rename x = perhaps (Symtab.lookup renaming) x;
  in if Symtab.forall (op =) renaming then NONE else Same.catch (map_abs_vars_same rename) t end;

(*Accumulate all 'loose' bound vars referring to level 'lev' or beyond.
   (Bound 0) is loose at level 0 *)
fun add_loose_bnos (Bound i, lev, js) =
        if i<lev then js else insert (op =) (i - lev) js
  | add_loose_bnos (Abs (_,_,t), lev, js) = add_loose_bnos (t, lev+1, js)
  | add_loose_bnos (f$t, lev, js) =
        add_loose_bnos (f, lev, add_loose_bnos (t, lev, js))
  | add_loose_bnos (_, _, js) = js;

fun loose_bnos t = add_loose_bnos (t, 0, []);

(* loose_bvar(t,k) iff t contains a 'loose' bound variable referring to
   level k or beyond. *)
fun loose_bvar(Bound i,k) = i >= k
  | loose_bvar(f$t, k) = loose_bvar(f,k) orelse loose_bvar(t,k)
  | loose_bvar(Abs(_,_,t),k) = loose_bvar(t,k+1)
  | loose_bvar _ = false;

fun loose_bvar1(Bound i,k) = i = k
  | loose_bvar1(f$t, k) = loose_bvar1(f,k) orelse loose_bvar1(t,k)
  | loose_bvar1(Abs(_,_,t),k) = loose_bvar1(t,k+1)
  | loose_bvar1 _ = false;

fun is_open t = loose_bvar (t, 0);
fun is_dependent t = loose_bvar1 (t, 0);

(*Substitute arguments for loose bound variables.
  Beta-reduction of arg(n-1)...arg0 into t replacing (Bound i) with (argi).
  Note that for ((\<lambda>x y. c) a b), the bound vars in c are x=1 and y=0
        and the appropriate call is  subst_bounds([b,a], c) .
  Loose bound variables >=n are reduced by "n" to
     compensate for the disappearance of lambdas.
*)
fun subst_bounds_same args =
  if null args then fn _ => Same.same
  else
    let
      val n = length args;
      fun term lev (Bound i) =
            if i < lev then raise Same.SAME   (*var is locally bound*)
            else if i - lev < n then incr_boundvars lev (nth args (i - lev))
            else Bound (i - n)  (*loose: change it*)
        | term lev (Abs (a, T, t)) = Abs (a, T, term (lev + 1) t)
        | term lev (t $ u) =
            (term lev t $ Same.commit (term lev) u
              handle Same.SAME => t $ term lev u)
        | term _ _ = raise Same.SAME;
    in term end;

fun subst_bounds (args: term list, body) : term =
  if null args then body else Same.commit (subst_bounds_same args 0) body;

(*Special case: one argument*)
fun subst_bound (arg, body) : term =
  let
    fun term lev (Bound i) =
          if i < lev then raise Same.SAME   (*var is locally bound*)
          else if i = lev then incr_boundvars lev arg
          else Bound (i - 1)   (*loose: change it*)
      | term lev (Abs (a, T, t)) = Abs (a, T, term (lev + 1) t)
      | term lev (t $ u) =
          (term lev t $ Same.commit (term lev) u
            handle Same.SAME => t $ term lev u)
      | term _ _ = raise Same.SAME;
  in Same.commit (term 0) body end;

(*beta-reduce if possible, else form application*)
fun betapply (Abs(_,_,t), u) = subst_bound (u,t)
  | betapply (f,u) = f$u;

val betapplys = Library.foldl betapply;


(*unfolding abstractions with substitution
  of bound variables and implicit eta-expansion*)
fun strip_abs_eta k t =
  let
    val used = Name.build_context (fold_aterms declare_free_names t);
    fun strip_abs t (0, used) = (([], t), (0, used))
      | strip_abs (Abs (v, T, t)) (k, used) =
          let
            val (v', used') = Name.variant v used;
            val t' = subst_bound (Free (v', T), t);
            val ((vs, t''), (k', used'')) = strip_abs t' (k - 1, used');
          in (((v', T) :: vs, t''), (k', used'')) end
      | strip_abs t (k, used) = (([], t), (k, used));
    fun expand_eta [] t _ = ([], t)
      | expand_eta (T::Ts) t used =
          let
            val (v, used') = Name.variant "" used;
            val (vs, t') = expand_eta Ts (t $ Free (v, T)) used';
          in ((v, T) :: vs, t') end;
    val ((vs1, t'), (k', used')) = strip_abs t (k, used);
    val Ts = fst (chop k' (binder_types (fastype_of t')));
    val (vs2, t'') = expand_eta Ts t' used';
  in (vs1 @ vs2, t'') end;


(*Substitute new for free occurrences of old in a term*)
fun subst_free [] = I
  | subst_free pairs =
      let fun substf u =
            case AList.lookup (op aconv) pairs u of
                SOME u' => u'
              | NONE => (case u of Abs(a,T,t) => Abs(a, T, substf t)
                                 | t$u' => substf t $ substf u'
                                 | _ => u)
      in  substf  end;

(*Abstraction of the term "body" over its occurrences of v,
    which must contain no loose bound variables.
  The resulting term is ready to become the body of an Abs.*)
fun abstract_over (v, body) =
  let
    fun term lev tm =
      if v aconv tm then Bound lev
      else
        (case tm of
          Abs (a, T, t) => Abs (a, T, term (lev + 1) t)
        | t $ u =>
            (term lev t $ Same.commit (term lev) u
              handle Same.SAME => t $ term lev u)
        | _ => raise Same.SAME);
  in Same.commit (term 0) body end;

fun term_name (Const (x, _)) = Long_Name.base_name x
  | term_name (Free (x, _)) = x
  | term_name (Var ((x, _), _)) = x
  | term_name _ = Name.uu;

fun dependent_lambda_name (x, v) t =
  let val t' = abstract_over (v, t)
  in if is_dependent t' then Abs (if x = "" then term_name v else x, fastype_of v, t') else t end;

fun lambda_name (x, v) t =
  Abs (if x = "" then term_name v else x, fastype_of v, abstract_over (v, t));

fun lambda v t = lambda_name ("", v) t;

fun absfree (a, T) body = Abs (a, T, abstract_over (Free (a, T), body));
fun absdummy T body = Abs (Name.uu_, T, body);

(*Replace the ATOMIC term ti by ui;    inst = [(t1,u1), ..., (tn,un)].
  A simultaneous substitution:  [ (a,b), (b,a) ] swaps a and b.  *)
fun subst_atomic [] tm = tm
  | subst_atomic inst tm =
      let
        fun subst (Abs (a, T, body)) = Abs (a, T, subst body)
          | subst (t $ u) = subst t $ subst u
          | subst t = the_default t (AList.lookup (op aconv) inst t);
      in subst tm end;

(*Replace the ATOMIC type Ti by Ui;    inst = [(T1,U1), ..., (Tn,Un)].*)
fun typ_subst_atomic [] ty = ty
  | typ_subst_atomic inst ty =
      let
        fun subst (Type (a, Ts)) = Type (a, map subst Ts)
          | subst T = the_default T (AList.lookup (op = : typ * typ -> bool) inst T);
      in subst ty end;

fun subst_atomic_types [] tm = tm
  | subst_atomic_types inst tm = map_types (typ_subst_atomic inst) tm;

fun typ_subst_TVars [] ty = ty
  | typ_subst_TVars inst ty =
      let
        fun subst (Type (a, Ts)) = Type (a, map subst Ts)
          | subst (T as TVar (xi, _)) = the_default T (AList.lookup (op =) inst xi)
          | subst T = T;
      in subst ty end;

fun subst_TVars [] tm = tm
  | subst_TVars inst tm = map_types (typ_subst_TVars inst) tm;

fun subst_Vars [] tm = tm
  | subst_Vars inst tm =
      let
        fun subst (t as Var (xi, _)) = the_default t (AList.lookup (op =) inst xi)
          | subst (Abs (a, T, t)) = Abs (a, T, subst t)
          | subst (t $ u) = subst t $ subst u
          | subst t = t;
      in subst tm end;

fun subst_vars ([], []) tm = tm
  | subst_vars ([], inst) tm = subst_Vars inst tm
  | subst_vars (instT, inst) tm =
      let
        fun subst (Const (a, T)) = Const (a, typ_subst_TVars instT T)
          | subst (Free (a, T)) = Free (a, typ_subst_TVars instT T)
          | subst (Var (xi, T)) =
              (case AList.lookup (op =) inst xi of
                NONE => Var (xi, typ_subst_TVars instT T)
              | SOME t => t)
          | subst (t as Bound _) = t
          | subst (Abs (a, T, t)) = Abs (a, typ_subst_TVars instT T, subst t)
          | subst (t $ u) = subst t $ subst u;
      in subst tm end;

fun close_schematic_term t =
  let
    val extra_types = map (fn v => Const ("Pure.type", itselfT (TVar v))) (hidden_polymorphism t);
    val extra_terms = map Var (add_vars t []);
  in fold lambda (extra_terms @ extra_types) t end;



(** Identifying first-order terms **)

(*Differs from proofterm/is_fun in its treatment of TVar*)
fun is_funtype (Type ("fun", [_, _])) = true
  | is_funtype _ = false;

(*Argument Ts is a reverse list of binder types, needed if term t contains Bound vars*)
fun has_not_funtype Ts t = not (is_funtype (fastype_of1 (Ts, t)));

(*First order means in all terms of the form f(t1,...,tn) no argument has a
  function type. The supplied quantifiers are excluded: their argument always
  has a function type through a recursive call into its body.*)
fun is_first_order quants =
  let fun first_order1 Ts (Abs (_,T,body)) = first_order1 (T::Ts) body
        | first_order1 Ts (Const(q,_) $ Abs(a,T,body)) =
            member (op =) quants q  andalso   (*it is a known quantifier*)
            not (is_funtype T)   andalso first_order1 (T::Ts) body
        | first_order1 Ts t =
            case strip_comb t of
                 (Var _, ts) => forall (first_order1 Ts andf has_not_funtype Ts) ts
               | (Free _, ts) => forall (first_order1 Ts andf has_not_funtype Ts) ts
               | (Const _, ts) => forall (first_order1 Ts andf has_not_funtype Ts) ts
               | (Bound _, ts) => forall (first_order1 Ts andf has_not_funtype Ts) ts
               | (Abs _, ts) => false (*not in beta-normal form*)
               | _ => error "first_order: unexpected case"
    in  first_order1 []  end;


(* maximum index of typs and terms *)

fun maxidx_typ (TVar ((_, j), _)) i = Int.max (i, j)
  | maxidx_typ (Type (_, Ts)) i = maxidx_typs Ts i
  | maxidx_typ (TFree _) i = i
and maxidx_typs [] i = i
  | maxidx_typs (T :: Ts) i = maxidx_typs Ts (maxidx_typ T i);

fun maxidx_term (Var ((_, j), T)) i = maxidx_typ T (Int.max (i, j))
  | maxidx_term (Const (_, T)) i = maxidx_typ T i
  | maxidx_term (Free (_, T)) i = maxidx_typ T i
  | maxidx_term (Bound _) i = i
  | maxidx_term (Abs (_, T, t)) i = maxidx_term t (maxidx_typ T i)
  | maxidx_term (t $ u) i = maxidx_term u (maxidx_term t i);

fun maxidx_of_typ T = maxidx_typ T ~1;
fun maxidx_of_typs Ts = maxidx_typs Ts ~1;
fun maxidx_of_term t = maxidx_term t ~1;



(** misc syntax operations **)

(* substructure *)

fun fold_subtypes f =
  let
    fun iter ty =
      (case ty of Type (_, Ts) => f ty #> fold iter Ts | _ => f ty);
  in iter end;

fun exists_subtype P =
  let
    fun ex ty = P ty orelse
      (case ty of Type (_, Ts) => exists ex Ts | _ => false);
  in ex end;

fun exists_type P =
  let
    fun ex (Const (_, T)) = P T
      | ex (Free (_, T)) = P T
      | ex (Var (_, T)) = P T
      | ex (Bound _) = false
      | ex (Abs (_, T, t)) = P T orelse ex t
      | ex (t $ u) = ex t orelse ex u;
  in ex end;

fun exists_subterm P =
  let
    fun ex tm = P tm orelse
      (case tm of
        t $ u => ex t orelse ex u
      | Abs (_, _, t) => ex t
      | _ => false);
  in ex end;

fun exists_Const P = exists_subterm (fn Const c => P c | _ => false);

fun is_schematic t =
  exists_subterm is_Var t orelse
  (exists_type o exists_subtype) is_TVar t;


(* contraction *)

fun could_beta_contract (Abs _ $ _) = true
  | could_beta_contract (t $ u) = could_beta_contract t orelse could_beta_contract u
  | could_beta_contract (Abs (_, _, b)) = could_beta_contract b
  | could_beta_contract _ = false;

fun could_eta_contract (Abs (_, _, _ $ Bound 0)) = true
  | could_eta_contract (Abs (_, _, b)) = could_eta_contract b
  | could_eta_contract (t $ u) = could_eta_contract t orelse could_eta_contract u
  | could_eta_contract _ = false;


(* dest abstraction *)

(*ASSUMPTION: x is fresh wrt. the current context, but the check
  of used_free merely guards against gross mistakes*)

fun used_free x =
  let
    fun used (Free (y, _)) = (x = y)
      | used (t $ u) = used t orelse used u
      | used (Abs (_, _, t)) = used t
      | used _ = false;
  in used end;

exception USED_FREE of string * term;

fun subst_abs v b = (v, subst_bound (Free v, b));

fun dest_abs_fresh x t =
  (case t of
    Abs (_, T, b) =>
      if used_free x b then raise USED_FREE (x, t)
      else subst_abs (x, T) b
  | _ => raise TERM ("dest_abs", [t]));

fun dest_abs_global t =
  (case t of
    Abs (x, T, b) =>
      if used_free x b then
        let
          val used = Name.build_context (declare_free_names b);
          val x' = #1 (Name.variant x used);
        in subst_abs (x', T) b end
      else subst_abs (x, T) b
  | _ => raise TERM ("dest_abs", [t]));


(* dummy patterns *)

fun dummy_pattern T = Const ("Pure.dummy_pattern", T);
val dummy = dummy_pattern dummyT;
val dummy_prop = dummy_pattern propT;

fun is_dummy_pattern (Const ("Pure.dummy_pattern", _)) = true
  | is_dummy_pattern _ = false;

fun no_dummy_patterns tm =
  if not (fold_aterms (fn t => fn b => b orelse is_dummy_pattern t) tm false) then tm
  else raise TERM ("Illegal occurrence of '_' dummy pattern", [tm]);

fun free_dummy_patterns (Const ("Pure.dummy_pattern", T)) used =
      let val [x] = Name.invent used Name.uu 1
      in (Free (Name.internal x, T), Name.declare x used) end
  | free_dummy_patterns (Abs (x, T, b)) used =
      let val (b', used') = free_dummy_patterns b used
      in (Abs (x, T, b'), used') end
  | free_dummy_patterns (t $ u) used =
      let
        val (t', used') = free_dummy_patterns t used;
        val (u', used'') = free_dummy_patterns u used';
      in (t' $ u', used'') end
  | free_dummy_patterns a used = (a, used);

fun replace_dummy Ts (Const ("Pure.dummy_pattern", T)) i =
      (list_comb (Var (("_dummy_", i), Ts ---> T), map_range Bound (length Ts)), i + 1)
  | replace_dummy Ts (Abs (x, T, t)) i =
      let val (t', i') = replace_dummy (T :: Ts) t i
      in (Abs (x, T, t'), i') end
  | replace_dummy Ts (t $ u) i =
      let
        val (t', i') = replace_dummy Ts t i;
        val (u', i'') = replace_dummy Ts u i';
      in (t' $ u', i'') end
  | replace_dummy _ a i = (a, i);

val replace_dummy_patterns = replace_dummy [];

fun show_dummy_patterns (Var (("_dummy_", _), T)) = dummy_pattern T
  | show_dummy_patterns (t $ u) = show_dummy_patterns t $ show_dummy_patterns u
  | show_dummy_patterns (Abs (x, T, t)) = Abs (x, T, show_dummy_patterns t)
  | show_dummy_patterns a = a;


(* display variables *)

fun string_of_vname (x, i) =
  let
    val idx = string_of_int i;
    val dot =
      (case rev (Symbol.explode x) of
        _ :: "\<^sub>" :: _ => false
      | c :: _ => Symbol.is_digit c
      | _ => true);
  in
    if dot then "?" ^ x ^ "." ^ idx
    else if i <> 0 then "?" ^ x ^ idx
    else "?" ^ x
  end;

fun string_of_vname' (x, ~1) = x
  | string_of_vname' xi = string_of_vname xi;

end;

structure Basic_Term: BASIC_TERM = Term;
open Basic_Term;
