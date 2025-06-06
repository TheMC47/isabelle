(*  Title:      Pure/type_infer.ML
    Author:     Stefan Berghofer and Markus Wenzel, TU Muenchen

Basic representation of type-inference problems.
*)

signature TYPE_INFER =
sig
  val is_param: indexname -> bool
  val is_paramT: typ -> bool
  val param_maxidx: term -> int -> int
  val param_maxidx_of: term list -> int
  val param: int -> string * sort -> typ
  val mk_param: int -> sort -> typ
  val anyT: sort -> typ
  val paramify_vars: typ -> typ
  val deref: typ Vartab.table -> typ -> typ
  val finish: Proof.context -> typ Vartab.table -> typ list * term list -> typ list * term list
  val object_logic: bool Config.T
  val fixate: Proof.context -> bool -> term list -> term list
end;

structure Type_Infer: TYPE_INFER =
struct

(** type parameters and constraints **)

(* type inference parameters -- may get instantiated *)

fun is_param (x, _: int) = String.isPrefix "?" x;

fun is_paramT (TVar (xi, _)) = is_param xi
  | is_paramT _ = false;

val param_maxidx =
  (Term.fold_types o Term.fold_atyps)
    (fn (TVar (xi as (_, i), _)) => if is_param xi then Integer.max i else I | _ => I);

fun param_maxidx_of ts = fold param_maxidx ts ~1;

fun param i (x, S) = TVar (("?" ^ x, i), S);

fun mk_param i S = TVar (("?'a", i), S);


(* pre-stage parameters *)

fun anyT S = TFree ("'_dummy_", S);

val paramify_vars =
  Term.map_atyps (fn TVar ((x, i), S) => param i (x, S) | _ => raise Same.SAME);



(** results **)

(* dereferenced views *)

fun deref tye (T as TVar (xi, _)) =
      (case Vartab.lookup tye xi of
        NONE => T
      | SOME U => deref tye U)
  | deref _ T = T;

fun add_parms tye T =
  (case deref tye T of
    Type (_, Ts) => fold (add_parms tye) Ts
  | TVar (xi, _) => if is_param xi then Indexnames.add_set xi else I
  | _ => I);

fun add_names tye T =
  (case deref tye T of
    Type (_, Ts) => fold (add_names tye) Ts
  | TFree (x, _) => Name.declare x
  | TVar ((x, i), _) => if is_param (x, i) then I else Name.declare x);


(* finish -- standardize remaining parameters *)

fun finish ctxt tye (Ts, ts) =
  let
    val used =
      (fold o fold_types) (add_names tye) ts (fold (add_names tye) Ts (Variable.names_of ctxt));
    val parms = (fold o fold_types) (add_parms tye) ts (fold (add_parms tye) Ts Indexnames.empty);
    val names = Name.invent used ("?" ^ Name.aT) (Indexnames.size parms);
    val tab = Vartab.make (Indexnames.list_set parms ~~ names);

    fun finish_typ T =
      (case deref tye T of
        Type (a, Ts) => Type (a, map finish_typ Ts)
      | U as TFree _ => U
      | U as TVar (xi, S) =>
          (case Vartab.lookup tab xi of
            NONE => U
          | SOME a => TVar ((a, 0), S)));
  in (map finish_typ Ts, map (Type.strip_constraints o Term.map_types finish_typ) ts) end;


(* fixate -- introduce fresh type variables *)

val object_logic = Config.declare_bool ("Type_Infer.object_logic", \<^here>) (K true);

fun fixate ctxt pattern ts =
  let
    val base_sort = Object_Logic.get_base_sort ctxt;
    val improve_sort =
      if is_some base_sort andalso not pattern andalso Config.get ctxt object_logic
      then fn [] => the base_sort | S => S else I;

    fun subst_param (xi, S) (inst, used) =
      if is_param xi then
        let val ([a], used') = Name.invent' Name.aT 1 used;
        in (TVars.add ((xi, S), TFree (a, improve_sort S)) inst, used') end
      else (inst, used);
    val params = TVars.build (fold TVars.add_tvars ts) |> TVars.list_set;
    val used = fold Term.declare_tfree_names ts (Variable.names_of ctxt);
    val (inst, _) = fold subst_param params (TVars.empty, used);
  in (Same.commit o Same.map o Term.map_types_same) (Term_Subst.instantiateT_same inst) ts end;

end;
