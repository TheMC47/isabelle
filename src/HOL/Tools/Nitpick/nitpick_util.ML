(*  Title:      HOL/Tools/Nitpick/nitpick_util.ML
    Author:     Jasmin Blanchette, TU Muenchen
    Copyright   2008, 2009, 2010

General-purpose functions used by the Nitpick modules.
*)

signature NITPICK_UTIL =
sig
  datatype polarity = Pos | Neg | Neut

  exception ARG of string * string
  exception BAD of string * string
  exception TOO_SMALL of string * string
  exception TOO_LARGE of string * string
  exception NOT_SUPPORTED of string
  exception SAME of unit

  val nitpick_prefix : string
  val curry3 : ('a * 'b * 'c -> 'd) -> 'a -> 'b -> 'c -> 'd
  val pairf : ('a -> 'b) -> ('a -> 'c) -> 'a -> 'b * 'c
  val pair_from_fun : (bool -> 'a) -> 'a * 'a
  val fun_from_pair : 'a * 'a -> bool -> 'a
  val int_from_bool : bool -> int
  val nat_minus : int -> int -> int
  val reasonable_power : int -> int -> int
  val exact_log : int -> int -> int
  val exact_root : int -> int -> int
  val offset_list : int list -> int list
  val index_seq : int -> int -> int list
  val filter_indices : int list -> 'a list -> 'a list
  val filter_out_indices : int list -> 'a list -> 'a list
  val fold1 : ('a -> 'a -> 'a) -> 'a list -> 'a
  val replicate_list : int -> 'a list -> 'a list
  val n_fold_cartesian_product : 'a list list -> 'a list list
  val all_distinct_unordered_pairs_of : ''a list -> (''a * ''a) list
  val nth_combination : (int * int) list -> int -> int list
  val all_combinations : (int * int) list -> int list list
  val all_permutations : 'a list -> 'a list list
  val chunk_list : int -> 'a list -> 'a list list
  val chunk_list_unevenly : int list -> 'a list -> 'a list list
  val double_lookup :
    ('a * 'a -> bool) -> ('a option * 'b) list -> 'a -> 'b option
  val triple_lookup :
    (''a * ''a -> bool) -> (''a option * 'b) list -> ''a -> 'b option
  val is_substring_of : string -> string -> bool
  val plural_s : int -> string
  val plural_s_for_list : 'a list -> string
  val serial_commas : string -> string list -> string list
  val pretty_serial_commas : string -> Pretty.T list -> Pretty.T list
  val parse_bool_option : bool -> string -> string -> bool option
  val parse_time : string -> string -> Time.time
  val string_of_time : Time.time -> string
  val nat_subscript : int -> string
  val flip_polarity : polarity -> polarity
  val prop_T : typ
  val bool_T : typ
  val nat_T : typ
  val int_T : typ
  val simple_string_of_typ : typ -> string
  val num_binder_types : typ -> int
  val varify_type : Proof.context -> typ -> typ
  val instantiate_type : theory -> typ -> typ -> typ -> typ
  val varify_and_instantiate_type : Proof.context -> typ -> typ -> typ -> typ
  val varify_and_instantiate_type_global : theory -> typ -> typ -> typ -> typ
  val is_of_class_const : theory -> string * typ -> bool
  val get_class_def : theory -> string -> (string * term) option
  val specialize_type : theory -> string * typ -> term -> term
  val eta_expand : typ list -> term -> int -> term
  val DETERM_TIMEOUT : Time.time -> tactic -> tactic
  val indent_size : int
  val pretty_maybe_quote : Proof.context -> Pretty.T -> Pretty.T
  val hash_term : term -> int
  val spying : bool -> (unit -> Proof.state * int * string) -> unit
end;

structure Nitpick_Util : NITPICK_UTIL =
struct

datatype polarity = Pos | Neg | Neut

exception ARG of string * string
exception BAD of string * string
exception TOO_SMALL of string * string
exception TOO_LARGE of string * string
exception NOT_SUPPORTED of string
exception SAME of unit

val nitpick_prefix = "Nitpick" ^ Long_Name.separator

val timestamp = ATP_Util.timestamp

fun curry3 f = fn x => fn y => fn z => f (x, y, z)

fun pairf f g x = (f x, g x)

fun pair_from_fun f = (f false, f true)
fun fun_from_pair (f, t) b = if b then t else f

fun int_from_bool b = if b then 1 else 0
fun nat_minus i j = if i > j then i - j else 0

val max_exponent = 16384

fun reasonable_power _ 0 = 1
  | reasonable_power a 1 = a
  | reasonable_power 0 _ = 0
  | reasonable_power 1 _ = 1
  | reasonable_power a b =
    if b < 0 then
      raise ARG ("Nitpick_Util.reasonable_power",
                 "negative exponent (" ^ signed_string_of_int b ^ ")")
    else if b > max_exponent then
      raise TOO_LARGE ("Nitpick_Util.reasonable_power",
                       "too large exponent (" ^ signed_string_of_int a ^ " ^ " ^
                       signed_string_of_int b ^ ")")
    else
      let val c = reasonable_power a (b div 2) in
        c * c * reasonable_power a (b mod 2)
      end

fun exact_log m n =
  let
    val r = Math.ln (Real.fromInt n) / Math.ln (Real.fromInt m) |> Real.round
  in
    if reasonable_power m r = n then
      r
    else
      raise ARG ("Nitpick_Util.exact_log",
                 commas (map signed_string_of_int [m, n]))
  end

fun exact_root m n =
  let val r = Math.pow (Real.fromInt n, 1.0 / (Real.fromInt m)) |> Real.round in
    if reasonable_power r m = n then
      r
    else
      raise ARG ("Nitpick_Util.exact_root",
                 commas (map signed_string_of_int [m, n]))
  end

fun fold1 f = foldl1 (uncurry f)

fun replicate_list 0 _ = []
  | replicate_list n xs = xs @ replicate_list (n - 1) xs

fun offset_list ns = rev (tl (fold (fn x => fn xs => (x + hd xs) :: xs) ns [0]))

fun index_seq j0 n = if j0 < 0 then j0 downto j0 - n + 1 else j0 upto j0 + n - 1

fun filter_indices js xs =
  let
    fun aux _ [] _ = []
      | aux i (j :: js) (x :: xs) =
        if i = j then x :: aux (i + 1) js xs else aux (i + 1) (j :: js) xs
      | aux _ _ _ = raise ARG ("Nitpick_Util.filter_indices",
                               "indices unordered or out of range")
  in aux 0 js xs end

fun filter_out_indices js xs =
  let
    fun aux _ [] xs = xs
      | aux i (j :: js) (x :: xs) =
        if i = j then aux (i + 1) js xs else x :: aux (i + 1) (j :: js) xs
      | aux _ _ _ = raise ARG ("Nitpick_Util.filter_out_indices",
                               "indices unordered or out of range")
  in aux 0 js xs end

fun cartesian_product [] _ = []
  | cartesian_product (x :: xs) yss = map (cons x) yss @ cartesian_product xs yss

fun n_fold_cartesian_product xss = fold_rev cartesian_product xss [[]]

fun all_distinct_unordered_pairs_of [] = []
  | all_distinct_unordered_pairs_of (x :: xs) =
    map (pair x) xs @ all_distinct_unordered_pairs_of xs

val nth_combination =
  let
    fun aux [] n = ([], n)
      | aux ((k, j0) :: xs) n =
        let val (js, n) = aux xs n in ((n mod k) + j0 :: js, n div k) end
  in fst oo aux end

val all_combinations = n_fold_cartesian_product o map (uncurry index_seq o swap)

fun all_permutations [] = [[]]
  | all_permutations xs =
    maps (fn j => map (cons (nth xs j)) (all_permutations (nth_drop j xs)))
         (index_seq 0 (length xs))

(* FIXME: use "Library.chop_groups" *)
val chunk_list = ATP_Util.chunk_list

(* FIXME: use "Library.unflat" *)
fun chunk_list_unevenly _ [] = []
  | chunk_list_unevenly [] xs = map single xs
  | chunk_list_unevenly (k :: ks) xs =
    let val (xs1, xs2) = chop k xs in xs1 :: chunk_list_unevenly ks xs2 end

fun double_lookup eq ps key =
  case AList.lookup (fn (SOME x, SOME y) => eq (x, y) | _ => false) ps
                    (SOME key) of
    SOME z => SOME z
  | NONE => ps |> find_first (is_none o fst) |> Option.map snd

fun triple_lookup _ [(NONE, z)] _ = SOME z
  | triple_lookup eq ps key =
    case AList.lookup (op =) ps (SOME key) of
      SOME z => SOME z
    | NONE => double_lookup eq ps key

fun is_substring_of needle stack =
  not (Substring.isEmpty (snd (Substring.position needle
                                                  (Substring.full stack))))

val plural_s = Sledgehammer_Util.plural_s
fun plural_s_for_list xs = plural_s (length xs)

val serial_commas = Try.serial_commas

fun pretty_serial_commas _ [] = []
  | pretty_serial_commas _ [p] = [p]
  | pretty_serial_commas conj [p1, p2] =
    [p1, Pretty.brk 1, Pretty.str conj, Pretty.brk 1, p2]
  | pretty_serial_commas conj [p1, p2, p3] =
    [p1, Pretty.str ",", Pretty.brk 1, p2, Pretty.str ",", Pretty.brk 1,
     Pretty.str conj, Pretty.brk 1, p3]
  | pretty_serial_commas conj (p :: ps) =
    p :: Pretty.str "," :: Pretty.brk 1 :: pretty_serial_commas conj ps

val parse_bool_option = Sledgehammer_Util.parse_bool_option
val parse_time = Sledgehammer_Util.parse_time
val string_of_time = ATP_Util.string_of_time

val subscript = implode o map (prefix "\<^sub>") o Symbol.explode

fun nat_subscript n =
  n |> signed_string_of_int |> not (print_mode_active Print_Mode.ASCII) ? subscript

fun flip_polarity Pos = Neg
  | flip_polarity Neg = Pos
  | flip_polarity Neut = Neut

val prop_T = \<^typ>\<open>prop\<close>
val bool_T = \<^typ>\<open>bool\<close>
val nat_T = \<^typ>\<open>nat\<close>
val int_T = \<^typ>\<open>int\<close>

fun simple_string_of_typ (Type (s, _)) = s
  | simple_string_of_typ (TFree (s, _)) = s
  | simple_string_of_typ (TVar ((s, _), _)) = s

val num_binder_types = BNF_Util.num_binder_types

val varify_type = ATP_Util.varify_type
val instantiate_type = ATP_Util.instantiate_type
val varify_and_instantiate_type = ATP_Util.varify_and_instantiate_type

fun varify_and_instantiate_type_global thy T1 T1' T2 =
  instantiate_type thy (Logic.varifyT_global T1) T1' (Logic.varifyT_global T2)

fun is_of_class_const thy (s, _) =
  member (op =) (map Logic.const_of_class (Sign.all_classes thy)) s

fun get_class_def thy class =
  let val axname = class ^ "_class_def" in
    Option.map (pair axname)
      (AList.lookup (op =) (Theory.all_axioms_of thy) axname)
  end;

val specialize_type = ATP_Util.specialize_type
val eta_expand = ATP_Util.eta_expand

fun DETERM_TIMEOUT delay tac st =
  Seq.of_list (the_list (Timeout.apply delay (fn () => SINGLE tac st) ()))

val indent_size = 2

val pretty_maybe_quote = ATP_Util.pretty_maybe_quote

val hashw = ATP_Util.hashw
val hashw_string = ATP_Util.hashw_string

fun hashw_term (t1 $ t2) = hashw (hashw_term t1, hashw_term t2)
  | hashw_term (Const (s, _)) = hashw_string (s, 0w0)
  | hashw_term (Free (s, _)) = hashw_string (s, 0w0)
  | hashw_term _ = 0w0

val hash_term = Word.toInt o hashw_term

val hackish_string_of_term = Sledgehammer_Util.hackish_string_of_term

val spying_version = "b"

fun spying false _ = ()
  | spying true f =
    let
      val (state, i, message) = f ()
      val ctxt = Proof.context_of state
      val goal = Logic.get_goal (Thm.prop_of (#goal (Proof.goal state))) i
      val hash = String.substring (SHA1.rep (SHA1.digest (hackish_string_of_term ctxt goal)), 0, 12)
    in
      File.append (Path.explode "$ISABELLE_HOME_USER/spy_nitpick")
        (spying_version ^ " " ^ timestamp () ^ ": " ^ hash ^ ": " ^ message ^ "\n")
    end

end;
