(*  Title:      Pure/theory.ML
    Author:     Lawrence C Paulson and Markus Wenzel

Logical theory content: axioms, definitions, and begin/end wrappers.
*)

signature THEORY =
sig
  val parents_of: theory -> theory list
  val ancestors_of: theory -> theory list
  val nodes_of: theory -> theory list
  val setup: (theory -> theory) -> unit
  val setup_result: (theory -> 'a * theory) -> 'a
  val local_setup: (Proof.context -> Proof.context) -> unit
  val local_setup_result: (Proof.context -> 'a * Proof.context) -> 'a
  val install_pure: theory -> unit
  val get_pure: unit -> theory
  val get_pure_bootstrap: unit -> theory
  val get_markup: theory -> Markup.T
  val check_theory: {get: string -> theory, all: unit -> string list} ->
    Proof.context -> string * Position.T -> theory
  val check: {long: bool} -> Proof.context -> string * Position.T -> theory
  val axiom_table: theory -> term Name_Space.table
  val axiom_space: theory -> Name_Space.T
  val all_axioms_of: theory -> (string * term) list
  val defs_of: theory -> Defs.T
  val at_begin: (theory -> theory option) -> theory -> theory
  val at_end: (theory -> theory option) -> theory -> theory
  val begin_theory: string * Position.T -> theory list -> theory
  val end_theory: theory -> theory
  val add_axiom: Proof.context -> binding * term -> theory -> theory
  val const_dep: theory -> string * typ -> Defs.entry
  val type_dep: string * typ list -> Defs.entry
  val add_deps: Defs.context -> string -> Defs.entry -> Defs.entry list -> theory -> theory
  val add_deps_global: string -> Defs.entry -> Defs.entry list -> theory -> theory
  val add_deps_const: string -> theory -> theory
  val add_deps_type: string -> theory -> theory
  val add_def: Defs.context -> bool -> bool -> binding * term -> theory -> theory
  val specify_const: (binding * typ) * mixfix -> theory -> term * theory
  val check_overloading: Proof.context -> bool -> string * typ -> unit
  val equality_axioms: (binding * term) list
end

structure Theory: THEORY =
struct


(** theory context operations **)

val parents_of = Context.parents_of;
val ancestors_of = Context.ancestors_of;
fun nodes_of thy = thy :: ancestors_of thy;

fun setup f = Context.>> (Context.map_theory f);
fun setup_result f = Context.>>> (Context.map_theory_result f);

fun local_setup f = Context.>> (Context.map_proof f);
fun local_setup_result f = Context.>>> (Context.map_proof_result f);


(* implicit theory Pure *)

val pure: theory Single_Assignment.var = Single_Assignment.var "pure";

fun install_pure thy = Single_Assignment.assign pure thy;

fun get_pure () =
  (case Single_Assignment.peek pure of
    SOME thy => thy
  | NONE => raise Fail "Theory Pure not present");

fun get_pure_bootstrap () =
  (case Single_Assignment.peek pure of
    SOME thy => thy
  | NONE => Context.the_global_context ());



(** datatype thy **)

type wrapper = (theory -> theory option) * stamp;

fun apply_wrappers (wrappers: wrapper list) =
  perhaps (perhaps_loop (perhaps_apply (map fst wrappers)));

datatype thy = Thy of
 {pos: Position.T,
  id: serial,
  axioms: term Name_Space.table,
  defs: Defs.T,
  wrappers: wrapper list * wrapper list};

fun rep_thy (Thy args) = args;

fun make_thy (pos, id, axioms, defs, wrappers) =
  Thy {pos = pos, id = id, axioms = axioms, defs = defs, wrappers = wrappers};

structure Thy = Theory_Data'
(
  type T = thy;
  val empty = make_thy (Position.none, 0, Name_Space.empty_table Markup.axiomN, Defs.empty, ([], []));
  fun merge args =
    let
      val thy0 = #1 (hd args);
      val {pos, id, ...} = rep_thy (#2 (hd args));

      val merge_defs = Defs.merge (Defs.global_context thy0);
      val merge_wrappers = Library.merge (eq_snd op =);

      val axioms' = Library.foldl1 Name_Space.merge_tables (map (#axioms o rep_thy o #2) args);
      val defs' = Library.foldl1 merge_defs (map (#defs o rep_thy o #2) args);
      val bgs' = Library.foldl1 merge_wrappers (map (#1 o #wrappers o rep_thy o #2) args);
      val ens' = Library.foldl1 merge_wrappers (map (#2 o #wrappers o rep_thy o #2) args);
    in make_thy (pos, id, axioms', defs', (bgs', ens')) end;
);

val rep_theory = rep_thy o Thy.get;

fun map_thy f = Thy.map (fn (Thy {pos, id, axioms, defs, wrappers}) =>
  make_thy (f (pos, id, axioms, defs, wrappers)));

fun map_axioms f =
  map_thy (fn (pos, id, axioms, defs, wrappers) => (pos, id, f axioms, defs, wrappers));

fun map_defs f =
  map_thy (fn (pos, id, axioms, defs, wrappers) => (pos, id, axioms, f defs, wrappers));

fun map_wrappers f =
  map_thy (fn (pos, id, axioms, defs, wrappers) => (pos, id, axioms, defs, f wrappers));


(* entity markup *)

fun theory_markup def name id pos =
  if id = 0 then Markup.empty
  else Position.make_entity_markup def id Markup.theoryN (name, pos);

fun init_markup (name, pos) thy =
  let
    val id = serial ();
    val _ = Context_Position.reports_global thy [(pos, theory_markup {def = true} name id pos)];
  in map_thy (fn (_, _, axioms, defs, wrappers) => (pos, id, axioms, defs, wrappers)) thy end;

fun get_markup thy =
  let val {pos, id, ...} = rep_theory thy
  in theory_markup {def = false} (Context.theory_long_name thy) id pos end;

fun check_theory {get, all} ctxt (name, pos) =
  let
    val thy = get name handle ERROR msg =>
      let
        val completion_report =
          Completion.make_report (name, pos)
            (fn completed =>
              all ()
              |> filter (completed o Long_Name.base_name)
              |> sort_strings
              |> map (fn a => (a, (Markup.theoryN, a))));
      in error (msg ^ Position.here pos ^ completion_report) end;
    val _ = Context_Position.report ctxt pos (get_markup thy);
  in thy end;

fun check long ctxt arg =
  let
    val thy = Proof_Context.theory_of ctxt;
    val get = Context.get_theory long thy;
    fun all () = map (Context.theory_name long) (ancestors_of thy);
  in check_theory {get = get, all = all} ctxt arg end;


(* basic operations *)

val axiom_table = #axioms o rep_theory;
val axiom_space = Name_Space.space_of_table o axiom_table;

val all_axioms_of = Name_Space.dest_table o axiom_table;

val defs_of = #defs o rep_theory;


(* begin/end theory *)

val begin_wrappers = rev o #1 o #wrappers o rep_theory;
val end_wrappers = rev o #2 o #wrappers o rep_theory;

fun at_begin f = map_wrappers (apfst (cons (f, stamp ())));
fun at_end f = map_wrappers (apsnd (cons (f, stamp ())));

fun begin_theory (name, pos) imports =
  if name = Context.PureN then
    (case imports of
      [thy] => init_markup (name, pos) thy
    | _ => error "Bad bootstrapping of theory Pure")
  else
    let
      val thy = Context.begin_thy name imports;
      val wrappers = begin_wrappers thy;
    in
      thy
      |> init_markup (name, pos)
      |> Sign.init_naming
      |> Sign.local_path
      |> apply_wrappers wrappers
      |> tap (Syntax.cache_syntax o Sign.syntax_of)
    end;

fun end_theory thy =
  thy
  |> apply_wrappers (end_wrappers thy)
  |> Sign.change_check
  |> Context.finish_thy;



(** primitive specifications **)

(* raw axioms *)

fun cert_axm ctxt (b, raw_tm) =
  let
    val thy = Proof_Context.theory_of ctxt;
    val t = Sign.cert_prop thy raw_tm
      handle TYPE (msg, _, _) => error msg
        | TERM (msg, _) => error msg;
    val _ = Term.no_dummy_patterns t handle TERM (msg, _) => error msg;

    val bad_sorts =
      rev ((fold_types o fold_atyps_sorts) (fn (_, []) => I | (T, _) => insert (op =) T) t []);
    val _ = null bad_sorts orelse
      error ("Illegal sort constraints in primitive specification: " ^
        commas (map (Syntax.string_of_typ (Config.put show_sorts true ctxt)) bad_sorts));
  in (b, Sign.no_vars ctxt t) end
  handle ERROR msg => cat_error msg ("The error(s) above occurred in axiom " ^ Binding.print b);

fun add_axiom ctxt raw_axm thy = thy |> map_axioms (fn axioms =>
  let
    val axm = apsnd Logic.varify_global (cert_axm ctxt raw_axm);
    val context = ctxt
      |> Sign.inherit_naming thy
      |> Context_Position.set_visible_generic false;
    val (_, axioms') = Name_Space.define context true axm axioms;
  in axioms' end);


(* dependencies *)

fun const_dep thy (c, T) = ((Defs.Const, c), Sign.const_typargs thy (c, T));
fun type_dep (c, args) = ((Defs.Type, c), args);

fun dependencies (context as (ctxt, _)) unchecked def description lhs rhs =
  let
    fun prep (item, args) =
      (case fold Term.add_tvarsT args [] of
        [] => (item, map Logic.varifyT_global args)
      | vs => raise TYPE ("Illegal schematic type variable(s)", map TVar vs, []));

    val lhs_vars = TFrees.build (fold TFrees.add_tfreesT (snd lhs));
    val rhs_extras =
      TFrees.build (rhs |> fold (fold (TFrees.add_tfreesT_unless (TFrees.defined lhs_vars)) o snd))
      |> TFrees.keys;
    val _ =
      if null rhs_extras then ()
      else error ("Specification depends on extra type variables: " ^
        commas_quote (map (Syntax.string_of_typ ctxt o TFree) rhs_extras) ^
        "\nThe error(s) above occurred in " ^ quote description);
  in Defs.define context unchecked def description (prep lhs) (map prep rhs) end;

fun cert_entry thy ((Defs.Const, c), args) =
      Sign.cert_term thy (Const (c, Sign.const_instance thy (c, args)))
      |> dest_Const |> const_dep thy
  | cert_entry thy ((Defs.Type, c), args) =
      Sign.certify_typ thy (Type (c, args)) |> dest_Type |> type_dep;

fun add_deps context a raw_lhs raw_rhs thy =
  let
    val (lhs as ((_, lhs_name), _)) :: rhs = map (cert_entry thy) (raw_lhs :: raw_rhs);
    val description = if a = "" then lhs_name ^ " axiom" else a;
  in thy |> map_defs (dependencies context false NONE description lhs rhs) end;

fun add_deps_global a x y thy =
  add_deps (Defs.global_context thy) a x y thy;

fun add_deps_const c thy =
  let val T = Logic.unvarifyT_global (Sign.the_const_type thy c);
  in thy |> add_deps_global "" (const_dep thy (c, T)) [] end;

fun add_deps_type c thy =
  let
    val n = Sign.arity_number thy c;
    val args = map (fn a => TFree (a, [])) (Name.invent_global_types n);
  in thy |> add_deps_global "" (type_dep (c, args)) [] end

fun specify_const decl thy =
  let val (t, thy') = Sign.declare_const_global decl thy;
  in (t, add_deps_global "" (const_dep thy' (dest_Const t)) [] thy') end;


(* overloading *)

fun check_overloading ctxt overloaded (c, T) =
  let
    val thy = Proof_Context.theory_of ctxt;

    val declT = Sign.the_const_constraint thy c
      handle TYPE (msg, _, _) => error msg;
    val T' = Logic.varifyT_global T;

    fun message sorts txt =
      [Pretty.block [Pretty.str "Specification of constant ",
        Pretty.str c, Pretty.str " ::", Pretty.brk 1,
        Pretty.quote (Syntax.pretty_typ (Config.put show_sorts sorts ctxt) T)],
        Pretty.str txt] |> Pretty.chunks |> Pretty.string_of;
  in
    if Sign.typ_instance thy (declT, T') then ()
    else if Type.raw_instance (declT, T') then
      error (message true "imposes additional sort constraints on the constant declaration")
    else if overloaded then ()
    else
      error (message false "is strictly less general than the declared type (overloading required)")
  end;


(* definitional axioms *)

local

fun check_def (context as (ctxt, _)) thy unchecked overloaded (b, tm) defs =
  let
    val name = Sign.full_name thy b;
    val ((lhs, rhs), _, _) =
      Primitive_Defs.dest_def ctxt
        {check_head = Term.is_Const,
         check_free_lhs = K true,
         check_free_rhs = K false,
         check_tfree = K false} tm
      handle TERM (msg, _) => error msg;
    val lhs_const = Term.dest_Const (Term.head_of lhs);

    val rhs_consts =
      fold_aterms (fn Const const => insert (op =) (const_dep thy const) | _ => I) rhs [];
    val rhs_types =
      (fold_types o fold_subtypes) (fn Type t => insert (op =) (type_dep t) | _ => I) rhs [];
    val rhs_deps = rhs_consts @ rhs_types;

    val _ = check_overloading ctxt overloaded lhs_const;
  in defs |> dependencies context unchecked (SOME name) name (const_dep thy lhs_const) rhs_deps end
  handle ERROR msg => cat_error msg (Pretty.string_of (Pretty.block
   [Pretty.str ("The error(s) above occurred in definition " ^ Binding.print b ^ ":"),
    Pretty.fbrk, Pretty.quote (Syntax.pretty_term ctxt tm)]));

in

fun add_def (context as (ctxt, _)) unchecked overloaded raw_axm thy =
  let val axm = cert_axm ctxt raw_axm in
    thy
    |> map_defs (check_def context thy unchecked overloaded axm)
    |> add_axiom ctxt axm
  end;

end;


(** axioms for equality **)

local

val aT = TFree ("'a", []);
val bT = TFree ("'b", []);
val x = Free ("x", aT);
val y = Free ("y", aT);
val z = Free ("z", aT);
val A = Free ("A", propT);
val B = Free ("B", propT);
val f = Free ("f", aT --> bT);
val g = Free ("g", aT --> bT);

in

val equality_axioms =
 [(Binding.make ("reflexive", \<^here>), Logic.mk_equals (x, x)),
  (Binding.make ("symmetric", \<^here>),
    Logic.mk_implies (Logic.mk_equals (x, y), Logic.mk_equals (y, x))),
  (Binding.make ("transitive", \<^here>),
    Logic.list_implies ([Logic.mk_equals (x, y), Logic.mk_equals (y, z)], Logic.mk_equals (x, z))),
  (Binding.make ("equal_intr", \<^here>),
    Logic.list_implies ([Logic.mk_implies (A, B), Logic.mk_implies (B, A)], Logic.mk_equals (A, B))),
  (Binding.make ("equal_elim", \<^here>), Logic.list_implies ([Logic.mk_equals (A, B), A], B)),
  (Binding.make ("abstract_rule", \<^here>),
    Logic.mk_implies
      (Logic.all x (Logic.mk_equals (f $ x, g $ x)),
        Logic.mk_equals (lambda x (f $ x), lambda x (g $ x)))),
  (Binding.make ("combination", \<^here>), Logic.list_implies
    ([Logic.mk_equals (f, g), Logic.mk_equals (x, y)], Logic.mk_equals (f $ x, g $ y)))];

end;

end;
