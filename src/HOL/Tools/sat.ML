(*  Title:      HOL/Tools/sat.ML
    Author:     Stephan Merz and Alwen Tiu, QSL Team, LORIA (http://qsl.loria.fr)
    Author:     Tjark Weber, TU Muenchen

Proof reconstruction from SAT solvers.

  Description:
    This file defines several tactics to invoke a proof-producing
    SAT solver on a propositional goal in clausal form.

    We use a sequent presentation of clauses to speed up resolution
    proof reconstruction.
    We call such clauses "raw clauses", which are of the form
          [x1, ..., xn, P] |- False
    (note the use of |- instead of ==>, i.e. of Isabelle's (meta-)hyps here),
    where each xi is a literal (see also comments in cnf.ML).

    This does not work for goals containing schematic variables!

      The tactic produces a clause representation of the given goal
      in DIMACS format and invokes a SAT solver, which should return
      a proof consisting of a sequence of resolution steps, indicating
      the two input clauses, and resulting in new clauses, leading to
      the empty clause (i.e. "False").  The tactic replays this proof
      in Isabelle and thus solves the overall goal.

  There are three SAT tactics available.  They differ in the CNF transformation
  used. "sat_tac" uses naive CNF transformation to transform the theorem to be
  proved before giving it to the SAT solver.  The naive transformation in the
  worst case can lead to an exponential blow up in formula size.  Another
  tactic, "satx_tac", uses "definitional CNF transformation" which attempts to
  produce a formula of linear size increase compared to the input formula, at
  the cost of possibly introducing new variables.  See cnf.ML for more
  comments on the CNF transformation.  "rawsat_tac" should be used with
  caution: no CNF transformation is performed, and the tactic's behavior is
  undefined if the subgoal is not already given as [| C1; ...; Cn |] ==> False,
  where each Ci is a disjunction.

  The SAT solver to be used can be set via the "solver" reference.  See
  sat_solvers.ML for possible values, and etc/settings for required (solver-
  dependent) configuration settings.  To replay SAT proofs in Isabelle, you
  must of course use a proof-producing SAT solver in the first place.

  Proofs are replayed only if "quick_and_dirty" is false.  If
  "quick_and_dirty" is true, the theorem (in case the SAT solver claims its
  negation to be unsatisfiable) is proved via an oracle.
*)

signature SAT =
sig
  val trace: bool Config.T  (* print trace messages *)
  val solver: string Config.T  (* name of SAT solver to be used *)
  val counter: int Unsynchronized.ref     (* output: number of resolution steps during last proof replay *)
  val rawsat_thm: Proof.context -> cterm list -> thm
  val rawsat_tac: Proof.context -> int -> tactic
  val sat_tac: Proof.context -> int -> tactic
  val satx_tac: Proof.context -> int -> tactic
end

structure SAT : SAT =
struct

val trace = Attrib.setup_config_bool \<^binding>\<open>sat_trace\<close> (K false);

fun cond_tracing ctxt msg =
  if Config.get ctxt trace then tracing (msg ()) else ();

val solver = Attrib.setup_config_string \<^binding>\<open>sat_solver\<close> (K "cdclite");
  (*see HOL/Tools/sat_solver.ML for possible values*)

val counter = Unsynchronized.ref 0;

(* ------------------------------------------------------------------------- *)
(* lit_ord: an order on integers that considers their absolute values only,  *)
(*      thereby treating integers that represent the same atom (positively   *)
(*      or negatively) as equal                                              *)
(* ------------------------------------------------------------------------- *)

fun lit_ord (i, j) = int_ord (abs i, abs j);

(* ------------------------------------------------------------------------- *)
(* CLAUSE: during proof reconstruction, three kinds of clauses are           *)
(*      distinguished:                                                       *)
(*      1. NO_CLAUSE: clause not proved (yet)                                *)
(*      2. ORIG_CLAUSE: a clause as it occurs in the original problem        *)
(*      3. RAW_CLAUSE: a raw clause, with additional precomputed information *)
(*         (a mapping from int's to its literals) for faster proof           *)
(*         reconstruction                                                    *)
(* ------------------------------------------------------------------------- *)

datatype CLAUSE =
    NO_CLAUSE
  | ORIG_CLAUSE of thm
  | RAW_CLAUSE of thm * (int * cterm) list;

(* ------------------------------------------------------------------------- *)
(* resolve_raw_clauses: given a non-empty list of raw clauses, we fold       *)
(*      resolution over the list (starting with its head), i.e. with two raw *)
(*      clauses                                                              *)
(*        [P, x1, ..., a, ..., xn] |- False                                  *)
(*      and                                                                  *)
(*        [Q, y1, ..., a', ..., ym] |- False                                 *)
(*      (where a and a' are dual to each other), we convert the first clause *)
(*      to                                                                   *)
(*        [P, x1, ..., xn] |- a ==> False ,                                  *)
(*      the second clause to                                                 *)
(*        [Q, y1, ..., ym] |- a' ==> False                                   *)
(*      and then perform resolution with                                     *)
(*        [| ?P ==> False; ~?P ==> False |] ==> False                        *)
(*      to produce                                                           *)
(*        [P, Q, x1, ..., xn, y1, ..., ym] |- False                          *)
(*      Each clause is accompanied with an association list mapping integers *)
(*      (positive for positive literals, negative for negative literals, and *)
(*      the same absolute value for dual literals) to the actual literals as *)
(*      cterms.                                                              *)
(* ------------------------------------------------------------------------- *)

fun resolve_raw_clauses _ [] =
      raise THM ("Proof reconstruction failed (empty list of resolvents)!", 0, [])
  | resolve_raw_clauses ctxt (c::cs) =
      let
        (* merges two sorted lists wrt. 'lit_ord', suppressing duplicates *)
        fun merge xs [] = xs
          | merge [] ys = ys
          | merge (x :: xs) (y :: ys) =
              (case lit_ord (apply2 fst (x, y)) of
                LESS => x :: merge xs (y :: ys)
              | EQUAL => x :: merge xs ys
              | GREATER => y :: merge (x :: xs) ys)

        (* find out which two hyps are used in the resolution *)
        fun find_res_hyps ([], _) _ =
              raise THM ("Proof reconstruction failed (no literal for resolution)!", 0, [])
          | find_res_hyps (_, []) _ =
              raise THM ("Proof reconstruction failed (no literal for resolution)!", 0, [])
          | find_res_hyps (h1 :: hyps1, h2 :: hyps2) acc =
              (case lit_ord  (apply2 fst (h1, h2)) of
                LESS  => find_res_hyps (hyps1, h2 :: hyps2) (h1 :: acc)
              | EQUAL =>
                  let
                    val (i1, chyp1) = h1
                    val (i2, chyp2) = h2
                  in
                    if i1 = ~ i2 then
                      (i1 < 0, chyp1, chyp2, rev acc @ merge hyps1 hyps2)
                    else (* i1 = i2 *)
                      find_res_hyps (hyps1, hyps2) (h1 :: acc)
                  end
          | GREATER => find_res_hyps (h1 :: hyps1, hyps2) (h2 :: acc))

        fun resolution (c1, hyps1) (c2, hyps2) =
          let
            val _ =
              cond_tracing ctxt (fn () =>
                "Resolving clause: " ^ Thm.string_of_thm ctxt c1 ^
                " (hyps: " ^ ML_Syntax.print_list (Syntax.string_of_term ctxt) (Thm.hyps_of c1) ^
                ")\nwith clause: " ^ Thm.string_of_thm ctxt c2 ^
                " (hyps: " ^ ML_Syntax.print_list (Syntax.string_of_term ctxt) (Thm.hyps_of c2) ^ ")")

            (* the two literals used for resolution *)
            val (hyp1_is_neg, hyp1, hyp2, new_hyps) = find_res_hyps (hyps1, hyps2) []

            val c1' = Thm.implies_intr hyp1 c1  (* Gamma1 |- hyp1 ==> False *)
            val c2' = Thm.implies_intr hyp2 c2  (* Gamma2 |- hyp2 ==> False *)

            val res_thm =
              let
                val P = HOLogic.dest_judgment (if hyp1_is_neg then hyp2 else hyp1)
              in
                \<^instantiate>\<open>P in
                  lemma \<open>(P \<Longrightarrow> False) \<Longrightarrow> (\<not> P \<Longrightarrow> False) \<Longrightarrow> False\<close> by (rule case_split)\<close>
              end

            val _ =
              cond_tracing ctxt
                (fn () => "Resolution theorem: " ^ Thm.string_of_thm ctxt res_thm)

            (* Gamma1, Gamma2 |- False *)
            val c_new =
              Thm.implies_elim
                (Thm.implies_elim res_thm (if hyp1_is_neg then c2' else c1'))
                (if hyp1_is_neg then c1' else c2')

            val _ =
              cond_tracing ctxt (fn () =>
                "Resulting clause: " ^ Thm.string_of_thm ctxt c_new ^
                " (hyps: " ^
                ML_Syntax.print_list (Syntax.string_of_term ctxt) (Thm.hyps_of c_new) ^ ")")

            val _ = Unsynchronized.inc counter
          in
            (c_new, new_hyps)
          end
        in
          fold resolution cs c
        end;

(* ------------------------------------------------------------------------- *)
(* replay_proof: replays the resolution proof returned by the SAT solver;    *)
(*      cf. SAT_Solver.proof for details of the proof format.  Updates the   *)
(*      'clauses' array with derived clauses, and returns the derived clause *)
(*      at index 'empty_id' (which should just be "False" if proof           *)
(*      reconstruction was successful, with the used clauses as hyps).       *)
(*      'atom_table' must contain an injective mapping from all atoms that   *)
(*      occur (as part of a literal) in 'clauses' to positive integers.      *)
(* ------------------------------------------------------------------------- *)

fun replay_proof ctxt atom_table clauses (clause_table, empty_id) =
  let
    fun index_of_literal chyp =
      (case (HOLogic.dest_Trueprop o Thm.term_of) chyp of
        (Const (\<^const_name>\<open>Not\<close>, _) $ atom) =>
          SOME (~ (the (Termtab.lookup atom_table atom)))
      | atom => SOME (the (Termtab.lookup atom_table atom)))
      handle TERM _ => NONE;  (* 'chyp' is not a literal *)

    fun prove_clause id =
      (case Array.nth clauses id of
        RAW_CLAUSE clause => clause
      | ORIG_CLAUSE thm =>
          (* convert the original clause *)
          let
            val _ = cond_tracing ctxt (fn () => "Using original clause #" ^ string_of_int id)
            val raw = CNF.clause2raw_thm ctxt thm
            val hyps = sort (lit_ord o apply2 fst) (map_filter (fn chyp =>
              Option.map (rpair chyp) (index_of_literal chyp)) (Thm.chyps_of raw))
            val clause = (raw, hyps)
            val _ = Array.upd clauses id (RAW_CLAUSE clause)
          in
            clause
          end
      | NO_CLAUSE =>
          (* prove the clause, using information from 'clause_table' *)
          let
            val _ = cond_tracing ctxt (fn () => "Proving clause #" ^ string_of_int id ^ " ...")
            val ids = the (Inttab.lookup clause_table id)
            val clause = resolve_raw_clauses ctxt (map prove_clause ids)
            val _ = Array.upd clauses id (RAW_CLAUSE clause)
            val _ =
              cond_tracing ctxt
                (fn () => "Replay chain successful; clause stored at #" ^ string_of_int id)
          in
            clause
          end)

    val _ = counter := 0
    val empty_clause = fst (prove_clause empty_id)
    val _ =
      cond_tracing ctxt (fn () =>
        "Proof reconstruction successful; " ^
        string_of_int (!counter) ^ " resolution step(s) total.")
  in
    empty_clause
  end;

(* ------------------------------------------------------------------------- *)
(* string_of_prop_formula: return a human-readable string representation of  *)
(*      a 'prop_formula' (just for tracing)                                  *)
(* ------------------------------------------------------------------------- *)

fun string_of_prop_formula Prop_Logic.True = "True"
  | string_of_prop_formula Prop_Logic.False = "False"
  | string_of_prop_formula (Prop_Logic.BoolVar i) = "x" ^ string_of_int i
  | string_of_prop_formula (Prop_Logic.Not fm) = "~" ^ string_of_prop_formula fm
  | string_of_prop_formula (Prop_Logic.Or (fm1, fm2)) =
      "(" ^ string_of_prop_formula fm1 ^ " v " ^ string_of_prop_formula fm2 ^ ")"
  | string_of_prop_formula (Prop_Logic.And (fm1, fm2)) =
      "(" ^ string_of_prop_formula fm1 ^ " & " ^ string_of_prop_formula fm2 ^ ")";

(* ------------------------------------------------------------------------- *)
(* rawsat_thm: run external SAT solver with the given clauses.  Reconstructs *)
(*      a proof from the resulting proof trace of the SAT solver.  The       *)
(*      theorem returned is just "False" (with some of the given clauses as  *)
(*      hyps).                                                               *)
(* ------------------------------------------------------------------------- *)

fun rawsat_thm ctxt clauses =
  let
    (* remove premises that equal "True" *)
    val clauses' = filter (fn clause =>
      (not_equal \<^term>\<open>True\<close> o HOLogic.dest_Trueprop o Thm.term_of) clause
        handle TERM ("dest_Trueprop", _) => true) clauses
    (* remove non-clausal premises -- of course this shouldn't actually   *)
    (* remove anything as long as 'rawsat_tac' is only called after the   *)
    (* premises have been converted to clauses                            *)
    val clauses'' = filter (fn clause =>
      ((CNF.is_clause o HOLogic.dest_Trueprop o Thm.term_of) clause
        handle TERM ("dest_Trueprop", _) => false)
      orelse
       (if Context_Position.is_visible ctxt then
          warning ("Ignoring non-clausal premise " ^ Syntax.string_of_term ctxt (Thm.term_of clause))
        else (); false)) clauses'
    (* remove trivial clauses -- this is necessary because zChaff removes *)
    (* trivial clauses during preprocessing, and otherwise our clause     *)
    (* numbering would be off                                             *)
    val nontrivial_clauses =
      filter (not o CNF.clause_is_trivial o HOLogic.dest_Trueprop o Thm.term_of) clauses''
    (* sort clauses according to the term order -- an optimization,       *)
    (* useful because forming the union of hypotheses, as done by         *)
    (* Conjunction.intr_balanced and fold Thm.weaken below, is quadratic for *)
    (* terms sorted in descending order, while only linear for terms      *)
    (* sorted in ascending order                                          *)
    val sorted_clauses = sort Thm.fast_term_ord nontrivial_clauses
    val _ =
      cond_tracing ctxt (fn () =>
        "Sorted non-trivial clauses:\n" ^
        cat_lines (map (Syntax.string_of_term ctxt o Thm.term_of) sorted_clauses))
    (* translate clauses from HOL terms to Prop_Logic.prop_formula *)
    val (fms, atom_table) =
      fold_map (Prop_Logic.prop_formula_of_term o HOLogic.dest_Trueprop o Thm.term_of)
        sorted_clauses Termtab.empty
    val _ =
      cond_tracing ctxt
        (fn () => "Invoking SAT solver on clauses:\n" ^ cat_lines (map string_of_prop_formula fms))
    val fm = Prop_Logic.all fms
    fun make_quick_and_dirty_thm () =
      let
        val _ =
          cond_tracing ctxt
            (fn () => "quick_and_dirty is set: proof reconstruction skipped, using oracle instead")
        val False_thm = Skip_Proof.make_thm_cterm \<^cprop>\<open>False\<close>
      in
        (* 'fold Thm.weaken (rev sorted_clauses)' is linear, while 'fold    *)
        (* Thm.weaken sorted_clauses' would be quadratic, since we sorted   *)
        (* clauses in ascending order (which is linear for                  *)
        (* 'Conjunction.intr_balanced', used below)                         *)
        fold Thm.weaken (rev sorted_clauses) False_thm
      end
  in
    case
      let
        val the_solver = Config.get ctxt solver
        val _ = cond_tracing ctxt (fn () => "Invoking solver " ^ the_solver)
      in SAT_Solver.invoke_solver the_solver fm end
    of
      SAT_Solver.UNSATISFIABLE (SOME (clause_table, empty_id)) =>
       (cond_tracing ctxt (fn () =>
          "Proof trace from SAT solver:\n" ^
          "clauses: " ^ ML_Syntax.print_list
            (ML_Syntax.print_pair ML_Syntax.print_int (ML_Syntax.print_list ML_Syntax.print_int))
            (Inttab.dest clause_table) ^ "\n" ^
          "empty clause: " ^ string_of_int empty_id);
        if Config.get ctxt quick_and_dirty then
          make_quick_and_dirty_thm ()
        else
          let
            (* optimization: convert the given clauses to "[c_1 && ... && c_n] |- c_i";  *)
            (*               this avoids accumulation of hypotheses during resolution    *)
            (* [c_1, ..., c_n] |- c_1 && ... && c_n *)
            val clauses_thm = Conjunction.intr_balanced (map Thm.assume sorted_clauses)
            (* [c_1 && ... && c_n] |- c_1 && ... && c_n *)
            val clauses_cprop = Thm.cprop_of clauses_thm
            (* [[c_1 && ... && c_n] |- c_1, ..., [c_1 && ... && c_n] |- c_n] *)
            val cnf_clauses =
              Conjunction.elim_balanced (length sorted_clauses) (Thm.assume clauses_cprop)
            (* initialize the clause array with the given clauses *)
            val max_idx = fst (the (Inttab.max clause_table))
            val clause_arr = Array.array (max_idx + 1, NO_CLAUSE)
            val _ =
              fold (fn thm => fn i => (Array.upd clause_arr i (ORIG_CLAUSE thm); i + 1))
                cnf_clauses 0
            (* replay the proof to derive the empty clause *)
            (* [c_1 && ... && c_n] |- False *)
            val raw_thm = replay_proof ctxt atom_table clause_arr (clause_table, empty_id)
          in
            (* [c_1, ..., c_n] |- False *)
            Thm.implies_elim (Thm.implies_intr clauses_cprop raw_thm) clauses_thm
          end)
    | SAT_Solver.UNSATISFIABLE NONE =>
        if Config.get ctxt quick_and_dirty then
         (if Context_Position.is_visible ctxt then
            warning "SAT solver claims the formula to be unsatisfiable, but did not provide a proof"
          else ();
          make_quick_and_dirty_thm ())
        else
          raise THM ("SAT solver claims the formula to be unsatisfiable, but did not provide a proof", 0, [])
    | SAT_Solver.SATISFIABLE assignment =>
        let
          val msg =
            "SAT solver found a countermodel:\n" ^
            (commas o map (fn (term, idx) =>
                Syntax.string_of_term_global \<^theory> term ^ ": " ^
                  (case assignment idx of NONE => "arbitrary"
                  | SOME true => "true" | SOME false => "false")))
              (Termtab.dest atom_table)
        in
          raise THM (msg, 0, [])
        end
    | SAT_Solver.UNKNOWN =>
        raise THM ("SAT solver failed to decide the formula", 0, [])
  end;

(* ------------------------------------------------------------------------- *)
(* Tactics                                                                   *)
(* ------------------------------------------------------------------------- *)

(* ------------------------------------------------------------------------- *)
(* rawsat_tac: solves the i-th subgoal of the proof state; this subgoal      *)
(*      should be of the form                                                *)
(*        [| c1; c2; ...; ck |] ==> False                                    *)
(*      where each cj is a non-empty clause (i.e. a disjunction of literals) *)
(*      or "True"                                                            *)
(* ------------------------------------------------------------------------- *)

fun rawsat_tac ctxt i =
  Subgoal.FOCUS (fn {context = ctxt', prems, ...} =>
    resolve_tac ctxt' [rawsat_thm ctxt' (map Thm.cprop_of prems)] 1) ctxt i;

(* ------------------------------------------------------------------------- *)
(* pre_cnf_tac: converts the i-th subgoal                                    *)
(*        [| A1 ; ... ; An |] ==> B                                          *)
(*      to                                                                   *)
(*        [| A1; ... ; An ; ~B |] ==> False                                  *)
(*      (handling meta-logical connectives in B properly before negating),   *)
(*      then replaces meta-logical connectives in the premises (i.e. "==>",  *)
(*      "!!" and "==") by connectives of the HOL object-logic (i.e. by       *)
(*      "-->", "!", and "="), then performs beta-eta-normalization on the    *)
(*      subgoal                                                              *)
(* ------------------------------------------------------------------------- *)

fun pre_cnf_tac ctxt =
  resolve_tac ctxt @{thms ccontr} THEN'
  Object_Logic.atomize_prems_tac ctxt THEN'
  CONVERSION Drule.beta_eta_conversion;

(* ------------------------------------------------------------------------- *)
(* cnfsat_tac: checks if the empty clause "False" occurs among the premises; *)
(*      if not, eliminates conjunctions (i.e. each clause of the CNF formula *)
(*      becomes a separate premise), then applies 'rawsat_tac' to solve the  *)
(*      subgoal                                                              *)
(* ------------------------------------------------------------------------- *)

fun cnfsat_tac ctxt i =
  (eresolve_tac  ctxt [FalseE] i) ORELSE
  (REPEAT_DETERM (eresolve_tac ctxt [conjE] i) THEN rawsat_tac ctxt i);

(* ------------------------------------------------------------------------- *)
(* cnfxsat_tac: checks if the empty clause "False" occurs among the          *)
(*      premises; if not, eliminates conjunctions (i.e. each clause of the   *)
(*      CNF formula becomes a separate premise) and existential quantifiers, *)
(*      then applies 'rawsat_tac' to solve the subgoal                       *)
(* ------------------------------------------------------------------------- *)

fun cnfxsat_tac ctxt i =
  (eresolve_tac ctxt [FalseE] i) ORELSE
    (REPEAT_DETERM (eresolve_tac ctxt [conjE] i ORELSE eresolve_tac ctxt [exE] i) THEN
      rawsat_tac ctxt i);

(* ------------------------------------------------------------------------- *)
(* sat_tac: tactic for calling an external SAT solver, taking as input an    *)
(*      arbitrary formula.  The input is translated to CNF, possibly causing *)
(*      an exponential blowup.                                               *)
(* ------------------------------------------------------------------------- *)

fun sat_tac ctxt i =
  pre_cnf_tac ctxt i THEN CNF.cnf_rewrite_tac ctxt i THEN cnfsat_tac ctxt i;

(* ------------------------------------------------------------------------- *)
(* satx_tac: tactic for calling an external SAT solver, taking as input an   *)
(*      arbitrary formula.  The input is translated to CNF, possibly         *)
(*      introducing new literals.                                            *)
(* ------------------------------------------------------------------------- *)

fun satx_tac ctxt i =
  pre_cnf_tac ctxt i THEN CNF.cnfx_rewrite_tac ctxt i THEN cnfxsat_tac ctxt i;

end;
