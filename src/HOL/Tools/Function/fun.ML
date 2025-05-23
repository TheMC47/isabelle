(*  Title:      HOL/Tools/Function/fun.ML
    Author:     Alexander Krauss, TU Muenchen

Command "fun": Function definitions with pattern splitting/completion
and automated termination proofs.
*)

signature FUNCTION_FUN =
sig
  val fun_config : Function_Common.function_config
  val add_fun : (binding * typ option * mixfix) list ->
    Specification.multi_specs -> Function_Common.function_config ->
    local_theory -> Proof.context
  val add_fun_cmd : (binding * string option * mixfix) list ->
    Specification.multi_specs_cmd -> Function_Common.function_config ->
    bool -> local_theory -> Proof.context
end

structure Function_Fun : FUNCTION_FUN =
struct

open Function_Lib
open Function_Common


fun check_pats ctxt geq =
  let
    fun err str = error (cat_lines ["Malformed definition:",
      str ^ " not allowed in sequential mode.",
      Syntax.string_of_term ctxt geq])

    fun check_constr_pattern (Bound _) = ()
      | check_constr_pattern t =
      let
        val (hd, args) = strip_comb t
      in
        (case hd of
          Const (hd_s, hd_T) =>
          (case body_type hd_T of
            Type (Tname, _) =>
            (case Ctr_Sugar.ctr_sugar_of ctxt Tname of
              SOME {ctrs, ...} => exists (fn Const (s, _) => s = hd_s) ctrs
            | NONE => false)
          | _ => false)
        | _ => false) orelse err "Non-constructor pattern";
        map check_constr_pattern args;
        ()
      end

    val (_, qs, gs, args, _) = split_def ctxt (K true) geq

    val _ = if not (null gs) then err "Conditional equations" else ()
    val _ = map check_constr_pattern args

    (* just count occurrences to check linearity *)
    val _ = if fold (fold_aterms (fn Bound _ => Integer.add 1 | _ => I)) args 0 > length qs
      then err "Nonlinear patterns" else ()
  in
    ()
  end

fun mk_catchall fixes arity_of =
  let
    fun mk_eqn ((fname, fT), _) =
      let
        val n = arity_of fname
        val (argTs, rT) = chop n (binder_types fT)
          |> apsnd (fn Ts => Ts ---> body_type fT)
        val qs = map Free (Name.invent_names_global "a" argTs)
      in
        HOLogic.mk_eq(list_comb (Free (fname, fT), qs),
          Const (\<^const_name>\<open>undefined\<close>, rT))
        |> HOLogic.mk_Trueprop
        |> fold_rev Logic.all qs
      end
  in
    map mk_eqn fixes
  end

fun add_catchall ctxt fixes spec =
  let val fqgars = map (split_def ctxt (K true)) spec
      val arity_of = map (fn (fname,_,_,args,_) => (fname, length args)) fqgars
                     |> AList.lookup (op =) #> the
  in
    spec @ mk_catchall fixes arity_of
  end

fun further_checks ctxt origs tss =
  let
    fun fail_redundant t =
      error (cat_lines ["Equation is redundant (covered by preceding clauses):", Syntax.string_of_term ctxt t])
    fun warn_missing strs =
      warning (cat_lines ("Missing patterns in function definition:" :: strs))

    val (tss', added) = chop (length origs) tss

    val _ = case chop 3 (flat added) of
       ([], []) => ()
     | (eqs, []) => warn_missing (map (Syntax.string_of_term ctxt) eqs)
     | (eqs, rest) => warn_missing (map (Syntax.string_of_term ctxt) eqs
         @ ["(" ^ string_of_int (length rest) ^ " more)"])

    val _ = (origs ~~ tss')
      |> map (fn (t, ts) => if null ts then fail_redundant t else ())
  in
    ()
  end

fun sequential_preproc (config as FunctionConfig {sequential, ...}) ctxt fixes spec =
  if sequential then
    let
      val (bnds, eqss) = split_list spec

      val eqs = map the_single eqss

      val feqs = eqs
        |> tap (check_defs ctxt fixes) (* Standard checks *)
        |> tap (map (check_pats ctxt)) (* More checks for sequential mode *)

      val compleqs = add_catchall ctxt fixes feqs (* Completion *)

      val spliteqs = Function_Split.split_all_equations ctxt compleqs
        |> tap (further_checks ctxt feqs)

      fun restore_spec thms =
        bnds ~~ take (length bnds) (unflat spliteqs thms)

      val spliteqs' = flat (take (length bnds) spliteqs)
      val fnames = map (fst o fst) fixes
      val indices = map (fn eq => find_index (curry op = (fname_of eq)) fnames) spliteqs'

      fun sort xs = partition_list (fn i => fn (j,_) => i = j) 0 (length fnames - 1) (indices ~~ xs)
        |> map (map snd)


      val bnds' = bnds @ replicate (length spliteqs - length bnds) Binding.empty_atts

      (* using theorem names for case name currently disabled *)
      val case_names = map_index (fn (i, (_, es)) => mk_case_names i "" (length es)) 
        (bnds' ~~ spliteqs) |> flat
    in
      (flat spliteqs, restore_spec, sort, case_names)
    end
  else
    Function_Common.empty_preproc check_defs config ctxt fixes spec

val _ = Theory.setup (Context.theory_map (Function_Common.set_preproc sequential_preproc))



val fun_config = FunctionConfig { sequential=true, default=NONE,
  domintros=false, partials=false }

fun gen_add_fun add lthy =
  let
    fun pat_completeness_auto ctxt =
      Pat_Completeness.pat_completeness_tac ctxt 1
      THEN auto_tac ctxt
    fun prove_termination lthy =
      Function.prove_termination NONE (Function_Common.termination_prover_tac false lthy) lthy
  in
    lthy
    |> add pat_completeness_auto |> snd
    |> prove_termination |> snd
  end

fun add_fun a b c = gen_add_fun (Function.add_function a b c)
fun add_fun_cmd a b c int = gen_add_fun (fn tac => Function.add_function_cmd a b c tac int)



val _ =
  Outer_Syntax.local_theory' \<^command_keyword>\<open>fun\<close>
    "define general recursive functions (short version)"
    (function_parser fun_config
      >> (fn (config, (fixes, specs)) => add_fun_cmd fixes specs config))

end
