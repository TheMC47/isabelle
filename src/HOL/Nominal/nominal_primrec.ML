(*  Title:      HOL/Nominal/nominal_primrec.ML
    Author:     Norbert Voelker, FernUni Hagen
    Author:     Stefan Berghofer, TU Muenchen

Package for defining functions on nominal datatypes by primitive recursion.
Taken from HOL/Tools/primrec.ML
*)

signature NOMINAL_PRIMREC =
sig
  val primrec: term list option -> term option ->
    (binding * typ option * mixfix) list ->
    (binding * typ option * mixfix) list ->
    Specification.multi_specs -> local_theory -> Proof.state
  val primrec_cmd: string list option -> string option ->
    (binding * string option * mixfix) list ->
    (binding * string option * mixfix) list ->
    Specification.multi_specs_cmd -> local_theory -> Proof.state
end;

structure NominalPrimrec : NOMINAL_PRIMREC =
struct

exception RecError of string;

fun primrec_err s = error ("Nominal primrec definition error:\n" ^ s);
fun primrec_eq_err lthy s eq =
  primrec_err (s ^ "\nin\n" ^ quote (Syntax.string_of_term lthy eq));


(* preprocessing of equations *)

fun unquantify t =
  let
    val (vs, Ts) = split_list (strip_qnt_vars \<^const_name>\<open>Pure.all\<close> t);
    val body = strip_qnt_body \<^const_name>\<open>Pure.all\<close> t;
    val vs' = vs |> Name.variants (Name.make_context (fold_aterms
      (fn Free (v, _) => insert (op =) v | _ => I) body []))
  in (curry subst_bounds (map2 (curry Free) vs' Ts |> rev) body) end;

fun process_eqn lthy is_fixed spec rec_fns =
  let
    val eq = unquantify spec;
    val (lhs, rhs) =
      HOLogic.dest_eq (HOLogic.dest_Trueprop (Logic.strip_imp_concl eq))
      handle TERM _ => raise RecError "not a proper equation";

    val (recfun, args) = strip_comb lhs;
    val fname = case recfun of Free (v, _) => if is_fixed v then v
          else raise RecError "illegal head of function equation"
      | _ => raise RecError "illegal head of function equation";

    val (ls', rest)  = chop_prefix is_Free args;
    val (middle, rs') = chop_suffix is_Free rest;
    val rpos = length ls';

    val (constr, cargs') = if null middle then raise RecError "constructor missing"
      else strip_comb (hd middle);
    val (cname, T) = dest_Const constr
      handle TERM _ => raise RecError "ill-formed constructor";
    val tname = dest_Type_name (body_type T) handle TYPE _ =>
      raise RecError "cannot determine datatype associated with function"

    val (ls, cargs, rs) =
      (map dest_Free ls', map dest_Free cargs', map dest_Free rs')
      handle TERM _ => raise RecError "illegal argument in pattern";
    val lfrees = ls @ rs @ cargs;

    fun check_vars _ [] = ()
      | check_vars s vars = raise RecError (s ^ commas_quote (map fst vars))
  in
    if length middle > 1 then
      raise RecError "more than one non-variable in pattern"
    else
     (check_vars "repeated variable names in pattern: " (duplicates (op =) lfrees);
      check_vars "extra variables on rhs: "
        (map dest_Free (Misc_Legacy.term_frees rhs) |> subtract (op =) lfrees
          |> filter_out (is_fixed o fst));
      case AList.lookup (op =) rec_fns fname of
        NONE =>
          (fname, (tname, rpos, [(cname, (ls, cargs, rs, rhs, eq))]))::rec_fns
      | SOME (_, rpos', eqns) =>
          if AList.defined (op =) eqns cname then
            raise RecError "constructor already occurred as pattern"
          else if rpos <> rpos' then
            raise RecError "position of recursive argument inconsistent"
          else
            AList.update (op =)
              (fname, (tname, rpos, (cname, (ls, cargs, rs, rhs, eq))::eqns))
              rec_fns)
  end
  handle RecError s => primrec_eq_err lthy s spec;

val param_err = "Parameters must be the same for all recursive functions";

fun process_fun lthy descr eqns (i, fname) (fnames, fnss) =
  let
    val (_, (tname, _, constrs)) = nth descr i;

    (* substitute "fname ls x rs" by "y" for (x, (_, y)) in subs *)

    fun subst [] t fs = (t, fs)
      | subst subs (Abs (a, T, t)) fs =
          fs
          |> subst subs t
          |-> (fn t' => pair (Abs (a, T, t')))
      | subst subs (t as (_ $ _)) fs =
          let
            val (f, ts) = strip_comb t;
          in
            if is_Free f
              andalso member (fn ((v, _), (w, _)) => v = w) eqns (dest_Free f) then
              let
                val (fname', _) = dest_Free f;
                val (_, rpos, eqns') = the (AList.lookup (op =) eqns fname');
                val (ls, rs'') = chop rpos ts
                val (x', rs) = case rs'' of
                    x' :: rs => (x', rs)
                  | [] => raise RecError ("not enough arguments in recursive application\n"
                      ^ "of function " ^ quote fname' ^ " on rhs");
                val rs' = (case eqns' of
                    (_, (ls', _, rs', _, _)) :: _ =>
                      let val (rs1, rs2) = chop (length rs') rs
                      in
                        if ls = map Free ls' andalso rs1 = map Free rs' then rs2
                        else raise RecError param_err
                      end
                  | _ => raise RecError ("no equations for " ^ quote fname'));
                val (x, xs) = strip_comb x'
              in case AList.lookup (op =) subs x
               of NONE =>
                    fs
                    |> fold_map (subst subs) ts
                    |-> (fn ts' => pair (list_comb (f, ts')))
                | SOME (i', y) =>
                    fs
                    |> fold_map (subst subs) (xs @ rs')
                    ||> process_fun lthy descr eqns (i', fname')
                    |-> (fn ts' => pair (list_comb (y, ts')))
              end
            else
              fs
              |> fold_map (subst subs) (f :: ts)
              |-> (fn (f'::ts') => pair (list_comb (f', ts')))
          end
      | subst _ t fs = (t, fs);

    (* translate rec equations into function arguments suitable for rec comb *)

    fun trans eqns (cname, cargs) (fnames', fnss', fns) =
      (case AList.lookup (op =) eqns cname of
          NONE => (warning ("No equation for constructor " ^ quote cname ^
            "\nin definition of function " ^ quote fname);
              (fnames', fnss', (Const (\<^const_name>\<open>undefined\<close>, dummyT))::fns))
        | SOME (ls, cargs', rs, rhs, eq) =>
            let
              val recs = filter (Old_Datatype_Aux.is_rec_type o snd) (cargs' ~~ cargs);
              val rargs = map (rpair dummyT o fst o fst) recs;
              val subs = Term.variant_bounds rhs rargs;
              val (rhs', (fnames'', fnss'')) = subst (map2 (fn (x, y) => fn z =>
                (Free x, (Old_Datatype_Aux.body_index y, Free z))) recs subs) rhs (fnames', fnss')
                  handle RecError s => primrec_eq_err lthy s eq
            in (fnames'', fnss'', fold_rev absfree (cargs' @ subs) rhs' :: fns)
            end)

  in (case AList.lookup (op =) fnames i of
      NONE =>
        if exists (fn (_, v) => fname = v) fnames then
          raise RecError ("inconsistent functions for datatype " ^ quote tname)
        else
          let
            val SOME (_, _, eqns' as (_, (ls, _, rs, _, _)) :: _) =
              AList.lookup (op =) eqns fname;
            val (fnames', fnss', fns) = fold_rev (trans eqns') constrs
              ((i, fname)::fnames, fnss, [])
          in
            (fnames', (i, (fname, ls, rs, fns))::fnss')
          end
    | SOME fname' =>
        if fname = fname' then (fnames, fnss)
        else raise RecError ("inconsistent functions for datatype " ^ quote tname))
  end;


(* prepare functions needed for definitions *)

fun get_fns fns ((i : int, (tname, _, constrs)), rec_name) (fs, defs) =
  case AList.lookup (op =) fns i of
     NONE =>
       let
         val dummy_fns = map (fn (_, cargs) => Const (\<^const_name>\<open>undefined\<close>,
           replicate (length cargs + length (filter Old_Datatype_Aux.is_rec_type cargs))
             dummyT ---> HOLogic.unitT)) constrs;
         val _ = warning ("No function definition for datatype " ^ quote tname)
       in
         (dummy_fns @ fs, defs)
       end
   | SOME (fname, ls, rs, fs') => (fs' @ fs, (fname, ls, rs, rec_name, tname) :: defs);


(* make definition *)

fun make_def ctxt fixes fs (fname, ls, rs, rec_name, tname) =
  let
    val used = map fst (fold Term.add_frees fs []);
    val x = (singleton (Name.variant_list used) "x", dummyT);
    val frees = ls @ x :: rs;
    val raw_rhs = fold_rev absfree frees
      (list_comb (Const (rec_name, dummyT), fs @ [Free x]))
    val def_name = Thm.def_name (Long_Name.base_name fname);
    val rhs = singleton (Syntax.check_terms ctxt) raw_rhs;
    val SOME var = get_first (fn ((b, _), mx) =>
      if Binding.name_of b = fname then SOME (b, mx) else NONE) fixes;
  in
    ((var, ((Binding.name def_name, []), rhs)),
     subst_bounds (rev (map Free frees), strip_abs_body rhs))
  end;


(* find datatypes which contain all datatypes in tnames' *)

fun find_dts (dt_info : NominalDatatype.nominal_datatype_info Symtab.table) _ [] = []
  | find_dts dt_info tnames' (tname::tnames) =
      (case Symtab.lookup dt_info tname of
          NONE => primrec_err (quote tname ^ " is not a nominal datatype")
        | SOME dt =>
            if subset (op =) (tnames', map (#1 o snd) (#descr dt)) then
              (tname, dt)::(find_dts dt_info tnames' tnames)
            else find_dts dt_info tnames' tnames);

fun common_prefix eq ([], _) = []
  | common_prefix eq (_, []) = []
  | common_prefix eq (x :: xs, y :: ys) =
      if eq (x, y) then x :: common_prefix eq (xs, ys) else [];

local

fun gen_primrec prep_spec prep_term invs fctxt raw_fixes raw_params raw_spec lthy =
  let
    val (fixes', spec) = fst (prep_spec (raw_fixes @ raw_params) raw_spec lthy);
    val fixes = List.take (fixes', length raw_fixes);
    val (names_atts, spec') = split_list spec;
    val eqns' = map unquantify spec'
    val eqns = fold_rev (process_eqn lthy (fn v => Variable.is_fixed lthy v
      orelse exists (fn ((w, _), _) => v = Binding.name_of w) fixes)) spec' [];
    val dt_info = NominalDatatype.get_nominal_datatypes (Proof_Context.theory_of lthy);
    val lsrs :: lsrss = maps (fn (_, (_, _, eqns)) =>
      map (fn (_, (ls, _, rs, _, _)) => ls @ rs) eqns) eqns
    val _ =
      (if forall (curry (eq_set (op =)) lsrs) lsrss andalso forall
         (fn (_, (_, _, (_, (ls, _, rs, _, _)) :: eqns)) =>
               forall (fn (_, (ls', _, rs', _, _)) =>
                 ls = ls' andalso rs = rs') eqns
           | _ => true) eqns
       then () else primrec_err param_err);
    val tnames = distinct (op =) (map (#1 o snd) eqns);
    val dts = find_dts dt_info tnames tnames;
    val main_fns =
      map (fn (tname, {index, ...}) =>
        (index,
          (fst o the o find_first (fn (_, x) => #1 x = tname)) eqns))
      dts;
    val {descr, rec_names, rec_rewrites, ...} =
      if null dts then
        primrec_err ("datatypes " ^ commas_quote tnames ^ "\nare not mutually recursive")
      else snd (hd dts);
    val descr = map (fn (i, (tname, args, constrs)) => (i, (tname, args,
      map (fn (cname, cargs) => (cname, fold (fn (dTs, dT) => fn dTs' =>
        dTs' @ dTs @ [dT]) cargs [])) constrs))) descr;
    val (fnames, fnss) = fold_rev (process_fun lthy descr eqns) main_fns ([], []);
    val (fs, defs) = fold_rev (get_fns fnss) (descr ~~ rec_names) ([], []);
    val defs' = map (make_def lthy fixes fs) defs;
    val names1 = map snd fnames;
    val names2 = map fst eqns;
    val _ = if eq_set (op =) (names1, names2) then ()
      else primrec_err ("functions " ^ commas_quote names2 ^
        "\nare not mutually recursive");
    val (defs_thms, lthy') = lthy |>
      fold_map (apfst (snd o snd) oo Local_Theory.define o fst) defs';
    val qualify = Binding.qualify false
      (space_implode "_" (map (Long_Name.base_name o #1) defs));
    val names_atts' = map (apfst qualify) names_atts;

    fun mk_idx eq =
      let
        val Free (name, _) = head_of (fst (HOLogic.dest_eq (HOLogic.dest_Trueprop
          (Logic.strip_imp_concl eq))));
        val SOME i = AList.lookup op = (map swap fnames) name;
        val SOME (_, _, constrs) = AList.lookup op = descr i;
        val SOME (_, _, eqns'') = AList.lookup op = eqns name;
        val SOME (cname, (_, cargs, _, _, _)) = find_first
          (fn (_, (_, _, _, _, eq')) => eq = eq') eqns''
      in (i, find_index (fn (cname', _) => cname = cname') constrs, cargs) end;

    val rec_rewritess =
      unflat (map (fn (_, (_, _, constrs)) => constrs) descr) rec_rewrites;
    val fvars = rec_rewrites |> hd |> Thm.concl_of |> HOLogic.dest_Trueprop |>
      HOLogic.dest_eq |> fst |> strip_comb |> snd |> take_prefix is_Var;
    val (pvars, ctxtvars) = List.partition
      (equal HOLogic.boolT o body_type o snd)
      (subtract (op =)
        (Term.add_vars (Thm.concl_of (hd rec_rewrites)) [])
        (fold_rev (Term.add_vars o Logic.strip_assums_concl)
           (Thm.prems_of (hd rec_rewrites)) []));
    val cfs = defs' |> hd |> snd |> strip_comb |> snd |>
      curry (List.take o swap) (length fvars) |> map (Thm.cterm_of lthy');
    val invs' = (case invs of
        NONE => map (fn (i, _) =>
          Abs ("x", fastype_of (snd (nth defs' i)), \<^term>\<open>True\<close>)) descr
      | SOME invs' => map (prep_term lthy') invs');
    val inst = (map (#1 o dest_Var) fvars ~~ cfs) @
      (map #1 pvars ~~ map (Thm.cterm_of lthy') invs') @
      (case ctxtvars of
         [ctxtvar] => [(#1 ctxtvar,
           Thm.cterm_of lthy' (the_default HOLogic.unit (Option.map (prep_term lthy') fctxt)))]
       | _ => []);
    val rec_rewrites' = map (fn eq =>
      let
        val (i, j, cargs) = mk_idx eq
        val th = nth (nth rec_rewritess i) j;
        val cargs' = th |> Thm.concl_of |> HOLogic.dest_Trueprop |>
          HOLogic.dest_eq |> fst |> strip_comb |> snd |> List.last |>
          strip_comb |> snd
      in (cargs, Logic.strip_imp_prems eq,
        infer_instantiate lthy' (inst @
          (map (#1 o dest_Var) cargs' ~~ map (Thm.cterm_of lthy' o Free) cargs)) th)
      end) eqns';

    val prems = foldr1 (common_prefix op aconv) (map (Thm.prems_of o #3) rec_rewrites');
    val cprems = map (Thm.cterm_of lthy') prems;
    val asms = map Thm.assume cprems;
    val premss = map (fn (cargs, eprems, eqn) =>
      map (fn t => fold_rev (Logic.all o Free) cargs (Logic.list_implies (eprems, t)))
        (List.drop (Thm.prems_of eqn, length prems))) rec_rewrites';
    val cpremss = map (map (Thm.cterm_of lthy')) premss;
    val asmss = map (map Thm.assume) cpremss;

    fun mk_eqn ((cargs, eprems, eqn), asms') =
      let
        val ceprems = map (Thm.cterm_of lthy') eprems;
        val asms'' = map Thm.assume ceprems;
        val ccargs = map (Thm.cterm_of lthy' o Free) cargs;
        val asms''' = map (fn th => implies_elim_list
          (forall_elim_list ccargs th) asms'') asms'
      in
        implies_elim_list eqn (asms @ asms''') |>
        implies_intr_list ceprems |>
        forall_intr_list ccargs
      end;

    val rule_prems = cprems @ flat cpremss;
    val rule = implies_intr_list rule_prems
      (Conjunction.intr_balanced (map mk_eqn (rec_rewrites' ~~ asmss)));

    val goals = map (fn ((cargs, _, _), eqn) =>
      (fold_rev (Logic.all o Free) cargs eqn, [])) (rec_rewrites' ~~ eqns');

  in
    lthy' |>
    Variable.add_fixes (map fst lsrs) |> snd |>
    Proof.theorem NONE
      (fn thss => fn goal_ctxt =>
        let
          val simps = Proof_Context.export goal_ctxt lthy' (flat thss);
          val (simps', lthy'') =
            fold_map Local_Theory.note (names_atts' ~~ map single simps) lthy';
        in
          lthy''
          |> Local_Theory.note
            ((qualify (Binding.name "simps"), @{attributes [simp, nitpick_simp]}), maps snd simps')
          |> snd
        end)
      [goals] |>
    Proof.refine_singleton (Method.Basic (fn ctxt => fn _ =>
      CONTEXT_TACTIC
       (rewrite_goals_tac ctxt defs_thms THEN
        compose_tac ctxt (false, rule, length rule_prems) 1)))
  end;

in

val primrec = gen_primrec Specification.check_multi_specs (K I);
val primrec_cmd = gen_primrec Specification.read_multi_specs Syntax.read_term;

end;


(* outer syntax *)

val freshness_context = Parse.reserved "freshness_context";
val invariant = Parse.reserved "invariant";

fun unless_flag scan = Scan.unless ((freshness_context || invariant) -- \<^keyword>\<open>:\<close>) scan;

val parser1 = (freshness_context -- \<^keyword>\<open>:\<close>) |-- unless_flag Parse.term >> SOME;
val parser2 = (invariant -- \<^keyword>\<open>:\<close>) |--
    (Scan.repeat1 (unless_flag Parse.term) >> SOME) -- Scan.optional parser1 NONE ||
  (parser1 >> pair NONE);
val options =
  Scan.optional (\<^keyword>\<open>(\<close> |-- Parse.!!! (parser2 --| \<^keyword>\<open>)\<close>)) (NONE, NONE);

val _ =
  Outer_Syntax.local_theory_to_proof \<^command_keyword>\<open>nominal_primrec\<close>
    "define primitive recursive functions on nominal datatypes"
    (options -- Parse.vars -- Parse.for_fixes -- Parse_Spec.where_multi_specs
      >> (fn ((((invs, fctxt), vars), params), specs) =>
        primrec_cmd invs fctxt vars params specs));

end;
