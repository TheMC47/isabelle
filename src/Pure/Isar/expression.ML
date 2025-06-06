(*  Title:      Pure/Isar/expression.ML
    Author:     Clemens Ballarin, TU Muenchen

Locale expressions and user interface layer of locales.
*)

signature EXPRESSION =
sig
  (* Locale expressions *)
  datatype 'term map = Positional of 'term option list | Named of (string * 'term) list
  type 'term rewrites = (Attrib.binding * 'term) list
  type ('name, 'term) expr = ('name * ((string * bool) * ('term map * 'term rewrites))) list
  type expression_i = (string, term) expr * (binding * typ option * mixfix) list
  type expression = (xstring * Position.T, string) expr * (binding * string option * mixfix) list

  (* Processing of context statements *)
  val cert_statement: Element.context_i list -> Element.statement_i ->
    Proof.context -> (Attrib.binding * (term * term list) list) list * Proof.context
  val read_statement: Element.context list -> Element.statement ->
    Proof.context -> (Attrib.binding * (term * term list) list) list * Proof.context

  (* Declaring locales *)
  val cert_declaration: expression_i -> (Proof.context -> Proof.context) ->
    Element.context_i list ->
    Proof.context -> (((string * typ) * mixfix) list * (string * morphism) list
      * Element.context_i list * Proof.context) * ((string * typ) list * Proof.context)
  val cert_read_declaration: expression_i -> (Proof.context -> Proof.context) ->
    Element.context list ->
    Proof.context -> (((string * typ) * mixfix) list * (string * morphism) list
      * Element.context_i list * Proof.context) * ((string * typ) list * Proof.context)
      (*FIXME*)
  val read_declaration: expression -> (Proof.context -> Proof.context) -> Element.context list ->
    Proof.context -> (((string * typ) * mixfix) list * (string * morphism) list
      * Element.context_i list * Proof.context) * ((string * typ) list * Proof.context)
  val add_locale: binding -> binding -> Bundle.name list ->
    expression_i -> Element.context_i list -> theory -> string * local_theory
  val add_locale_cmd: binding -> binding -> Bundle.xname list ->
    expression -> Element.context list -> theory -> string * local_theory

  (* Processing of locale expressions *)
  val cert_goal_expression: expression_i -> Proof.context ->
    (term list list * term list list * (string * morphism) list * (Attrib.binding * term) list list * morphism) * Proof.context
  val read_goal_expression: expression -> Proof.context ->
    (term list list * term list list * (string * morphism) list * (Attrib.binding * term) list list * morphism) * Proof.context
end;

structure Expression : EXPRESSION =
struct

datatype ctxt = datatype Element.ctxt;


(*** Expressions ***)

datatype 'term map =
  Positional of 'term option list |
  Named of (string * 'term) list;

type 'term rewrites = (Attrib.binding * 'term) list;

type ('name, 'term) expr = ('name * ((string * bool) * ('term map * 'term rewrites))) list;

type expression_i = (string, term) expr * (binding * typ option * mixfix) list;
type expression = (xstring * Position.T, string) expr * (binding * string option * mixfix) list;


(** Internalise locale names in expr **)

fun check_expr ctxt instances = map (apfst (Locale.check ctxt)) instances;


(** Parameters of expression **)

(*Sanity check of instantiations and extraction of implicit parameters.
  The latter only occurs iff strict = false.
  Positional instantiations are extended to match full length of parameter list
  of instantiated locale.*)

fun parameters_of thy strict (expr, fixed) =
  let
    val ctxt = Proof_Context.init_global thy;

    fun reject_dups message xs =
      (case duplicates (op =) xs of
        [] => ()
      | dups => error (message ^ commas dups));

    fun parm_eq ((p1, mx1), (p2, mx2)) =
      p1 = p2 andalso
        (Mixfix.equal (mx1, mx2) orelse
          error ("Conflicting syntax for parameter " ^ quote p1 ^ " in expression" ^
            Position.here_list [Mixfix.pos_of mx1, Mixfix.pos_of mx2]));

    fun params_loc loc = Locale.params_of thy loc |> map (apfst #1);
    fun params_inst (loc, (prfx, (Positional insts, eqns))) =
          let
            val ps = params_loc loc;
            val d = length ps - length insts;
            val insts' =
              if d < 0 then
                error ("More arguments than parameters in instantiation of locale " ^
                  quote (Locale.markup_name ctxt loc))
              else insts @ replicate d NONE;
            val ps' = (ps ~~ insts') |>
              map_filter (fn (p, NONE) => SOME p | (_, SOME _) => NONE);
          in (ps', (loc, (prfx, (Positional insts', eqns)))) end
      | params_inst (loc, (prfx, (Named insts, eqns))) =
          let
            val _ =
              reject_dups "Duplicate instantiation of the following parameter(s): "
                (map fst insts);
            val ps' = (insts, params_loc loc) |-> fold (fn (p, _) => fn ps =>
              if AList.defined (op =) ps p then AList.delete (op =) p ps
              else error (quote p ^ " not a parameter of instantiated expression"));
          in (ps', (loc, (prfx, (Named insts, eqns)))) end;
    fun params_expr is =
      let
        val (is', ps') = fold_map (fn i => fn ps =>
          let
            val (ps', i') = params_inst i;
            val ps'' = distinct parm_eq (ps @ ps');
          in (i', ps'') end) is []
      in (ps', is') end;

    val (implicit, expr') = params_expr expr;

    val implicit' = map #1 implicit;
    val fixed' = map (Variable.check_name o #1) fixed;
    val _ = reject_dups "Duplicate fixed parameter(s): " fixed';
    val implicit'' =
      if strict then []
      else
        let
          val _ =
            reject_dups
              "Parameter(s) declared simultaneously in expression and for clause: "
              (implicit' @ fixed');
        in map (fn (x, mx) => (Binding.name x, NONE, mx)) implicit end;

  in (expr', implicit'' @ fixed) end;


(** Read instantiation **)

(* Parse positional or named instantiation *)

local

fun prep_inst prep_term ctxt parms (Positional insts) =
      (insts ~~ parms) |> map
        (fn (NONE, p) => Free (p, dummyT)
          | (SOME t, _) => prep_term ctxt t)
  | prep_inst prep_term ctxt parms (Named insts) =
      parms |> map (fn p =>
        (case AList.lookup (op =) insts p of
          SOME t => prep_term ctxt t |
          NONE => Free (p, dummyT)));

in

fun parse_inst x = prep_inst Syntax.parse_term x;
fun make_inst x = prep_inst (K I) x;

end;


(* Instantiation morphism *)

fun inst_morphism params ((prfx, mandatory), insts') ctxt =
  let
    (* parameters *)
    val parm_types = map #2 params;
    val type_parms = fold Term.add_tfreesT parm_types [];

    (* type inference *)
    val parm_types' = map (Type_Infer.paramify_vars o Logic.varifyT_global) parm_types;
    val type_parms' = fold Term.add_tvarsT parm_types' [];
    val checked =
      (map (Logic.mk_type o TVar) type_parms' @ map2 Type.constraint parm_types' insts')
      |> Syntax.check_terms (Config.put Type_Infer.object_logic false ctxt)
    val (type_parms'', insts'') = chop (length type_parms') checked;

    (* context *)
    val ctxt' = fold Proof_Context.augment checked ctxt;
    val certT = Thm.trim_context_ctyp o Thm.ctyp_of ctxt';
    val cert = Thm.trim_context_cterm o Thm.cterm_of ctxt';

    (* instantiation *)
    val instT =
      TFrees.build
        (fold2 (fn v => fn T => not (TFree v = T) ? TFrees.add (v, T))
          type_parms (map Logic.dest_type type_parms''));
    val cert_inst =
      Frees.build
        (fold2 (fn v => fn t => not (Free v = t) ? Frees.add (v, cert t))
          (map #1 params ~~ map (Term_Subst.instantiateT_frees instT) parm_types) insts'');
  in
    (Element.instantiate_normalize_morphism (TFrees.map (K certT) instT, cert_inst) $>
      Morphism.binding_morphism "Expression.inst" (Binding.prefix mandatory prfx), ctxt')
  end;


(*** Locale processing ***)

(** Parsing **)

fun parse_elem prep_typ prep_term ctxt =
  Element.map_ctxt
   {binding = I,
    typ = prep_typ ctxt,
    term = prep_term (Proof_Context.set_mode Proof_Context.mode_schematic ctxt),
    pattern = prep_term (Proof_Context.set_mode Proof_Context.mode_pattern ctxt),
    fact = I,
    attrib = I};

fun prepare_stmt prep_prop prep_obtains ctxt stmt =
  (case stmt of
    Element.Shows raw_shows =>
      raw_shows |> (map o apsnd o map) (fn (t, ps) =>
        (prep_prop (Proof_Context.set_mode Proof_Context.mode_schematic ctxt) t,
          map (prep_prop (Proof_Context.set_mode Proof_Context.mode_pattern ctxt)) ps))
  | Element.Obtains raw_obtains =>
      let
        val ((_, thesis), thesis_ctxt) = Obtain.obtain_thesis ctxt;
        val obtains = prep_obtains thesis_ctxt thesis raw_obtains;
      in map (fn (b, t) => ((b, []), [(t, [])])) obtains end);


(** Simultaneous type inference: instantiations + elements + statement **)

local

fun mk_type T = (Logic.mk_type T, []);
fun mk_term t = (t, []);
fun mk_propp (p, pats) = (Type.constraint propT p, pats);

fun dest_type (T, []) = Logic.dest_type T;
fun dest_term (t, []) = t;
fun dest_propp (p, pats) = (p, pats);

fun extract_inst (_, (_, ts)) = map mk_term ts;
fun restore_inst ((l, (p, _)), cs) = (l, (p, map dest_term cs));

fun extract_eqns es = map (mk_term o snd) es;
fun restore_eqns (es, cs) = map2 (fn (b, _) => fn c => (b, dest_term c)) es cs;

fun extract_elem (Fixes fixes) = map (#2 #> the_list #> map mk_type) fixes
  | extract_elem (Constrains csts) = map (#2 #> single #> map mk_type) csts
  | extract_elem (Assumes asms) = map (#2 #> map mk_propp) asms
  | extract_elem (Defines defs) = map (fn (_, (t, ps)) => [mk_propp (t, ps)]) defs
  | extract_elem (Notes _) = []
  | extract_elem (Lazy_Notes _) = [];

fun restore_elem (Fixes fixes, css) =
      (fixes ~~ css) |> map (fn ((x, _, mx), cs) =>
        (x, cs |> map dest_type |> try hd, mx)) |> Fixes
  | restore_elem (Constrains csts, css) =
      (csts ~~ css) |> map (fn ((x, _), cs) =>
        (x, cs |> map dest_type |> hd)) |> Constrains
  | restore_elem (Assumes asms, css) =
      (asms ~~ css) |> map (fn ((b, _), cs) => (b, map dest_propp cs)) |> Assumes
  | restore_elem (Defines defs, css) =
      (defs ~~ css) |> map (fn ((b, _), [c]) => (b, dest_propp c)) |> Defines
  | restore_elem (elem as Notes _, _) = elem
  | restore_elem (elem as Lazy_Notes _, _) = elem;

fun prep (_, pats) (ctxt, t :: ts) =
  let val ctxt' = Proof_Context.augment t ctxt
  in
    ((t, Syntax.check_props (Proof_Context.set_mode Proof_Context.mode_pattern ctxt') pats),
      (ctxt', ts))
  end;

fun check cs ctxt =
  let
    val (cs', (ctxt', _)) = fold_map prep cs
      (ctxt, Syntax.check_terms
        (Proof_Context.set_mode Proof_Context.mode_schematic ctxt) (map fst cs));
  in (cs', ctxt') end;

in

fun check_autofix insts eqnss elems concl ctxt =
  let
    val inst_cs = map extract_inst insts;
    val eqns_cs = map extract_eqns eqnss;
    val elem_css = map extract_elem elems;
    val concl_cs = (map o map) mk_propp (map snd concl);
    (* Type inference *)
    val (inst_cs' :: eqns_cs' :: css', ctxt') =
      (fold_burrow o fold_burrow) check (inst_cs :: eqns_cs :: elem_css @ [concl_cs]) ctxt;
    val (elem_css', [concl_cs']) = chop (length elem_css) css';
  in
    ((map restore_inst (insts ~~ inst_cs'),
      map restore_eqns (eqnss ~~ eqns_cs'),
      map restore_elem (elems ~~ elem_css'),
      map fst concl ~~ concl_cs'), ctxt')
  end;

end;


(** Prepare locale elements **)

fun declare_elem prep_var (Fixes fixes) ctxt =
      let val (vars, _) = fold_map prep_var fixes ctxt
      in ctxt |> Proof_Context.add_fixes vars |> snd end
  | declare_elem prep_var (Constrains csts) ctxt =
      ctxt |> fold_map (fn (x, T) => prep_var (Binding.name x, SOME T, NoSyn)) csts |> snd
  | declare_elem _ (Assumes _) ctxt = ctxt
  | declare_elem _ (Defines _) ctxt = ctxt
  | declare_elem _ (Notes _) ctxt = ctxt
  | declare_elem _ (Lazy_Notes _) ctxt = ctxt;


(** Finish locale elements **)

fun finish_inst ctxt (loc, (prfx, inst)) =
  let
    val thy = Proof_Context.theory_of ctxt;
    val (morph, _) = inst_morphism (map #1 (Locale.params_of thy loc)) (prfx, inst) ctxt;
  in (loc, morph) end;

fun finish_fixes (parms: (string * typ) list) = map (fn (binding, _, mx) =>
  let val x = Binding.name_of binding
  in (binding, AList.lookup (op =) parms x, mx) end);

local

fun closeup _ _ false elem = elem
  | closeup (outer_ctxt, ctxt) parms true elem =
      let
        (* FIXME consider closing in syntactic phase -- before type checking *)
        fun close_frees t =
          let
            val rev_frees =
              Term.fold_aterms (fn Free (x, T) =>
                if Variable.is_fixed outer_ctxt x orelse AList.defined (op =) parms x then I
                else insert (op =) (x, T) | _ => I) t [];
          in fold (Logic.all o Free) rev_frees t end;

        fun no_binds [] = []
          | no_binds _ = error "Illegal term bindings in context element";
      in
        (case elem of
          Assumes asms => Assumes (asms |> map (fn (a, propps) =>
            (a, map (fn (t, ps) => (close_frees t, no_binds ps)) propps)))
        | Defines defs => Defines (defs |> map (fn ((name, atts), (t, ps)) =>
            let val ((c, _), t') = Local_Defs.cert_def ctxt (K []) (close_frees t)
            in ((Thm.def_binding_optional (Binding.name c) name, atts), (t', no_binds ps)) end))
        | e => e)
      end;

in

fun finish_elem _ parms _ (Fixes fixes) = Fixes (finish_fixes parms fixes)
  | finish_elem _ _ _ (Constrains _) = Constrains []
  | finish_elem ctxts parms do_close (Assumes asms) = closeup ctxts parms do_close (Assumes asms)
  | finish_elem ctxts parms do_close (Defines defs) = closeup ctxts parms do_close (Defines defs)
  | finish_elem _ _ _ (elem as Notes _) = elem
  | finish_elem _ _ _ (elem as Lazy_Notes _) = elem;

end;


(** Process full context statement: instantiations + elements + statement **)

(* Interleave incremental parsing and type inference over entire parsed stretch. *)

local

fun abs_def ctxt =
  Thm.cterm_of ctxt #> Assumption.assume ctxt #> Local_Defs.abs_def_rule ctxt #> Thm.prop_of;

fun prep_full_context_statement
    parse_typ parse_prop prep_obtains prep_var_elem prep_inst prep_eqns prep_attr prep_var_inst prep_expr
    {strict, do_close, fixed_frees} raw_import init_body raw_elems raw_stmt ctxt1 =
  let
    val thy = Proof_Context.theory_of ctxt1;

    val (raw_insts, fixed) = parameters_of thy strict (apfst (prep_expr ctxt1) raw_import);

    fun prep_insts_cumulative (loc, (prfx, (inst, eqns))) (i, insts, eqnss, ctxt) =
      let
        val params = map #1 (Locale.params_of thy loc);
        val inst' = prep_inst ctxt (map #1 params) inst;
        val parm_types' =
          params |> map (#2 #> Logic.varifyT_global #>
              Term.map_type_tvar (fn ((x, _), S) => TVar ((x, i), S)) #>
              Type_Infer.paramify_vars);
        val inst'' = map2 Type.constraint parm_types' inst';
        val insts' = insts @ [(loc, (prfx, inst''))];
        val ((insts'', _, _, _), ctxt2) = check_autofix insts' [] [] [] ctxt;
        val inst''' = insts'' |> List.last |> snd |> snd;
        val (inst_morph, _) = inst_morphism params (prfx, inst''') ctxt;
        val ctxt' = Locale.activate_declarations (loc, inst_morph) ctxt2
          handle ERROR msg => if null eqns then error msg else
            (Locale.tracing ctxt1
             (msg ^ "\nFalling back to reading rewrites clause before activation.");
             ctxt2);

        val attrss = map (apsnd (map (prep_attr ctxt)) o fst) eqns;
        val eqns' = (prep_eqns ctxt' o map snd) eqns;
        val eqnss' = [attrss ~~ eqns'];
        val ((_, [eqns''], _, _), _) = check_autofix insts'' eqnss' [] [] ctxt';
        val rewrite_morph = eqns'
          |> map (abs_def ctxt')
          |> Variable.export_terms ctxt' ctxt
          |> Element.eq_term_morphism ctxt
          |> Morphism.default;
       val ctxt'' = Locale.activate_declarations (loc, inst_morph $> rewrite_morph) ctxt;
       val eqnss' = eqnss @ [attrss ~~ Variable.export_terms ctxt' ctxt eqns'];
      in (i + 1, insts', eqnss', ctxt'') end;

    fun prep_elem raw_elem ctxt =
      let
        val ctxt' = ctxt
          |> Context_Position.set_visible false
          |> declare_elem prep_var_elem raw_elem
          |> Context_Position.restore_visible ctxt;
        val elems' = parse_elem parse_typ parse_prop ctxt' raw_elem;
      in (elems', ctxt') end;

    val fors = fold_map prep_var_inst fixed ctxt1 |> fst;
    val ctxt2 = ctxt1 |> Proof_Context.add_fixes fors |> snd;
    val (_, insts', eqnss', ctxt3) = fold prep_insts_cumulative raw_insts (0, [], [], ctxt2);

    fun prep_stmt elems ctxt =
      check_autofix insts' [] elems (prepare_stmt parse_prop prep_obtains ctxt raw_stmt) ctxt;

    val _ =
      if fixed_frees then ()
      else
        (case fold (fold (Variable.add_frees ctxt3) o snd o snd) insts' [] of
          [] => ()
        | frees => error ("Illegal free variables in expression: " ^
            commas_quote (map (Syntax.string_of_term ctxt3 o Free) (rev frees))));

    val ((insts, _, elems', concl), ctxt4) = ctxt3
      |> init_body
      |> fold_map prep_elem raw_elems
      |-> prep_stmt;


    (* parameters from expression and elements *)

    val xs = maps (fn Fixes fixes => map (Variable.check_name o #1) fixes | _ => [])
      (Fixes fors :: elems');
    val (parms, ctxt5) = fold_map Proof_Context.inferred_param xs ctxt4;

    val fors' = finish_fixes parms fors;
    val fixed = map (fn (b, SOME T, mx) => ((Binding.name_of b, T), mx)) fors';
    val deps = map (finish_inst ctxt5) insts;
    val elems'' = map (finish_elem (ctxt1, ctxt5) parms do_close) elems';

  in ((fixed, deps, eqnss', elems'', concl), (parms, ctxt5)) end;

in

fun cert_full_context_statement x =
  prep_full_context_statement (K I) (K I) Obtain.cert_obtains
    Proof_Context.cert_var make_inst Syntax.check_props (K I) Proof_Context.cert_var (K I) x;

fun cert_read_full_context_statement x =
  prep_full_context_statement Syntax.parse_typ Syntax.parse_prop Obtain.parse_obtains
    Proof_Context.read_var make_inst Syntax.check_props (K I) Proof_Context.cert_var (K I) x;

fun read_full_context_statement x =
  prep_full_context_statement Syntax.parse_typ Syntax.parse_prop Obtain.parse_obtains
    Proof_Context.read_var parse_inst Syntax.read_props Attrib.check_src Proof_Context.read_var check_expr x;

end;


(* Context statement: elements + statement *)

local

fun prep_statement prep activate raw_elems raw_stmt ctxt =
  let
    val ((_, _, _, elems, concl), _) =
      prep {strict = true, do_close = false, fixed_frees = true}
        ([], []) I raw_elems raw_stmt ctxt;
    val ctxt' = ctxt
      |> Proof_Context.set_stmt true
      |> fold_map activate elems |> #2
      |> Proof_Context.restore_stmt ctxt;
  in (concl, ctxt') end;

in

fun cert_statement x = prep_statement cert_full_context_statement Element.activate_i x;
fun read_statement x = prep_statement read_full_context_statement Element.activate x;

end;


(* Locale declaration: import + elements *)

fun fix_params params =
  Proof_Context.add_fixes (map (fn ((x, T), mx) => (Binding.name x, SOME T, mx)) params) #> snd;

local

fun prep_declaration prep activate raw_import init_body raw_elems ctxt =
  let
    val ((fixed, deps, eqnss, elems, _), (parms, ctxt0)) =
      prep {strict = false, do_close = true, fixed_frees = false}
        raw_import init_body raw_elems (Element.Shows []) ctxt;
    val _ = null (flat eqnss) orelse error "Illegal rewrites clause(s) in declaration of locale";
    (* Declare parameters and imported facts *)
    val ctxt' = ctxt
      |> fix_params fixed
      |> fold (Context.proof_map o Locale.activate_facts NONE) deps;
    val (elems', ctxt'') = ctxt'
      |> Proof_Context.set_stmt true
      |> fold_map activate elems
      ||> Proof_Context.restore_stmt ctxt';
  in ((fixed, deps, elems', ctxt''), (parms, ctxt0)) end;

in

fun cert_declaration x = prep_declaration cert_full_context_statement Element.activate_i x;
fun cert_read_declaration x = prep_declaration cert_read_full_context_statement Element.activate x;
fun read_declaration x = prep_declaration read_full_context_statement Element.activate x;

end;


(* Locale expression to set up a goal *)

local

fun props_of thy (name, morph) =
  let val (asm, defs) = Locale.specification_of thy name
  in map (Morphism.term morph) (the_list asm @ defs) end;

fun prep_goal_expression prep expression ctxt =
  let
    val thy = Proof_Context.theory_of ctxt;

    val ((fixed, deps, eqnss, _, _), _) =
      prep {strict = true, do_close = true, fixed_frees = true} expression I []
        (Element.Shows []) ctxt;
    (* proof obligations *)
    val propss = map (props_of thy) deps;
    val eq_propss = (map o map) snd eqnss;

    val goal_ctxt = ctxt
      |> fix_params fixed
      |> (fold o fold) Proof_Context.augment (propss @ eq_propss);

    val export = Proof_Context.export_morphism goal_ctxt ctxt;
    val exp_fact = Drule.zero_var_indexes_list o map Thm.strip_shyps o Morphism.fact export;
    val exp_term = Term_Subst.zero_var_indexes o Morphism.term export;
    val exp_typ = Logic.type_map exp_term;
    val export' =
      Morphism.morphism "Expression.prep_goal"
        {binding = [], typ = [K exp_typ], term = [K exp_term], fact = [K exp_fact]};
  in ((propss, eq_propss, deps, eqnss, export'), goal_ctxt) end;

in

fun cert_goal_expression x = prep_goal_expression cert_full_context_statement x;
fun read_goal_expression x = prep_goal_expression read_full_context_statement x;

end;


(*** Locale declarations ***)

(* extract specification text *)

val norm_term = Envir.beta_norm oo Term.subst_atomic;

fun bind_def ctxt eq (env, eqs) =
  let
    val _ = Local_Defs.cert_def ctxt (K []) eq;
    val ((y, T), b) = Local_Defs.abs_def eq;
    val b' = norm_term env b;
    fun err msg = error (msg ^ ": " ^ quote y);
  in
    (case filter (fn (Free (y', _), _) => y = y' | _ => false) env of
      [] => ((Free (y, T), b') :: env, eq :: eqs)
    | dups =>
        if forall (fn (_, b'') => b' aconv b'') dups then (env, eqs)
        else err "Attempt to redefine variable")
  end;

(* text has the following structure:
       (((exts, exts'), (ints, ints')), (xs, env, defs))
   where
     exts: external assumptions (terms in assumes elements)
     exts': dito, normalised wrt. env
     ints: internal assumptions (terms in assumptions from insts)
     ints': dito, normalised wrt. env
     xs: the free variables in exts' and ints' and rhss of definitions,
       this includes parameters except defined parameters
     env: list of term pairs encoding substitutions, where the first term
       is a free variable; substitutions represent defines elements and
       the rhs is normalised wrt. the previous env
     defs: the equations from the defines elements
   *)

fun eval_text _ _ (Fixes _) text = text
  | eval_text _ _ (Constrains _) text = text
  | eval_text _ is_ext (Assumes asms)
        (((exts, exts'), (ints, ints')), (env, defs)) =
      let
        val ts = maps (map #1 o #2) asms;
        val ts' = map (norm_term env) ts;
        val spec' =
          if is_ext then ((exts @ ts, exts' @ ts'), (ints, ints'))
          else ((exts, exts'), (ints @ ts, ints' @ ts'));
      in (spec', (env, defs)) end
  | eval_text ctxt _ (Defines defs) (spec, binds) =
      (spec, fold (bind_def ctxt o #1 o #2) defs binds)
  | eval_text _ _ (Notes _) text = text
  | eval_text _ _ (Lazy_Notes _) text = text;

fun eval_inst ctxt (loc, morph) text =
  let
    val thy = Proof_Context.theory_of ctxt;
    val (asm, defs) = Locale.specification_of thy loc;
    val asm' = Option.map (Morphism.term morph) asm;
    val defs' = map (Morphism.term morph) defs;
    val text' =
      text |>
       (if is_some asm then
          eval_text ctxt false (Assumes [(Binding.empty_atts, [(the asm', [])])])
        else I) |>
       (if not (null defs) then
          eval_text ctxt false (Defines (map (fn def => (Binding.empty_atts, (def, []))) defs'))
        else I)
(* FIXME clone from locale.ML *)
  in text' end;

fun eval_elem ctxt elem text =
  eval_text ctxt true elem text;

fun eval ctxt deps elems =
  let
    val text' = fold (eval_inst ctxt) deps ((([], []), ([], [])), ([], []));
    val ((spec, (_, defs))) = fold (eval_elem ctxt) elems text';
  in (spec, defs) end;

(* axiomsN: name of theorem set with destruct rules for locale predicates,
     also name suffix of delta predicates and assumptions. *)

val axiomsN = "axioms";

local

(* introN: name of theorems for introduction rules of locale and
     delta predicates *)

val introN = "intro";

fun atomize_spec ctxt ts =
  let
    val t = Logic.mk_conjunction_balanced ts;
    val body = Object_Logic.atomize_term ctxt t;
    val bodyT = Term.fastype_of body;
  in
    if bodyT = propT
    then (t, propT, Thm.reflexive (Thm.cterm_of ctxt t))
    else (body, bodyT, Object_Logic.atomize ctxt (Thm.cterm_of ctxt t))
  end;

(* achieve plain syntax for locale predicates (without "PROP") *)

fun aprop_tr' n c =
  let
    val c' = Lexicon.mark_const c;
    fun tr' (_: Proof.context) T args =
      if T <> dummyT andalso length args = n
      then Syntax.const "_aprop" $ Term.list_comb (Syntax.const c', args)
      else raise Match;
  in (c', tr') end;

(* define one predicate including its intro rule and axioms
   - binding: predicate name
   - parms: locale parameters
   - defs: thms representing substitutions from defines elements
   - ts: terms representing locale assumptions (not normalised wrt. defs)
   - norm_ts: terms representing locale assumptions (normalised wrt. defs)
   - thy: the theory
*)

fun def_pred binding parms defs ts norm_ts thy =
  let
    val name = Sign.full_name thy binding;

    val thy_ctxt = Proof_Context.init_global thy;

    val (body, bodyT, body_eq) = atomize_spec thy_ctxt norm_ts;
    val env = Names.build (Names.add_free_names body);
    val xs = filter (Names.defined env o #1) parms;
    val Ts = map #2 xs;
    val type_tfrees = TFrees.build (fold TFrees.add_tfreesT Ts);
    val extra_tfrees =
      TFrees.build (TFrees.add_tfrees_unless (TFrees.defined type_tfrees) body)
      |> TFrees.keys |> map TFree;
    val predT = map Term.itselfT extra_tfrees ---> Ts ---> bodyT;

    val args = map Logic.mk_type extra_tfrees @ map Free xs;
    val head = Term.list_comb (Const (name, predT), args);
    val statement = Object_Logic.ensure_propT thy_ctxt head;

    val (pred_def, defs_thy) =
      thy
      |> bodyT = propT ? Sign.typed_print_translation [aprop_tr' (length args) name]
      |> Sign.declare_const_global ((binding, predT), NoSyn) |> snd
      |> Global_Theory.add_def (Thm.def_binding binding, Logic.mk_equals (head, body));
    val defs_ctxt = Proof_Context.init_global defs_thy |> Variable.declare_term head;

    val intro = Goal.prove_global defs_thy [] norm_ts statement
      (fn {context = ctxt, ...} =>
        rewrite_goals_tac ctxt [pred_def] THEN
        compose_tac defs_ctxt (false, body_eq RS Drule.equal_elim_rule1, 1) 1 THEN
        compose_tac defs_ctxt
          (false,
            Conjunction.intr_balanced (map (Thm.assume o Thm.cterm_of defs_ctxt) norm_ts), 0) 1);

    val conjuncts =
      (Drule.equal_elim_rule2 OF
        [body_eq, rewrite_rule defs_ctxt [pred_def] (Thm.assume (Thm.cterm_of defs_ctxt statement))])
      |> Conjunction.elim_balanced (length ts);

    val (_, axioms_ctxt) = defs_ctxt
      |> Assumption.add_assumes (maps Thm.chyps_of (defs @ conjuncts));
    val axioms = ts ~~ conjuncts |> map (fn (t, ax) =>
      Element.prove_witness axioms_ctxt t
       (rewrite_goals_tac axioms_ctxt defs THEN compose_tac axioms_ctxt (false, ax, 0) 1));
  in ((statement, intro, axioms), defs_thy) end;

in

(* main predicate definition function *)

fun define_preds binding parms (((exts, exts'), (ints, ints')), defs) thy =
  let
    val ctxt = Proof_Context.init_global thy;
    val defs' = map (Thm.cterm_of ctxt #> Assumption.assume ctxt #> Drule.abs_def) defs;

    val (a_pred, a_intro, a_axioms, thy2) =
      if null exts then (NONE, NONE, [], thy)
      else
        let
          val abinding =
            if null ints then binding else Binding.suffix_name ("_" ^ axiomsN) binding;
          val ((statement, intro, axioms), thy1) =
            thy
            |> def_pred abinding parms defs' exts exts';
          val ((_, [intro']), thy2) =
            thy1
            |> Sign.qualified_path true abinding
            |> Global_Theory.note_thms ""
              ((Binding.name introN, []), [([intro], [Locale.unfold_add])])
            ||> Sign.restore_naming thy1;
          in (SOME statement, SOME intro', axioms, thy2) end;
    val (b_pred, b_intro, b_axioms, thy4) =
      if null ints then (NONE, NONE, [], thy2)
      else
        let
          val ((statement, intro, axioms), thy3) =
            thy2
            |> def_pred binding parms defs' (ints @ the_list a_pred) (ints' @ the_list a_pred);
          val conclude_witness =
            Drule.export_without_context o Element.conclude_witness (Proof_Context.init_global thy3);
          val ([(_, [intro']), _], thy4) =
            thy3
            |> Sign.qualified_path true binding
            |> Global_Theory.note_thmss ""
                 [((Binding.name introN, []), [([intro], [Locale.intro_add])]),
                  ((Binding.name axiomsN, []),
                    [(map conclude_witness axioms, [])])]
            ||> Sign.restore_naming thy3;
        in (SOME statement, SOME intro', axioms, thy4) end;
  in ((a_pred, a_intro, a_axioms), (b_pred, b_intro, b_axioms), thy4) end;

end;


local

fun assumes_to_notes (Assumes asms) axms =
      fold_map (fn (a, spec) => fn axs =>
          let val (ps, qs) = chop (length spec) axs
          in ((a, [(ps, [])]), qs) end) asms axms
      |> apfst (curry Notes "")
  | assumes_to_notes e axms = (e, axms);

fun defines_to_notes ctxt (Defines defs) =
      Notes ("", map (fn (a, (def, _)) =>
        (a, [([Assumption.assume ctxt (Thm.cterm_of ctxt def)],
          [Attrib.internal \<^here> (K Locale.witness_add)])])) defs)
  | defines_to_notes _ e = e;

val is_hyp = fn Assumes _ => true | Defines _ => true | _ => false;

fun gen_add_locale prep_include prep_decl
    binding raw_predicate_binding raw_includes raw_import raw_body thy =
  let
    val name = Sign.full_name thy binding;
    val _ = Locale.defined thy name andalso
      error ("Duplicate definition of locale " ^ quote name);

    val ctxt = Proof_Context.init_global thy;
    val includes = map (prep_include ctxt) raw_includes;

    val ((fixed, deps, body_elems, _), (parms, ctxt')) =
      ctxt
      |> Bundle.includes includes
      |> prep_decl raw_import I raw_body;
    val text as (((_, exts'), _), defs) = eval ctxt' deps body_elems;

    val type_tfrees = TFrees.build (fold (TFrees.add_tfreesT o #2) parms);
    val extra_tfrees =
      TFrees.build (fold (TFrees.add_tfrees_unless (TFrees.defined type_tfrees)) exts')
      |> TFrees.keys;
    val _ =
      if null extra_tfrees then ()
      else warning ("Additional type variable(s) in locale specification " ^
          Binding.print binding ^ ": " ^
          commas (map (Syntax.string_of_typ ctxt' o TFree) extra_tfrees));

    val predicate_binding =
      if Binding.is_empty raw_predicate_binding then binding
      else raw_predicate_binding;
    val ((a_statement, a_intro, a_axioms), (b_statement, b_intro, b_axioms), thy') =
      define_preds predicate_binding parms text thy;
    val pred_ctxt = Proof_Context.init_global thy';

    val a_satisfy = Element.satisfy_morphism a_axioms;
    val b_satisfy = Element.satisfy_morphism b_axioms;

    val params = fixed @
      maps (fn Fixes fixes =>
        map (fn (b, SOME T, mx) => ((Binding.name_of b, T), mx)) fixes | _ => []) body_elems;
    val asm = if is_some b_statement then b_statement else a_statement;

    val hyp_spec = filter is_hyp body_elems;

    val notes =
      if is_some asm then
        [("", [((Binding.suffix_name ("_" ^ axiomsN) binding, []),
          [([Assumption.assume pred_ctxt (Thm.cterm_of pred_ctxt (the asm))],
            [Attrib.internal \<^here> (K Locale.witness_add)])])])]
      else [];

    val notes' =
      body_elems
      |> map (Element.transfer_ctxt thy')
      |> map (defines_to_notes pred_ctxt)
      |> map (Element.transform_ctxt a_satisfy)
      |> (fn elems =>
        fold_map assumes_to_notes elems (map (Element.conclude_witness pred_ctxt) a_axioms))
      |> fst
      |> map (Element.transform_ctxt b_satisfy)
      |> map_filter (fn Notes notes => SOME notes | _ => NONE);

    val deps' = map (fn (l, morph) => (l, morph $> b_satisfy)) deps;
    val axioms = map (Element.conclude_witness pred_ctxt) b_axioms;

    val loc_ctxt = thy'
      |> Locale.register_locale binding (extra_tfrees, params)
          (asm, rev defs) (a_intro, b_intro) axioms hyp_spec [] (rev notes) (rev deps')
      |> Named_Target.init includes name
      |> fold (fn (kind, facts) => Local_Theory.notes_kind kind facts #> snd) notes';

  in (name, loc_ctxt) end;

in

val add_locale = gen_add_locale (K I) cert_declaration;
val add_locale_cmd = gen_add_locale Bundle.check_name read_declaration;

end;

end;
