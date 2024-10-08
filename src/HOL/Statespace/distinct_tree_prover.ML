(*  Title:      HOL/Statespace/distinct_tree_prover.ML
    Author:     Norbert Schirmer, TU Muenchen, 2007
    Author:     Norbert Schirmer, Apple, 2021
*)

signature DISTINCT_TREE_PROVER =
sig
  datatype direction = Left | Right
  val mk_tree : ('a -> term) -> typ -> 'a list -> term
  val dest_tree : term -> term list
  val find_tree : term -> term -> direction list option

  val in_set: Proof.context -> direction list -> cterm -> thm
  val find_in_set: Proof.context -> term -> cterm -> thm
  val neq_to_eq_False : thm
  val distinctTreeProver : Proof.context -> thm -> direction list -> direction list -> thm
  val neq_x_y : Proof.context -> term -> term -> string -> thm option
  val distinctFieldSolver : string list -> solver
  val distinctTree_tac : string list -> Proof.context -> int -> tactic
  val distinct_implProver : Proof.context -> thm -> cterm -> thm
  val subtractProver : Proof.context -> term -> cterm -> thm -> thm
  val distinct_simproc : string list -> simproc

  val discharge : Proof.context -> thm list -> thm -> thm
end;

structure DistinctTreeProver : DISTINCT_TREE_PROVER =
struct

val neq_to_eq_False = @{thm neq_to_eq_False};

datatype direction = Left | Right;

fun treeT T = Type (\<^type_name>\<open>tree\<close>, [T]);

fun mk_tree' e T n [] = Const (\<^const_name>\<open>Tip\<close>, treeT T)
  | mk_tree' e T n xs =
     let
       val m = (n - 1) div 2;
       val (xsl,x::xsr) = chop m xs;
       val l = mk_tree' e T m xsl;
       val r = mk_tree' e T (n-(m+1)) xsr;
     in
       Const (\<^const_name>\<open>Node\<close>, treeT T --> T --> HOLogic.boolT--> treeT T --> treeT T) $
         l $ e x $ \<^term>\<open>False\<close> $ r
     end

fun mk_tree e T xs = mk_tree' e T (length xs) xs;

fun dest_tree (Const (\<^const_name>\<open>Tip\<close>, _)) = []
  | dest_tree (Const (\<^const_name>\<open>Node\<close>, _) $ l $ e $ _ $ r) = dest_tree l @ e :: dest_tree r
  | dest_tree t = raise TERM ("dest_tree", [t]);



fun lin_find_tree e (Const (\<^const_name>\<open>Tip\<close>, _)) = NONE
  | lin_find_tree e (Const (\<^const_name>\<open>Node\<close>, _) $ l $ x $ _ $ r) =
      if e aconv x
      then SOME []
      else
        (case lin_find_tree e l of
          SOME path => SOME (Left :: path)
        | NONE =>
            (case lin_find_tree e r of
              SOME path => SOME (Right :: path)
            | NONE => NONE))
  | lin_find_tree e t = raise TERM ("find_tree: input not a tree", [t])

fun bin_find_tree order e (Const (\<^const_name>\<open>Tip\<close>, _)) = NONE
  | bin_find_tree order e (Const (\<^const_name>\<open>Node\<close>, _) $ l $ x $ _ $ r) =
      (case order (e, x) of
        EQUAL => SOME []
      | LESS => Option.map (cons Left) (bin_find_tree order e l)
      | GREATER => Option.map (cons Right) (bin_find_tree order e r))
  | bin_find_tree order e t = raise TERM ("find_tree: input not a tree", [t])

fun find_tree e t =
  (case bin_find_tree Term_Ord.fast_term_ord e t of
    NONE => lin_find_tree e t
  | x => x);


fun split_common_prefix xs [] = ([], xs, [])
  | split_common_prefix [] ys = ([], [], ys)
  | split_common_prefix (xs as (x :: xs')) (ys as (y :: ys')) =
      if x = y
      then let val (ps, xs'', ys'') = split_common_prefix xs' ys' in (x :: ps, xs'', ys'') end
      else ([], xs, ys)


(* Wrapper around Thm.instantiate. The type instiations of instTs are applied to
 * the right hand sides of insts
 *)
fun instantiate ctxt instTs insts =
  let
    val instTs' = map (fn (T, U) => (dest_TVar (Thm.typ_of T), Thm.typ_of U)) instTs;
    fun substT x = (case AList.lookup (op =) instTs' x of NONE => TVar x | SOME T' => T');
    fun mapT_and_recertify ct =
      (Thm.cterm_of ctxt (Term.map_types (Term.map_type_tvar substT) (Thm.term_of ct)));
    val insts' = map (apfst mapT_and_recertify) insts;
  in
    Thm.instantiate
     (TVars.make (map (apfst (dest_TVar o Thm.typ_of)) instTs),
      Vars.make (map (apfst (dest_Var o Thm.term_of)) insts'))
  end;

fun tvar_clash ixn S S' =
  raise TYPE ("Type variable has two distinct sorts", [TVar (ixn, S), TVar (ixn, S')], []);

fun lookup (tye, (ixn, S)) =
  (case AList.lookup (op =) tye ixn of
    NONE => NONE
  | SOME (S', T) => if S = S' then SOME T else tvar_clash ixn S S');

val naive_typ_match =
  let
    fun match (TVar (v, S), T) subs =
          (case lookup (subs, (v, S)) of
            NONE => ((v, (S, T))::subs)
          | SOME _ => subs)
      | match (Type (a, Ts), Type (b, Us)) subs =
          if a <> b then raise Type.TYPE_MATCH
          else matches (Ts, Us) subs
      | match (TFree x, TFree y) subs =
          if x = y then subs else raise Type.TYPE_MATCH
      | match _ _ = raise Type.TYPE_MATCH
    and matches (T :: Ts, U :: Us) subs = matches (Ts, Us) (match (T, U) subs)
      | matches _ subs = subs;
  in match end;


(* expects that relevant type variables are already contained in
 * term variables. First instantiation of variables is returned without further
 * checking.
 *)
fun naive_cterm_first_order_match (t, ct) env =
  let
    fun mtch (env as (tyinsts, insts)) =
      fn (Var (ixn, T), ct) =>
          (case AList.lookup (op =) insts ixn of
            NONE => (naive_typ_match (T, Thm.typ_of_cterm ct) tyinsts, (ixn, ct) :: insts)
          | SOME _ => env)
       | (f $ t, ct) =>
          let val (cf, ct') = Thm.dest_comb ct;
          in mtch (mtch env (f, cf)) (t, ct') end
       | _ => env;
  in mtch env (t, ct) end;


fun discharge ctxt prems rule =
  let
    val (tyinsts,insts) =
      fold naive_cterm_first_order_match (Thm.prems_of rule ~~ map Thm.cprop_of prems) ([], []);
    val tyinsts' =
      map (fn (v, (S, U)) => ((v, S), Thm.ctyp_of ctxt U)) tyinsts;
    val insts' =
      map (fn (idxn, ct) => ((idxn, Thm.typ_of_cterm ct), ct)) insts;
    val rule' = Thm.instantiate (TVars.make tyinsts', Vars.make insts') rule;
  in fold Thm.elim_implies prems rule' end;

local

val (l_in_set_root, x_in_set_root, r_in_set_root) =
  let
    val (Node_l_x_d, r) =
      Thm.cprop_of @{thm in_set_root}
      |> Thm.dest_comb |> #2
      |> Thm.dest_comb |> #2 |> Thm.dest_comb |> #2 |> Thm.dest_comb;
    val (Node_l, x) = Node_l_x_d |> Thm.dest_comb |> #1 |> Thm.dest_comb;
    val l = Node_l |> Thm.dest_comb |> #2;
  in (l,x,r) end;

val (x_in_set_left, r_in_set_left) =
  let
    val (Node_l_x_d, r) =
      Thm.cprop_of @{thm in_set_left}
      |> Thm.dest_comb |> #2 |> Thm.dest_comb |> #2
      |> Thm.dest_comb |> #2 |> Thm.dest_comb |> #2 |> Thm.dest_comb;
    val x = Node_l_x_d |> Thm.dest_comb |> #1 |> Thm.dest_comb |> #2;
  in (x, r) end;

val (x_in_set_right, l_in_set_right) =
  let
    val (Node_l, x) =
      Thm.cprop_of @{thm in_set_right}
      |> Thm.dest_comb |> #2 |> Thm.dest_comb |> #2
      |> Thm.dest_comb |> #2 |> Thm.dest_comb |> #2
      |> Thm.dest_comb |> #1 |> Thm.dest_comb |> #1
      |> Thm.dest_comb;
    val l = Node_l |> Thm.dest_comb |> #2;
  in (x, l) end;

in

fun in_set ctxt ps tree =
  let
    val in_set = in_set ctxt
    val (_, [l, x, _, r]) = Drule.strip_comb tree;
    val xT = Thm.ctyp_of_cterm x;
  in
    (case ps of
      [] =>
        instantiate ctxt
          [(Thm.ctyp_of_cterm x_in_set_root, xT)]
          [(l_in_set_root, l), (x_in_set_root, x), (r_in_set_root, r)] @{thm in_set_root}
    | Left :: ps' =>
        let
          val in_set_l = in_set ps' l;
          val in_set_left' =
            instantiate ctxt
              [(Thm.ctyp_of_cterm x_in_set_left, xT)]
              [(x_in_set_left, x), (r_in_set_left, r)] @{thm in_set_left};
        in discharge ctxt [in_set_l] in_set_left' end
    | Right :: ps' =>
        let
          val in_set_r = in_set ps' r;
          val in_set_right' =
            instantiate ctxt
              [(Thm.ctyp_of_cterm x_in_set_right, xT)]
              [(x_in_set_right, x), (l_in_set_right, l)] @{thm in_set_right};
        in discharge ctxt [in_set_r] in_set_right' end)
  end;

fun find_in_set ctxt t ct =
  case find_tree t (Thm.term_of ct) of
    SOME ps => in_set ctxt ps ct
  | NONE => raise TERM ("find_in_set", [t, Thm.term_of ct])
 
(*
1. First get paths x_path y_path of x and y in the tree.
2. For the common prefix descend into the tree according to the path
   and lemmas all_distinct_left/right
3. If one restpath is empty use distinct_left/right,
   otherwise all_distinct_left_right
*)

fun distinctTreeProver ctxt dist_thm x_path y_path =
  let
    fun dist_subtree [] thm = thm
      | dist_subtree (p :: ps) thm =
         let
           val rule =
            (case p of Left => @{thm all_distinct_left} | Right => @{thm all_distinct_right})
         in dist_subtree ps (discharge ctxt [thm] rule) end;

    val (ps, x_rest, y_rest) = split_common_prefix x_path y_path;
    val dist_subtree_thm = dist_subtree ps dist_thm;
    val subtree = Thm.cprop_of dist_subtree_thm |> Thm.dest_comb |> #2 |> Thm.dest_comb |> #2;
    val (_, [l, _, _, r]) = Drule.strip_comb subtree;

  val in_set = in_set ctxt
  fun in_set' [] = raise TERM ("distinctTreeProver", [])
    | in_set' (Left :: ps) = in_set ps l
    | in_set' (Right :: ps) = in_set ps r;

  fun distinct_lr node_in_set Left =
        discharge ctxt [dist_subtree_thm, node_in_set] @{thm distinct_left}
    | distinct_lr node_in_set Right =
        discharge ctxt [dist_subtree_thm, node_in_set] @{thm distinct_right}

  val (swap, neq) =
    (case x_rest of
      [] =>
        let val y_in_set = in_set' y_rest;
        in (false, distinct_lr y_in_set (hd y_rest)) end
    | xr :: xrs =>
        (case y_rest of
          [] =>
            let val x_in_set = in_set' x_rest;
            in (true, distinct_lr x_in_set (hd x_rest)) end
        | yr :: yrs =>
            let
              val x_in_set = in_set' x_rest;
              val y_in_set = in_set' y_rest;
            in
              (case xr of
                Left =>
                  (false,
                    discharge ctxt [dist_subtree_thm, x_in_set, y_in_set] @{thm distinct_left_right})
              | Right =>
                  (true,
                    discharge ctxt [dist_subtree_thm, y_in_set, x_in_set] @{thm distinct_left_right}))
           end));
  in if swap then discharge ctxt [neq] @{thm swap_neq} else neq end;


fun deleteProver _ dist_thm [] = @{thm delete_root} OF [dist_thm]
  | deleteProver ctxt dist_thm (p::ps) =
      let
        val dist_rule =
          (case p of Left => @{thm all_distinct_left} | Right => @{thm all_distinct_right});
        val dist_thm' = discharge ctxt [dist_thm] dist_rule;
        val del_rule = (case p of Left => @{thm delete_left} | Right => @{thm delete_right});
        val del = deleteProver ctxt dist_thm' ps;
      in discharge ctxt [dist_thm, del] del_rule end;

local
  val (alpha, v) =
    let
      val ct =
        @{thm subtract_Tip} |> Thm.cprop_of |> Thm.dest_comb |> #2 |> Thm.dest_comb |> #2
        |> Thm.dest_comb |> #2;
      val [alpha] = ct |> Thm.ctyp_of_cterm |> Thm.dest_ctyp;
    in (dest_TVar (Thm.typ_of alpha), #1 (dest_Var (Thm.term_of ct))) end;
in

fun subtractProver ctxt (Const (\<^const_name>\<open>Tip\<close>, T)) ct dist_thm =
      let
        val ct' = dist_thm |> Thm.cprop_of |> Thm.dest_comb |> #2 |> Thm.dest_comb |> #2;
        val [alphaI] = dest_Type_args T;
      in
        Thm.instantiate
          (TVars.make1 (alpha, Thm.ctyp_of ctxt alphaI),
           Vars.make1 ((v, treeT alphaI), ct')) @{thm subtract_Tip}
      end
  | subtractProver ctxt (Const (\<^const_name>\<open>Node\<close>, nT) $ l $ x $ d $ r) ct dist_thm =
      let
        val ct' = dist_thm |> Thm.cprop_of |> Thm.dest_comb |> #2 |> Thm.dest_comb |> #2;
        val (_, [cl, _, _, cr]) = Drule.strip_comb ct;
        val ps = the (find_tree x (Thm.term_of ct'));
        val del_tree = deleteProver ctxt dist_thm ps;
        val dist_thm' = discharge ctxt [del_tree, dist_thm] @{thm delete_Some_all_distinct};
        val sub_l = subtractProver ctxt (Thm.term_of cl) cl (dist_thm');
        val sub_r =
          subtractProver ctxt (Thm.term_of cr) cr
            (discharge ctxt [sub_l, dist_thm'] @{thm subtract_Some_all_distinct_res});
      in discharge ctxt [del_tree, sub_l, sub_r] @{thm subtract_Node} end;

end;

fun distinct_implProver ctxt dist_thm ct =
  let
    val ctree = ct |> Thm.dest_comb |> #2 |> Thm.dest_comb |> #2;
    val sub = subtractProver ctxt (Thm.term_of ctree) ctree dist_thm;
  in @{thm subtract_Some_all_distinct} OF [sub, dist_thm] end;

fun get_fst_success f [] = NONE
  | get_fst_success f (x :: xs) =
      (case f x of
        NONE => get_fst_success f xs
      | SOME v => SOME v);

fun neq_x_y ctxt x y name =
  (let
    val dist_thm = the (try (Proof_Context.get_thm ctxt) name);
    val ctree = Thm.cprop_of dist_thm |> Thm.dest_comb |> #2 |> Thm.dest_comb |> #2;
    val tree = Thm.term_of ctree;
    val x_path = the (find_tree x tree);
    val y_path = the (find_tree y tree);
    val thm = distinctTreeProver ctxt dist_thm x_path y_path;
  in SOME thm
  end handle Option.Option => NONE);

fun distinctTree_tac names ctxt = SUBGOAL (fn (goal, i) =>
    (case goal of
      Const (\<^const_name>\<open>Trueprop\<close>, _) $
          (Const (\<^const_name>\<open>Not\<close>, _) $ (Const (\<^const_name>\<open>HOL.eq\<close>, _) $ x $ y)) =>
        (case get_fst_success (neq_x_y ctxt x y) names of
          SOME neq => resolve_tac ctxt [neq] i
        | NONE => no_tac)
    | _ => no_tac))

fun distinctFieldSolver names =
  mk_solver "distinctFieldSolver" (distinctTree_tac names);

fun distinct_simproc names =
  Simplifier.make_simproc \<^context>
   {name = "DistinctTreeProver.distinct_simproc",
    kind = Simproc,
    lhss = [\<^term>\<open>x = y\<close>],
    proc = fn _ => fn ctxt => fn ct =>
      (case Thm.term_of ct of
        Const (\<^const_name>\<open>HOL.eq\<close>, _) $ x $ y =>
          Option.map (fn neq => @{thm neq_to_eq_False} OF [neq])
            (get_fst_success (neq_x_y ctxt x y) names)
      | _ => NONE),
    identifier = []};

end;

end;
