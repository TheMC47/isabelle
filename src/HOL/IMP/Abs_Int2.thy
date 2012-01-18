(* Author: Tobias Nipkow *)

theory Abs_Int2
imports Abs_Int1_ivl
begin

subsection "Widening and Narrowing"

class WN = SL_top +
fixes widen :: "'a \<Rightarrow> 'a \<Rightarrow> 'a" (infix "\<nabla>" 65)
assumes widen1: "x \<sqsubseteq> x \<nabla> y"
assumes widen2: "y \<sqsubseteq> x \<nabla> y"
fixes narrow :: "'a \<Rightarrow> 'a \<Rightarrow> 'a" (infix "\<triangle>" 65)
assumes narrow1: "y \<sqsubseteq> x \<Longrightarrow> y \<sqsubseteq> x \<triangle> y"
assumes narrow2: "y \<sqsubseteq> x \<Longrightarrow> x \<triangle> y \<sqsubseteq> x"


subsubsection "Intervals"

instantiation ivl :: WN
begin

definition "widen_ivl ivl1 ivl2 =
  ((*if is_empty ivl1 then ivl2 else
   if is_empty ivl2 then ivl1 else*)
     case (ivl1,ivl2) of (I l1 h1, I l2 h2) \<Rightarrow>
       I (if le_option False l2 l1 \<and> l2 \<noteq> l1 then None else l1)
         (if le_option True h1 h2 \<and> h1 \<noteq> h2 then None else h1))"

definition "narrow_ivl ivl1 ivl2 =
  ((*if is_empty ivl1 \<or> is_empty ivl2 then empty else*)
     case (ivl1,ivl2) of (I l1 h1, I l2 h2) \<Rightarrow>
       I (if l1 = None then l2 else l1)
         (if h1 = None then h2 else h1))"

instance
proof qed
  (auto simp add: widen_ivl_def narrow_ivl_def le_option_def le_ivl_def empty_def split: ivl.split option.split if_splits)

end


subsubsection "Abstract State"

instantiation st :: (WN)WN
begin

definition "widen_st F1 F2 =
  FunDom (\<lambda>x. fun F1 x \<nabla> fun F2 x) (inter_list (dom F1) (dom F2))"

definition "narrow_st F1 F2 =
  FunDom (\<lambda>x. fun F1 x \<triangle> fun F2 x) (inter_list (dom F1) (dom F2))"

instance
proof
  case goal1 thus ?case
    by(simp add: widen_st_def le_st_def lookup_def widen1)
next
  case goal2 thus ?case
    by(simp add: widen_st_def le_st_def lookup_def widen2)
next
  case goal3 thus ?case
    by(auto simp: narrow_st_def le_st_def lookup_def narrow1)
next
  case goal4 thus ?case
    by(auto simp: narrow_st_def le_st_def lookup_def narrow2)
qed

end


subsubsection "Option"

instantiation option :: (WN)WN
begin

fun widen_option where
"None \<nabla> x = x" |
"x \<nabla> None = x" |
"(Some x) \<nabla> (Some y) = Some(x \<nabla> y)"

fun narrow_option where
"None \<triangle> x = None" |
"x \<triangle> None = None" |
"(Some x) \<triangle> (Some y) = Some(x \<triangle> y)"

instance
proof
  case goal1 show ?case
    by(induct x y rule: widen_option.induct) (simp_all add: widen1)
next
  case goal2 show ?case
    by(induct x y rule: widen_option.induct) (simp_all add: widen2)
next
  case goal3 thus ?case
    by(induct x y rule: narrow_option.induct) (simp_all add: narrow1)
next
  case goal4 thus ?case
    by(induct x y rule: narrow_option.induct) (simp_all add: narrow2)
qed

end


subsubsection "Annotated commands"

fun map2_acom :: "('a \<Rightarrow> 'a \<Rightarrow> 'a) \<Rightarrow> 'a acom \<Rightarrow> 'a acom \<Rightarrow> 'a acom" where
"map2_acom f (SKIP {a1}) (SKIP {a2}) = (SKIP {f a1 a2})" |
"map2_acom f (x ::= e {a1}) (x' ::= e' {a2}) = (x ::= e {f a1 a2})" |
"map2_acom f (c1;c2) (c1';c2') = (map2_acom f c1 c1'; map2_acom f c2 c2')" |
"map2_acom f (IF b THEN c1 ELSE c2 {a1}) (IF b' THEN c1' ELSE c2' {a2}) =
  (IF b THEN map2_acom f c1 c1' ELSE map2_acom f c2 c2' {f a1 a2})" |
"map2_acom f ({a1} WHILE b DO c {a2}) ({a3} WHILE b' DO c' {a4}) =
  ({f a1 a3} WHILE b DO map2_acom f c c' {f a2 a4})"

abbreviation widen_acom :: "('a::WN)acom \<Rightarrow> 'a acom \<Rightarrow> 'a acom" (infix "\<nabla>\<^sub>c" 65)
where "widen_acom == map2_acom (op \<nabla>)"

abbreviation narrow_acom :: "('a::WN)acom \<Rightarrow> 'a acom \<Rightarrow> 'a acom" (infix "\<triangle>\<^sub>c" 65)
where "narrow_acom == map2_acom (op \<triangle>)"

lemma widen1_acom: "strip c = strip c' \<Longrightarrow> c \<sqsubseteq> c \<nabla>\<^sub>c c'"
by(induct c c' rule: le_acom.induct)(simp_all add: widen1)

lemma widen2_acom: "strip c = strip c' \<Longrightarrow> c' \<sqsubseteq> c \<nabla>\<^sub>c c'"
by(induct c c' rule: le_acom.induct)(simp_all add: widen2)

lemma narrow1_acom: "y \<sqsubseteq> x \<Longrightarrow> y \<sqsubseteq> x \<triangle>\<^sub>c y"
by(induct y x rule: le_acom.induct) (simp_all add: narrow1)

lemma narrow2_acom: "y \<sqsubseteq> x \<Longrightarrow> x \<triangle>\<^sub>c y \<sqsubseteq> x"
by(induct y x rule: le_acom.induct) (simp_all add: narrow2)

(*
lemma strip_widen_acom:
  "strip c' = strip (c::'a::WN acom) \<Longrightarrow>  strip (c \<nabla>\<^sub>c c') = strip c"
by(induction "widen::'a\<Rightarrow>'a\<Rightarrow>'a" c c' rule: map2_acom.induct) simp_all

lemma annos_widen_acom[simp]: "strip c1 = strip (c2::'a::WN acom) \<Longrightarrow>
  annos(c1 \<nabla>\<^sub>c c2) = map (%(x,y).x\<nabla>y) (zip (annos c1) (annos(c2::'a::WN acom)))"
by(induction "widen::'a\<Rightarrow>'a\<Rightarrow>'a" c1 c2 rule: map2_acom.induct)
  (simp_all add: size_annos_same2)
*)

subsubsection "Post-fixed point computation"

definition iter_widen :: "('a acom \<Rightarrow> 'a acom) \<Rightarrow> 'a acom \<Rightarrow> ('a::WN)acom option"
where "iter_widen f = while_option (\<lambda>c. \<not> f c \<sqsubseteq> c) (\<lambda>c. c \<nabla>\<^sub>c f c)"

definition
  prefp :: "(('a::preord) \<Rightarrow> 'a) \<Rightarrow> 'a \<Rightarrow> 'a option" where
"prefp f = while_option (\<lambda>x. \<not> x \<sqsubseteq> f x) f"


definition
  pfp_WN :: "(('a::WN)option acom \<Rightarrow> 'a option acom) \<Rightarrow> com \<Rightarrow> 'a option acom option"
where "pfp_WN f c = (case iter_widen f (\<bottom>\<^sub>c c) of None \<Rightarrow> None
                     | Some c' \<Rightarrow> prefp (\<lambda>c. c \<triangle>\<^sub>c f c) c')"

lemma strip_map2_acom:
 "strip c1 = strip c2 \<Longrightarrow> strip(map2_acom f c1 c2) = strip c1"
by(induct f c1 c2 rule: map2_acom.induct) simp_all

lemma iter_widen_pfp: "iter_widen f c = Some c' \<Longrightarrow> f c' \<sqsubseteq> c'"
by(auto simp add: iter_widen_def dest: while_option_stop)

lemma strip_while: fixes f :: "'a acom \<Rightarrow> 'a acom"
assumes "\<forall>c. strip (f c) = strip c" and "while_option P f c = Some c'"
shows "strip c' = strip c"
using while_option_rule[where P = "\<lambda>c'. strip c' = strip c", OF _ assms(2)]
by (metis assms(1))

lemma strip_iter_widen: fixes f :: "'a::WN acom \<Rightarrow> 'a acom"
assumes "\<forall>c. strip (f c) = strip c" and "iter_widen f c = Some c'"
shows "strip c' = strip c"
proof-
  have "\<forall>c. strip(c \<nabla>\<^sub>c f c) = strip c" by (metis assms(1) strip_map2_acom)
  from strip_while[OF this] assms(2) show ?thesis by(simp add: iter_widen_def)
qed

lemma iter_down_pfp: assumes "mono f" and "f x0 \<sqsubseteq> x0" 
and "prefp (\<lambda>c. c \<triangle>\<^sub>c f c) x0 = Some x"
shows "f x \<sqsubseteq> x \<and> x \<sqsubseteq> x0" (is "?P x")
proof-
  { fix x assume "?P x"
    note 1 = conjunct1[OF this] and 2 = conjunct2[OF this]
    let ?x' = "x \<triangle>\<^sub>c f x"
    have "?P ?x'"
    proof
      have "f ?x' \<sqsubseteq> f x"  by(rule monoD[OF `mono f` narrow2_acom[OF 1]])
      also have "\<dots> \<sqsubseteq> ?x'" by(rule narrow1_acom[OF 1])
      finally show "f ?x' \<sqsubseteq> ?x'" .
      have "?x' \<sqsubseteq> x" by (rule narrow2_acom[OF 1])
      also have "x \<sqsubseteq> x0" by(rule 2)
      finally show "?x' \<sqsubseteq> x0" .
    qed
  }
  with while_option_rule[where P = ?P, OF _ assms(3)[simplified prefp_def]]
    assms(2) le_refl
  show ?thesis by blast
qed

lemma pfp_WN_pfp:
  "\<lbrakk> mono f;  pfp_WN f c = Some c' \<rbrakk> \<Longrightarrow> f c' \<sqsubseteq> c'"
unfolding pfp_WN_def
by (auto dest: iter_down_pfp iter_widen_pfp split: option.splits)

lemma strip_pfp_WN:
  "\<lbrakk> \<forall>c. strip(f c) = strip c; pfp_WN f c = Some c' \<rbrakk> \<Longrightarrow> strip c' = c"
apply(auto simp add: pfp_WN_def prefp_def split: option.splits)
by (metis (no_types) strip_map2_acom strip_while strip_bot_acom strip_iter_widen)

locale Abs_Int2 = Abs_Int1_mono
where \<gamma>=\<gamma> for \<gamma> :: "'av::{WN,L_top_bot} \<Rightarrow> val set"
begin

definition AI_WN :: "com \<Rightarrow> 'av st option acom option" where
"AI_WN = pfp_WN (step' \<top>)"

lemma AI_WN_sound: "AI_WN c = Some c' \<Longrightarrow> CS c \<le> \<gamma>\<^isub>c c'"
proof(simp add: CS_def AI_WN_def)
  assume 1: "pfp_WN (step' \<top>) c = Some c'"
  from pfp_WN_pfp[OF mono_step'2 1]
  have 2: "step' \<top> c' \<sqsubseteq> c'" .
  have 3: "strip (\<gamma>\<^isub>c (step' \<top> c')) = c" by(simp add: strip_pfp_WN[OF _ 1])
  have "lfp (step UNIV) c \<le> \<gamma>\<^isub>c (step' \<top> c')"
  proof(rule lfp_lowerbound[simplified,OF 3])
    show "step UNIV (\<gamma>\<^isub>c (step' \<top> c')) \<le> \<gamma>\<^isub>c (step' \<top> c')"
    proof(rule step_preserves_le[OF _ _])
      show "UNIV \<subseteq> \<gamma>\<^isub>o \<top>" by simp
      show "\<gamma>\<^isub>c (step' \<top> c') \<le> \<gamma>\<^isub>c c'" by(rule mono_gamma_c[OF 2])
    qed
  qed
  from this 2 show "lfp (step UNIV) c \<le> \<gamma>\<^isub>c c'"
    by (blast intro: mono_gamma_c order_trans)
qed

end

interpretation Abs_Int2
where \<gamma> = \<gamma>_ivl and num' = num_ivl and plus' = plus_ivl
and filter_plus' = filter_plus_ivl and filter_less' = filter_less_ivl
defines AI_ivl' is AI_WN
proof qed


subsubsection "Tests"

definition "step_up_ivl n = ((\<lambda>c. c \<nabla>\<^sub>c step_ivl \<top> c)^^n)"
definition "step_down_ivl n = ((\<lambda>c. c \<triangle>\<^sub>c step_ivl \<top> c)^^n)"

text{* For @{const test3_ivl}, @{const AI_ivl} needed as many iterations as
the loop took to execute. In contrast, @{const AI_ivl} converges in a
constant number of steps: *}

value [code] "show_acom (step_up_ivl 1 (\<bottom>\<^sub>c test3_ivl))"
value [code] "show_acom (step_up_ivl 2 (\<bottom>\<^sub>c test3_ivl))"
value [code] "show_acom (step_up_ivl 3 (\<bottom>\<^sub>c test3_ivl))"
value [code] "show_acom (step_up_ivl 4 (\<bottom>\<^sub>c test3_ivl))"
value [code] "show_acom (step_up_ivl 5 (\<bottom>\<^sub>c test3_ivl))"
value [code] "show_acom (step_down_ivl 1 (step_up_ivl 5 (\<bottom>\<^sub>c test3_ivl)))"
value [code] "show_acom (step_down_ivl 2 (step_up_ivl 5 (\<bottom>\<^sub>c test3_ivl)))"
value [code] "show_acom (step_down_ivl 3 (step_up_ivl 5 (\<bottom>\<^sub>c test3_ivl)))"

text{* Now all the analyses terminate: *}

value [code] "show_acom_opt (AI_ivl' test4_ivl)"
value [code] "show_acom_opt (AI_ivl' test5_ivl)"
value [code] "show_acom_opt (AI_ivl' test6_ivl)"


subsubsection "Termination: Intervals"

(* interesting(?) relic
lemma widen_assoc:
  "~ (y::ivl) \<sqsubseteq> x \<Longrightarrow> ~ z \<sqsubseteq> x \<nabla> y \<Longrightarrow> ((x::ivl) \<nabla> y) \<nabla> z = x \<nabla> (y \<nabla> z)"
apply(cases x)
apply(cases y)
apply(cases z)
apply(rename_tac x1 x2 y1 y2 z1 z2)
apply(simp add: le_ivl_def)
apply(case_tac x1)
apply(case_tac x2)
apply(simp add:le_option_def widen_ivl_def split: if_splits option.splits)
apply(simp add:le_option_def widen_ivl_def split: if_splits option.splits)
apply(case_tac x2)
apply(simp add:le_option_def widen_ivl_def split: if_splits option.splits)
apply(case_tac y1)
apply(case_tac y2)
apply(simp add:le_option_def widen_ivl_def split: if_splits option.splits)
apply(case_tac z1)
apply(auto simp add:le_option_def widen_ivl_def split: if_splits option.splits ivl.splits)[1]
apply(auto simp add:le_option_def widen_ivl_def split: if_splits option.splits ivl.splits)[1]
apply(case_tac y2)
apply(auto simp add:le_option_def widen_ivl_def split: if_splits option.splits ivl.splits)[1]
apply(case_tac z1)
apply(auto simp add:le_option_def widen_ivl_def split: if_splits ivl.splits option.splits)[1]
apply(case_tac z2)
apply(auto simp add:le_option_def widen_ivl_def split: if_splits option.splits)[1]
apply(auto simp add:le_option_def widen_ivl_def split: if_splits option.splits)[1]
done

lemma widen_step_trans:
  "~ (y::ivl) \<sqsubseteq> x \<Longrightarrow> ~ z \<sqsubseteq> x \<nabla> y \<Longrightarrow> EX u. (x \<nabla> y) \<nabla> z = x \<nabla> u \<and> ~ u \<sqsubseteq> x"
by (metis widen_assoc preord_class.le_trans widen1)
*)

definition m_ivl :: "ivl \<Rightarrow> nat" where
"m_ivl ivl = (case ivl of I l h \<Rightarrow>
     (case l of None \<Rightarrow> 0 | Some _ \<Rightarrow> 1) + (case h of None \<Rightarrow> 0 | Some _ \<Rightarrow> 1))"

lemma m_ivl_height: "m_ivl ivl \<le> 2"
by(simp add: m_ivl_def split: ivl.split option.split)

lemma m_ivl_anti_mono: "(y::ivl) \<sqsubseteq> x \<Longrightarrow> m_ivl x \<le> m_ivl y"
by(auto simp: m_ivl_def le_option_def le_ivl_def
        split: ivl.split option.split if_splits)

lemma less_m_ivl_widen:
  "~ y \<sqsubseteq> x \<Longrightarrow> m_ivl(x \<nabla> y) < m_ivl x"
by(auto simp: m_ivl_def widen_ivl_def le_option_def le_ivl_def
        split: ivl.splits option.splits if_splits)

lemma Top_less_ivl: "\<top> \<sqsubseteq> x \<Longrightarrow> m_ivl x = 0"
by(auto simp: m_ivl_def le_option_def le_ivl_def empty_def Top_ivl_def
        split: ivl.split option.split if_splits)


subsubsection "Termination: Abstract State"

definition "m_st m st = (\<Sum>x\<in>set(dom st). m(fun st x))"

lemma m_st_height: assumes "finite X" and "set (dom S) \<subseteq> X"
shows "m_st m_ivl S \<le> 2 * card X"
proof(auto simp: m_st_def)
  have "(\<Sum>x\<in>set(dom S). m_ivl (fun S x)) \<le> (\<Sum>x\<in>set(dom S). 2)" (is "?L \<le> _")
    by(rule setsum_mono)(simp add:m_ivl_height)
  also have "\<dots> \<le> (\<Sum>x\<in>X. 2)"
    by(rule setsum_mono3[OF assms]) simp
  also have "\<dots> = 2 * card X" by(simp add: setsum_constant)
  finally show "?L \<le> \<dots>" .
qed

lemma m_st_anti_mono:
  "S1 \<sqsubseteq> S2 \<Longrightarrow> m_st m_ivl S2 \<le> m_st m_ivl S1"
proof(auto simp: m_st_def le_st_def lookup_def split: if_splits)
  let ?X = "set(dom S1)" let ?Y = "set(dom S2)"
  let ?f = "fun S1" let ?g = "fun S2"
  assume asm: "\<forall>x\<in>?Y. (x \<in> ?X \<longrightarrow> ?f x \<sqsubseteq> ?g x) \<and> (x \<in> ?X \<or> \<top> \<sqsubseteq> ?g x)"
  hence 1: "\<forall>y\<in>?Y\<inter>?X. m_ivl(?g y) \<le> m_ivl(?f y)" by(simp add: m_ivl_anti_mono)
  have 0: "\<forall>x\<in>?Y-?X. m_ivl(?g x) = 0" using asm by (auto simp: Top_less_ivl)
  have "(\<Sum>y\<in>?Y. m_ivl(?g y)) = (\<Sum>y\<in>(?Y-?X) \<union> (?Y\<inter>?X). m_ivl(?g y))"
    by (metis Un_Diff_Int)
  also have "\<dots> = (\<Sum>y\<in>?Y-?X. m_ivl(?g y)) + (\<Sum>y\<in>?Y\<inter>?X. m_ivl(?g y))"
    by(subst setsum_Un_disjoint) auto
  also have "(\<Sum>y\<in>?Y-?X. m_ivl(?g y)) = 0" using 0 by simp
  also have "0 + (\<Sum>y\<in>?Y\<inter>?X. m_ivl(?g y)) = (\<Sum>y\<in>?Y\<inter>?X. m_ivl(?g y))" by simp
  also have "\<dots> \<le> (\<Sum>y\<in>?Y\<inter>?X. m_ivl(?f y))"
    by(rule setsum_mono)(simp add: 1)
  also have "\<dots> \<le> (\<Sum>y\<in>?X. m_ivl(?f y))"
    by(simp add: setsum_mono3[of "?X" "?Y Int ?X", OF _ Int_lower2])
  finally show "(\<Sum>y\<in>?Y. m_ivl(?g y)) \<le> (\<Sum>x\<in>?X. m_ivl(?f x))"
    by (metis add_less_cancel_left)
qed

lemma less_m_st_widen:
assumes "\<not> S2 \<sqsubseteq> S1" shows "m_st m_ivl (S1 \<nabla> S2) < m_st m_ivl S1"
proof-
  { let ?X = "set(dom S1)" let ?Y = "set(dom S2)"
    let ?f = "fun S1" let ?g = "fun S2"
    fix x assume "x \<in> ?X" "\<not> lookup S2 x \<sqsubseteq> ?f x"
    have "(\<Sum>x\<in>?X\<inter>?Y. m_ivl(?f x \<nabla> ?g x)) < (\<Sum>x\<in>?X. m_ivl(?f x))" (is "?L < ?R")
    proof cases
      assume "x : ?Y"
      have "?L < (\<Sum>x\<in>?X\<inter>?Y. m_ivl(?f x))"
      proof(rule setsum_strict_mono1, simp)
        show "\<forall>x\<in>?X\<inter>?Y. m_ivl(?f x \<nabla> ?g x) \<le> m_ivl (?f x)"
          by (metis m_ivl_anti_mono widen1)
      next
        show "\<exists>x\<in>?X\<inter>?Y. m_ivl(?f x \<nabla> ?g x) < m_ivl(?f x)"
          using `x:?X` `x:?Y` `\<not> lookup S2 x \<sqsubseteq> ?f x`
          by (metis IntI less_m_ivl_widen lookup_def)
      qed
      also have "\<dots> \<le> ?R" by(simp add: setsum_mono3[OF _ Int_lower1])
      finally show ?thesis .
    next
      assume "x ~: ?Y"
      have "?L \<le> (\<Sum>x\<in>?X\<inter>?Y. m_ivl(?f x))"
      proof(rule setsum_mono, simp)
        fix x assume "x:?X \<and> x:?Y" show "m_ivl(?f x \<nabla> ?g x) \<le> m_ivl (?f x)"
          by (metis m_ivl_anti_mono widen1)
      qed
      also have "\<dots> < m_ivl(?f x) + \<dots>"
        using less_m_ivl_widen[OF `\<not> lookup S2 x \<sqsubseteq> ?f x`]
        by (metis Nat.le_refl add_strict_increasing gr0I not_less0)
      also have "\<dots> = (\<Sum>y\<in>insert x (?X\<inter>?Y). m_ivl(?f y))"
        using `x ~: ?Y` by simp
      also have "\<dots> \<le> (\<Sum>x\<in>?X. m_ivl(?f x))"
        by(rule setsum_mono3)(insert `x:?X`, auto)
      finally show ?thesis .
    qed
  } with assms show ?thesis
    by(auto simp: le_st_def widen_st_def m_st_def Int_def)
qed


subsubsection "Termination: Option"

definition "m_o m n opt = (case opt of None \<Rightarrow> n+1 | Some x \<Rightarrow> m x)"

lemma m_o_anti_mono: "finite X \<Longrightarrow> domo S2 \<subseteq> X \<Longrightarrow> S1 \<sqsubseteq> S2 \<Longrightarrow>
  m_o (m_st m_ivl) (2 * card X) S2 \<le> m_o (m_st m_ivl) (2 * card X) S1"
apply(induction S1 S2 rule: le_option.induct)
apply(auto simp: domo_def m_o_def m_st_anti_mono le_SucI m_st_height
           split: option.splits)
done

lemma less_m_o_widen: "\<lbrakk> finite X; domo S2 \<subseteq> X; \<not> S2 \<sqsubseteq> S1 \<rbrakk> \<Longrightarrow>
  m_o (m_st m_ivl) (2 * card X) (S1 \<nabla> S2) < m_o (m_st m_ivl) (2 * card X) S1"
by(auto simp: m_o_def domo_def m_st_height less_Suc_eq_le less_m_st_widen
        split: option.split)


lemma domo_widen_subset: "domo (S1 \<nabla> S2) \<subseteq> domo S1 \<union> domo S2"
apply(induction S1 S2 rule: widen_option.induct)
apply (auto simp: domo_def widen_st_def)
done

subsubsection "Termination: Commands"

lemma strip_widen_acom:
  "strip c' = strip (c::'a::WN acom) \<Longrightarrow>  strip (c \<nabla>\<^sub>c c') = strip c"
by(induction "widen::'a\<Rightarrow>'a\<Rightarrow>'a" c c' rule: map2_acom.induct) simp_all

lemma annos_widen_acom[simp]: "strip c1 = strip (c2::'a::WN acom) \<Longrightarrow>
  annos(c1 \<nabla>\<^sub>c c2) = map (%(x,y).x\<nabla>y) (zip (annos c1) (annos(c2::'a::WN acom)))"
by(induction "widen::'a\<Rightarrow>'a\<Rightarrow>'a" c1 c2 rule: map2_acom.induct)
  (simp_all add: size_annos_same2)

lemma Com_widen_acom: "strip c2 = strip c1 \<Longrightarrow>
  c1 : Com X \<Longrightarrow> c2 : Com X \<Longrightarrow> (c1 \<nabla>\<^sub>c c2) : Com X"
apply(auto simp add: Com_def split: option.splits)
apply(rename_tac S S' x)
apply(erule in_set_zipE)
apply(simp add: domo_def)
apply (auto split: option.splits)
apply(case_tac S)
apply(case_tac S')
apply simp
apply fastforce
apply(case_tac S')
apply fastforce
apply (fastforce simp: widen_st_def)
done

definition "m_c m c = (let as = annos c in \<Sum>i=0..<size as. m(as!i))"

lemma measure_m_c: "finite X \<Longrightarrow> {(c, c \<nabla>\<^sub>c c') |c c'\<Colon>ivl st option acom.
     strip c' = strip c \<and> c : Com X \<and> c' : Com X \<and> \<not> c' \<sqsubseteq> c}\<inverse>
    \<subseteq> measure(m_c(m_o (m_st m_ivl) (2*card(X))))"
apply(auto simp: m_c_def Let_def Com_def)
apply(subgoal_tac "length(annos c') = length(annos c)")
prefer 2 apply (simp add: size_annos_same2)
apply (auto)
apply(rule setsum_strict_mono1)
apply simp
apply (clarsimp)
apply(erule m_o_anti_mono)
apply(rule subset_trans[OF domo_widen_subset])
apply fastsimp
apply(rule widen1)
apply(auto simp: le_iff_le_annos listrel_iff_nth)
apply(rule_tac x=n in bexI)
prefer 2 apply simp
apply(erule less_m_o_widen)
apply (simp)+
done


subsubsection "Termination: Post-Fixed Point Iterations"

lemma iter_widen_termination:
fixes c0 :: "'a::WN acom"
assumes P_f: "\<And>c. P c \<Longrightarrow> P(f c)"
assumes P_widen: "\<And>c c'. P c \<Longrightarrow> P c' \<Longrightarrow> P(c \<nabla>\<^sub>c c')"
and "wf({(c::'a acom,c \<nabla>\<^sub>c c')|c c'. P c \<and> P c' \<and> ~ c' \<sqsubseteq> c}^-1)"
and "P c0" and "c0 \<sqsubseteq> f c0" shows "EX c. iter_widen f c0 = Some c"
proof(simp add: iter_widen_def, rule wf_while_option_Some[where P = "P"])
  show "wf {(cc', c). (P c \<and> \<not> f c \<sqsubseteq> c) \<and> cc' = c \<nabla>\<^sub>c f c}"
    apply(rule wf_subset[OF assms(3)]) by(blast intro: P_f)
next
  show "P c0" by(rule `P c0`)
next
  fix c assume "P c" thus "P (c \<nabla>\<^sub>c f c)" by(simp add: P_f P_widen)
qed

lemma
  "EX c::ivl st option acom. iter_widen (step_ivl \<top>) (\<bottom>\<^sub>c c0) = Some c"
apply(rule iter_widen_termination[where
  P = "%c. strip c = c0 \<and> c : Com(vars c0)"])
apply (simp_all add: strip_step' step'_Com Com_widen_acom strip_widen_acom bot_acom domo_Top)
apply(rule wf_subset)
apply(rule wf_measure)
apply(rule subset_trans)
prefer 2
apply(rule measure_m_c[where X = "vars c0", OF finite_cvars])
apply blast
done

end
