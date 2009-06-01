(*  Title:      HOL/Transitive_Closure.thy
    Author:     Lawrence C Paulson, Cambridge University Computer Laboratory
    Copyright   1992  University of Cambridge
*)

header {* Reflexive and Transitive closure of a relation *}

theory Transitive_Closure
imports Predicate
uses "~~/src/Provers/trancl.ML"
begin

text {*
  @{text rtrancl} is reflexive/transitive closure,
  @{text trancl} is transitive closure,
  @{text reflcl} is reflexive closure.

  These postfix operators have \emph{maximum priority}, forcing their
  operands to be atomic.
*}

inductive_set
  rtrancl :: "('a \<times> 'a) set \<Rightarrow> ('a \<times> 'a) set"   ("(_^*)" [1000] 999)
  for r :: "('a \<times> 'a) set"
where
    rtrancl_refl [intro!, Pure.intro!, simp]: "(a, a) : r^*"
  | rtrancl_into_rtrancl [Pure.intro]: "(a, b) : r^* ==> (b, c) : r ==> (a, c) : r^*"

inductive_set
  trancl :: "('a \<times> 'a) set \<Rightarrow> ('a \<times> 'a) set"  ("(_^+)" [1000] 999)
  for r :: "('a \<times> 'a) set"
where
    r_into_trancl [intro, Pure.intro]: "(a, b) : r ==> (a, b) : r^+"
  | trancl_into_trancl [Pure.intro]: "(a, b) : r^+ ==> (b, c) : r ==> (a, c) : r^+"

notation
  rtranclp  ("(_^**)" [1000] 1000) and
  tranclp  ("(_^++)" [1000] 1000)

abbreviation
  reflclp :: "('a => 'a => bool) => 'a => 'a => bool"  ("(_^==)" [1000] 1000) where
  "r^== == sup r op ="

abbreviation
  reflcl :: "('a \<times> 'a) set => ('a \<times> 'a) set"  ("(_^=)" [1000] 999) where
  "r^= == r \<union> Id"

notation (xsymbols)
  rtranclp  ("(_\<^sup>*\<^sup>*)" [1000] 1000) and
  tranclp  ("(_\<^sup>+\<^sup>+)" [1000] 1000) and
  reflclp  ("(_\<^sup>=\<^sup>=)" [1000] 1000) and
  rtrancl  ("(_\<^sup>*)" [1000] 999) and
  trancl  ("(_\<^sup>+)" [1000] 999) and
  reflcl  ("(_\<^sup>=)" [1000] 999)

notation (HTML output)
  rtranclp  ("(_\<^sup>*\<^sup>*)" [1000] 1000) and
  tranclp  ("(_\<^sup>+\<^sup>+)" [1000] 1000) and
  reflclp  ("(_\<^sup>=\<^sup>=)" [1000] 1000) and
  rtrancl  ("(_\<^sup>*)" [1000] 999) and
  trancl  ("(_\<^sup>+)" [1000] 999) and
  reflcl  ("(_\<^sup>=)" [1000] 999)


subsection {* Reflexive closure *}

lemma refl_reflcl[simp]: "refl(r^=)"
by(simp add:refl_on_def)

lemma antisym_reflcl[simp]: "antisym(r^=) = antisym r"
by(simp add:antisym_def)

lemma trans_reflclI[simp]: "trans r \<Longrightarrow> trans(r^=)"
unfolding trans_def by blast


subsection {* Reflexive-transitive closure *}

lemma reflcl_set_eq [pred_set_conv]: "(sup (\<lambda>x y. (x, y) \<in> r) op =) = (\<lambda>x y. (x, y) \<in> r Un Id)"
  by (simp add: expand_fun_eq)

lemma r_into_rtrancl [intro]: "!!p. p \<in> r ==> p \<in> r^*"
  -- {* @{text rtrancl} of @{text r} contains @{text r} *}
  apply (simp only: split_tupled_all)
  apply (erule rtrancl_refl [THEN rtrancl_into_rtrancl])
  done

lemma r_into_rtranclp [intro]: "r x y ==> r^** x y"
  -- {* @{text rtrancl} of @{text r} contains @{text r} *}
  by (erule rtranclp.rtrancl_refl [THEN rtranclp.rtrancl_into_rtrancl])

lemma rtranclp_mono: "r \<le> s ==> r^** \<le> s^**"
  -- {* monotonicity of @{text rtrancl} *}
  apply (rule predicate2I)
  apply (erule rtranclp.induct)
   apply (rule_tac [2] rtranclp.rtrancl_into_rtrancl, blast+)
  done

lemmas rtrancl_mono = rtranclp_mono [to_set]

theorem rtranclp_induct [consumes 1, case_names base step, induct set: rtranclp]:
  assumes a: "r^** a b"
    and cases: "P a" "!!y z. [| r^** a y; r y z; P y |] ==> P z"
  shows "P b"
proof -
  from a have "a = a --> P b"
    by (induct "%x y. x = a --> P y" a b) (iprover intro: cases)+
  then show ?thesis by iprover
qed

lemmas rtrancl_induct [induct set: rtrancl] = rtranclp_induct [to_set]

lemmas rtranclp_induct2 =
  rtranclp_induct[of _ "(ax,ay)" "(bx,by)", split_rule,
                 consumes 1, case_names refl step]

lemmas rtrancl_induct2 =
  rtrancl_induct[of "(ax,ay)" "(bx,by)", split_format (complete),
                 consumes 1, case_names refl step]

lemma refl_rtrancl: "refl (r^*)"
by (unfold refl_on_def) fast

text {* Transitivity of transitive closure. *}
lemma trans_rtrancl: "trans (r^*)"
proof (rule transI)
  fix x y z
  assume "(x, y) \<in> r\<^sup>*"
  assume "(y, z) \<in> r\<^sup>*"
  then show "(x, z) \<in> r\<^sup>*"
  proof induct
    case base
    show "(x, y) \<in> r\<^sup>*" by fact
  next
    case (step u v)
    from `(x, u) \<in> r\<^sup>*` and `(u, v) \<in> r`
    show "(x, v) \<in> r\<^sup>*" ..
  qed
qed

lemmas rtrancl_trans = trans_rtrancl [THEN transD, standard]

lemma rtranclp_trans:
  assumes xy: "r^** x y"
  and yz: "r^** y z"
  shows "r^** x z" using yz xy
  by induct iprover+

lemma rtranclE [cases set: rtrancl]:
  assumes major: "(a::'a, b) : r^*"
  obtains
    (base) "a = b"
  | (step) y where "(a, y) : r^*" and "(y, b) : r"
  -- {* elimination of @{text rtrancl} -- by induction on a special formula *}
  apply (subgoal_tac "(a::'a) = b | (EX y. (a,y) : r^* & (y,b) : r)")
   apply (rule_tac [2] major [THEN rtrancl_induct])
    prefer 2 apply blast
   prefer 2 apply blast
  apply (erule asm_rl exE disjE conjE base step)+
  done

lemma rtrancl_Int_subset: "[| Id \<subseteq> s; r O (r^* \<inter> s) \<subseteq> s|] ==> r^* \<subseteq> s"
  apply (rule subsetI)
  apply (rule_tac p="x" in PairE, clarify)
  apply (erule rtrancl_induct, auto) 
  done

lemma converse_rtranclp_into_rtranclp:
  "r a b \<Longrightarrow> r\<^sup>*\<^sup>* b c \<Longrightarrow> r\<^sup>*\<^sup>* a c"
  by (rule rtranclp_trans) iprover+

lemmas converse_rtrancl_into_rtrancl = converse_rtranclp_into_rtranclp [to_set]

text {*
  \medskip More @{term "r^*"} equations and inclusions.
*}

lemma rtranclp_idemp [simp]: "(r^**)^** = r^**"
  apply (auto intro!: order_antisym)
  apply (erule rtranclp_induct)
   apply (rule rtranclp.rtrancl_refl)
  apply (blast intro: rtranclp_trans)
  done

lemmas rtrancl_idemp [simp] = rtranclp_idemp [to_set]

lemma rtrancl_idemp_self_comp [simp]: "R^* O R^* = R^*"
  apply (rule set_ext)
  apply (simp only: split_tupled_all)
  apply (blast intro: rtrancl_trans)
  done

lemma rtrancl_subset_rtrancl: "r \<subseteq> s^* ==> r^* \<subseteq> s^*"
  apply (drule rtrancl_mono)
  apply simp
  done

lemma rtranclp_subset: "R \<le> S ==> S \<le> R^** ==> S^** = R^**"
  apply (drule rtranclp_mono)
  apply (drule rtranclp_mono)
  apply simp
  done

lemmas rtrancl_subset = rtranclp_subset [to_set]

lemma rtranclp_sup_rtranclp: "(sup (R^**) (S^**))^** = (sup R S)^**"
  by (blast intro!: rtranclp_subset intro: rtranclp_mono [THEN predicate2D])

lemmas rtrancl_Un_rtrancl = rtranclp_sup_rtranclp [to_set]

lemma rtranclp_reflcl [simp]: "(R^==)^** = R^**"
  by (blast intro!: rtranclp_subset)

lemmas rtrancl_reflcl [simp] = rtranclp_reflcl [to_set]

lemma rtrancl_r_diff_Id: "(r - Id)^* = r^*"
  apply (rule sym)
  apply (rule rtrancl_subset, blast, clarify)
  apply (rename_tac a b)
  apply (case_tac "a = b")
   apply blast
  apply (blast intro!: r_into_rtrancl)
  done

lemma rtranclp_r_diff_Id: "(inf r op ~=)^** = r^**"
  apply (rule sym)
  apply (rule rtranclp_subset)
   apply blast+
  done

theorem rtranclp_converseD:
  assumes r: "(r^--1)^** x y"
  shows "r^** y x"
proof -
  from r show ?thesis
    by induct (iprover intro: rtranclp_trans dest!: conversepD)+
qed

lemmas rtrancl_converseD = rtranclp_converseD [to_set]

theorem rtranclp_converseI:
  assumes "r^** y x"
  shows "(r^--1)^** x y"
  using assms
  by induct (iprover intro: rtranclp_trans conversepI)+

lemmas rtrancl_converseI = rtranclp_converseI [to_set]

lemma rtrancl_converse: "(r^-1)^* = (r^*)^-1"
  by (fast dest!: rtrancl_converseD intro!: rtrancl_converseI)

lemma sym_rtrancl: "sym r ==> sym (r^*)"
  by (simp only: sym_conv_converse_eq rtrancl_converse [symmetric])

theorem converse_rtranclp_induct[consumes 1]:
  assumes major: "r^** a b"
    and cases: "P b" "!!y z. [| r y z; r^** z b; P z |] ==> P y"
  shows "P a"
  using rtranclp_converseI [OF major]
  by induct (iprover intro: cases dest!: conversepD rtranclp_converseD)+

lemmas converse_rtrancl_induct = converse_rtranclp_induct [to_set]

lemmas converse_rtranclp_induct2 =
  converse_rtranclp_induct [of _ "(ax,ay)" "(bx,by)", split_rule,
                 consumes 1, case_names refl step]

lemmas converse_rtrancl_induct2 =
  converse_rtrancl_induct [of "(ax,ay)" "(bx,by)", split_format (complete),
                 consumes 1, case_names refl step]

lemma converse_rtranclpE:
  assumes major: "r^** x z"
    and cases: "x=z ==> P"
      "!!y. [| r x y; r^** y z |] ==> P"
  shows P
  apply (subgoal_tac "x = z | (EX y. r x y & r^** y z)")
   apply (rule_tac [2] major [THEN converse_rtranclp_induct])
    prefer 2 apply iprover
   prefer 2 apply iprover
  apply (erule asm_rl exE disjE conjE cases)+
  done

lemmas converse_rtranclE = converse_rtranclpE [to_set]

lemmas converse_rtranclpE2 = converse_rtranclpE [of _ "(xa,xb)" "(za,zb)", split_rule]

lemmas converse_rtranclE2 = converse_rtranclE [of "(xa,xb)" "(za,zb)", split_rule]

lemma r_comp_rtrancl_eq: "r O r^* = r^* O r"
  by (blast elim: rtranclE converse_rtranclE
    intro: rtrancl_into_rtrancl converse_rtrancl_into_rtrancl)

lemma rtrancl_unfold: "r^* = Id Un r O r^*"
  by (auto intro: rtrancl_into_rtrancl elim: rtranclE)


subsection {* Transitive closure *}

lemma trancl_mono: "!!p. p \<in> r^+ ==> r \<subseteq> s ==> p \<in> s^+"
  apply (simp add: split_tupled_all)
  apply (erule trancl.induct)
   apply (iprover dest: subsetD)+
  done

lemma r_into_trancl': "!!p. p : r ==> p : r^+"
  by (simp only: split_tupled_all) (erule r_into_trancl)

text {*
  \medskip Conversions between @{text trancl} and @{text rtrancl}.
*}

lemma tranclp_into_rtranclp: "r^++ a b ==> r^** a b"
  by (erule tranclp.induct) iprover+

lemmas trancl_into_rtrancl = tranclp_into_rtranclp [to_set]

lemma rtranclp_into_tranclp1: assumes r: "r^** a b"
  shows "!!c. r b c ==> r^++ a c" using r
  by induct iprover+

lemmas rtrancl_into_trancl1 = rtranclp_into_tranclp1 [to_set]

lemma rtranclp_into_tranclp2: "[| r a b; r^** b c |] ==> r^++ a c"
  -- {* intro rule from @{text r} and @{text rtrancl} *}
  apply (erule rtranclp.cases)
   apply iprover
  apply (rule rtranclp_trans [THEN rtranclp_into_tranclp1])
    apply (simp | rule r_into_rtranclp)+
  done

lemmas rtrancl_into_trancl2 = rtranclp_into_tranclp2 [to_set]

text {* Nice induction rule for @{text trancl} *}
lemma tranclp_induct [consumes 1, case_names base step, induct pred: tranclp]:
  assumes "r^++ a b"
  and cases: "!!y. r a y ==> P y"
    "!!y z. r^++ a y ==> r y z ==> P y ==> P z"
  shows "P b"
proof -
  from `r^++ a b` have "a = a --> P b"
    by (induct "%x y. x = a --> P y" a b) (iprover intro: cases)+
  then show ?thesis by iprover
qed

lemmas trancl_induct [induct set: trancl] = tranclp_induct [to_set]

lemmas tranclp_induct2 =
  tranclp_induct [of _ "(ax,ay)" "(bx,by)", split_rule,
    consumes 1, case_names base step]

lemmas trancl_induct2 =
  trancl_induct [of "(ax,ay)" "(bx,by)", split_format (complete),
    consumes 1, case_names base step]

lemma tranclp_trans_induct:
  assumes major: "r^++ x y"
    and cases: "!!x y. r x y ==> P x y"
      "!!x y z. [| r^++ x y; P x y; r^++ y z; P y z |] ==> P x z"
  shows "P x y"
  -- {* Another induction rule for trancl, incorporating transitivity *}
  by (iprover intro: major [THEN tranclp_induct] cases)

lemmas trancl_trans_induct = tranclp_trans_induct [to_set]

lemma tranclE [cases set: trancl]:
  assumes "(a, b) : r^+"
  obtains
    (base) "(a, b) : r"
  | (step) c where "(a, c) : r^+" and "(c, b) : r"
  using assms by cases simp_all

lemma trancl_Int_subset: "[| r \<subseteq> s; r O (r^+ \<inter> s) \<subseteq> s|] ==> r^+ \<subseteq> s"
  apply (rule subsetI)
  apply (rule_tac p = x in PairE)
  apply clarify
  apply (erule trancl_induct)
   apply auto
  done

lemma trancl_unfold: "r^+ = r Un r O r^+"
  by (auto intro: trancl_into_trancl elim: tranclE)

text {* Transitivity of @{term "r^+"} *}
lemma trans_trancl [simp]: "trans (r^+)"
proof (rule transI)
  fix x y z
  assume "(x, y) \<in> r^+"
  assume "(y, z) \<in> r^+"
  then show "(x, z) \<in> r^+"
  proof induct
    case (base u)
    from `(x, y) \<in> r^+` and `(y, u) \<in> r`
    show "(x, u) \<in> r^+" ..
  next
    case (step u v)
    from `(x, u) \<in> r^+` and `(u, v) \<in> r`
    show "(x, v) \<in> r^+" ..
  qed
qed

lemmas trancl_trans = trans_trancl [THEN transD, standard]

lemma tranclp_trans:
  assumes xy: "r^++ x y"
  and yz: "r^++ y z"
  shows "r^++ x z" using yz xy
  by induct iprover+

lemma trancl_id [simp]: "trans r \<Longrightarrow> r^+ = r"
  apply auto
  apply (erule trancl_induct)
   apply assumption
  apply (unfold trans_def)
  apply blast
  done

lemma rtranclp_tranclp_tranclp:
  assumes "r^** x y"
  shows "!!z. r^++ y z ==> r^++ x z" using assms
  by induct (iprover intro: tranclp_trans)+

lemmas rtrancl_trancl_trancl = rtranclp_tranclp_tranclp [to_set]

lemma tranclp_into_tranclp2: "r a b ==> r^++ b c ==> r^++ a c"
  by (erule tranclp_trans [OF tranclp.r_into_trancl])

lemmas trancl_into_trancl2 = tranclp_into_tranclp2 [to_set]

lemma trancl_insert:
  "(insert (y, x) r)^+ = r^+ \<union> {(a, b). (a, y) \<in> r^* \<and> (x, b) \<in> r^*}"
  -- {* primitive recursion for @{text trancl} over finite relations *}
  apply (rule equalityI)
   apply (rule subsetI)
   apply (simp only: split_tupled_all)
   apply (erule trancl_induct, blast)
   apply (blast intro: rtrancl_into_trancl1 trancl_into_rtrancl r_into_trancl trancl_trans)
  apply (rule subsetI)
  apply (blast intro: trancl_mono rtrancl_mono
    [THEN [2] rev_subsetD] rtrancl_trancl_trancl rtrancl_into_trancl2)
  done

lemma tranclp_converseI: "(r^++)^--1 x y ==> (r^--1)^++ x y"
  apply (drule conversepD)
  apply (erule tranclp_induct)
  apply (iprover intro: conversepI tranclp_trans)+
  done

lemmas trancl_converseI = tranclp_converseI [to_set]

lemma tranclp_converseD: "(r^--1)^++ x y ==> (r^++)^--1 x y"
  apply (rule conversepI)
  apply (erule tranclp_induct)
  apply (iprover dest: conversepD intro: tranclp_trans)+
  done

lemmas trancl_converseD = tranclp_converseD [to_set]

lemma tranclp_converse: "(r^--1)^++ = (r^++)^--1"
  by (fastsimp simp add: expand_fun_eq
    intro!: tranclp_converseI dest!: tranclp_converseD)

lemmas trancl_converse = tranclp_converse [to_set]

lemma sym_trancl: "sym r ==> sym (r^+)"
  by (simp only: sym_conv_converse_eq trancl_converse [symmetric])

lemma converse_tranclp_induct:
  assumes major: "r^++ a b"
    and cases: "!!y. r y b ==> P(y)"
      "!!y z.[| r y z;  r^++ z b;  P(z) |] ==> P(y)"
  shows "P a"
  apply (rule tranclp_induct [OF tranclp_converseI, OF conversepI, OF major])
   apply (rule cases)
   apply (erule conversepD)
  apply (blast intro: prems dest!: tranclp_converseD conversepD)
  done

lemmas converse_trancl_induct = converse_tranclp_induct [to_set]

lemma tranclpD: "R^++ x y ==> EX z. R x z \<and> R^** z y"
  apply (erule converse_tranclp_induct)
   apply auto
  apply (blast intro: rtranclp_trans)
  done

lemmas tranclD = tranclpD [to_set]

lemma tranclD2:
  "(x, y) \<in> R\<^sup>+ \<Longrightarrow> \<exists>z. (x, z) \<in> R\<^sup>* \<and> (z, y) \<in> R"
  by (blast elim: tranclE intro: trancl_into_rtrancl)

lemma irrefl_tranclI: "r^-1 \<inter> r^* = {} ==> (x, x) \<notin> r^+"
  by (blast elim: tranclE dest: trancl_into_rtrancl)

lemma irrefl_trancl_rD: "!!X. ALL x. (x, x) \<notin> r^+ ==> (x, y) \<in> r ==> x \<noteq> y"
  by (blast dest: r_into_trancl)

lemma trancl_subset_Sigma_aux:
    "(a, b) \<in> r^* ==> r \<subseteq> A \<times> A ==> a = b \<or> a \<in> A"
  by (induct rule: rtrancl_induct) auto

lemma trancl_subset_Sigma: "r \<subseteq> A \<times> A ==> r^+ \<subseteq> A \<times> A"
  apply (rule subsetI)
  apply (simp only: split_tupled_all)
  apply (erule tranclE)
   apply (blast dest!: trancl_into_rtrancl trancl_subset_Sigma_aux)+
  done

lemma reflcl_tranclp [simp]: "(r^++)^== = r^**"
  apply (safe intro!: order_antisym)
   apply (erule tranclp_into_rtranclp)
  apply (blast elim: rtranclp.cases dest: rtranclp_into_tranclp1)
  done

lemmas reflcl_trancl [simp] = reflcl_tranclp [to_set]

lemma trancl_reflcl [simp]: "(r^=)^+ = r^*"
  apply safe
   apply (drule trancl_into_rtrancl, simp)
  apply (erule rtranclE, safe)
   apply (rule r_into_trancl, simp)
  apply (rule rtrancl_into_trancl1)
   apply (erule rtrancl_reflcl [THEN equalityD2, THEN subsetD], fast)
  done

lemma trancl_empty [simp]: "{}^+ = {}"
  by (auto elim: trancl_induct)

lemma rtrancl_empty [simp]: "{}^* = Id"
  by (rule subst [OF reflcl_trancl]) simp

lemma rtranclpD: "R^** a b ==> a = b \<or> a \<noteq> b \<and> R^++ a b"
  by (force simp add: reflcl_tranclp [symmetric] simp del: reflcl_tranclp)

lemmas rtranclD = rtranclpD [to_set]

lemma rtrancl_eq_or_trancl:
  "(x,y) \<in> R\<^sup>* = (x=y \<or> x\<noteq>y \<and> (x,y) \<in> R\<^sup>+)"
  by (fast elim: trancl_into_rtrancl dest: rtranclD)

text {* @{text Domain} and @{text Range} *}

lemma Domain_rtrancl [simp]: "Domain (R^*) = UNIV"
  by blast

lemma Range_rtrancl [simp]: "Range (R^*) = UNIV"
  by blast

lemma rtrancl_Un_subset: "(R^* \<union> S^*) \<subseteq> (R Un S)^*"
  by (rule rtrancl_Un_rtrancl [THEN subst]) fast

lemma in_rtrancl_UnI: "x \<in> R^* \<or> x \<in> S^* ==> x \<in> (R \<union> S)^*"
  by (blast intro: subsetD [OF rtrancl_Un_subset])

lemma trancl_domain [simp]: "Domain (r^+) = Domain r"
  by (unfold Domain_def) (blast dest: tranclD)

lemma trancl_range [simp]: "Range (r^+) = Range r"
unfolding Range_def by(simp add: trancl_converse [symmetric])

lemma Not_Domain_rtrancl:
    "x ~: Domain R ==> ((x, y) : R^*) = (x = y)"
  apply auto
  apply (erule rev_mp)
  apply (erule rtrancl_induct)
   apply auto
  done

lemma trancl_subset_Field2: "r^+ <= Field r \<times> Field r"
  apply clarify
  apply (erule trancl_induct)
   apply (auto simp add: Field_def)
  done

lemma finite_trancl: "finite (r^+) = finite r"
  apply auto
   prefer 2
   apply (rule trancl_subset_Field2 [THEN finite_subset])
   apply (rule finite_SigmaI)
    prefer 3
    apply (blast intro: r_into_trancl' finite_subset)
   apply (auto simp add: finite_Field)
  done

text {* More about converse @{text rtrancl} and @{text trancl}, should
  be merged with main body. *}

lemma single_valued_confluent:
  "\<lbrakk> single_valued r; (x,y) \<in> r^*; (x,z) \<in> r^* \<rbrakk>
  \<Longrightarrow> (y,z) \<in> r^* \<or> (z,y) \<in> r^*"
  apply (erule rtrancl_induct)
  apply simp
  apply (erule disjE)
   apply (blast elim:converse_rtranclE dest:single_valuedD)
  apply(blast intro:rtrancl_trans)
  done

lemma r_r_into_trancl: "(a, b) \<in> R ==> (b, c) \<in> R ==> (a, c) \<in> R^+"
  by (fast intro: trancl_trans)

lemma trancl_into_trancl [rule_format]:
    "(a, b) \<in> r\<^sup>+ ==> (b, c) \<in> r --> (a,c) \<in> r\<^sup>+"
  apply (erule trancl_induct)
   apply (fast intro: r_r_into_trancl)
  apply (fast intro: r_r_into_trancl trancl_trans)
  done

lemma tranclp_rtranclp_tranclp:
    "r\<^sup>+\<^sup>+ a b ==> r\<^sup>*\<^sup>* b c ==> r\<^sup>+\<^sup>+ a c"
  apply (drule tranclpD)
  apply (elim exE conjE)
  apply (drule rtranclp_trans, assumption)
  apply (drule rtranclp_into_tranclp2, assumption, assumption)
  done

lemmas trancl_rtrancl_trancl = tranclp_rtranclp_tranclp [to_set]

lemmas transitive_closure_trans [trans] =
  r_r_into_trancl trancl_trans rtrancl_trans
  trancl.trancl_into_trancl trancl_into_trancl2
  rtrancl.rtrancl_into_rtrancl converse_rtrancl_into_rtrancl
  rtrancl_trancl_trancl trancl_rtrancl_trancl

lemmas transitive_closurep_trans' [trans] =
  tranclp_trans rtranclp_trans
  tranclp.trancl_into_trancl tranclp_into_tranclp2
  rtranclp.rtrancl_into_rtrancl converse_rtranclp_into_rtranclp
  rtranclp_tranclp_tranclp tranclp_rtranclp_tranclp

declare trancl_into_rtrancl [elim]

subsection {* The power operation on relations *}

text {* @{text "R ^^ n = R O ... O R"}, the n-fold composition of @{text R} *}

overloading
  relpow == "compow :: nat \<Rightarrow> ('a \<times> 'a) set \<Rightarrow> ('a \<times> 'a) set"
begin

primrec relpow :: "nat \<Rightarrow> ('a \<times> 'a) set \<Rightarrow> ('a \<times> 'a) set" where
    "relpow 0 R = Id"
  | "relpow (Suc n) R = R O (R ^^ n)"

end

lemma rel_pow_1 [simp]:
  fixes R :: "('a \<times> 'a) set"
  shows "R ^^ 1 = R"
  by simp

lemma rel_pow_0_I: 
  "(x, x) \<in> R ^^ 0"
  by simp

lemma rel_pow_Suc_I:
  "(x, y) \<in>  R ^^ n \<Longrightarrow> (y, z) \<in> R \<Longrightarrow> (x, z) \<in> R ^^ Suc n"
  by auto

lemma rel_pow_Suc_I2:
  "(x, y) \<in> R \<Longrightarrow> (y, z) \<in> R ^^ n \<Longrightarrow> (x, z) \<in> R ^^ Suc n"
  by (induct n arbitrary: z) (simp, fastsimp)

lemma rel_pow_0_E:
  "(x, y) \<in> R ^^ 0 \<Longrightarrow> (x = y \<Longrightarrow> P) \<Longrightarrow> P"
  by simp

lemma rel_pow_Suc_E:
  "(x, z) \<in> R ^^ Suc n \<Longrightarrow> (\<And>y. (x, y) \<in> R ^^ n \<Longrightarrow> (y, z) \<in> R \<Longrightarrow> P) \<Longrightarrow> P"
  by auto

lemma rel_pow_E:
  "(x, z) \<in>  R ^^ n \<Longrightarrow>  (n = 0 \<Longrightarrow> x = z \<Longrightarrow> P)
   \<Longrightarrow> (\<And>y m. n = Suc m \<Longrightarrow> (x, y) \<in>  R ^^ m \<Longrightarrow> (y, z) \<in> R \<Longrightarrow> P)
   \<Longrightarrow> P"
  by (cases n) auto

lemma rel_pow_Suc_D2:
  "(x, z) \<in> R ^^ Suc n \<Longrightarrow> (\<exists>y. (x, y) \<in> R \<and> (y, z) \<in> R ^^ n)"
  apply (induct n arbitrary: x z)
   apply (blast intro: rel_pow_0_I elim: rel_pow_0_E rel_pow_Suc_E)
  apply (blast intro: rel_pow_Suc_I elim: rel_pow_0_E rel_pow_Suc_E)
  done

lemma rel_pow_Suc_E2:
  "(x, z) \<in> R ^^ Suc n \<Longrightarrow> (\<And>y. (x, y) \<in> R \<Longrightarrow> (y, z) \<in> R ^^ n \<Longrightarrow> P) \<Longrightarrow> P"
  by (blast dest: rel_pow_Suc_D2)

lemma rel_pow_Suc_D2':
  "\<forall>x y z. (x, y) \<in> R ^^ n \<and> (y, z) \<in> R \<longrightarrow> (\<exists>w. (x, w) \<in> R \<and> (w, z) \<in> R ^^ n)"
  by (induct n) (simp_all, blast)

lemma rel_pow_E2:
  "(x, z) \<in> R ^^ n \<Longrightarrow>  (n = 0 \<Longrightarrow> x = z \<Longrightarrow> P)
     \<Longrightarrow> (\<And>y m. n = Suc m \<Longrightarrow> (x, y) \<in> R \<Longrightarrow> (y, z) \<in> R ^^ m \<Longrightarrow> P)
   \<Longrightarrow> P"
  apply (cases n, simp)
  apply (cut_tac n=nat and R=R in rel_pow_Suc_D2', simp, blast)
  done

lemma rel_pow_add: "R ^^ (m+n) = R^^n O R^^m"
by(induct n) auto

lemma rtrancl_imp_UN_rel_pow:
  assumes "p \<in> R^*"
  shows "p \<in> (\<Union>n. R ^^ n)"
proof (cases p)
  case (Pair x y)
  with assms have "(x, y) \<in> R^*" by simp
  then have "(x, y) \<in> (\<Union>n. R ^^ n)" proof induct
    case base show ?case by (blast intro: rel_pow_0_I)
  next
    case step then show ?case by (blast intro: rel_pow_Suc_I)
  qed
  with Pair show ?thesis by simp
qed

lemma rel_pow_imp_rtrancl:
  assumes "p \<in> R ^^ n"
  shows "p \<in> R^*"
proof (cases p)
  case (Pair x y)
  with assms have "(x, y) \<in> R ^^ n" by simp
  then have "(x, y) \<in> R^*" proof (induct n arbitrary: x y)
    case 0 then show ?case by simp
  next
    case Suc then show ?case
      by (blast elim: rel_pow_Suc_E intro: rtrancl_into_rtrancl)
  qed
  with Pair show ?thesis by simp
qed

lemma rtrancl_is_UN_rel_pow:
  "R^* = (\<Union>n. R ^^ n)"
  by (blast intro: rtrancl_imp_UN_rel_pow rel_pow_imp_rtrancl)

lemma rtrancl_power:
  "p \<in> R^* \<longleftrightarrow> (\<exists>n. p \<in> R ^^ n)"
  by (simp add: rtrancl_is_UN_rel_pow)

lemma trancl_power:
  "p \<in> R^+ \<longleftrightarrow> (\<exists>n > 0. p \<in> R ^^ n)"
  apply (cases p)
  apply simp
  apply (rule iffI)
   apply (drule tranclD2)
   apply (clarsimp simp: rtrancl_is_UN_rel_pow)
   apply (rule_tac x="Suc n" in exI)
   apply (clarsimp simp: rel_comp_def)
   apply fastsimp
  apply clarsimp
  apply (case_tac n, simp)
  apply clarsimp
  apply (drule rel_pow_imp_rtrancl)
  apply (drule rtrancl_into_trancl1) apply auto
  done

lemma rtrancl_imp_rel_pow:
  "p \<in> R^* \<Longrightarrow> \<exists>n. p \<in> R ^^ n"
  by (auto dest: rtrancl_imp_UN_rel_pow)

lemma single_valued_rel_pow:
  fixes R :: "('a * 'a) set"
  shows "single_valued R \<Longrightarrow> single_valued (R ^^ n)"
  apply (induct n arbitrary: R)
  apply simp_all
  apply (rule single_valuedI)
  apply (fast dest: single_valuedD elim: rel_pow_Suc_E)
  done

subsection {* Setup of transitivity reasoner *}

ML {*

structure Trancl_Tac = Trancl_Tac_Fun (
  struct
    val r_into_trancl = @{thm trancl.r_into_trancl};
    val trancl_trans  = @{thm trancl_trans};
    val rtrancl_refl = @{thm rtrancl.rtrancl_refl};
    val r_into_rtrancl = @{thm r_into_rtrancl};
    val trancl_into_rtrancl = @{thm trancl_into_rtrancl};
    val rtrancl_trancl_trancl = @{thm rtrancl_trancl_trancl};
    val trancl_rtrancl_trancl = @{thm trancl_rtrancl_trancl};
    val rtrancl_trans = @{thm rtrancl_trans};

  fun decomp (@{const Trueprop} $ t) =
    let fun dec (Const ("op :", _) $ (Const ("Pair", _) $ a $ b) $ rel ) =
        let fun decr (Const ("Transitive_Closure.rtrancl", _ ) $ r) = (r,"r*")
              | decr (Const ("Transitive_Closure.trancl", _ ) $ r)  = (r,"r+")
              | decr r = (r,"r");
            val (rel,r) = decr (Envir.beta_eta_contract rel);
        in SOME (a,b,rel,r) end
      | dec _ =  NONE
    in dec t end
    | decomp _ = NONE;

  end);

structure Tranclp_Tac = Trancl_Tac_Fun (
  struct
    val r_into_trancl = @{thm tranclp.r_into_trancl};
    val trancl_trans  = @{thm tranclp_trans};
    val rtrancl_refl = @{thm rtranclp.rtrancl_refl};
    val r_into_rtrancl = @{thm r_into_rtranclp};
    val trancl_into_rtrancl = @{thm tranclp_into_rtranclp};
    val rtrancl_trancl_trancl = @{thm rtranclp_tranclp_tranclp};
    val trancl_rtrancl_trancl = @{thm tranclp_rtranclp_tranclp};
    val rtrancl_trans = @{thm rtranclp_trans};

  fun decomp (@{const Trueprop} $ t) =
    let fun dec (rel $ a $ b) =
        let fun decr (Const ("Transitive_Closure.rtranclp", _ ) $ r) = (r,"r*")
              | decr (Const ("Transitive_Closure.tranclp", _ ) $ r)  = (r,"r+")
              | decr r = (r,"r");
            val (rel,r) = decr rel;
        in SOME (a, b, rel, r) end
      | dec _ =  NONE
    in dec t end
    | decomp _ = NONE;

  end);
*}

declaration {* fn _ =>
  Simplifier.map_ss (fn ss => ss
    addSolver (mk_solver "Trancl" (fn _ => Trancl_Tac.trancl_tac))
    addSolver (mk_solver "Rtrancl" (fn _ => Trancl_Tac.rtrancl_tac))
    addSolver (mk_solver "Tranclp" (fn _ => Tranclp_Tac.trancl_tac))
    addSolver (mk_solver "Rtranclp" (fn _ => Tranclp_Tac.rtrancl_tac)))
*}

(* Optional methods *)

method_setup trancl =
  {* Scan.succeed (K (SIMPLE_METHOD' Trancl_Tac.trancl_tac)) *}
  {* simple transitivity reasoner *}
method_setup rtrancl =
  {* Scan.succeed (K (SIMPLE_METHOD' Trancl_Tac.rtrancl_tac)) *}
  {* simple transitivity reasoner *}
method_setup tranclp =
  {* Scan.succeed (K (SIMPLE_METHOD' Tranclp_Tac.trancl_tac)) *}
  {* simple transitivity reasoner (predicate version) *}
method_setup rtranclp =
  {* Scan.succeed (K (SIMPLE_METHOD' Tranclp_Tac.rtrancl_tac)) *}
  {* simple transitivity reasoner (predicate version) *}

end
