(*  Title:      HOL/Real/HahnBanach/FunctionNorm.thy
    ID:         $Id$
    Author:     Gertrud Bauer, TU Munich
*)

header {* The norm of a function *};

theory FunctionNorm = NormedSpace + FunctionOrder:;

subsection {* Continous linearforms*};

text{* A linearform $f$ on a normed vector space $(V, \norm{\cdot})$
is \emph{continous}, iff it is bounded, i.~e.
\[\exists\ap c\in R.\ap \forall\ap x\in V.\ap 
|f\ap x| \leq c \cdot \norm x\]
In our application no other functions than linearforms are considered,
so we can define continous linearforms as follows:
*};

constdefs
  is_continous ::
  "['a::{minus, plus} set, 'a => real, 'a => real] => bool" 
  "is_continous V norm f == 
    is_linearform V f & (EX c. ALL x:V. rabs (f x) <= c * norm x)";

lemma continousI [intro]: 
  "[| is_linearform V f; !! x. x:V ==> rabs (f x) <= c * norm x |] 
  ==> is_continous V norm f";
proof (unfold is_continous_def, intro exI conjI ballI);
  assume r: "!! x. x:V ==> rabs (f x) <= c * norm x"; 
  fix x; assume "x:V"; show "rabs (f x) <= c * norm x"; by (rule r);
qed;
  
lemma continous_linearform [intro!!]: 
  "is_continous V norm f ==> is_linearform V f";
  by (unfold is_continous_def) force;

lemma continous_bounded [intro!!]:
  "is_continous V norm f 
  ==> EX c. ALL x:V. rabs (f x) <= c * norm x";
  by (unfold is_continous_def) force;

subsection{* The norm of a linearform *};


text{* The least real number $c$ for which holds
\[\forall\ap x\in V.\ap |f\ap x| \leq c \cdot \norm x\]
is called the \emph{norm} of $f$.

For the non-trivial vector space $V$ the norm can be defined as 
\[\fnorm {f} =\sup_{x\neq\zero}\frac{|f\ap x|}{\norm x} \] 

For the case that the vector space $V$ contains only the $\zero$
vector, the set $B$ this supremum is taken from would be empty, and
any real number is a supremum of $B$. So it must be guarateed that
there is an element in $B$. This element must be ${} \ge 0$ so that
$\idt{function{\dsh}norm}$ has the norm properties. Furthermore it
does not have to change the norm in all other cases, so it must be
$0$, as all other elements of $B$ are ${} \ge 0$.

Thus $B$ is defined as follows.
*};

constdefs
  B :: "[ 'a set, 'a => real, 'a => real ] => real set"
  "B V norm f == 
  {z. z = 0r | (EX x:V. x ~= <0> & z = rabs (f x) * rinv (norm x))}";

text{* $\idt{function{\dsh}norm}$ is equal to the supremum of $B$, 
if there exists a supremum. *};

constdefs 
  function_norm :: " ['a set, 'a => real, 'a => real] => real"
  "function_norm V norm f == Sup UNIV (B V norm f)";

text{* $\idt{is{\dsh}function{\dsh}norm}$ also guarantees that there 
is a funciton norm .*};

constdefs 
  is_function_norm :: 
  " ['a set, 'a => real, 'a => real] => real => bool"
  "is_function_norm V norm f fn == is_Sup UNIV (B V norm f) fn";

lemma B_not_empty: "0r : B V norm f";
  by (unfold B_def, force);

text {* The following lemma states every continous linearform on a 
normed space $(V, \norm{\cdot})$ has a function norm. *};

lemma ex_fnorm [intro!!]: 
  "[| is_normed_vectorspace V norm; is_continous V norm f|]
     ==> is_function_norm V norm f (function_norm V norm f)"; 
proof (unfold function_norm_def is_function_norm_def 
  is_continous_def Sup_def, elim conjE, rule selectI2EX);
  assume "is_normed_vectorspace V norm";
  assume "is_linearform V f" 
  and e: "EX c. ALL x:V. rabs (f x) <= c * norm x";

  txt {* The existence of the supremum is shown using the 
  completeness of the reals. Completeness means, that
  for every non-empty and bounded set of reals there exists a 
  supremum. *};
  show  "EX a. is_Sup UNIV (B V norm f) a"; 
  proof (unfold is_Sup_def, rule reals_complete);

    txt {* First we have to show that $B$ is non-empty. *}; 

    show "EX X. X : B V norm f"; 
    proof (intro exI);
      show "0r : (B V norm f)"; by (unfold B_def, force);
    qed;

    txt {* Then we have to show that $B$ is bounded. *};

    from e; show "EX Y. isUb UNIV (B V norm f) Y";
    proof;

      txt {* We know that $f$ is bounded by some value $c$. *};  
  
      fix c; assume a: "ALL x:V. rabs (f x) <= c * norm x";
      def b == "max c 0r";

      show "?thesis";
      proof (intro exI isUbI setleI ballI, unfold B_def, 
	elim CollectE disjE bexE conjE);

        txt{* To proof the thesis, we have to show that there is 
        some constant b, which is greater than every $y$ in $B$. 
        Due to the definition of $B$ there are two cases for
        $y\in B$. If $y = 0$ then $y$ is less than 
        $\idt{max}\ap c\ap 0$: *};

	fix y; assume "y = 0r";
        show "y <= b"; by (simp! add: le_max2);

        txt{* The second case is 
        $y = \frac{|f\ap x|}{\norm x}$ for some 
        $x\in V$ with $x \neq \zero$. *};

      next;
	fix x y;
        assume "x:V" "x ~= <0>"; (***

         have ge: "0r <= rinv (norm x)";
          by (rule real_less_imp_le, rule real_rinv_gt_zero, 
                rule normed_vs_norm_gt_zero); (***
          proof (rule real_less_imp_le);
            show "0r < rinv (norm x)";
            proof (rule real_rinv_gt_zero);
              show "0r < norm x"; ..;
            qed;
          qed; ***)
        have nz: "norm x ~= 0r"; 
          by (rule not_sym, rule lt_imp_not_eq, 
              rule normed_vs_norm_gt_zero); (***
          proof (rule not_sym);
            show "0r ~= norm x"; 
            proof (rule lt_imp_not_eq);
              show "0r < norm x"; ..;
            qed;
          qed; ***)***)

        txt {* The thesis follows by a short calculation using the 
        fact that $f$ is bounded. *};
    
        assume "y = rabs (f x) * rinv (norm x)";
        also; have "... <= c * norm x * rinv (norm x)";
        proof (rule real_mult_le_le_mono2);
          show "0r <= rinv (norm x)";
            by (rule real_less_imp_le, rule real_rinv_gt_zero, 
                rule normed_vs_norm_gt_zero);
          from a; show "rabs (f x) <= c * norm x"; ..;
        qed;
        also; have "... = c * (norm x * rinv (norm x))"; 
          by (rule real_mult_assoc);
        also; have "(norm x * rinv (norm x)) = 1r"; 
        proof (rule real_mult_inv_right);
          show nz: "norm x ~= 0r"; 
            by (rule not_sym, rule lt_imp_not_eq, 
              rule normed_vs_norm_gt_zero);
        qed;
        also; have "c * ... <= b "; by (simp! add: le_max1);
	finally; show "y <= b"; .;
      qed simp;
    qed;
  qed;
qed;

text {* The norm of a continous function is always $\geq 0$. *};

lemma fnorm_ge_zero [intro!!]: 
  "[| is_continous V norm f; is_normed_vectorspace V norm|]
   ==> 0r <= function_norm V norm f";
proof -;
  assume c: "is_continous V norm f" 
     and n: "is_normed_vectorspace V norm";

  txt {* The function norm is defined as the supremum of $B$. 
  So it is $\geq 0$ if all elements in $B$ are $\geq 0$, provided
  the supremum exists and $B$ is not empty. *};

  show ?thesis; 
  proof (unfold function_norm_def, rule sup_ub1);
    show "ALL x:(B V norm f). 0r <= x"; 
    proof (intro ballI, unfold B_def, 
           elim CollectE bexE conjE disjE);
      fix x r; 
      assume "x : V" "x ~= <0>" 
        and r: "r = rabs (f x) * rinv (norm x)";

      have ge: "0r <= rabs (f x)"; by (simp! only: rabs_ge_zero);
      have "0r <= rinv (norm x)"; 
        by (rule real_less_imp_le, rule real_rinv_gt_zero, rule);(***
        proof (rule real_less_imp_le);
          show "0r < rinv (norm x)"; 
          proof (rule real_rinv_gt_zero);
            show "0r < norm x"; ..;
          qed;
        qed; ***)
      with ge; show "0r <= r";
       by (simp only: r,rule real_le_mult_order);
    qed (simp!);

    txt {* Since $f$ is continous the function norm exists. *};

    have "is_function_norm V norm f (function_norm V norm f)"; ..;
    thus "is_Sup UNIV (B V norm f) (Sup UNIV (B V norm f))"; 
      by (unfold is_function_norm_def, unfold function_norm_def);

    txt {* $B$ is non-empty by construction. *};

    show "0r : B V norm f"; by (rule B_not_empty);
  qed;
qed;
  
text{* The basic property of function norms is: 
\begin{matharray}{l}
| f\ap x | \leq {\fnorm {f}} \cdot {\norm x}  
\end{matharray}
*};

lemma norm_fx_le_norm_f_norm_x: 
  "[| is_normed_vectorspace V norm; x:V; is_continous V norm f |] 
    ==> rabs (f x) <= (function_norm V norm f) * norm x"; 
proof -; 
  assume "is_normed_vectorspace V norm" "x:V" 
  and c: "is_continous V norm f";
  have v: "is_vectorspace V"; ..;
  assume "x:V";

 txt{* The proof is by case analysis on $x$. *};
 
  show ?thesis;
  proof (rule case_split);

    txt {* For the case $x = \zero$ the thesis follows
    from the linearity of $f$: for every linear function
    holds $f\ap \zero = 0$. *};

    assume "x = <0>";
    have "rabs (f x) = rabs (f <0>)"; by (simp!);
    also; from v continous_linearform; have "f <0> = 0r"; ..;
    also; note rabs_zero;
    also; have "0r <= function_norm V norm f * norm x";
    proof (rule real_le_mult_order);
      show "0r <= function_norm V norm f"; ..;
      show "0r <= norm x"; ..;
    qed;
    finally; 
    show "rabs (f x) <= function_norm V norm f * norm x"; .;

  next;
    assume "x ~= <0>";
    have n: "0r <= norm x"; ..;
    have nz: "norm x ~= 0r";
    proof (rule lt_imp_not_eq [RS not_sym]);
      show "0r < norm x"; ..;
    qed;

    txt {* For the case $x\neq \zero$ we derive the following
    fact from the definition of the function norm:*};

    have l: "rabs (f x) * rinv (norm x) <= function_norm V norm f";
    proof (unfold function_norm_def, rule sup_ub);
      from ex_fnorm [OF _ c];
      show "is_Sup UNIV (B V norm f) (Sup UNIV (B V norm f))";
         by (simp! add: is_function_norm_def function_norm_def);
      show "rabs (f x) * rinv (norm x) : B V norm f"; 
        by (unfold B_def, intro CollectI disjI2 bexI [of _ x]
            conjI, simp);
    qed;

    txt {* The thesis follows by a short calculation: *};

    have "rabs (f x) = rabs (f x) * 1r"; by (simp!);
    also; from nz; have "1r = rinv (norm x) * norm x"; 
      by (rule real_mult_inv_left [RS sym]);
    also; 
    have "rabs (f x) * ... = rabs (f x) * rinv (norm x) * norm x";
      by (simp! add: real_mult_assoc [of "rabs (f x)"]);
    also; have "... <= function_norm V norm f * norm x"; 
      by (rule real_mult_le_le_mono2 [OF n l]);
    finally; 
    show "rabs (f x) <= function_norm V norm f * norm x"; .;
  qed;
qed;

text{* The function norm is the least positive real number for 
which the inequation
\begin{matharray}{l}
| f\ap x | \leq c \cdot {\norm x}  
\end{matharray} 
holds.
*};

lemma fnorm_le_ub: 
  "[| is_normed_vectorspace V norm; is_continous V norm f;
     ALL x:V. rabs (f x) <= c * norm x; 0r <= c |]
  ==> function_norm V norm f <= c";
proof (unfold function_norm_def);
  assume "is_normed_vectorspace V norm"; 
  assume c: "is_continous V norm f";
  assume fb: "ALL x:V. rabs (f x) <= c * norm x"
         and "0r <= c";

  txt {* Suppose the inequation holds for some $c\geq 0$.
  If $c$ is an upper bound of $B$, then $c$ is greater 
  than the function norm since the function norm is the
  least upper bound.
  *};

  show "Sup UNIV (B V norm f) <= c"; 
  proof (rule sup_le_ub);
    from ex_fnorm [OF _ c]; 
    show "is_Sup UNIV (B V norm f) (Sup UNIV (B V norm f))"; 
      by (simp! add: is_function_norm_def function_norm_def); 
  
    txt {* $c$ is an upper bound of $B$, i.~e.~every
    $y\in B$ is less than $c$. *};

    show "isUb UNIV (B V norm f) c";  
    proof (intro isUbI setleI ballI);
      fix y; assume "y: B V norm f";
      thus le: "y <= c";
      proof (unfold B_def, elim CollectE disjE bexE conjE);

       txt {* The first case for $y\in B$ is $y=0$. *};

        assume "y = 0r";
        show "y <= c"; by (force!);

        txt{* The second case is 
        $y = \frac{|f\ap x|}{\norm x}$ for some 
        $x\in V$ with $x \neq \zero$. *};  

      next;
	fix x; 
        assume "x : V" "x ~= <0>"; 

        have lz: "0r < norm x"; 
          by (simp! add: normed_vs_norm_gt_zero);
          
        have nz: "norm x ~= 0r"; 
        proof (rule not_sym);
          from lz; show "0r ~= norm x";
            by (simp! add: order_less_imp_not_eq);
        qed;
            
	from lz; have "0r < rinv (norm x)";
	  by (simp! add: real_rinv_gt_zero);
	hence rinv_gez: "0r <= rinv (norm x)";
          by (rule real_less_imp_le);

	assume "y = rabs (f x) * rinv (norm x)"; 
	also; from rinv_gez; have "... <= c * norm x * rinv (norm x)";
	  proof (rule real_mult_le_le_mono2);
	    show "rabs (f x) <= c * norm x"; by (rule bspec);
	  qed;
	also; have "... <= c"; by (simp add: nz real_mult_assoc);
	finally; show ?thesis; .;
      qed;
    qed force;
  qed;
qed;

end;