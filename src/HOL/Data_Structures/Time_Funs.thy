(*
  File:    Data_Structures/Time_Functions.thy
  Author:  Manuel Eberl, TU München
*)
section \<open>Time functions for various standard library operations\<close>
theory Time_Funs
  imports Define_Time_Function
begin

time_fun "(@)"

lemma T_append: "T_append xs ys = length xs + 1"
by(induction xs) auto

class T_size =
  fixes T_size :: "'a \<Rightarrow> nat"

instantiation list :: (_) T_size
begin

time_fun length

instance ..

end

abbreviation T_length :: "'a list \<Rightarrow> nat" where
"T_length \<equiv> T_size"

lemma T_length_eq: "T_length xs = length xs + 1"
  by (induction xs) auto

lemmas [simp del] = T_size_list.simps

time_fun map

lemma T_map_simps [simp,code]:
  "T_map T_f [] = 1"
  "T_map T_f (x # xs) = T_f x + T_map T_f xs + 1"
by (simp_all add: T_map_def)

lemma T_map_eq: "T_map T_f xs = (\<Sum>x\<leftarrow>xs. T_f x) + length xs + 1"
  by (induction xs) auto

lemmas [simp del] = T_map_simps

time_fun filter

lemma T_filter_simps [code]:
  "T_filter T_P [] = 1"
  "T_filter T_P (x # xs) = T_P x + T_filter T_P xs + 1"
by (simp_all add: T_filter_def)

lemma T_filter_eq: "T_filter T_P xs = (\<Sum>x\<leftarrow>xs. T_P x) + length xs + 1"
by (induction xs) (auto simp: T_filter_simps)

time_fun nth

lemma T_nth_eq: "n < length xs \<Longrightarrow> T_nth xs n = n + 1"
  by (induction xs n rule: T_nth.induct) (auto split: nat.splits)

lemmas [simp del] = T_nth.simps

time_fun take
time_fun drop

lemma T_take_eq: "T_take n xs = min n (length xs) + 1"
  by (induction xs arbitrary: n) (auto split: nat.splits)

lemma T_drop_eq: "T_drop n xs = min n (length xs) + 1"
  by (induction xs arbitrary: n) (auto split: nat.splits)
  
end
