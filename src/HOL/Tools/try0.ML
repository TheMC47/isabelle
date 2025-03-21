(*  Title:      HOL/Tools/try0.ML
    Author:     Jasmin Blanchette, TU Muenchen

Try a combination of proof methods.
*)

signature TRY0 =
sig
  val silence_methods : bool -> Proof.context -> Proof.context
  datatype modifier = Use | Simp | Intro | Elim | Dest
  type xthm = Facts.ref * Token.src list
  type tagged_xthm = xthm * modifier list
  type result = {name: string, command: string, time: Time.time, state: Proof.state}
  val apply_proof_method : string -> Time.time option -> (xthm * modifier list) list ->
    Proof.state -> result option
  val try0 : Time.time option -> tagged_xthm list -> Proof.state -> result list
end

structure Try0 : TRY0 =
struct

val noneN = "none"

datatype mode = Auto_Try | Try | Normal

val default_timeout = seconds 5.0

fun run_tac timeout_opt tac st =
  let val with_timeout =
    (case timeout_opt of SOME timeout => Timeout.apply_physical timeout | NONE => I)
  in with_timeout (Seq.pull o tac) st |> Option.map fst end

val num_goals = Thm.nprems_of o #goal o Proof.goal
fun apply_recursive recurse elapsed0 timeout_opt apply st =
  (case Timing.timing (Option.join o try (run_tac timeout_opt apply)) st of
    ({elapsed, ...}, SOME st') =>
      if recurse andalso num_goals st' > 0 andalso num_goals st' < num_goals st then
        let val timeout_opt1 = (Option.map (fn timeout => timeout - elapsed) timeout_opt)
        in apply_recursive recurse (elapsed0 + elapsed) timeout_opt1 apply st' end
      else (elapsed0 + elapsed, st')
   |_ => (elapsed0, st))

fun parse_method ctxt s =
  enclose "(" ")" s
  |> Token.explode (Thy_Header.get_keywords' ctxt) Position.start
  |> filter Token.is_proper
  |> Scan.read Token.stopper Method.parse
  |> (fn SOME (Method.Source src, _) => src | _ => raise Fail "expected Source")

datatype modifier = Use | Simp | Intro | Elim | Dest
type xthm = Facts.ref * Token.src list
type tagged_xthm = xthm * modifier list

fun string_of_xthm ((xref, args) : xthm) =
  (case xref of
    Facts.Fact literal => literal |> Symbol_Pos.explode0 |> Symbol_Pos.implode |> cartouche
  | _ =>
      Facts.string_of_ref xref) ^ implode
        (map (enclose "[" "]" o Pretty.unformatted_string_of o Token.pretty_src \<^context>) args)

fun add_attr_text (tagged : tagged_xthm list) (tag, src) s =
  let
    val fs = tagged |> filter (fn (_, tags) => member (op =) tags tag) |> map (string_of_xthm o fst)
  in if null fs then s else s ^ " " ^ (if src = "" then "" else src ^ ": ") ^ implode_space fs end

fun attrs_text tags (tagged : tagged_xthm list) =
  "" |> fold (add_attr_text tagged) tags

type result = {name: string, command: string, time: Time.time, state: Proof.state}

local

val full_attrs = [(Simp, "simp"), (Intro, "intro"), (Elim, "elim"), (Dest, "dest")]
val clas_attrs = [(Intro, "intro"), (Elim, "elim"), (Dest, "dest")]
val simp_attrs = [(Simp, "add")]
val metis_attrs = [(Simp, ""), (Intro, ""), (Elim, ""), (Dest, "")]
val no_attrs = []

(* name * ((all_goals, run_if_auto_try), tags *)
val raw_named_methods =
  [("simp", ((false, true), simp_attrs)),
   ("auto", ((true, true), full_attrs)),
   ("blast", ((false, true), clas_attrs)),
   ("metis", ((false, true), metis_attrs)),
   ("argo", ((false, true), no_attrs)),
   ("linarith", ((false, true), no_attrs)),
   ("presburger", ((false, true), no_attrs)),
   ("algebra", ((false, true), no_attrs)),
   ("fast", ((false, false), clas_attrs)),
   ("fastforce", ((false, false), full_attrs)),
   ("force", ((false, false), full_attrs)),
   ("meson", ((false, false), metis_attrs)),
   ("satx", ((false, false), no_attrs)),
   ("order", ((false, true), no_attrs))]

fun apply_raw_named_method (name, ((all_goals, _), attrs)) timeout_opt tagged st :
    result option =
  let
    val unused =
      tagged
      |> filter
        (fn (_, tags) => not (null tags) andalso null (inter (op =) tags (attrs |> map fst)))
      |> map fst

    val attrs = attrs_text attrs tagged

    val ctxt = Proof.context_of st

    val text =
      name ^ attrs
      |> parse_method ctxt
      |> Method.method_cmd ctxt
      |> Method.Basic
      |> (fn m => Method.Combinator (Method.no_combinator_info, Method.Select_Goals 1, [m]))

    val apply =
      Proof.using [Attrib.eval_thms ctxt unused |> map (rpair [] o single)]
      #> Proof.refine text #> Seq.filter_results
    val num_before = num_goals st
    val multiple_goals = all_goals andalso num_before > 1
    val (time, st') = apply_recursive multiple_goals Time.zeroTime timeout_opt apply st
    val num_after = num_goals st'
    val select = "[" ^ string_of_int (num_before - num_after)  ^ "]"
    val unused = implode_space (unused |> map string_of_xthm)
    val command =
      (if unused <> "" then "using " ^ unused ^ " " else "") ^
      (if num_after = 0 then "by " else "apply ") ^
      (name ^ attrs |> attrs <> "" ? enclose "(" ")") ^
      (if multiple_goals andalso num_after > 0 then select else "")
  in
    if num_before > num_after then
      SOME {name = name, command = command, time = time, state = st'}
    else NONE
  end

in

val named_methods = map fst raw_named_methods

fun apply_proof_method name timeout_opt tagged st :
  result option =
  (case AList.lookup (op =) raw_named_methods name of
    NONE => NONE
  | SOME raw_method => apply_raw_named_method (name, raw_method) timeout_opt tagged st)

fun maybe_apply_proof_method name mode timeout_opt tagged st :
  result option =
  (case AList.lookup (op =) raw_named_methods name of
    NONE => NONE
  | SOME (raw_method as ((_, run_if_auto_try), _)) =>
    if mode <> Auto_Try orelse run_if_auto_try then
      apply_raw_named_method (name, raw_method) timeout_opt tagged st
    else
      NONE)

end

val maybe_apply_methods = map maybe_apply_proof_method named_methods

fun time_string time = string_of_int (Time.toMilliseconds time) ^ " ms"
fun tool_time_string (s, time) = s ^ ": " ^ time_string time

(* Makes reconstructor tools as silent as possible. The "set_visible" calls suppresses "Unification
   bound exceeded" warnings and the like. *)
fun silence_methods debug =
  Config.put Metis_Tactic.verbose debug
  #> not debug ? (fn ctxt =>
      ctxt
      |> Simplifier_Trace.disable
      |> Context_Position.set_visible false
      |> Config.put Unify.unify_trace false
      |> Config.put Argo_Tactic.trace "none")

fun generic_try0 mode timeout_opt (tagged : tagged_xthm list) st =
  let
    val st = Proof.map_contexts (silence_methods false) st
    fun try_method method = method mode timeout_opt tagged st
    fun get_message {command, time, ...} = "Found proof: " ^ Active.sendback_markup_command
      command ^ " (" ^ time_string time ^ ")"
    val print_step = Option.map (tap (writeln o get_message))
    fun get_results methods : result list =
      if mode = Normal then
        methods
        |> Par_List.map (try_method #> print_step)
        |> map_filter I
        |> sort (Time.compare o apply2 #time)
      else
        methods
        |> Par_List.get_some try_method
        |> the_list
  in
    if mode = Normal then
      "Trying " ^ implode_space (Try.serial_commas "and" (map quote named_methods)) ^
      "..."
      |> writeln
    else
      ();
    (case get_results maybe_apply_methods of
      [] => (if mode = Normal then writeln "No proof found" else (); ((false, (noneN, [])), []))
    | results as {name, command, ...} :: _ =>
      let
        val method_times =
          results
          |> map (fn {name, time, ...} => (time, name))
          |> AList.coalesce (op =)
          |> map (swap o apsnd commas)
        val message =
          (case mode of
             Auto_Try => "Auto Try0 found a proof"
           | Try => "Try0 found a proof"
           | Normal => "Try this") ^ ": " ^
          Active.sendback_markup_command command ^
          (case method_times of
            [(_, ms)] => " (" ^ time_string ms ^ ")"
          | method_times => "\n(" ^ space_implode "; " (map tool_time_string method_times) ^ ")")
      in
        ((true, (name, if mode = Auto_Try then [message] else (writeln message; []))), results)
      end)
  end

fun try0 timeout_opt = snd oo generic_try0 Normal timeout_opt

fun try0_trans tagged =
  Toplevel.keep_proof
    (ignore o generic_try0 Normal (SOME default_timeout) tagged o Toplevel.proof_of)

val parse_fact_refs = Scan.repeat1 (Scan.unless (Parse.name -- Args.colon) Parse.thm)

val parse_attr =
  Args.$$$ "simp" |-- Args.colon |-- parse_fact_refs >> (map (rpair [Simp]))
  || Args.$$$ "intro" |-- Args.colon |-- parse_fact_refs >> (map (rpair [Intro]))
  || Args.$$$ "elim" |-- Args.colon |-- parse_fact_refs >> (map (rpair [Elim]))
  || Args.$$$ "dest" |-- Args.colon |-- parse_fact_refs >> (map (rpair [Dest]))

fun parse_attrs x =
  (Args.parens parse_attrs
   || Scan.repeat parse_attr >> (fn tagged => fold (curry (op @)) tagged [])) x

val _ =
  Outer_Syntax.command \<^command_keyword>\<open>try0\<close> "try a combination of proof methods"
    (Scan.optional parse_attrs [] #>> try0_trans)

val _ =
  Try.tool_setup
   {name = "try0", weight = 30, auto_option = \<^system_option>\<open>auto_methods\<close>,
    body = fn auto => fst o generic_try0 (if auto then Auto_Try else Try) NONE []}

end
