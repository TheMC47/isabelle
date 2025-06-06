(*  Title:      Pure/Isar/runtime.ML
    Author:     Makarius

Isar toplevel runtime support.
*)

signature RUNTIME =
sig
  exception UNDEF
  exception EXCURSION_FAIL of exn * string
  exception TOPLEVEL_ERROR
  val pretty_exn: exn -> Pretty.T
  val exn_context: Proof.context option -> exn -> exn
  val thread_context: exn -> exn
  type error = ((serial * string) * string option)
  val exn_messages: exn -> error list
  val exn_message_list: exn -> string list
  val exn_message: exn -> string
  val exn_error_message: exn -> unit
  val exn_system_message: exn -> unit
  val exn_trace: (unit -> 'a) -> 'a
  val exn_trace_system: (unit -> 'a) -> 'a
  val exn_debugger: (unit -> 'a) -> 'a
  val exn_debugger_system: (unit -> 'a) -> 'a
  val debugging: Context.generic option -> ('a -> 'b) -> 'a -> 'b
  val controlled_execution: Context.generic option -> ('a -> 'b) -> 'a -> 'b
  val toplevel_error: (exn -> unit) -> ('a -> 'b) -> 'a -> 'b
  val toplevel_program: (unit -> 'a) -> 'a
end;

structure Runtime: RUNTIME =
struct

(** exceptions **)

exception UNDEF;
exception EXCURSION_FAIL of exn * string;
exception TOPLEVEL_ERROR;


(* pretty *)

fun pretty_exn (exn: exn) =
  Pretty.from_ML (ML_system_pretty (exn, FixedInt.fromInt (ML_Print_Depth.get_print_depth ())));


(* exn_context *)

exception CONTEXT of Proof.context * exn;

fun exn_context NONE exn = exn
  | exn_context (SOME ctxt) exn = if Exn.is_interrupt exn then exn else CONTEXT (ctxt, exn);

fun thread_context exn =
  exn_context (Option.map Context.proof_of (Context.get_generic_context ())) exn;


(* exn_message *)

type error = ((serial * string) * string option);

local

fun robust f x =
  (case try f x of
    SOME s => s
  | NONE => Markup.markup Markup.intensify "<malformed>");

fun robust2 f x y = robust (fn () => f x y) ();

fun robust_context NONE _ _ = []
  | robust_context (SOME ctxt) f xs = map (robust2 f ctxt) xs;

fun identify exn =
  let
    val exn' = Exn_Properties.identify [] exn;
    val exec_id = Properties.get (Exn_Properties.get exn') Markup.exec_idN;
    val i = Exn_Properties.the_serial exn' handle Option.Option => serial ();
  in ((i, exn'), exec_id) end;

fun flatten _ (CONTEXT (ctxt, exn)) = flatten (SOME ctxt) exn
  | flatten context (Exn.EXCEPTIONS exns) = maps (flatten context) exns
  | flatten context exn =
      (case Par_Exn.dest exn of
        SOME exns => maps (flatten context) exns
      | NONE => [(context, identify exn)]);

val print_thy = Pretty.unformatted_string_of o Context.pretty_abbrev_thy;

in

fun exn_messages e =
  let
    fun raised exn name msgs =
      let val pos = Position.here (Exn_Properties.position exn) in
        (case msgs of
          [] => "exception " ^ name ^ " raised" ^ pos
        | [msg] => "exception " ^ name ^ " raised" ^ pos ^ ": " ^ msg
        | _ =>
            cat_lines (("exception " ^ name ^ " raised" ^ pos ^ ":") ::
              map (Markup.markup Markup.item) msgs))
      end;

    fun exn_msgs (context, ((i, exn), id)) =
      (case exn of
        EXCURSION_FAIL (exn, loc) =>
          map (fn ((i, msg), id) => ((i, msg ^ Markup.markup Markup.no_report ("\n" ^ loc)), id))
            (sorted_msgs context exn)
      | Morphism.MORPHISM (name, exn) =>
          map (fn ((i, msg), id) => ((i, "MORPHISM " ^ quote name ^ "\n" ^ msg), id))
            (sorted_msgs context exn)
      | _ =>
        let
          val msg =
            (case exn of
              Timeout.TIMEOUT t => Timeout.message t
            | TOPLEVEL_ERROR => "Error"
            | ERROR "" => "Error"
            | ERROR msg => msg
            | Fail msg => raised exn "Fail" [msg]
            | THEORY (msg, thys) => raised exn "THEORY" (msg :: map (robust print_thy) thys)
            | Ast.AST (msg, asts) =>
                raised exn "AST" (msg :: map (robust (Pretty.string_of o Ast.pretty_ast)) asts)
            | TYPE (msg, Ts, ts) =>
                raised exn "TYPE" (msg ::
                  (robust_context context Syntax.string_of_typ Ts @
                    robust_context context Syntax.string_of_term ts))
            | TERM (msg, ts) =>
                raised exn "TERM" (msg :: robust_context context Syntax.string_of_term ts)
            | CTERM (msg, cts) =>
                raised exn "CTERM"
                  (msg :: robust_context context Syntax.string_of_term (map Thm.term_of cts))
            | THM (msg, i, thms) =>
                raised exn ("THM " ^ string_of_int i)
                  (msg :: robust_context context Thm.string_of_thm thms)
            | _ => raised exn (robust (Pretty.string_of o pretty_exn) exn) []);
        in [((i, msg), id)] end)
      and sorted_msgs context exn =
        sort_distinct (int_ord o apply2 (fst o fst)) (maps exn_msgs (flatten context exn));

  in sorted_msgs NONE e end;

end;

fun exn_message_list exn =
  (case exn_messages exn of
    [] => ["Interrupt"]
  | msgs => map (#2 o #1) msgs);

val exn_message = cat_lines o exn_message_list;

val exn_error_message = Output.error_message o exn_message;
val exn_system_message = Output.system_message o exn_message;
fun exn_trace e = Exn.trace exn_message tracing e;
fun exn_trace_system e = Exn.trace exn_message Output.system_message e;


(* exception debugger *)

local

fun print_entry (name, loc) =
  Markup.markup Markup.item (name ^ Position.here (Exn_Properties.position_of_polyml_location loc));

fun exception_debugger output e =
  let
    val (trace, result) = Exn_Debugger.capture_exception_trace e;
    val _ =
      (case (trace, result) of
        (_ :: _, Exn.Exn exn) =>
          output (cat_lines ("Exception trace - " ^ exn_message exn :: map print_entry trace))
      | _ => ());
  in Exn.release result end;

in

fun exn_debugger e = exception_debugger tracing e;
fun exn_debugger_system e = exception_debugger Output.system_message e;

end;



(** controlled execution **)

fun debugging1 opt_context f x =
  if ML_Options.exception_trace_enabled opt_context
  then exn_trace (fn () => f x) else f x;

fun debugging2 opt_context f x =
  if ML_Options.exception_debugger_enabled opt_context
  then exn_debugger (fn () => f x) else f x;

fun debugging opt_context f =
  f |> debugging1 opt_context |> debugging2 opt_context;

fun controlled_execution opt_context f x =
  (f |> debugging opt_context |> Future.interruptible_task
    |> ML_Profiling.profile (Options.default_string "profiling")) x;

fun toplevel_error output_exn f x =
  Isabelle_Thread.try_catch (fn () => f x)
    (fn exn =>
      let
        val opt_ctxt =
          (case Context.get_generic_context () of
            NONE => NONE
          | SOME context => try Context.proof_of context);
        val _ = output_exn (exn_context opt_ctxt exn);
      in raise TOPLEVEL_ERROR end);

fun toplevel_program body =
  (body |> controlled_execution NONE |> toplevel_error exn_error_message) ();

end;
