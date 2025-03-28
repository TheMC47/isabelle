(*  Title:      Pure/System/bash.ML
    Author:     Makarius

Support for GNU bash.
*)

signature BASH =
sig
  val string: string -> string
  val strings: string list -> string
  type params
  val dest_params: params ->
   {script: string, input: Bytes.T, cwd: Path.T option, putenv: (string * string) list,
    redirect: bool, timeout: Time.time, description: string}
  val script: string -> params
  val input: Bytes.T -> params -> params
  val cwd: Path.T -> params -> params
  val putenv: (string * string) list -> params -> params
  val redirect: params -> params
  val timeout: Time.time -> params -> params
  val description: string -> params -> params
  val server_run: string
  val server_kill: string
  val server_uuid: string
  val server_interrupt: string
  val server_failure: string
  val server_result: string
end;

structure Bash: BASH =
struct

(* concrete syntax *)

fun string "" = "\"\""
  | string str =
      str |> translate_string (fn ch =>
        let val c = ord ch in
          (case ch of
            "\t" => "$'\\t'"
          | "\n" => "$'\\n'"
          | "\f" => "$'\\f'"
          | "\r" => "$'\\r'"
          | _ =>
              if Symbol.is_ascii_letter ch orelse Symbol.is_ascii_digit ch orelse
                member_string "+,-./:_" ch then ch
              else if c < 16 then "$'\\x0" ^ Int.fmt StringCvt.HEX c ^ "'"
              else if c < 32 orelse c >= 127 then "$'\\x" ^ Int.fmt StringCvt.HEX c ^ "'"
              else "\\" ^ ch)
        end);

val strings = implode_space o map string;


(* server parameters *)

abstype params =
  Params of
   {script: string, input: Bytes.T, cwd: Path.T option, putenv: (string * string) list,
    redirect: bool, timeout: Time.time, description: string}
with

fun dest_params (Params args) = args;

fun make_params (script, input, cwd, putenv, redirect, timeout, description) =
  Params {script = script, input = input, cwd = cwd, putenv = putenv, redirect = redirect,
    timeout = timeout, description = description};

fun map_params f (Params {script, input, cwd, putenv, redirect, timeout, description}) =
  make_params (f (script, input, cwd, putenv, redirect, timeout, description));

fun script script = make_params (script, Bytes.empty, NONE, [], false, Time.zeroTime, "");

fun input input =
  map_params (fn (script, _, cwd, putenv, redirect, timeout, description) =>
    (script, input, cwd, putenv, redirect, timeout, description));

fun cwd cwd =
  map_params (fn (script, input, _, putenv, redirect, timeout, description) =>
    (script, input, SOME cwd, putenv, redirect, timeout, description));

fun putenv putenv =
  map_params (fn (script, input, cwd, _, redirect, timeout, description) =>
    (script, input, cwd, putenv, redirect, timeout, description));

val redirect =
  map_params (fn (script, input, cwd, putenv, _, timeout, description) =>
    (script, input, cwd, putenv, true, timeout, description));

fun timeout timeout =
  map_params (fn (script, input, cwd, putenv, redirect, _, description) =>
    (script, input, cwd, putenv, redirect, timeout, description));

fun description description =
  map_params (fn (script, input, cwd, putenv, redirect, timeout, _) =>
    (script, input, cwd, putenv, redirect, timeout, description));

end;


(* server messages *)

val server_run = "run";
val server_kill = "kill";

val server_uuid = "uuid";
val server_interrupt = "interrupt";
val server_failure = "failure";
val server_result = "result";

end;
