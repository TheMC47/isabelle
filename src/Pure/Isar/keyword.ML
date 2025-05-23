(*  Title:      Pure/Isar/keyword.ML
    Author:     Makarius

Isar keyword classification.
*)

signature KEYWORD =
sig
  val diag: string
  val document_heading: string
  val document_body: string
  val document_raw: string
  val thy_begin: string
  val thy_end: string
  val thy_decl: string
  val thy_decl_block: string
  val thy_defn: string
  val thy_stmt: string
  val thy_load: string
  val thy_goal: string
  val thy_goal_defn: string
  val thy_goal_stmt: string
  val qed: string
  val qed_script: string
  val qed_block: string
  val qed_global: string
  val prf_goal: string
  val prf_block: string
  val next_block: string
  val prf_open: string
  val prf_close: string
  val prf_chain: string
  val prf_decl: string
  val prf_asm: string
  val prf_asm_goal: string
  val prf_script: string
  val prf_script_goal: string
  val prf_script_asm_goal: string
  val before_command: string
  val quasi_command: string
  type spec = {kind: string, load_command: string * Position.T, tags: string list}
  val command_spec: string * string list -> spec
  val no_spec: spec
  val before_command_spec: spec
  val quasi_command_spec: spec
  val document_heading_spec: spec
  val document_body_spec: spec
  type keywords
  val minor_keywords: keywords -> Scan.lexicon
  val major_keywords: keywords -> Scan.lexicon
  val empty_keywords: keywords
  val merge_keywords: keywords * keywords -> keywords
  val add_keywords: ((string * Position.T) * spec) list -> keywords -> keywords
  val add_minor_keywords: string list -> keywords -> keywords
  val add_major_keywords: string list -> keywords -> keywords
  val no_major_keywords: keywords -> keywords
  val is_keyword: keywords -> string -> bool
  val is_command: keywords -> string -> bool
  val is_literal: keywords -> string -> bool
  val dest_commands: keywords -> string list
  val command_markup: keywords -> string -> Markup.T option
  val command_kind: keywords -> string -> string option
  val command_tags: keywords -> string -> string list
  val is_vacuous: keywords -> string -> bool
  val is_diag: keywords -> string -> bool
  val is_document_heading: keywords -> string -> bool
  val is_document_body: keywords -> string -> bool
  val is_document_raw: keywords -> string -> bool
  val is_document: keywords -> string -> bool
  val is_theory_end: keywords -> string -> bool
  val is_theory_load: keywords -> string -> bool
  val is_theory: keywords -> string -> bool
  val is_theory_body: keywords -> string -> bool
  val is_proof: keywords -> string -> bool
  val is_proof_body: keywords -> string -> bool
  val is_theory_goal: keywords -> string -> bool
  val is_proof_goal: keywords -> string -> bool
  val is_qed: keywords -> string -> bool
  val is_qed_global: keywords -> string -> bool
  val is_proof_open: keywords -> string -> bool
  val is_proof_close: keywords -> string -> bool
  val is_proof_asm: keywords -> string -> bool
  val is_proof_asm_goal: keywords -> string -> bool
  val is_improper: keywords -> string -> bool
  val is_printed: keywords -> string -> bool
end;

structure Keyword: KEYWORD =
struct

(** keyword classification **)

(* kinds *)

val diag = "diag";
val document_heading = "document_heading";
val document_body = "document_body";
val document_raw = "document_raw";
val thy_begin = "thy_begin";
val thy_end = "thy_end";
val thy_decl = "thy_decl";
val thy_decl_block = "thy_decl_block";
val thy_defn = "thy_defn";
val thy_stmt = "thy_stmt";
val thy_load = "thy_load";
val thy_goal = "thy_goal";
val thy_goal_defn = "thy_goal_defn";
val thy_goal_stmt = "thy_goal_stmt";
val qed = "qed";
val qed_script = "qed_script";
val qed_block = "qed_block";
val qed_global = "qed_global";
val prf_goal = "prf_goal";
val prf_block = "prf_block";
val next_block = "next_block";
val prf_open = "prf_open";
val prf_close = "prf_close";
val prf_chain = "prf_chain";
val prf_decl = "prf_decl";
val prf_asm = "prf_asm";
val prf_asm_goal = "prf_asm_goal";
val prf_script = "prf_script";
val prf_script_goal = "prf_script_goal";
val prf_script_asm_goal = "prf_script_asm_goal";

val before_command = "before_command";
val quasi_command = "quasi_command";

val command_kinds =
  [diag, document_heading, document_body, document_raw, thy_begin, thy_end, thy_load, thy_decl,
    thy_decl_block, thy_defn, thy_stmt, thy_goal, thy_goal_defn, thy_goal_stmt, qed, qed_script,
    qed_block, qed_global, prf_goal, prf_block, next_block, prf_open, prf_close, prf_chain,
    prf_decl, prf_asm, prf_asm_goal, prf_script, prf_script_goal, prf_script_asm_goal];


(* specifications *)

type spec = {kind: string, load_command: string * Position.T, tags: string list};

fun command_spec (kind, tags) : spec =
  {kind = kind, load_command = ("", Position.none), tags = tags};

val no_spec = command_spec ("", []);
val before_command_spec = command_spec (before_command, []);
val quasi_command_spec = command_spec (quasi_command, []);
val document_heading_spec = command_spec ("document_heading", ["document"]);
val document_body_spec = command_spec ("document_body", ["document"]);

type entry =
 {pos: Position.T,
  id: serial,
  kind: string,
  tags: string list};

fun check_spec pos ({kind, tags, ...}: spec) : entry =
  if not (member (op =) command_kinds kind) then
    error ("Unknown outer syntax keyword kind " ^ quote kind)
  else {pos = pos, id = serial (), kind = kind, tags = tags};


(** keyword tables **)

(* type keywords *)

datatype keywords = Keywords of
 {minor: Scan.lexicon,
  major: Scan.lexicon,
  commands: entry Symtab.table};

fun minor_keywords (Keywords {minor, ...}) = minor;
fun major_keywords (Keywords {major, ...}) = major;

fun make_keywords (minor, major, commands) =
  Keywords {minor = minor, major = major, commands = commands};

fun map_keywords f (Keywords {minor, major, commands}) =
  make_keywords (f (minor, major, commands));


(* build keywords *)

val empty_keywords =
  make_keywords (Scan.empty_lexicon, Scan.empty_lexicon, Symtab.empty);

fun merge_keywords
  (Keywords {minor = minor1, major = major1, commands = commands1},
    Keywords {minor = minor2, major = major2, commands = commands2}) =
  make_keywords
   (Scan.merge_lexicons (minor1, minor2),
    Scan.merge_lexicons (major1, major2),
    Symtab.merge (K true) (commands1, commands2));

val add_keywords =
  fold (fn ((name, pos), spec as {kind, ...}: spec) => map_keywords (fn (minor, major, commands) =>
    if kind = "" orelse kind = before_command orelse kind = quasi_command then
      let
        val minor' = Scan.extend_lexicon (Symbol.explode name) minor;
      in (minor', major, commands) end
    else
      let
        val entry = check_spec pos spec;
        val major' = Scan.extend_lexicon (Symbol.explode name) major;
        val commands' = Symtab.update (name, entry) commands;
      in (minor, major', commands') end));

val add_minor_keywords =
  add_keywords o map (fn name => ((name, Position.none), no_spec));

val add_major_keywords =
  add_keywords o map (fn name => ((name, Position.none), command_spec (diag, [])));

val no_major_keywords =
  map_keywords (fn (minor, _, _) => (minor, Scan.empty_lexicon, Symtab.empty));


(* keyword status *)

fun is_keyword keywords s = Scan.is_literal (minor_keywords keywords) (Symbol.explode s);
fun is_command (Keywords {commands, ...}) = Symtab.defined commands;
fun is_literal keywords = is_keyword keywords orf is_command keywords;

fun dest_commands (Keywords {commands, ...}) = Symtab.keys commands;


(* command keywords *)

fun lookup_command (Keywords {commands, ...}) = Symtab.lookup commands;

fun command_markup keywords name =
  lookup_command keywords name
  |> Option.map (fn {pos, id, ...} =>
      Position.make_entity_markup {def = false} id Markup.command_keywordN (name, pos));

fun command_kind keywords = Option.map #kind o lookup_command keywords;

fun command_tags keywords name =
  (case lookup_command keywords name of
    SOME {tags, ...} => tags
  | NONE => []);


(* command categories *)

fun command_category ks =
  let
    val set = Symset.make ks;
    fun pred keywords name =
      (case lookup_command keywords name of
        NONE => false
      | SOME {kind, ...} => Symset.member set kind);
  in pred end;

val is_vacuous = command_category [diag, document_heading, document_body, document_raw];

val is_diag = command_category [diag];

val is_document_heading = command_category [document_heading];
val is_document_body = command_category [document_body];
val is_document_raw = command_category [document_raw];
val is_document = command_category [document_heading, document_body, document_raw];

val is_theory_end = command_category [thy_end];

val is_theory_load = command_category [thy_load];

val is_theory = command_category
  [thy_begin, thy_end, thy_load, thy_decl, thy_decl_block, thy_defn, thy_stmt, thy_goal,
    thy_goal_defn, thy_goal_stmt];

val is_theory_body = command_category
  [thy_load, thy_decl, thy_decl_block, thy_defn, thy_stmt, thy_goal, thy_goal_defn, thy_goal_stmt];

val is_proof = command_category
  [qed, qed_script, qed_block, qed_global, prf_goal, prf_block, next_block, prf_open,
    prf_close, prf_chain, prf_decl, prf_asm, prf_asm_goal, prf_script, prf_script_goal,
    prf_script_asm_goal];

val is_proof_body = command_category
  [diag, document_heading, document_body, document_raw, prf_block, next_block, prf_open,
    prf_close, prf_chain, prf_decl, prf_asm, prf_asm_goal, prf_script, prf_script_goal,
    prf_script_asm_goal];

val is_theory_goal = command_category [thy_goal, thy_goal_defn, thy_goal_stmt];
val is_proof_goal = command_category [prf_goal, prf_asm_goal, prf_script_goal, prf_script_asm_goal];
val is_qed = command_category [qed, qed_script, qed_block];
val is_qed_global = command_category [qed_global];

val is_proof_open =
  command_category [prf_goal, prf_asm_goal, prf_script_goal, prf_script_asm_goal, prf_open];
val is_proof_close = command_category [qed, qed_script, qed_block, prf_close];

val is_proof_asm = command_category [prf_asm, prf_asm_goal];
val is_proof_asm_goal = command_category [prf_asm_goal];
val is_improper = command_category [qed_script, prf_script, prf_script_goal, prf_script_asm_goal];

fun is_printed keywords = is_theory_goal keywords orf is_proof keywords;

end;
