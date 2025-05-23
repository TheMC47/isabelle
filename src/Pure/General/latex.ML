(*  Title:      Pure/General/latex.ML
    Author:     Makarius

Support for LaTeX.
*)

signature LATEX =
sig
  type text = XML.body
  val text: string * Position.T -> text
  val string: string -> text
  val block: text -> XML.tree
  val output: text -> text
  val macro0: string -> text
  val macro: string -> text -> text
  val environment: string -> text -> text
  val output_name: string -> string
  val output_ascii: string -> string
  val output_ascii_breakable: string -> string -> string
  val output_symbols: Symbol.symbol list -> string
  val output_syms: string -> string
  val symbols: Symbol_Pos.T list -> text
  val symbols_output: Symbol_Pos.T list -> text
  val isabelle_body: string -> text -> text
  val theory_entry: string -> string
  val cite: {kind: string, citations: (string * Position.T) list, location: text} -> text
  type index_item = {text: text, like: string}
  type index_entry = {items: index_item list, def: bool}
  val index_entry: index_entry -> text
  val index_variants: (binding -> bool option -> 'a -> 'a) -> binding -> 'a -> 'a
  val output_: string -> Output.output
  val output_width: string -> Output.output * int
  val escape: Output.output -> string
  val output_ops: int option -> Pretty.output_ops
end;

structure Latex: LATEX =
struct

(* text with positions *)

type text = XML.body;

fun text (s, pos) =
  if s = "" then []
  else if pos = Position.none then [XML.Text s]
  else [XML.Elem (Position.markup_properties pos Markup.document_latex, [XML.Text s])];

fun string s = text (s, Position.none);

fun block body = XML.Elem (Markup.document_latex, body);

fun output body = [XML.Elem (Markup.latex_output, body)];

fun macro0 name = [XML.Elem (Markup.latex_macro0 name, [])];
fun macro name body = [XML.Elem (Markup.latex_macro name, body)];
fun environment name body = [XML.Elem (Markup.latex_environment name, body)];


(* output name for LaTeX macros *)

val output_name =
  translate_string
    (fn "_" => "UNDERSCORE"
      | "'" => "PRIME"
      | "0" => "ZERO"
      | "1" => "ONE"
      | "2" => "TWO"
      | "3" => "THREE"
      | "4" => "FOUR"
      | "5" => "FIVE"
      | "6" => "SIX"
      | "7" => "SEVEN"
      | "8" => "EIGHT"
      | "9" => "NINE"
      | s => s);

fun enclose_name bg en = enclose bg en o output_name;


(* output verbatim ASCII *)

val output_ascii =
  translate_string
    (fn " " => "\\ "
      | "\t" => "\\ "
      | "\n" => "\\isanewline\n"
      | s =>
          s
          |> member_string "\"#$%&',-<>\\^_`{}~" s ? enclose "{\\char`\\" "}"
          |> suffix "{\\kern0pt}");

fun output_ascii_breakable sep =
  space_explode sep
  #> map output_ascii
  #> space_implode (output_ascii sep ^ "\\discretionary{}{}{}");


(* output symbols *)

local

val char_table =
  Symtab.make
   [("\007", "{\\isacharbell}"),
    ("!", "{\\isacharbang}"),
    ("\"", "{\\isachardoublequote}"),
    ("#", "{\\isacharhash}"),
    ("$", "{\\isachardollar}"),
    ("%", "{\\isacharpercent}"),
    ("&", "{\\isacharampersand}"),
    ("'", "{\\isacharprime}"),
    ("(", "{\\isacharparenleft}"),
    (")", "{\\isacharparenright}"),
    ("*", "{\\isacharasterisk}"),
    ("+", "{\\isacharplus}"),
    (",", "{\\isacharcomma}"),
    ("-", "{\\isacharminus}"),
    (".", "{\\isachardot}"),
    ("/", "{\\isacharslash}"),
    (":", "{\\isacharcolon}"),
    (";", "{\\isacharsemicolon}"),
    ("<", "{\\isacharless}"),
    ("=", "{\\isacharequal}"),
    (">", "{\\isachargreater}"),
    ("?", "{\\isacharquery}"),
    ("@", "{\\isacharat}"),
    ("[", "{\\isacharbrackleft}"),
    ("\\", "{\\isacharbackslash}"),
    ("]", "{\\isacharbrackright}"),
    ("^", "{\\isacharcircum}"),
    ("_", "{\\isacharunderscore}"),
    ("`", "{\\isacharbackquote}"),
    ("{", "{\\isacharbraceleft}"),
    ("|", "{\\isacharbar}"),
    ("}", "{\\isacharbraceright}"),
    ("~", "{\\isachartilde}")];

fun output_chr " " = "\\ "
  | output_chr "\t" = "\\ "
  | output_chr "\n" = "\\isanewline\n"
  | output_chr c =
      (case Symtab.lookup char_table c of
        SOME s => s ^ "{\\kern0pt}"
      | NONE => if Symbol.is_ascii_digit c then enclose "{\\isadigit{" "}}" c else c);

fun output_sym sym =
  (case Symbol.decode sym of
    Symbol.Char s => output_chr s
  | Symbol.UTF8 s => s
  | Symbol.Sym s => enclose_name "{\\isasym" "}" s
  | Symbol.Control s => enclose_name "\\isactrl" " " s
  | Symbol.Malformed s => error (Symbol.malformed_msg s)
  | Symbol.EOF => error "Bad EOF symbol");

open Basic_Symbol_Pos;

val scan_latex_length =
  Scan.many1 (fn (s, _) => s <> Symbol.latex andalso Symbol.not_eof s)
    >> (Symbol.length o map Symbol_Pos.symbol) ||
  $$ Symbol.latex -- Scan.option (Scan.permissive Symbol_Pos.scan_cartouche "") >> K 0;

val scan_latex =
  $$ Symbol.latex |-- Symbol_Pos.scan_cartouche_content "Embedded LaTeX: "
    >> (implode o map Symbol_Pos.symbol) ||
  Scan.one (Symbol.not_eof o Symbol_Pos.symbol) >> (output_sym o Symbol_Pos.symbol);

fun read scan syms =
  Scan.read Symbol_Pos.stopper (Scan.repeat scan) (map (rpair Position.none) syms);

in

val length_symbols = Integer.sum o these o read scan_latex_length;

fun output_symbols syms =
  if member (op =) syms Symbol.latex then
    (case read scan_latex syms of
      SOME ss => implode ss
    | NONE => error ("Malformed embedded LaTeX: " ^ quote (Symbol.beginning 10 syms)))
  else implode (map output_sym syms);

val output_syms = output_symbols o Symbol.explode;

end;

fun symbols syms = text (Symbol_Pos.content syms, #1 (Symbol_Pos.range syms));
fun symbols_output syms =
  text (output_symbols (map Symbol_Pos.symbol syms), #1 (Symbol_Pos.range syms));


(* theory presentation *)

fun isabelle_body name =
  XML.enclose
   ("%\n\\begin{isabellebody}%\n\\setisabellecontext{" ^ output_syms name ^ "}%\n")
   "%\n\\end{isabellebody}%\n";

fun theory_entry name = "\\input{" ^ name ^ ".tex}\n\n";


(* cite: references to bibliography *)

fun cite {kind, location, citations} =
  let
    val _ =
      citations |> List.app (fn (s, pos) =>
        if member_string s ","
        then error ("Single citation expected, without commas" ^ Position.here pos)
        else ());
    val citations' = space_implode "," (map #1 citations);
    val markup = Markup.latex_cite {kind = kind, citations = citations'};
  in [XML.Elem (markup, location)] end;


(* index entries *)

type index_item = {text: text, like: string};
type index_entry = {items: index_item list, def: bool};

fun index_item (item: index_item) =
  XML.wrap_elem ((Markup.latex_index_item, #text item), XML.string (#like item));

fun index_entry (entry: index_entry) =
  [XML.Elem (Markup.latex_index_entry (if #def entry then "isaindexdef" else "isaindexref"),
    map index_item (#items entry))];

fun index_binding NONE = I
  | index_binding (SOME def) = Binding.map_name (suffix (if def then "_def" else "_ref"));

fun index_variants setup binding =
  fold (fn index => setup (index_binding index binding) index) [NONE, SOME true, SOME false];


(* markup and formatting *)

val output_ = output_symbols o Symbol.explode;

fun output_width str =
  let val syms = Symbol.explode str
  in (output_symbols syms, length_symbols syms) end;

val escape = enclose (Symbol.latex ^ Symbol.open_) Symbol.close;

local

val markup_macro = YXML.output_markup o Markup.latex_macro;

val markup_latex =
 Symtab.make
  [(Markup.commandN, markup_macro "isakeywordONE"),
   (Markup.keyword1N, markup_macro "isakeywordONE"),
   (Markup.keyword2N, markup_macro "isakeywordTWO"),
   (Markup.keyword3N, markup_macro "isakeywordTHREE"),
   (Markup.tclassN, markup_macro "isatclass"),
   (Markup.tconstN, markup_macro "isatconst"),
   (Markup.tfreeN, markup_macro "isatfree"),
   (Markup.tvarN, markup_macro "isatvar"),
   (Markup.constN, markup_macro "isaconst"),
   (Markup.freeN, markup_macro "isafree"),
   (Markup.skolemN, markup_macro "isaskolem"),
   (Markup.boundN, markup_macro "isabound"),
   (Markup.varN, markup_macro "isavar")];

fun latex_markup (a, props) =
  (if Markup.has_syntax props then NONE else Symtab.lookup markup_latex a)
  |> the_default Markup.no_output;

fun latex_markup_output (bg, en) =
  (case try YXML.parse (bg ^ en) of
    SOME (XML.Elem (m, _)) => latex_markup m
  | _ => Markup.no_output);

in

fun output_ops opt_margin : Pretty.output_ops =
 {symbolic = false,
  output = output_width,
  markup = latex_markup_output,
  indent_markup = markup_macro "isaindent",
  margin = ML_Pretty.get_margin opt_margin};

end;

end;
