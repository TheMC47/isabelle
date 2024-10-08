(*  Title:      Pure/PIDE/xml.ML
    Author:     David Aspinall
    Author:     Stefan Berghofer
    Author:     Makarius

Untyped XML trees and representation of ML values.
*)

signature XML_DATA_OPS =
sig
  type 'a A
  type 'a T
  type 'a V
  type 'a P
  val int_atom: int A
  val bool_atom: bool A
  val unit_atom: unit A
  val self: XML.body T
  val tree: XML.tree T
  val properties: Properties.T T
  val string: string T
  val int: int T
  val bool: bool T
  val unit: unit T
  val pair: 'a T -> 'b T -> ('a * 'b) T
  val triple: 'a T -> 'b T -> 'c T -> ('a * 'b * 'c) T
  val list: 'a T -> 'a list T
  val option: 'a T -> 'a option T
  val variant: 'a V list -> 'a T
end;

signature XML =
sig
  include XML
  val blob: string list -> body
  val is_empty: tree -> bool
  val is_empty_body: body -> bool
  val string: string -> body
  val enclose: string -> string -> body -> body
  val trim_blanks: body -> body
  val filter_elements: {remove: string -> bool, expose: string -> bool} -> body -> body
  val header: string
  val text: string -> string
  val element: string -> attributes -> string list -> string
  val output_markup: Markup.T -> Markup.output
  val string_of: tree -> string
  val pretty: int -> tree -> Pretty.T
  val parse_comments: string list -> unit * string list
  val parse_string : string -> string option
  val parse_element: string list -> tree * string list
  val parse_document: string list -> tree * string list
  val parse: string -> tree
  exception XML_ATOM of string
  exception XML_BODY of body
  structure Encode: XML_DATA_OPS
  structure Decode: XML_DATA_OPS
end;

structure XML: XML =
struct

(** XML trees **)

open XML;

val blob = map Text;

fun is_empty (Text "") = true
  | is_empty _ = false;

val is_empty_body = forall is_empty;

fun string "" = []
  | string s = [Text s];

fun enclose bg en body = string bg @ body @ string en;


(* trim blanks *)

fun trim_blanks trees =
  trees |> maps
    (fn Elem (markup, body) => [Elem (markup, trim_blanks body)]
      | Text s => s |> raw_explode |> trim Symbol.is_blank |> implode |> string);


(* filter markup elements *)

fun filter_elements {remove, expose} =
  let
    fun filter ts =
      ts |> maps (fn t =>
        (case XML.unwrap_elem t of
          SOME ((markup, body1), body2) =>
            if remove (#1 markup) then []
            else if expose (#1 markup) then filter body2
            else [XML.wrap_elem ((markup, body1), filter body2)]
        | NONE =>
            (case t of
              XML.Elem (markup, body) =>
                if remove (#1 markup) then []
                else if expose (#1 markup) then filter body
                else [XML.Elem (markup, filter body)]
            | _ => [t])));
  in filter end;


(** string representation **)

val header = "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n";


(* escaped text *)

fun decode "&lt;" = "<"
  | decode "&gt;" = ">"
  | decode "&amp;" = "&"
  | decode "&apos;" = "'"
  | decode "&quot;" = "\""
  | decode c = c;

fun encode "<" = "&lt;"
  | encode ">" = "&gt;"
  | encode "&" = "&amp;"
  | encode "'" = "&apos;"
  | encode "\"" = "&quot;"
  | encode c = c;

val text = translate_string encode;


(* elements *)

fun elem name atts =
  implode_space (name :: map (fn (a, x) => a ^ "=\"" ^ text x ^ "\"") atts);

fun element name atts body =
  let val b = implode body in
    if b = "" then Library.enclose "<" "/>" (elem name atts)
    else Library.enclose "<" ">" (elem name atts) ^ b ^ Library.enclose "</" ">" name
  end;

fun output_markup (markup as (name, atts)) =
  if Markup.is_empty markup then Markup.no_output
  else (Library.enclose "<" ">" (elem name atts), Library.enclose "</" ">" name);


(* output content *)

fun content_depth depth =
  let
    fun traverse _ (Elem ((name, atts), [])) =
          Buffer.add "<" #> Buffer.add (elem name atts) #> Buffer.add "/>"
      | traverse d (Elem ((name, atts), ts)) =
          Buffer.add "<" #> Buffer.add (elem name atts) #> Buffer.add ">" #>
          traverse_body d ts #>
          Buffer.add "</" #> Buffer.add name #> Buffer.add ">"
      | traverse _ (Text s) = Buffer.add (text s)
    and traverse_body 0 _ = Buffer.add "..."
      | traverse_body d ts = fold (traverse (d - 1)) ts;
  in Buffer.build_content o traverse depth end;

val string_of = content_depth ~1;

fun pretty depth tree = Pretty.str (content_depth (Int.max (0, depth)) tree);

val _ = ML_system_pp (fn depth => fn _ => Pretty.to_ML o pretty (FixedInt.toInt depth));



(** XML parsing **)

local

fun err msg (xs, _) =
  fn () => "XML parsing error: " ^ msg () ^ "\nfound: " ^ quote (Symbol.beginning 100 xs);

fun ignored _ = [];

fun name_start_char c = Symbol.is_ascii_letter c orelse c = ":" orelse c = "_";
fun name_char c = name_start_char c orelse Symbol.is_ascii_digit c orelse c = "-" orelse c = ".";
val parse_name = Scan.one name_start_char ::: Scan.many name_char;

val blanks = Scan.many Symbol.is_blank;
val special = $$ "&" ^^ (parse_name >> implode) ^^ $$ ";" >> decode;
val regular = Scan.one Symbol.not_eof;
fun regular_except x = Scan.one (fn c => Symbol.not_eof c andalso c <> x);

val parse_chars = Scan.repeat1 (special || regular_except "<") >> implode;

val parse_cdata =
  Scan.this_string "<![CDATA[" |--
  (Scan.repeat (Scan.unless (Scan.this_string "]]>") regular) >> implode) --|
  Scan.this_string "]]>";

val parse_att =
  ((parse_name >> implode) --| (blanks -- $$ "=" -- blanks)) --
  (($$ "\"" || $$ "'") :|-- (fn s =>
    (Scan.repeat (special || regular_except s) >> implode) --| $$ s));

val parse_comment =
  Scan.this_string "<!--" --
  Scan.repeat (Scan.unless (Scan.this_string "-->") regular) --
  Scan.this_string "-->" >> ignored;

val parse_processing_instruction =
  Scan.this_string "<?" --
  Scan.repeat (Scan.unless (Scan.this_string "?>") regular) --
  Scan.this_string "?>" >> ignored;

val parse_doctype =
  Scan.this_string "<!DOCTYPE" --
  Scan.repeat (Scan.unless ($$ ">") regular) --
  $$ ">" >> ignored;

val parse_misc =
  Scan.one Symbol.is_blank >> ignored ||
  parse_processing_instruction ||
  parse_comment;

val parse_optional_text =
  Scan.optional (parse_chars >> (single o Text)) [];

in

val parse_comments =
  blanks -- Scan.repeat (parse_comment -- blanks >> K ()) >> K ();

val parse_string = Scan.read Symbol.stopper parse_chars o raw_explode;

fun parse_content xs =
  (parse_optional_text @@@
    Scan.repeats
      ((parse_element >> single ||
        parse_cdata >> (single o Text) ||
        parse_processing_instruction ||
        parse_comment)
      @@@ parse_optional_text)) xs

and parse_element xs =
  ($$ "<" |-- parse_name -- Scan.repeat (blanks |-- parse_att) --| blanks :--
    (fn (name, _) =>
      !! (err (fn () => "Expected > or />"))
       ($$ "/" -- $$ ">" >> ignored ||
        $$ ">" |-- parse_content --|
          !! (err (fn () => "Expected </" ^ implode name ^ ">"))
              ($$ "<" -- $$ "/" -- Scan.this name -- blanks -- $$ ">")))
    >> (fn ((name, atts), body) => Elem ((implode name, atts), body))) xs;

val parse_document =
  (Scan.repeat parse_misc -- Scan.option parse_doctype -- Scan.repeat parse_misc)
  |-- parse_element;

fun parse s =
  (case Scan.finite Symbol.stopper (Scan.error (!! (err (fn () => "Malformed element"))
      (blanks |-- parse_document --| blanks))) (raw_explode s) of
    (x, []) => x
  | (_, ys) => error ("XML parsing error: unprocessed input\n" ^ Symbol.beginning 100 ys));

end;



(** XML as data representation language **)

exception XML_ATOM of string;
exception XML_BODY of tree list;


structure Encode =
struct

type 'a A = 'a -> string;
type 'a T = 'a -> body;
type 'a V = 'a -> string list * body;
type 'a P = 'a -> string list;


(* atomic values *)

fun int_atom i = Value.print_int i;

fun bool_atom false = "0"
  | bool_atom true = "1";

fun unit_atom () = "";


(* structural nodes *)

fun node ts = Elem ((":", []), ts);

fun vector xs = map_index (fn (i, x) => (int_atom i, x)) xs;

fun tagged (tag, (xs, ts)) = Elem ((int_atom tag, vector xs), ts);


(* representation of standard types *)

fun self (x: body) = x;

fun tree (t: tree) = [t];

fun properties props = [Elem ((":", props), [])];

fun string "" = []
  | string s = [Text s];

val int = string o int_atom;

val bool = string o bool_atom;

val unit = string o unit_atom;

fun pair f g (x, y) = [node (f x), node (g y)];

fun triple f g h (x, y, z) = [node (f x), node (g y), node (h z)];

fun list f xs = map (node o f) xs;

fun option _ NONE = []
  | option f (SOME x) = [node (f x)];

fun variant fs x =
  [tagged (the (get_index (fn f => SOME (f x) handle General.Match => NONE) fs))];

end;


structure Decode =
struct

type 'a A = string -> 'a;
type 'a T = body -> 'a;
type 'a V = string list * body -> 'a;
type 'a P = string list -> 'a;


(* atomic values *)

fun int_atom s =
  Value.parse_int s
    handle Fail _ => raise XML_ATOM s;

fun bool_atom "0" = false
  | bool_atom "1" = true
  | bool_atom s = raise XML_ATOM s;

fun unit_atom "" = ()
  | unit_atom s = raise XML_ATOM s;


(* structural nodes *)

fun node (Elem ((":", []), ts)) = ts
  | node t = raise XML_BODY [t];

fun vector atts =
  map_index (fn (i, (a, x)) => if int_atom a = i then x else raise XML_ATOM a) atts;

fun tagged (Elem ((name, atts), ts)) = (int_atom name, (vector atts, ts))
  | tagged t = raise XML_BODY [t];


(* representation of standard types *)

fun self (x: body) = x;

fun tree [t] = t
  | tree ts = raise XML_BODY ts;

fun properties [Elem ((":", props), [])] = props
  | properties ts = raise XML_BODY ts;

fun string [] = ""
  | string [Text s] = s
  | string ts = raise XML_BODY ts;

val int = int_atom o string;

val bool = bool_atom o string;

val unit = unit_atom o string;

fun pair f g [t1, t2] = (f (node t1), g (node t2))
  | pair _ _ ts = raise XML_BODY ts;

fun triple f g h [t1, t2, t3] = (f (node t1), g (node t2), h (node t3))
  | triple _ _ _ ts = raise XML_BODY ts;

fun list f ts = map (f o node) ts;

fun option _ [] = NONE
  | option f [t] = SOME (f (node t))
  | option _ ts = raise XML_BODY ts;

fun variant fs [t] =
      let
        val (tag, (xs, ts)) = tagged t;
        val f = nth fs tag handle General.Subscript => raise XML_BODY [t];
      in f (xs, ts) end
  | variant _ ts = raise XML_BODY ts;

end;

end;
