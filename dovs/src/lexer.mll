(* lexer.mll -*- tuareg -*- *)
{
  open Parser
  
  let get = Lexing.lexeme
    
  let keyword_table = Hashtbl.create 53

  let _ =
    List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
      ["abstract"    , ABSTRACT;
       "boolean"     , BOOLEAN;
       "break"       , KEYWORD "break";
       "byte"        , BYTE;
       "case"        , KEYWORD "case";   
       "catch"       , KEYWORD "catch"; 
       "char"        , CHAR;
       "class"       , CLASS; 
       "const"       , KEYWORD "const";
       "continue"    , KEYWORD "continue";
       "default"     , KEYWORD "default";
       "do"          , KEYWORD "do";
       "double"      , KEYWORD "double";  
       "else"        , ELSE;  
       "extends"     , EXTENDS; 
       "final"       , FINAL;
       "finally"     , KEYWORD "finally";
       "float"       , KEYWORD "float";
       "for"         , FOR;   
       "goto"        , KEYWORD "goto";
       "if"          , IF;
       "implements"  , IMPLEMENTS;
       "import"      , IMPORT;
       "instanceof"  , INSTANCEOF;      
       "int"         , INT;
       "interface"   , INTERFACE;
       "long"        , KEYWORD "long";
       "native"      , KEYWORD "native";
       "new"         , NEW;
       "package"     , PACKAGE;
       "private"     , KEYWORD "private";
       "protected"   , PROTECTED;   
       "public"      , PUBLIC;
       "return"      , RETURN;
       "short"       , SHORT;   
       "static"      , STATIC;
       "strictfp"    , KEYWORD "strictfp";
       "super"       , SUPER; 
       "switch"      , KEYWORD "switch";
       "synchronized", KEYWORD "synchronized";
       "this"        , THIS;
       "throw"       , THROW;
       "throws"      , THROWS;
       "transient"   , KEYWORD "transient";
       "try"         , KEYWORD "try";
       "void"        , VOID;
       "volatile"    , KEYWORD "volatile";
       "while"       , WHILE;
       "true"        , TRUE;
       "false"       , FALSE;
       "null"        , NULL ]

  let tok_as_string t = match t with
    | EOF -> "eof"
  (* Keywords *)
    | ABSTRACT -> "abstract"
    | BOOLEAN -> "boolean"
(*    | BREAK -> "break" *)
    | BYTE -> "byte"
(*    | CASE -> "case" *)
(*    | CATCH -> "catch" *)
    | CHAR -> "char"
    | CLASS -> "class" 
(*    | CONST -> "const" *)
(*    | CONTINUE -> "continue" *)
(*    | DEFAULT -> "default" *)
(*    | DO -> "do" *)
(*    | DOUBLE -> "double" *)
    | ELSE -> "else"  
    | EXTENDS -> "extends" 
    | FINAL -> "final"
(*    | FINALLY -> "finally" *)
(*    | FLOAT -> "float" *)
    | FOR -> "for"   
(*    | GOTO -> "goto" *)
    | IF -> "if"
    | IMPLEMENTS -> "implements"
    | IMPORT -> "import"
    | INSTANCEOF -> "instanceof"      
    | INT -> "int"
    | INTERFACE -> "interface"
(*    | LONG -> "long" *)
(*    | NATIVE -> "native" *)
    | NEW -> "new"
    | PACKAGE -> "package"
(*    | PRIVATE -> "private" *)
    | PROTECTED -> "protected"   
    | PUBLIC -> "public"
    | RETURN -> "return"
    | SHORT -> "short"   
    | STATIC -> "static"
(*    | STRICTFP -> "strictfp" *)
    | SUPER -> "super" 
(*    | SWITCH -> "switch" *)
(*    | SYNCHRONIZED -> "synchronized" *)
    | THIS -> "this"
    | THROW -> "throw"
    | THROWS -> "throws"
(*    | TRANSIENT -> "transient" *)
(*    | TRY -> "try" *)
    | VOID -> "void"
(*    | VOLATILE -> "volatile" *)
    | WHILE -> "while"

    | KEYWORD s -> s
      
    | TRUE -> "true"
    | FALSE -> "false"
    | NULL -> "null"
      
  (* Delimiters *)
    | L_PAREN -> "("
    | R_PAREN -> ")"
    | L_BRACE -> "{"
    | R_BRACE -> "}"
    | L_BRACKET -> "["
    | R_BRACKET -> "]"
    | SEMICOLON -> ";"
    | COMMA -> ","
    | DOT -> "."
      
  (* Assignment and logic *)
    | ASSIGN -> "="
    | COMPLEMENT -> "!"
    | AND_AND -> "&&"
    | OR_OR -> "||"
      
  (* Comparison *)
    | LT -> "<"
    | GT -> ">"
    | EQ -> "=="
    | LTEQ -> "<="
    | GTEQ -> ">="
    | NEQ -> "!="

  (* Arithmetic *)
    | PLUS -> "+"
    | MINUS -> "-"
    | STAR -> "*"
    | DIV -> "/"
    | AND -> "&"
    | OR -> "|"
    | XOR -> "^"
    | MOD -> "%"
    | PLUS_PLUS -> "++"
    | MINUS_MINUS -> "--"

  (* Literals and identifiers *)
    | INTEGER_LITERAL i -> "INTEGER_LITERAL" ^ i
    | CHAR_LITERAL s -> "'CHAR_LITERAL'" ^ s
    | STRING_LITERAL s -> "\"STRING_LITERAL\"" ^ s
    | IDENTIFIER s -> s (*"IDENTIFIER("^s^")"*)

}

(*******************************************************************
 * Helpers                                                         *
 *******************************************************************)

let latin1_input_character = ['\000'- '\255']
let ht = '\t'
let lf = '\n'
let ff = '\012'
let cr = '\r'
let sp = ' '

let line_terminator = lf | cr | cr lf 
let input_character = ['\000'-'\255'] # ['\r' '\n'] (* # (lf | cr) *)

let not_star_not_newline = (['\000'-'\255'] # ['\r''\n''*'])
let not_star_not_slash_not_newline = (['\000'-'\255'] # ['\r''\n''*''/'])

let digit = ['0'-'9']
let non_zero_digit = ['1'-'9']
let octal_digit = ['0'-'7']
let zero_to_three = ['0'-'3']

let decimal_numeral = '0' | non_zero_digit digit*

let latin1_letter =
       ['A'-'Z'] | ['a'-'z'] | ['\170'-'\170'] | ['\181'-'\181'] |
       ['\186'-'\186'] | ['\192'-'\214'] | ['\216'-'\246'] | ['\248'-'\255']

let java_letter = latin1_letter | '$' | '_'
let java_letter_or_digit = latin1_letter | digit | '$' | '_'

let octal_escape = '\\' (octal_digit octal_digit? | zero_to_three octal_digit octal_digit)
let escape_sequence = "\\b" | "\\t" | "\\n" | "\\f" | "\\r" | "\\" '"' | "\\" "'" | "\\\\" | octal_escape
let single_character = (['\000'-'\255'] # ['\r''\n'''''\\']) | escape_sequence
let string_character = (['\000'-'\255'] # ['\r''\n''"''\\']) | escape_sequence

(*******************************************************************
 * Tokens                                                          *
 *******************************************************************)

rule token = parse
  | eof    { EOF }
(* Whitespace *)
  | (sp | ht | ff )                 { token lexbuf }  (* white_space *)
  | line_terminator                 { Lexing.new_line lexbuf; token lexbuf }
  | "/*" line_terminator            { inside_comment false lexbuf }
  | "/*" not_star_not_newline       { inside_comment false lexbuf }
  | "/**"                           { inside_comment true lexbuf }
  | "//" input_character*
                                    { token lexbuf } (* end_of_line_comment *)
  | "//" input_character* line_terminator
                                    { Lexing.new_line lexbuf; token lexbuf } (* end_of_line_comment *)

(* Keywords *)
(*| "abstract"      { ABSTRACT }
  | "boolean"       { BOOLEAN }
  | "break"         { KEYWORD "break" }
  | "byte"          { BYTE }
  | "case"          { KEYWORD "case" }   
  | "catch"         { KEYWORD "catch" } 
  | "char"          { CHAR }
  | "class"         { CLASS } 
  | "const"         { KEYWORD "const" }
  | "continue"      { KEYWORD "continue" }
  | "default"       { KEYWORD "default" }
  | "do"            { KEYWORD "do" }
  | "double"        { KEYWORD "double" }  
  | "else"          { ELSE }  
  | "extends"       { EXTENDS } 
  | "final"         { FINAL }
  | "finally"       { KEYWORD "finally" }
  | "float"         { KEYWORD "float" }
  | "for"           { FOR }   
  | "goto"          { KEYWORD "goto" }
  | "if"            { IF }
  | "implements"    { IMPLEMENTS }
  | "import"        { IMPORT }
  | "instanceof"    { INSTANCEOF }      
  | "int"           { INT }
  | "interface"     { INTERFACE }
  | "long"          { KEYWORD "long" }
  | "native"        { KEYWORD "native" }
  | "new"           { NEW }
  | "package"       { PACKAGE }
  | "private"       { KEYWORD "private" }
  | "protected"     { PROTECTED }   
  | "public"        { PUBLIC }
  | "return"        { RETURN }
  | "short"         { SHORT }   
  | "static"        { STATIC }
  | "strictfp"      { KEYWORD "strictfp" }
  | "super"         { SUPER } 
  | "switch"        { KEYWORD "switch" }
  | "synchronized"  { KEYWORD "synchronized" }
  | "this"          { THIS }
  | "throw"         { THROW }
  | "throws"        { THROWS }
  | "transient"     { KEYWORD "transient" }
  | "try"           { KEYWORD "try" }
  | "void"          { VOID }
  | "volatile"      { KEYWORD "volatile" }
  | "while"         { WHILE }

  | "true"          { TRUE }
  | "false"         { FALSE }
  | "null"          { NULL } *)

(* Delimiters *)
  | '('             { L_PAREN }
  | ')'             { R_PAREN }
  | '{'             { L_BRACE }
  | '}'             { R_BRACE }
  | '['             { L_BRACKET }
  | ']'             { R_BRACKET }
  | ';'             { SEMICOLON }
  | ','             { COMMA }
  | '.'             { DOT }

(* Assignment and logic *)
  | '='             { ASSIGN }
  | '!'             { COMPLEMENT }
  | "&&"            { AND_AND }
  | "||"            { OR_OR }

(* Comparison *)
  | '<'             { LT }
  | '>'             { GT }
  | "=="            { EQ }
  | "<="            { LTEQ }
  | ">="            { GTEQ }
  | "!="            { NEQ }

(* Arithmetic *)
  | '+'             { PLUS }
  | '-'             { MINUS }
  | '*'             { STAR }
  | '/'             { DIV }
  | '&'             { AND }
  | '|'             { OR }
  | '^'             { XOR }
  | '%'             { MOD }
  | "++"            { PLUS_PLUS }
  | "--"            { MINUS_MINUS }

(* Literals and identifiers *)
  | decimal_numeral as i                    { INTEGER_LITERAL i }
  | "'" single_character "'" as s           { CHAR_LITERAL s }
  | '"' string_character* '"' as s          { STRING_LITERAL s }
  | java_letter java_letter_or_digit* as id { try
						Hashtbl.find keyword_table id
                                              with Not_found ->
						IDENTIFIER id }

(* Two-state finite automata for recognizing end-of-comment
    bool laststar represents the state of the automata      *)
and inside_comment laststar = parse
  | '/'                            { if laststar
                                     then token lexbuf
                                     else inside_comment false lexbuf }
  | '*'                            { inside_comment true lexbuf }
  | line_terminator                { Lexing.new_line lexbuf;
				     inside_comment false lexbuf }
  | not_star_not_slash_not_newline { inside_comment false lexbuf }
