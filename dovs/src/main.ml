(** Commandline interface for the compiler. *)

(** Parse a java file *)
let parse_file file_name =
  try 
    let inch = open_in file_name in
    let () = print_endline ("Opening \"" ^ file_name ^ "\"") in
    let lexbuf = Lexing.from_channel inch in
    let lcp = lexbuf.Lexing.lex_curr_p in
    let () = lexbuf.Lexing.lex_curr_p <- { lcp with Lexing.pos_fname = file_name }
    in try
         let sf = Parser.goal Lexer.token lexbuf in
         close_in inch;
         flush stdout;
         sf
      with
        | End_of_file ->
          let curr_pos = lexbuf.Lexing.lex_curr_p in
          close_in inch;
          Error.error Error.PARSER_EXCEPTION curr_pos "Parse error: end of file"
        | Parser.Error ->
          let curr_pos = lexbuf.Lexing.lex_curr_p in
          close_in inch;
          Error.error Error.PARSER_EXCEPTION curr_pos "Parse error"
        | Failure msg ->
          let curr_pos = lexbuf.Lexing.lex_curr_p in
          close_in inch;
          Error.error Error.LEXER_EXCEPTION curr_pos msg
  with
    | End_of_file ->
      Error.error Error.PARSER_EXCEPTION Lexing.dummy_pos
        "Parse error: end of file from lexer"
    | Sys_error msg ->
      Error.error Error.FILE_OPEN_ERROR Lexing.dummy_pos
        ("Unable to open file " ^ msg)

(* Pretty printing functions *)
let pp_ast = Astpp.pp_program
let pp_type_env = Typespp.pp_type_env

(** Compile a set of files *)
let compile filenames =
  (* helper to apply a phase *)
  let apply phase ast msg =
    print_string "*** ";
    print_endline msg;
    flush stdout;
    let ast' = phase ast in
    flush stdout;
    ast'
  in
  let ()   = print_endline "Applying phases:" in
  let prog = apply (List.map parse_file) filenames "parsing" in 
(*  let ()   = apply Astpp.pp_program prog "pretty-printing" in *)
  let wast = apply Weeding.weed_program prog "weeding" in
  let east = apply Nameresolving.nres_program wast "name resolving" in
  let tenv = apply Environment.env_program east "environment building" in
  (*let ()   = apply Typespp.pp_type_env tenv "pretty-printing type env" in*)
  let ()   = apply (Hierarchy.hier_program east) tenv "hierarchy building" in
  let dast = apply (Disambiguation.disamb_program east) tenv "disambiguating" in
  let tast = apply (Typechecking.tcheck_program dast) tenv "type checking" in
  let cast = apply (Constantfolding.cfold_program tast) tenv "constant folding" in
  let rast = apply Reachability.reach_program cast "reachability analyzing" in
  let ()   = apply Definiteassignment.defass_program rast "assignment analyzing" in
  let rast = apply Resource.res_program rast "resource analyzing" in
  let cast = apply (Codegeneration.codegen_program rast) tenv "code generating" in
  let oast =

    if !Globals.peephole
    then apply Optimization.opt_program cast "peephole optimizing"
    else cast in

  let last = apply Limits.limit_program oast "limit analyzing and verifying" in
  let ()   = apply Codeemission.emit_program last "code emitting" in ()

let _ =
  let filenames = ref [] in
  let usagemsg =
    "Usage: joos [-joos1] [-noverify] [-cp <classpath>] <filenames>" in
  let argspec = Arg.align
    [("-joos1", Arg.Set Globals.joos1,
      " Compile as Joos1 program");
     ("-peephole", Arg.Set Globals.peephole,
      " Enable peephole optimization");
     ("-noverify", Arg.Set Globals.noverify,
      " Don't check consistency in Limits (fx to debug codegen)");
     ("-classpath", Arg.Set_string Globals.classpath,
      "<classpath> Path searched for class files");
     ("-cp", Arg.Set_string Globals.classpath,
      "<classpath> As above") ]
  in
  Arg.parse argspec (fun s -> filenames := s::(!filenames)) usagemsg;
  if !Sys.interactive
  then begin
    (* We are in the interactive toplevel *)
    print_newline ();
    print_endline "Welcome to the dOvs CamlJoos Compiler, version 2012.\n";
    print_endline "To parse a file use:";
    print_endline "  # Main.parse_file \"myfile.java\";;\n";
    print_endline "To compile a file use:";
    print_endline "  # Main.compile [\"myfile1.java\"; \"myfile2.java\"];;\n";
    print_endline "You can also access other modules or open a module:";
    print_endline "  # open Globals;;";
    print_endline "  # !joos1;;";
    print_endline "  - : bool = false\n";
    print_endline "Happy hacking!\n"
  end
  else begin
    (* We are in the batch compiler *)
    print_endline "The CamlJoos compiler, version 2012";
    print_newline ();
    if !filenames = []
    then (print_endline "Error: No filename(s) provided";
          Arg.usage argspec usagemsg)
    else compile (List.rev !filenames)
  end
