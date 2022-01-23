(** Final compiler phase to emit the generated code for all classes and
    interfaces to jasmin files. *)

module LAst = Limitsast
module Inst = Instruction

(* ********************************************************************** *)
(* HELPER FUNCTIONS                                                       *)
(* ********************************************************************** *)

let output_line ch str =
  begin
    output_string ch str;
    output_char ch '\n';
    flush ch
  end

let output_newline ch =
  output_char ch '\n';
  flush ch

let string_of_access a = match a with
  | Ast.Public -> "public "
  | Ast.Protected -> "protected "
  
(* ********************************************************************** *)
(* CODE GENERATION TRAVERSAL                                              *)
(* ********************************************************************** *)

let emit_throws_single nt ch =
  output_line ch (".throws " ^ (Types.cname_to_sig nt))

let emit_throws ntypes ch =
  List.iter (fun nt -> emit_throws_single nt ch) ntypes

let emit_implements_single nt ch =
  output_line ch (".implements " ^ (Types.cname_to_sig nt))

let emit_implements ntypes ch =
  List.iter (fun nt -> emit_implements_single nt ch) ntypes

let emit_instruction inst ch =
  output_line ch (Inst.to_asm inst)

let emit_instructions insts ch =
  List.iter (fun i -> emit_instruction i ch) insts

let emit_body body ch =
  begin
    output_line ch (".limit stack " ^ (string_of_int body.LAst.body_max_stack));
    output_line ch (".limit locals " ^ (string_of_int body.LAst.body_max_locals));
    emit_instructions body.LAst.body_instructions ch;
  end

let emit_body_opt body ch = match body with
  | None -> ()
  | Some body -> emit_body body ch

let emit_field_decl fdecl ch =
  let access_str = string_of_access fdecl.LAst.field_access in
  let field_name = fdecl.LAst.field_name.Ast.identifier in
  let field_type = Types.typeexp_to_desc fdecl.LAst.field_type in
  begin
    output_string ch (".field " ^ access_str);
    if fdecl.LAst.field_static then output_string ch "static " else ();
    if fdecl.LAst.field_final then output_string ch "final " else ();
    output_line ch ("\"" ^ field_name ^ "\" " ^ field_type);
  end

let make_sig method_name fullsig =
  method_name ^ (Str.string_after fullsig (String.index fullsig '('))

let emit_method_decl mdecl ch =
  let access_str = string_of_access mdecl.LAst.method_access in
  let method_name = mdecl.LAst.method_name.Ast.identifier in
  let fullsig = mdecl.LAst.method_signature in
  let msig = make_sig method_name fullsig in (* chop off full sig appropriately *)
  begin
    output_string ch (".method " ^ access_str);
    if mdecl.LAst.method_static then output_string ch "static " else ();
    if mdecl.LAst.method_final then output_string ch "final " else ();
    if mdecl.LAst.method_abstract then output_string ch "abstract " else ();
    output_line ch msig;
    emit_throws mdecl.LAst.method_throws ch;
    emit_body_opt mdecl.LAst.method_body ch;
    output_line ch ".end method";
  end

let emit_constructor_decl cdecl ch =
  let access_str = string_of_access cdecl.LAst.constructor_access in
  let csig = cdecl.LAst.constructor_signature in
  let initsig = make_sig "<init>" csig in (* chop off full sig appropriately *)
  begin
    output_string ch (".method " ^ access_str);
    output_line ch initsig;
    emit_throws cdecl.LAst.constructor_throws ch;
    emit_body cdecl.LAst.constructor_body ch;
    output_line ch ".end method";
  end

let emit_decl decl ch = match decl.LAst.decl with
  | LAst.Field fdecl -> emit_field_decl fdecl ch
  | LAst.Method mdecl -> emit_method_decl mdecl ch
  | LAst.Constructor cdecl -> emit_constructor_decl cdecl ch

let emit_decls decls ch =
  let fields,rest =
    List.partition (fun d -> match d.LAst.decl with LAst.Field _ -> true | _ -> false) decls in
  begin
    List.iter (fun decl -> emit_decl decl ch) fields;
    List.iter (fun decl -> begin output_newline ch; emit_decl decl ch; end) rest
  end

let emit_class_decl cdecl ch tsig =
  begin
    output_string ch ".class public ";
    if cdecl.LAst.class_abstract then output_string ch "abstract " else ();
    if cdecl.LAst.class_final then output_string ch "final " else ();
    output_line ch tsig;
    output_line ch (".super " ^ (Types.cname_to_sig (cdecl.LAst.class_extends)));
    emit_implements cdecl.LAst.class_implements ch;
    emit_decls cdecl.LAst.class_members ch
  end

let emit_interface_decl idecl ch tsig =
  begin
    output_line ch (".interface public abstract " ^ tsig);
    output_line ch ".super java/lang/Object";
    emit_implements idecl.LAst.interface_extends ch;
    emit_decls idecl.LAst.interface_members ch
  end

let emit_type_decl tdecl ch = 
  let tsig = tdecl.LAst.type_decl_signature in
  match tdecl.LAst.type_decl with
  | LAst.Class cdecl -> emit_class_decl cdecl ch tsig
  | LAst.Interface idecl -> emit_interface_decl idecl ch tsig

let emit_source_file src_file =
  let src_name = src_file.LAst.source_file_name in
  let file_name = (Filename.chop_extension src_name) ^ ".j" in
  try
    let ch = open_out file_name in
    begin
      output_line ch (".source " ^ (Filename.basename src_name));
      emit_type_decl src_file.LAst.source_file_decl ch;
      close_out ch
    end
  with Sys_error msg ->
    Error.error Error.FILE_OPEN_ERROR Lexing.dummy_pos
      ("Unable to open file " ^ msg)

let emit_program prog = List.iter emit_source_file prog

