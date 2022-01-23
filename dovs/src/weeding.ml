(** Compiler phase to weed out unwanted trees produced by the parser
    and calculate the value of all constant expressions. *)

(* Locally let-bind module name to a shorter one *)
module WAst = Weedingast
module Option = Utils.OptionPlus
module StringPlus = Utils.StringPlus
module CharPlus = Utils.CharPlus
module ListPlus = Utils.ListPlus

(*************************************************************************)
(** {2 Error Message Functions}                                          *)
(*************************************************************************)

(** Report the error that an abstract class cannot be final.
     @param id    the class name
     @param pos   the error position
*)
let error_abstract_final_class name =
  let pos = name.Ast.identifier_pos in
  let id = name.Ast.identifier in
  Error.error Error.ABSTRACT_FINAL_CLASS pos
    ("The abstract class " ^ id ^ " cannot be final.")

(** Report the error that an abstract method has a body. *)
let error_abstract_method_body pos =
  Error.error Error.ABSTRACT_METHOD_BODY pos 
    "An abstract method must not have a body."

(** Reports the error that a nonabstract method must have a body. *)
let error_non_abstract_method_body pos =
  Error.error Error.NON_ABSTRACT_METHOD_BODY pos 
    "A non-abstract method must have a body."

(** Reports the error that an abstract method cannot be final or static.
     @param id    the method name
     @param pos   the error position
*)
let error_abstract_method_final_or_static id pos =
  Error.error Error.ABSTRACT_METHOD_FINAL_OR_STATIC pos
    ("The abstract method " ^ id ^ " cannot be static or final.")

(** Reports the error that a static method cannot be final. *)
let error_static_final_method pos =
  Error.error Error.STATIC_FINAL_METHOD pos "A static method cannot be final."

(** Reports the error that an interface method cannot be static or final. *)
let error_static_or_final_interface_method pos =
  Error.error Error.STATIC_OR_FINAL_INTERFACE_METHOD pos
    "An interface method cannot be static or final."

(** Reports the error that an interface must declare no fields. *)
let error_interface_field pos =
  Error.error Error.INTERFACE_FIELD pos "An interface must declare no fields."

(** Reports the error that an interface must declare no constructors. *)
let error_interface_constructor pos  =
  Error.error Error.INTERFACE_CONSTRUCTOR pos
    "An interface must declare no constructors."

(** Reports the error that an interface method cannot have a body. *)
let error_interface_method_with_body pos =
  Error.error Error.INTERFACE_METHOD_WITH_BODY pos
    "The interface method must not have a body."

(** Reports the error that a type declaration must reside in a file by the
    same name as the declared class or interface.
     @param sf    the violating source file
*)
let error_invalid_source_filename sf =
  let kind = Ast.type_decl_kind sf.Ast.source_file_decl in
  let id = Ast.type_decl_name sf.Ast.source_file_decl in
  let pos = sf.Ast.source_file_decl.Ast.type_decl_pos in
  Error.error Error.INVALID_SOURCE_FILE_NAME pos
    ("The " ^ kind ^ " " ^ id ^ " must be declared in a file called " ^ id ^ ".java")

(** Reports the error that an array element type cannot be void. *)
let error_void_type_array pos =
  Error.error Error.VOID_TYPE_ARRAY pos "Array element type cannot be void"

(** Reports the error that a variable cannot have type void. *)
let error_void_type_variable pos =
  Error.error Error.VOID_TYPE_VARIABLE pos "Variables cannot have void type"

(** Reports the error that the type of an instanceof expression cannot be void. *)
let error_void_type_instanceof pos = 
  Error.error Error.VOID_TYPE_INSTANCEOF pos "Instanceof type cannot be void"

(** Reports a syntax error for syntax handled in the weeder.
     @param message  an error message for the syntax error
     @param pos      the error position
*)
let error_syntax_error message pos =
  Error.error Error.SYNTAX_ERROR pos message

(** Reports the error that a field cannot have type void. *)
let error_void_type_field pos =
  Error.error Error.VOID_TYPE_FIELD pos "Fields cannot have void type"

(** Reports the error that void can only be used as return type. *)
let error_void_type_not_return_type pos =
  Error.error Error.VOID_TYPE_NOT_RETURN_TYPE pos 
    "void can only be used as return type"

(** Reports the error that void can not used as cast type. *)
let error_void_type_cast pos =
  Error.error Error.VOID_TYPE_CAST pos "Cast to void type is illegal"

(** Reports the error that a constructor does not have the same name as its
    enclosing class. *)
let error_constructor_name pos =
  Error.error Error.CONSTRUCTOR_NAME pos "Constructor must have the same name as its enclosing class"

(** Reports the error that a super call statement was not the first statement
    in the constructor. *)
let error_supercall_not_first_statement pos =
  Error.error Error.SUPER_CALL_NOT_FIRST_STATEMENT pos
    "Super constructor call must be the first statement in a constructor body"

(** Reports the error that a this statement is not the first statement in the
    constructor. *)
let error_thiscall_not_first_statement pos =
  Error.error Error.THIS_CALL_NOT_FIRST_STATEMENT pos 
    "Constructor call must be the first statement in a constructor body"

(** Reports the error that a final field declaration has no initializer. *)
let error_missing_final_field_initializer pos =
  Error.error Error.MISSING_FINAL_FIELD_INITIALIZER pos "A final field must have an initializer"

(** Reports an error for when an integer literal is out of range.
     @param message  an error message for the invalid integer
     @param int      the integer literal
     @param pos      the error position
*)
let error_invalid_integer message int pos =
  Error.error Error.INVALID_INTEGER pos
    ("Integer value " ^ int ^ " is out of range: " ^ message)

(** Reports the error that an instanceof is invalid. *)
let error_invalid_instanceof pos =
  Error.error Error.INVALID_INSTANCEOF pos 
    "Invalid instanceof"
    
(*************************************************************************)
(** {2 Joos1 Error Check Functions}                                      *)
(*************************************************************************)

(** If compiling with the [-joos1] option, reports the error that the
    increment or decrement expression is used. *)
let check_joos1_incdec pos =
  Error.check_joos1 Error.JOOS1_INC_DEC pos "increment and decrement"

(** If compiling with the [-joos1] option, reports the error that
    multidimensional arrays are used. *)
let check_joos1_multiarray pos =
  Error.check_joos1 Error.JOOS1_MULTI_ARRAY pos "multidimensional array"

(** If compiling with the [-joos1] option, reports the error that throw
    is used. *)
let check_joos1_throw pos =
  Error.check_joos1 Error.JOOS1_THROW pos "throw"

(** If compiling with the [-joos1] option, reports the error that the
    constructor is omitted. *)
let check_joos1_omitted_constructor name =
  let pos = name.Ast.identifier_pos in
  Error.check_joos1 Error.JOOS1_OMITTED_CONSTRUCTOR pos "omitted constructor"
    
(** If compiling with the [-joos1] option, reports the error that
    interface declaration is used. *)
let check_joos1_interface pos =
  Error.check_joos1 Error.JOOS1_INTERFACE pos "interface"

(** If compiling with the [-joos1] option, reports the error that
    explicit super call is used. *)
let check_joos1_explicit_supercall pos =
  Error.check_joos1 Error.JOOS1_EXPLICIT_SUPER_CALL pos "explicit super call"

(** If compiling with the [-joos1] option, reports the error that this
    call is used. *)
let check_joos1_thiscall pos =
  Error.check_joos1 Error.JOOS1_THIS_CALL pos "this call"

(** If compiling with the [-joos1] option, reports the error that final
    field declaration is used. *)
let check_joos1_final_field_declaration pos =
  Error.check_joos1 Error.JOOS1_FINAL_FIELD_DECLARATION pos "final field declaration"

(** If compiling with the [-joos1] option, reports the error that
    static field declaration is used. *)
let check_joos1_static_field_declaration pos =
  Error.check_joos1 Error.JOOS1_STATIC_FIELD_DECLARATION pos "static field declaration"

(*************************************************************************)
(** {2 Helper Functions}                                                 *)
(*************************************************************************)

(** Helper function to parse an [int32] from a string.

    Int32.of_string in the standard library is buggy and will not return a failure for [max_int + 1].
    Might be fixed in OCaml 3.12 *)
let string_to_int32 s =
  let i = Int64.of_string s in
    if i < Int64.of_int32 Int32.min_int ||
       i > Int64.of_int32 Int32.max_int
    then raise (Failure "int_of_string")
    else Int64.to_int32 i

let verify (b : bool) (thunk : unit -> unit) : unit =
  if b
  then ()
  else thunk ()

let reject (b : bool) (thunk : unit -> unit) : unit =
  if b
  then thunk ()
  else ()

(*************************************************************************)
(** {2 Checking Traversal Functions}                                      *)
(*************************************************************************)

let check_integer_literal (s : string) (pos : Lexing.position) : unit =
  try 
    let _ = string_to_int32 s in ()
  with
    | Failure _ ->
      error_invalid_integer s "" pos

let check_negated_integer_literal (s : string) (pos : Lexing.position) : unit =
  check_integer_literal ("-" ^ s) pos

let rec check_method_return_type (te : Ast.typeexp) : unit =
  let pos = te.Ast.typeexp_pos in
  match te.Ast.typeexp with
    | Ast.TArray t ->
      begin
	reject (Ast.is_array t)
	  (fun () -> check_joos1_multiarray pos);
	check_typeexp t
      end
    | _ -> 
      ()

and check_typeexp (te : Ast.typeexp) : unit =
  let pos = te.Ast.typeexp_pos in
  match te.Ast.typeexp with
    | Ast.Base Ast.Void ->
      error_void_type_not_return_type pos
    | _ ->
      check_method_return_type te

let check_possibly_type_in_cast (e : Ast.exp) : unit =
  let pos = e.Ast.exp_pos in
  match e.Ast.exp with
    | Ast.Lvalue { Ast.lvalue = Ast.AmbiguousName _ } ->
      ()
    | _ -> 
      error_syntax_error "Not a valid type expression!" pos

let rec check_expression (exp : Ast.exp) : unit =
  let pos = exp.Ast.exp_pos in
  match exp.Ast.exp with
    | Ast.Binop(e1, _, e2) ->
      begin 
	check_expression e1;
	check_expression e2;
      end
    | Ast.Unop(Ast.Negate, { Ast.exp = Ast.IntConst s }) ->
      check_negated_integer_literal s pos
    | Ast.Unop(_, e) ->
      check_expression e
    | Ast.IntConst s ->
      check_integer_literal s pos
    | Ast.SimpleInvoke(_, exps)
    | Ast.AmbiguousInvoke(_, _, exps) 
    | Ast.StaticInvoke(_, _, exps) ->
      List.iter check_expression exps
    | Ast.NonstaticInvoke(e, _, exps) ->
      begin
	check_expression e;
	List.iter check_expression exps
      end
    | Ast.NewArray(t, exps)
    | Ast.New(t, exps) ->
      begin
	check_typeexp t;
	List.iter check_expression exps
      end
    | Ast.Lvalue l ->
      check_lvalue l
    | Ast.Assignment(l, e) ->
      begin
	check_lvalue l;
	check_expression e
      end
    | Ast.IncDec(_, _) ->
      check_joos1_incdec pos
    | Ast.Cast(t, e) ->
      begin
	check_typeexp t;
	check_expression e
      end
    | Ast.Instanceof(e, t) ->
      begin
	check_typeexp t;
	check_expression e
      end
    | Ast.PossiblyCast(pe, e) ->
      begin
	check_possibly_type_in_cast pe;
	check_expression e;
      end
    | Ast.CharConst _
    | Ast.StringConst _ 
    | Ast.BooleanConst _
    | Ast.Null
    | Ast.This ->
      ()
    | Ast.InnerExp e ->
      check_expression e

and check_lvalue (l : Ast.lvalue) : unit =
  match l.Ast.lvalue with
    | Ast.NonstaticField(e, _) ->
      check_expression e
    | Ast.Array(e1, e2) ->
      begin
	check_expression e1;
	check_expression e2
      end
    | Ast.AmbiguousName _ ->
      ()

let check_formal (f : Ast.formal_param) : unit =
  let (typeexp, _) = f in
  check_typeexp typeexp

let check_field_decl (fd : Ast.field_decl) : unit = 
  let static = fd.Ast.field_static in
  let final = fd.Ast.field_final in
  let typ = fd.Ast.field_type in
  let id = fd.Ast.field_name in
  let init = fd.Ast.field_init in
  let pos = id.Ast.identifier_pos in
  begin
    reject final
      (fun () -> check_joos1_final_field_declaration pos);
    reject (final && Option.is_none init)
      (fun () -> error_missing_final_field_initializer pos);
    reject static
      (fun () -> check_joos1_static_field_declaration pos);
    check_typeexp typ;
    Option.map_default check_expression () init
  end

let rec check_statement (stm : Ast.stm) : unit = 
  let pos = stm.Ast.stm_pos in
  begin
    match stm.Ast.stm with
      | Ast.Exp e -> 
	check_expression e
      | Ast.IfThen(e,s) ->
	begin 
	  check_expression e;
	  check_statement s
	end
      | Ast.IfThenElse(e,s1,s2) ->
	begin
	  check_expression e;
	  check_statement s1;
	  check_statement s2
	end
      | Ast.While(e,s) -> 
	begin
	  check_expression e;
	  check_statement s
	end
      | Ast.Empty ->
	()
      | Ast.Block b ->
	check_block b
      | Ast.VoidReturn ->
	()
      | Ast.ValueReturn e ->
	check_expression e
      | Ast.LocalDecl(typ,_,None) ->
	  check_typeexp typ
      | Ast.LocalDecl(typ,_,Some e) ->
	begin
	  check_typeexp typ;
	  check_expression e
	end
      | Ast.Throw e ->
	check_joos1_throw pos
      | Ast.SuperCall _ ->
	begin
	  check_joos1_explicit_supercall pos;
	  error_supercall_not_first_statement pos
	end
      | Ast.ThisCall _ ->
	begin 
	  check_joos1_thiscall pos;
	  error_thiscall_not_first_statement pos
	end
  end

and check_block (b : Ast.block) : unit = 
  List.iter check_statement b

let check_body (b : Ast.body) : unit = 
  check_block b

let check_method_decl (md : Ast.method_decl) : unit = 
  let static = md.Ast.method_static in
  let final = md.Ast.method_final in
  let abstract = md.Ast.method_abstract in
  let result = md.Ast.method_result in
  let name = md.Ast.method_name in
  let formals = md.Ast.method_formals in
  let body = md.Ast.method_body in
  let pos = name.Ast.identifier_pos in
  begin
    reject (abstract && Option.is_some body)
      (fun () -> error_abstract_method_body pos);
    verify (abstract || Option.is_some body)
      (fun () -> error_non_abstract_method_body pos);
    reject (abstract && (static || final))
      (fun () -> error_abstract_method_final_or_static name.Ast.identifier pos);
    reject (static && final)
      (fun () -> error_static_final_method pos);
    List.iter check_formal formals;
    check_method_return_type result;
    Option.map_default check_body () body;
  end
  
let check_constructor_body (b : Ast.block) : unit =
  match b with
    | [] -> ()
    | stm :: b' ->
      let pos = stm.Ast.stm_pos in
      match stm.Ast.stm with
	| Ast.SuperCall exps ->
	  begin
	    check_joos1_explicit_supercall pos;
	    List.iter check_expression exps;
	    check_block b'
	  end	  
	| Ast.ThisCall exps ->
	  begin
	    check_joos1_thiscall pos;
	    List.iter check_expression exps;
	    check_block b'
	  end
	| _ -> check_block b
	  
let check_constructor_decl (cd : Ast.constructor_decl) (enclosing_class : string) : unit =
  let name = cd.Ast.constructor_name in
  let formals = cd.Ast.constructor_formals in
  let body = cd.Ast.constructor_body in
  let pos = name.Ast.identifier_pos in
  begin
    verify (enclosing_class = name.Ast.identifier)
      (fun () -> error_constructor_name pos);
    List.iter check_formal formals;
    check_constructor_body body
  end

let check_decl (enclosing_class : string) (d : Ast.decl) : unit = 
  match d.Ast.decl with
    | Ast.Constructor cd ->
      check_constructor_decl cd enclosing_class 
    | Ast.Method md ->
      check_method_decl md
    | Ast.Field fd ->
      check_field_decl fd

let check_class_decl (cd : Ast.class_decl) : unit =
  let final = cd.Ast.class_final in
  let abstract = cd.Ast.class_abstract in
  let name = cd.Ast.class_name in
  let members = cd.Ast.class_members in
  begin
    reject (final && abstract)
      (fun () -> error_abstract_final_class name);
    verify (List.exists Ast.is_constructor members)
      (fun () -> check_joos1_omitted_constructor name);
    List.iter (check_decl name.Ast.identifier) members
  end

let extract_method_decls (members : Ast.decl list) : Ast.method_decl list =
  let extract member =
    match member.Ast.decl with
      | Ast.Field _ 
      | Ast.Constructor _ -> []
      | Ast.Method m -> [m] in
  List.concat (List.map extract members)

let check_interface_method (m : Ast.method_decl) : unit =
  let pos = m.Ast.method_name.Ast.identifier_pos in
  let static = m.Ast.method_static in
  let final = m.Ast.method_final in
  let body = m.Ast.method_body in
  begin
    reject (static || final) 
      (fun () -> error_static_or_final_interface_method pos);
    reject (Option.is_some body)
      (fun () -> error_interface_method_with_body pos)
  end

let check_interface_decl (id : Ast.interface_decl) : unit =
  let name = id.Ast.interface_name in
  let pos = name.Ast.identifier_pos in
  let members = id.Ast.interface_members in
  let methods = extract_method_decls members in
  begin
    reject (List.exists Ast.is_constructor members)
      (fun () -> error_interface_constructor pos);
    reject (List.exists Ast.is_field members)
      (fun () -> error_interface_field pos);
    List.iter check_interface_method methods;
  end
  
let check_type_decl (td : Ast.type_decl) : unit = 
  match td.Ast.type_decl with
    | Ast.Class c -> 
      check_class_decl c
    | Ast.Interface i -> 
      check_interface_decl i

let check_source_file (sf : Ast.source_file) : unit =
  let file_name = sf.Ast.source_file_name in
  let file_decl = sf.Ast.source_file_decl in
  let is_java_file = Filename.check_suffix file_name ".java" in
  let file_base = Filename.basename file_name in
  let decl_name = Ast.type_decl_name file_decl in
  begin
    reject (Ast.is_interface file_decl)
      (fun () -> check_joos1_interface file_decl.Ast.type_decl_pos);
    verify is_java_file
      (fun () -> error_invalid_source_filename sf);
    verify (file_base = decl_name ^ ".java")
      (fun () -> error_invalid_source_filename sf);
    check_type_decl file_decl
  end

let rec check_program (sfs : Ast.program) : unit = 
  List.iter check_source_file sfs

(*************************************************************************)
(** {2 Weeding Traversal Functions}                                      *)
(*************************************************************************)

let escape_char_literal (s : string) (pos : Lexing.position) : char = 
  let is_octal = CharPlus.is_octal in
  let chars = StringPlus.list_of_string s in
  match chars with   
    | ['\''; '\\'; 'b'; '\''] -> '\b'
    | ['\''; '\\'; 't'; '\''] -> '\t'
    | ['\''; '\\'; 'n'; '\''] -> '\n'
    | ['\''; '\\'; 'f'; '\''] -> '\x0C'
    | ['\''; '\\'; 'r'; '\''] -> '\r'
    | ['\''; '\\'; '"'; '\''] -> '"'
    | ['\''; '\\'; '\''; '\''] -> '\''
    | ['\''; '\\'; '\\'; '\''] -> '\\'
    | ['\''; '\\'; o; '\''] when is_octal o ->
      Char.chr (CharPlus.int_of_oct o)
    | ['\''; '\\'; a; b; '\''] when is_octal a && is_octal b ->
      Char.chr (CharPlus.int_of_octs [b; a])
    | ['\''; '\\'; '0' as a; b; c; '\'']
    | ['\''; '\\'; '1' as a; b; c; '\'']
    | ['\''; '\\'; '2' as a; b; c; '\'']
    | ['\''; '\\'; '3' as a; b; c; '\''] when is_octal b && is_octal c -> 
      Char.chr (CharPlus.int_of_octs [c; b; a])
    | ['\''; '\\'; '\''] ->
      error_syntax_error ("Illegal escape sequence: " ^ s) pos
    | ['\''; c; '\''] ->
      c
    | _ -> 
      error_syntax_error ("Illegal escape sequence: " ^ s) pos

let rec escape_string_literal (s : string) (pos : Lexing.position) : string =
  let is_octal = CharPlus.is_octal in
  let chars = ListPlus.all_but_last (List.tl (StringPlus.list_of_string s)) in
  let rec aux chars =
    match chars with      
      | [] -> []
      | '\\' :: 'b' :: rest-> '\b' :: aux rest
      | '\\' :: 't' :: rest -> '\t' :: aux rest
      | '\\' :: 'n' :: rest -> '\n' :: aux rest
      | '\\' :: 'f' :: rest -> '\x0C' :: aux rest
      | '\\' :: 'r' :: rest -> '\r' :: aux rest
      | '\\' :: '"' :: rest -> '"' :: aux rest
      | '\\' :: '\'' :: rest -> '\'' :: aux rest
      | '\\' :: '\\' :: rest -> '\\' :: aux rest
      | '\\' :: ('0' as a) :: b :: c :: rest
      | '\\' :: ('1' as a) :: b :: c :: rest
      | '\\' :: ('2' as a) :: b :: c :: rest
      | '\\' :: ('3' as a) :: b :: c :: rest when is_octal b && is_octal c -> 
	Char.chr (CharPlus.int_of_octs [c; b; a]) :: aux rest
      | '\\' :: a :: b :: rest when is_octal a && is_octal b ->
	Char.chr (CharPlus.int_of_octs [b; a]) :: aux rest
      | '\\' :: o :: rest when is_octal o ->
	Char.chr (CharPlus.int_of_oct o) :: aux rest
      | '\\' :: _ ->
	error_syntax_error ("Illegal escape sequence: " ^ s) pos
      | c :: rest -> c :: aux rest in
  StringPlus.string_of_list (aux chars)
      
let rec transform_expression (exp : Ast.exp) : WAst.exp = 
  let pos = exp.Ast.exp_pos in 
  let exp_desc = exp.Ast.exp in 
  { WAst.exp_pos = pos;
    WAst.exp = transform_expression_desc exp_desc pos }

and transform_expression_desc (exp_desc : Ast.exp_desc) (pos : Lexing.position) : WAst.exp_desc =
  match exp_desc with 
    | Ast.Binop(e1, op ,e2) -> 
      let transformed_e1 = transform_expression e1 in
      let transformed_e2 = transform_expression e2 in
      WAst.Binop(transformed_e1, op, transformed_e2)
    | Ast.Unop(Ast.Negate, { Ast.exp = Ast.IntConst s }) ->
      let transformed_negative_number = string_to_int32 ("-" ^ s) in
      WAst.IntConst transformed_negative_number
    | Ast.Unop(u, e) ->
      let transformed_e = transform_expression e in
      WAst.Unop(u, transformed_e)
    | Ast.IntConst i ->
      WAst.IntConst(string_to_int32 i)
    | Ast.CharConst c ->
      WAst.CharConst(escape_char_literal c pos)
    | Ast.StringConst s ->
      WAst.StringConst(escape_string_literal s pos)
    | Ast.BooleanConst b ->
      WAst.BooleanConst b
    | Ast.Null ->
      WAst.Null
    | Ast.This ->
      WAst.This
    | Ast.StaticInvoke(nt, id, exp_list)->
      let transformed_exp_list = List.map transform_expression exp_list in
      WAst.StaticInvoke(nt, id, transformed_exp_list)
    | Ast.NonstaticInvoke(e, id, exp_list) ->
      let transformed_e = transform_expression e in 
      let transformed_exp_list = List.map transform_expression exp_list in
      WAst.NonstaticInvoke(transformed_e, id, transformed_exp_list)
    | Ast.SimpleInvoke(id, exp_list) ->
      let transformed_exp_list = List.map transform_expression exp_list in
      WAst.SimpleInvoke(id, transformed_exp_list)
    | Ast.AmbiguousInvoke(name, id, exp_list) ->
      let transformed_exp_list = List.map transform_expression exp_list in
      WAst.AmbiguousInvoke(name, id, transformed_exp_list)
    | Ast.New(typeexp, exp_list) ->
      let transformed_exp_list = List.map transform_expression exp_list in
      WAst.New(typeexp, transformed_exp_list)
    | Ast.NewArray(typeexp, exp_list) ->
      let transformed_exp_list = List.map transform_expression exp_list in
      WAst.NewArray(typeexp, transformed_exp_list)
    | Ast.Lvalue lvalue -> 
      WAst.Lvalue(transform_lvalue lvalue)
    | Ast.Assignment(lvalue, exp) ->
      let transformed_lvalue = transform_lvalue lvalue in
      let transformed_exp  = transform_expression exp in
      WAst.Assignment(transformed_lvalue, transformed_exp)
    | Ast.IncDec(lvalue, inc_op) ->
      let transformed_lvalue = transform_lvalue lvalue; in
      WAst.IncDec(transformed_lvalue, inc_op)
    | Ast.Cast(typeexp, exp) ->
      let transformed_exp = transform_expression exp in 
      WAst.Cast(typeexp, transformed_exp)
    | Ast.Instanceof(exp, typeexp) ->
      let transformed_exp  = transform_expression exp in
      WAst.Instanceof(transformed_exp, typeexp)
    | Ast.PossiblyCast({Ast.exp = Ast.Lvalue {Ast.lvalue = Ast.AmbiguousName n}}, e2) ->
      let transformed_e2 = transform_expression e2 in
      WAst.Cast({Ast.typeexp_pos = pos; Ast.typeexp = Ast.Named n }, transformed_e2);
    | Ast.PossiblyCast _ ->
      error_syntax_error "Broken precondition" pos
    | Ast.InnerExp exp ->
      let {WAst.exp = e} = transform_expression exp in e		  			    

and transform_lvalue (lvalue : Ast.lvalue) : WAst.lvalue = 
  let pos = lvalue.Ast.lvalue_pos in 
  let lvalue_desc = lvalue.Ast.lvalue in
  { WAst.lvalue_pos = pos;
    WAst.lvalue = transform_lvalue_desc lvalue_desc }

and transform_lvalue_desc (desc : Ast.lvalue_desc) : WAst.lvalue_desc =
  match desc with
    | Ast.NonstaticField(exp, id) ->
      let transformed_exp = transform_expression exp in
      WAst.NonstaticField(transformed_exp, id)
    | Ast.Array(e1, e2) ->
      let transformed_e1 = transform_expression e1 in
      let transformed_e2 = transform_expression e2 in
      WAst.Array(transformed_e1, transformed_e2)
    | Ast.AmbiguousName name ->
      WAst.AmbiguousName name
      
let rec transform_block (block : Ast.body) : WAst.block = 
  List.map transform_statement block

and transform_statement (s : Ast.stm) : WAst.stm =
  let pos = s.Ast.stm_pos in 
  let desc = s.Ast.stm in
  { WAst.stm_pos = pos;
    WAst.stm = transform_statement_desc desc }

and transform_statement_desc (desc : Ast.stm_desc) : WAst.stm_desc  =
  match desc with 
    | Ast.Exp e -> 
      let transformed_exp = transform_expression e in
      WAst.Exp transformed_exp
    | Ast.IfThen(exp, stm) ->
      let transformed_exp = transform_expression exp in
      let transformed_stm = transform_statement stm in
      WAst.IfThen(transformed_exp, transformed_stm)
    | Ast.IfThenElse(exp, s1, s2) ->
      let transformed_exp = transform_expression exp in
      let transformed_s1 = transform_statement s1 in
      let transformed_s2 = transform_statement s2 in 
      WAst.IfThenElse(transformed_exp, transformed_s1, transformed_s2)
    | Ast.While(exp, stm) -> 
      let transformed_exp = transform_expression exp in
      let transformed_stm = transform_statement stm in
      WAst.While(transformed_exp, transformed_stm)
    | Ast.Empty ->
      WAst.Empty
    | Ast.Block b ->
      let transformed_block = transform_block b in
      WAst.Block transformed_block
    | Ast.VoidReturn ->
      WAst.VoidReturn
    | Ast.ValueReturn exp ->
      let transformed_exp = transform_expression exp in
      WAst.ValueReturn(transformed_exp)
    | Ast.LocalDecl(typeexp, id, exp_option) ->
      let transformed_exp_option = Option.map transform_expression exp_option in
      WAst.LocalDecl(typeexp, id, transformed_exp_option)
    | Ast.Throw exp ->
      let transformed_exp = transform_expression exp in
      WAst.Throw transformed_exp
    | Ast.SuperCall exp_list ->
      let transformed_exp_list = List.map transform_expression exp_list in
      WAst.SuperCall transformed_exp_list
    | Ast.ThisCall exp_list ->
      let transformed_exp_list = List.map transform_expression exp_list in
      WAst.ThisCall transformed_exp_list

let transform_field (field : Ast.field_decl) : WAst.field_decl = 
  let access = field.Ast.field_access in 
  let static = field.Ast.field_static in
  let final = field.Ast.field_final in 
  let field_type = field.Ast.field_type in
  let name = field.Ast.field_name in
  let init = field.Ast.field_init in 
  { WAst.field_access = access;
    WAst.field_static = static;
    WAst.field_final = final;
    WAst.field_type = field_type;
    WAst.field_name = name;
    WAst.field_init = Option.map transform_expression init }

let transform_method (me : Ast.method_decl) : WAst.method_decl =
  let access = me.Ast.method_access in
  let static = me.Ast.method_static in 
  let final = me.Ast.method_final in
  let abstract = me.Ast.method_abstract in
  let result = me.Ast.method_result in
  let name = me.Ast.method_name in
  let formals = me.Ast.method_formals in
  let throws = me.Ast.method_throws in
  let body = me.Ast.method_body in
  { WAst.method_access = access;
    WAst.method_static = static;
    WAst.method_final = final;
    WAst.method_abstract = abstract;
    WAst.method_result = result;
    WAst.method_name = name;
    WAst.method_formals = formals;
    WAst.method_throws = throws;
    WAst.method_body = Option.map transform_block body }

let transform_interface_method (me : Ast.method_decl) : WAst.method_decl =
  let access = me.Ast.method_access in
  let result = me.Ast.method_result in
  let name = me.Ast.method_name in
  let formals = me.Ast.method_formals in
  let throws = me.Ast.method_throws in
  { WAst.method_access = access;
    WAst.method_static = false;
    WAst.method_final = false;
    WAst.method_abstract = true;
    WAst.method_result = result;
    WAst.method_name = name;
    WAst.method_formals = formals;
    WAst.method_throws = throws;
    WAst.method_body = None }

let transform_constructor (constructor : Ast.constructor_decl) (pos : Lexing.position) : WAst.constructor_decl =
  let access = constructor.Ast.constructor_access in 
  let name = constructor.Ast.constructor_name in
  let formals = constructor.Ast.constructor_formals in 
  let throws = constructor.Ast.constructor_throws in
  let body = constructor.Ast.constructor_body in
  let supercall = 
    if List.exists Ast.is_supercall body
      || List.exists Ast.is_thiscall body
    then []
    else [{Ast.stm_pos = pos; Ast.stm = Ast.SuperCall []}] in
  { WAst.constructor_access = access;
    WAst.constructor_name = name;
    WAst.constructor_formals = formals;
    WAst.constructor_throws = throws;
    WAst.constructor_body = transform_block (supercall @ body) }

let transform_class_decl_desc (decl_desc : Ast.decl_desc) (pos : Lexing.position) : WAst.decl_desc =
  match decl_desc with 
    | Ast.Field f ->
      WAst.Field (transform_field f)
    | Ast.Method m ->
      WAst.Method (transform_method m)
    | Ast.Constructor c ->
      WAst.Constructor (transform_constructor c pos) 

let transform_class_decl (decl : Ast.decl) : WAst.decl = 
  let pos = decl.Ast.decl_pos in
  let decl_desc = decl.Ast.decl in
  { WAst.decl_pos = pos;
    WAst.decl = transform_class_decl_desc decl_desc pos }

let transform_interface_decl_desc (decl_desc : Ast.decl_desc) (pos : Lexing.position) : WAst.decl_desc =
  match decl_desc with
    | Ast.Field _
    | Ast.Constructor _ ->
      error_syntax_error "Broken precondition!" pos
    | Ast.Method m ->
      WAst.Method (transform_interface_method m)

let transform_interface_decl (decl : Ast.decl) : WAst.decl =
  let pos = decl.Ast.decl_pos in
  let decl_desc = decl.Ast.decl in
  { WAst.decl_pos = pos;
    WAst.decl = transform_interface_decl_desc decl_desc pos }

let make_implicit_constructor (name : Ast.identifier) : WAst.decl =
  let pos = name.Ast.identifier_pos in
  let body = { WAst.stm_pos = pos;
	       WAst.stm = WAst.SuperCall [] } in
  let decl_desc = WAst.Constructor { WAst.constructor_access = Ast.Public;
					   WAst.constructor_name = name;
					   WAst.constructor_formals = [];
					   WAst.constructor_throws = [];
					   WAst.constructor_body = [body] } in
  { WAst.decl_pos = Lexing.dummy_pos;
    WAst.decl = decl_desc }

let transform_class (clazz : Ast.class_decl) : WAst.class_decl = 
  let final = clazz.Ast.class_final in
  let abstract = clazz.Ast.class_abstract in
  let name = clazz.Ast.class_name in
  let extends = clazz.Ast.class_extends in
  let implements = clazz.Ast.class_implements in
  let members = clazz.Ast.class_members in
  let members'= List.map transform_class_decl members in
  let extras =
    if List.exists WAst.is_constructor members'
    then []
    else [make_implicit_constructor name] in
  { WAst.class_final = final;
    WAst.class_abstract = abstract;
    WAst.class_name = name;
    WAst.class_extends = extends;
    WAst.class_implements = implements;
    WAst.class_members = extras @ (List.map transform_class_decl members) }

let transform_interface (interface : Ast.interface_decl) : WAst.interface_decl =
  let name = interface.Ast.interface_name in
  let extends = interface.Ast.interface_extends in
  let members = interface.Ast.interface_members in
  { WAst.interface_name = name;
    WAst.interface_extends = extends;
    WAst.interface_members = List.map transform_interface_decl members }

let transform_type_decl_desc (type_desc : Ast.type_decl_desc) : WAst.type_decl_desc = 
  match type_desc with 
    | Ast.Class c ->
      WAst.Class (transform_class c)
    | Ast.Interface i ->
      WAst.Interface (transform_interface i)

let transform_source_file_decl (sfd : Ast.type_decl) : WAst.type_decl =
  let pos = sfd.Ast.type_decl_pos in
  let type_decl_desc = sfd.Ast.type_decl in
  { WAst.type_decl_pos = pos;
    WAst.type_decl = transform_type_decl_desc type_decl_desc  }

let transform_source_file (sf : Ast.source_file) : WAst.source_file =
  let name = sf.Ast.source_file_name in
  let package = sf.Ast.source_file_package in
  let imports = sf.Ast.source_file_imports in
  let decl = sf.Ast.source_file_decl in
  { WAst.source_file_name = name;
    WAst.source_file_package = package;
    WAst.source_file_imports = imports;
    WAst.source_file_decl = transform_source_file_decl decl }

let rec transform_program (sfs : Ast.program) : WAst.program =
  List.map transform_source_file sfs
  
(*************************************************************************)
(** {2 Main Weeding Function      }                                      *)
(*************************************************************************)

(** Transform a parse AST [Ast.program] to a weeded AST [WAst.program] *)
let weed_program (sfs : Ast.program) : WAst.program =
  let () = check_program sfs in
  transform_program sfs
