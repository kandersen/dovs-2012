(** Compiler phase to build maps from names to
    AST nodes for all named entities in the program.
*)

(* Locally let-bind module names to shorter ones *)
module NAst = Nameresolvingast
module T = Types
module TEnv = T.Env

(*************************************************************************)
(** {2 Error Messages }                                                  *)
(*************************************************************************)

(** Reports the error that two fields in the same class have the same name.
     @param field  the name of one of the fields
     @param pos    the position at which the error occured
*)
let error_duplicate_field field_name pos =
  Error.error Error.DUPLICATE_FIELD pos
    ("The field '" ^ field_name ^ "' is already defined")

(** Reports the error that two types with the same canonical name have been
    declared.
     @param name  the name of one of the types
     @param pos   the position at which the error occured
*)
let error_duplicate_type name pos =
  Error.error Error.DUPLICATE_TYPE pos
    ("Duplicate type " ^ name)

(** Reports the error that two variables with overlapping scope have the same name.
     @param var_name  the variable name of one of the locals
     @param pos       the position at which the error occured
*)
let error_duplicate_variable var_name pos =
  Error.error Error.DUPLICATE_VARIABLE pos
    ("The variable '" ^ var_name ^ "' is already defined")

(*************************************************************************)
(** {2 Helper Functions }                                                *)
(*************************************************************************)

let package_to_prefix pkg = match pkg with
  | None -> ""
  | Some name -> Ast.name_to_string name ^ "."

(*************************************************************************)
(** {2 Environment Lookup }                                              *)
(*************************************************************************)

(** Look up a class or interface with the given name by searching first the
    program and then the classpath.
    @param cname  the fully qualified name of the class or interface to look
                  for.
    @param tenv   the type environment.
    @return       [Some T.named_type] if [cname] resolved, or
                  [None] if there is no type of [cname].
*)
let lookup_named_type cname tenv =
  if TEnv.mem cname tenv
  then Some (TEnv.find cname tenv)
  else Classenvironment.lookup_named_type cname

(*************************************************************************)
(** {2 Environment Traversal }                                           *)
(*************************************************************************)

let check_for_local_variables_in_scope (id_list : Ast.identifier list) (stm_list : NAst.stm list option) : unit =
  let rec check_acc (id_list : Ast.identifier list) (stm_list : NAst.stm list) (acc : string list) : unit =
    match id_list, stm_list with
    | [],[] -> ()
    | {Ast.identifier_pos = pos; Ast.identifier = name} :: id_list', _ ->
      if List.exists (fun x -> x = name ) acc
      then error_duplicate_variable name pos
      else check_acc id_list' stm_list (name :: acc)
    | [],stm::stm_list' ->
      match stm.NAst.stm with
      | NAst.LocalDecl (typeexp,{Ast.identifier_pos = pos; Ast.identifier = name},exp) ->
	if List.exists (fun x -> x = name ) acc
	then error_duplicate_variable name pos
	else check_acc [] stm_list' (name :: acc)
      | NAst.IfThen (exp,stm_if) ->
	begin
	  check_acc [] (stm_if::[]) acc;
	  check_acc [] stm_list' acc;
	end
      | NAst.IfThenElse (exp,stm_if,stm_else) ->
	begin
	  check_acc [] (stm_if::[]) acc;
	  check_acc [] (stm_else::[]) acc;
	  check_acc [] stm_list' acc;
	end
      | NAst.While (exp,stm_while) ->
	begin
	  check_acc [] (stm_while::[]) acc;
	  check_acc [] stm_list' acc;
	end
      | NAst.Block stm_block ->
	begin
	  check_acc [] stm_block acc;
	  check_acc [] stm_list' acc;
	end
      | _ -> check_acc [] stm_list' acc
  in
  match stm_list with
  | None -> check_acc id_list [] []
  | Some l -> check_acc id_list l []

(** Construct a type environment [T.type_env] from an name resolved AST [NAst.program] *)
let create_type_from_field (f : NAst.field_decl) : T.field_type = 
  let access = f.NAst.field_access in
  let final = f.NAst.field_final in
  let static = f.NAst.field_static in 
  let t  = f.NAst.field_type in
  let name = f.NAst.field_name.Ast.identifier in
  {T.field_access = access;
   T.field_static = static;
   T.field_final = final;
   T.field_type = t;
   T.field_name = name;
   T.field_constval = None}

let create_type_from_constructor (c : NAst.constructor_decl) : T.constructor_type = 
  let access = c.NAst.constructor_access in
  let name = c.NAst.constructor_name.Ast.identifier in
  let formals = List.map (fun (typeexp,id) -> typeexp) c.NAst.constructor_formals in
  let identifiers = List.map (fun (typeexp,id) -> id) c.NAst.constructor_formals in
  let body = c.NAst.constructor_body in
  let throws = c.NAst.constructor_throws in
  begin
    check_for_local_variables_in_scope identifiers (Some body);
    {T.constructor_access = access;
     T.constructor_name = name;
     T.constructor_formals = formals;
     T.constructor_throws = throws}
  end

let check_field_name (id : NAst.identifier) (list : NAst.identifier list) : unit = 
  let pos = id.Ast.identifier_pos in 
  let field_name = id.Ast.identifier in
  if List.exists (fun i -> field_name = i.Ast.identifier) list
  then error_duplicate_field field_name pos
  else ()

let create_type_from_method (m : NAst.method_decl) : T.method_type =
  let access = m.NAst.method_access in
  let static = m.NAst.method_static in
  let final = m.NAst.method_final in
  let abstract = m.NAst.method_abstract in
  let result = m.NAst.method_result in
  let name = m.NAst.method_name.Ast.identifier in
  let formals = List.map (fun (typeexp,id) -> typeexp) m.NAst.method_formals in
  let identifiers = List.map (fun (typeexp,id) -> id) m.NAst.method_formals in
  let body = m.NAst.method_body in
  let throws = m.NAst.method_throws in
  begin 
    check_for_local_variables_in_scope identifiers body;
    {T.method_access = access;
     T.method_static = static;
     T.method_final = final;
     T.method_abstract = abstract;
     T.method_result = result;
     T.method_name = name;
     T.method_formals = formals; 
     T.method_throws = throws}
  end

let create_types_from_member_list (mem_list : NAst.decl list) : T.member_type list =
  let rec create_types_from_members (mems : NAst.decl list) (seen_fields : Ast.identifier list) (acc : T.member_type list ) : T.member_type list = 
    match mems with
    | [] -> acc 
    | m :: mems' ->
      match m.NAst.decl with
      | NAst.Field f ->
	begin
	  check_field_name f.NAst.field_name seen_fields;
	  let new_member = T.Field (create_type_from_field f) in 
	  create_types_from_members mems' (f.NAst.field_name :: seen_fields) (new_member :: acc)
	end
      | NAst.Method m -> 
	begin
	  let new_member = T.Method (create_type_from_method m) in 
	  create_types_from_members mems' seen_fields (new_member :: acc)
	end
      | NAst.Constructor c ->
	begin
	  let new_member = T.Constructor (create_type_from_constructor c) in 
	  create_types_from_members mems' seen_fields (new_member :: acc)
	end
  in
  create_types_from_members mem_list [] []

let create_type_environment_from_class (cl : NAst.class_decl) : T.class_type = 
  let abstract = cl.NAst.class_abstract in
  let final = cl.NAst.class_final in 
  let name = cl.NAst.class_name.Ast.identifier in
  let extends  = cl.NAst.class_extends in
  let implements = cl.NAst.class_implements in
  let members = cl.NAst.class_members in
  {T.class_final = final;
   T.class_abstract  = abstract;
   T.class_name = name;
   T.class_extends = extends;
   T.class_implements = implements;
   T.class_members = create_types_from_member_list members}

let create_type_environment_from_interface (cl : NAst.interface_decl) : T.interface_type = 
  let name = cl.NAst.interface_name.Ast.identifier in
  let extends  = cl.NAst.interface_extends in
  let members = cl.NAst.interface_members in
  {T.interface_name = name;
   T.interface_extends = extends;
   T.interface_members = create_types_from_member_list members}

let create_type_environment_from_type_decl (sf: NAst.type_decl_desc) : T.named_type =
  match sf with 
    | NAst.Class c -> T.Class (create_type_environment_from_class c)
    | NAst.Interface i -> T.Interface (create_type_environment_from_interface i)

let check_for_no_duplicate_canonical_names (type_decl : NAst.type_decl) (env : T.type_env) : unit =
  let name = type_decl.NAst.type_canonical_name in
  let pos = type_decl.NAst.type_decl_pos in
  if   TEnv.mem name env
  then error_duplicate_type (CanonicalName.to_string name) pos
  else ()

let rec create_type_environment_from_source_files (source_files : NAst.source_file list) (env : T.type_env) : T.type_env =
  match source_files with 
    | [] -> env
    | sf::sfs ->
      let source_file_decl = sf.NAst.source_file_decl in 
      let type_decl = source_file_decl.NAst.type_decl in
      let can_name = source_file_decl.NAst.type_canonical_name in
      let obj_val = create_type_environment_from_type_decl type_decl in
      begin
        check_for_no_duplicate_canonical_names source_file_decl env;
	create_type_environment_from_source_files sfs (TEnv.add can_name obj_val env)
      end

let env_program (prog : NAst.program) : T.type_env =
  create_type_environment_from_source_files prog TEnv.empty
 
(* ************************************************************ *)
