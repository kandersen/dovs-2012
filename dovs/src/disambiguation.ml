(** Compiler phase to disambiguate ambiguous uses of names. *)

module T = Types
module NAst = Nameresolvingast
module DAst = Disambiguationast

(************************************************************************)
(** {2 Error Messages }                                                 *)
(************************************************************************)

(** Reports the error that a variable could not be found through the
    disambiguation rules.
     @param name  the [name] of the variable
*)
let error_variable_not_found name =
  Error.error Error.VARIABLE_NOT_FOUND name.Ast.name_pos
    ("Could not find variable " ^ Ast.name_to_string name)

(** Reports the error that a name could not resolved to neither a variable
    nor a type through the disambiguation rules.
     @param name  the [name] of the variable or type
*)
let error_variable_or_type_not_found name =
  Error.error Error.VARIABLE_OR_TYPE_NOT_FOUND name.Ast.name_pos
    ("Could not find variable or type " ^ Ast.name_to_string name)

(** Reports the error of an illegal forward reference of a nonstatic field.
     @param field  the field type of the referred field
     @param pos    the position at which the error occured
*)
let error_illegal_forward_field_reference field pos =
  Error.error Error.ILLEGAL_FORWARD_FIELD_REFERENCE pos
    ("Illegal forward reference of instance field " ^ field.T.field_name)

(** Reports the error of an illegal forward reference of a static field.Joos 2) The initializer for a static field must not refer by simple name to itself or a static field declared later in the same class, except as the direct left-hand side of an assignment. (error_illegal_forward_static_field_reference)
     @param field  the field type of the referred field
     @param pos    the position at which the error occured

*)
let error_illegal_forward_static_field_reference field pos =
  Error.error Error.ILLEGAL_FORWARD_STATIC_FIELD_REFERENCE pos
    ("Illegal forward reference of static field " ^ field.T.field_name)

(** If compiling with the [-joos1] option, reports the error that
    implicit this class is used for static fields.
     @param name  the disambiguated field name
*)
let check_joos1_implicit_this_class_static_field name =
  Error.check_joos1 Error.JOOS1_IMPLICIT_THIS_CLASS_STATIC_FIELD 
    name.Ast.name_pos "Implicit this class for static fields"

let check_joos1_implicit_this_class_static_field_with_pos pos =
  Error.check_joos1 Error.JOOS1_IMPLICIT_THIS_CLASS_STATIC_FIELD 
    pos "Implicit this class for static fields"

(*************************************************************************)
(** {2 Disambiguation Transformation }                                   *)
(*************************************************************************)

type info = 
    { program : NAst.program;           (* top level program *)
      tenv : Types.type_env;            (* global typing environment *)
      source_file : NAst.source_file;   (* current source file *)
      lenv : string list;               (* current local environment *)
      fields : DAst.field_decl list;    (* list of initialized fields *)
      current_field : NAst.field_decl option;  (* current NAst.field assigning to *)
      should_check_field_init : bool    (* true when left side *)
    }

let lookup_in_local_environment id lenv =
  let name_str = id.Ast.identifier in
  let localvar = List.filter (fun e -> e = name_str) lenv in
  match localvar with
  | x::xs -> Some id
  | [] -> None

let lookup_in_init_fields id info is_static =
  List.exists (fun f ->
    let is_same_id = (f.DAst.field_name.Ast.identifier = id.Ast.identifier) in
    let is_same_static = (f.DAst.field_static = is_static) in
    is_same_id && is_same_static
  ) info.fields
    
let extract_types (sfs : NAst.program) : Types.canonical_name list =
  let extract_type (sf : NAst.source_file) : Types.canonical_name =
    let type_name = NAst.type_decl_name sf.NAst.source_file_decl in
    let prefix = Nameresolving.package_to_prefix sf.NAst.source_file_package in
    CanonicalName.make (prefix ^ type_name) in
  List.map extract_type sfs

(* helper to lookup a qualified name *)
let lookup_canonical_name qname info =
  let cname = CanonicalName.make qname in
    match Environment.lookup_named_type cname info.tenv with
      | None -> None
      | Some _ -> Some cname

let create_identifier string pos =
  { Ast.identifier_pos = pos; Ast.identifier = string }

let create_exp (exp_desc : DAst.exp_desc) pos : DAst.exp =
  { DAst.exp_pos = pos; DAst.exp = exp_desc }

let rec build_non_static_field (id_list : Ast.identifier list) (lvalue : DAst.lvalue) : DAst.lvalue =
  match id_list with
  | [] -> lvalue
  | x::xs ->
    let pos = x.Ast.identifier_pos in
    let exp = create_exp (DAst.Lvalue lvalue) pos in
    let new_lval = {DAst.lvalue_pos = pos; DAst.lvalue = DAst.NonstaticField (exp,x)} in
    build_non_static_field xs new_lval

let get_unqualified_type a1 info : CanonicalName.t option =
  let type_name = info.source_file.NAst.source_file_name in
  let named_types = extract_types info.program in
  let package_prefix = Nameresolving.package_to_prefix info.source_file.NAst.source_file_package in
  let single_imports = info.source_file.NAst.source_file_single_imports in
  let ondemand_imports = info.source_file.NAst.source_file_ondemand_imports in
  Nameresolving.resolve_name_from_identifier a1 package_prefix type_name named_types single_imports ondemand_imports

let get_qualified_type id_list info : CanonicalName.t option =
  let strs = List.map (fun id -> id.Ast.identifier) id_list in
  let qual_name = String.concat "." strs in
  lookup_canonical_name qual_name info

let lookup_field id host info =
  let name_str = id.Ast.identifier in
  Hierarchy.lookup_field host name_str info.tenv

let check_forward_reference id info =
  let host = info.source_file.NAst.source_file_decl.NAst.type_canonical_name in
  let pos = id.Ast.identifier_pos in
  match lookup_field id host info with
  | None -> ();
  | Some (can,field) ->
    let is_static = field.T.field_static in
    let is_non_static_referencing_static = match info.current_field with
      | None -> false
      | Some d -> (not d.NAst.field_static && is_static) in
    let is_initialized = lookup_in_init_fields id info field.T.field_static in
    let is_same_class = (can = host) in (* not inherited fields *)
    if is_initialized || not is_same_class || not info.should_check_field_init || is_non_static_referencing_static
    then ()
    else
      if is_static
      then error_illegal_forward_static_field_reference field pos
      else error_illegal_forward_field_reference field pos
  
let try_local id info : DAst.lvalue option =
  let pos = id.Ast.identifier_pos in
  let localvar = lookup_in_local_environment id info.lenv in
  match localvar with
  | Some id -> Some {DAst.lvalue_pos = pos; DAst.lvalue = DAst.Local id}
  | None -> None
      
let try_field identifier host info : DAst.lvalue option =
  let pos = identifier.Ast.identifier_pos in
  match lookup_field identifier host info with
  | None -> None
  | Some (can_name,field) -> 
    Some {DAst.lvalue_pos = pos; DAst.lvalue = DAst.NonstaticField (create_exp DAst.This pos, identifier)}

let try_field_simple identifier host info : DAst.lvalue option =
  let pos = identifier.Ast.identifier_pos in
  match lookup_field identifier host info with
  | None -> None
  | Some (can_name,field) -> 
    (if field.T.field_static
    then check_joos1_implicit_this_class_static_field_with_pos pos
    else ());
    check_forward_reference identifier info;
    if field.T.field_static
    then Some {DAst.lvalue_pos = pos; DAst.lvalue = DAst.StaticField (can_name, identifier)}
    else Some {DAst.lvalue_pos = pos; DAst.lvalue = DAst.NonstaticField (create_exp DAst.This pos, identifier)}
	
let try_unqualified_field a1 a2 info : DAst.lvalue option =
  let pos = a1.Ast.identifier_pos in
  let can_name = get_unqualified_type a1 info in
  match can_name with
  | None -> None
  | Some c -> Some {DAst.lvalue_pos=pos;DAst.lvalue=DAst.StaticField (c,a2)}

let try_qualified_field id_list a_plus_1 info : DAst.lvalue option =
  let can_name = get_qualified_type id_list info in
  let pos = a_plus_1.Ast.identifier_pos in
  match can_name with
  | None -> None
  | Some c -> Some {DAst.lvalue_pos=pos;DAst.lvalue=DAst.StaticField (c,a_plus_1)}

let rec disamb_qualified_name id_list seen_list info : DAst.lvalue option =
  match id_list, seen_list with
  | [], _ -> None
  | x::xs, [] -> disamb_qualified_name xs [x] info
  | x::xs, y::[] -> disamb_qualified_name xs (seen_list @ [x]) info
  | x::xs, y::ys ->
    let type_result = try_qualified_field seen_list x info in
    match type_result with
    | Some lval -> Some (build_non_static_field xs lval)
    | None -> disamb_qualified_name xs (seen_list @ [x]) info

(** Returns an [lvalue] of the disambiguated name, cf. rules 1-4 and 1-2,4,6 of
    Scopes and Environments slides 43 and 44-45, respectively.
     @param name  the ambiguous name (not including method name for invocations)
     @param info  the inherited information
     @return  [Some lvalue] the resulting [lvalue]
              or [None] if the name could not be disambiguated
*)

(* -> lvalue option *)
let disamb_name (name : Ast.name) info : DAst.lvalue option =
  let host = info.source_file.NAst.source_file_decl.NAst.type_canonical_name in
  match name.Ast.name with
    | Ast.Simple id -> 
      (let local_result = try_local id info in 
       let field_result = try_field_simple id host info in
       match local_result,field_result with
       | Some lval, _ -> Some lval
       | None, Some lval -> Some lval
       | _ -> None)
    | Ast.Qualified (a1::a2::rest) ->
      (let local_result = try_local a1 info in 
       let field_result = try_field_simple a1 host info in
       let type_unqualified = try_unqualified_field a1 a2 info in 
       match local_result,field_result,type_unqualified with
       | Some lval, _, _ -> Some (build_non_static_field (a2::rest) lval)
       | None, Some lval, _ -> Some (build_non_static_field (a2::rest) lval)
       | None, None, Some lval -> Some (build_non_static_field rest lval)
       | _ -> disamb_qualified_name rest (a1::[a2]) info)
    | _ -> None

let rec disamb_lvalue lv info =
  { DAst.lvalue_pos = lv.NAst.lvalue_pos;
    DAst.lvalue = match lv.NAst.lvalue with
    | NAst.NonstaticField (e, id) ->
      (let new_exp = disamb_exp e info in
      match new_exp.DAst.exp with
      | DAst.This -> 
	DAst.NonstaticField (new_exp, id)
      | _ -> 
	check_forward_reference id info;
	DAst.NonstaticField (new_exp, id))
    | NAst.Array (e1, e2) ->
      DAst.Array (disamb_exp e1 info, disamb_exp e2 info)
    | NAst.AmbiguousName n ->
      (match disamb_name n info with
      | None -> error_variable_not_found n 
      | Some lv -> lv.DAst.lvalue) }

and disamb_exp e info =
  { DAst.exp_pos = e.NAst.exp_pos;
    DAst.exp = match e.NAst.exp with
      | NAst.Binop (e1, op, e2) ->
          DAst.Binop (disamb_exp e1 info, op, disamb_exp e2 info)
      | NAst.Unop (op, e) ->
          DAst.Unop (op, disamb_exp e info)
      | NAst.IntConst i ->
          DAst.IntConst i
      | NAst.CharConst c ->
          DAst.CharConst c
      | NAst.StringConst s ->
          DAst.StringConst s
      | NAst.BooleanConst b ->
          DAst.BooleanConst b
      | NAst.Null ->
          DAst.Null
      | NAst.This ->
          DAst.This
      | NAst.StaticInvoke (nt, id, es) ->
          DAst.StaticInvoke (nt, id, disamb_exp_list es info)
      | NAst.NonstaticInvoke (e, id, es) ->
          DAst.NonstaticInvoke
            (disamb_exp e info, id, disamb_exp_list es info)
      | NAst.SimpleInvoke (id, es) ->
          DAst.SimpleInvoke (id, disamb_exp_list es info)
      | NAst.AmbiguousInvoke (n, id, es) ->
	begin
	  let pos = id.Ast.identifier_pos in
	  let exp_list = disamb_exp_list es info in
	  let dis_name_for_field = disamb_name n info in
	  match dis_name_for_field with
	  | Some lval -> DAst.NonstaticInvoke ({DAst.exp_pos = pos;DAst.exp = DAst.Lvalue lval}, id, exp_list)
	  | None -> (* Now it has to be a static invoke, rule 3 or 5 *)
	    let can_name = match n.Ast.name with
	      | Ast.Simple id -> get_unqualified_type id info
	      | Ast.Qualified ids -> get_qualified_type ids info in
	    match can_name with
	    | None -> error_variable_or_type_not_found n
	    | Some c ->
	      let method_lookup = Hierarchy.lookup_method c id.Ast.identifier info.tenv in
	      match method_lookup with
	      | [] -> error_variable_or_type_not_found n
	      | _ -> DAst.StaticInvoke (c, id, exp_list)
	end
      | NAst.New (t, es) ->
          DAst.New (t, disamb_exp_list es info)
      | NAst.NewArray (t, es) ->
          DAst.NewArray (t, disamb_exp_list es info)
      | NAst.Lvalue lv -> DAst.Lvalue
	(disamb_lvalue lv info)
      | NAst.Assignment (lv, e) ->
	(match lv.NAst.lvalue with
	|NAst.NonstaticField (e1, id) -> DAst.Assignment ((disamb_lvalue lv info), (disamb_exp e info))
	| _ -> DAst.Assignment ((disamb_lvalue lv { info with should_check_field_init = false }),(disamb_exp e info)))
      | NAst.IncDec (lv, op) ->
          DAst.IncDec (disamb_lvalue lv info, op)
      | NAst.Cast (t, e) ->
          DAst.Cast (t, disamb_exp e info)
      | NAst.Instanceof (e, t) ->
          DAst.Instanceof (disamb_exp e info, t) }
  
and disamb_exp_list es info =
  List.map (fun e -> disamb_exp e info) es

let disamb_exp_opt exp_opt info = match exp_opt with
  | None -> None
  | Some e -> Some (disamb_exp e info)

(* -> DAst.stm * info *)
let rec disamb_stm stm info = 
  let (stm', info') = match stm.NAst.stm with
    | NAst.Exp e ->
        (DAst.Exp (disamb_exp e info), info)
    | NAst.IfThen (e, s) ->
        let (s', info') = disamb_stm s info in
          (DAst.IfThen (disamb_exp e info, s'), info')
    | NAst.IfThenElse (p, t, f) ->
        let (t', info') = disamb_stm t info in
        let (f', info'') = disamb_stm f info in
          (DAst.IfThenElse (disamb_exp p info, t', f'), info'')
    | NAst.While (e, s) ->
        let (s', info') = disamb_stm s info in
          (DAst.While (disamb_exp e info, s'), info')
    | NAst.Empty ->
        (DAst.Empty, info)
    | NAst.Block stms ->
        (DAst.Block (disamb_block stms info), info)
    | NAst.VoidReturn ->
        (DAst.VoidReturn, info)
    | NAst.ValueReturn e ->
        (DAst.ValueReturn (disamb_exp e info), info)
    | NAst.LocalDecl (t,id,e) ->
        (* note: locals are visible in their own initializer *)
        let info = { info with lenv = id.Ast.identifier :: info.lenv } in
          (DAst.LocalDecl (t, id, disamb_exp_opt e info), info)
    | NAst.Throw e ->
        (DAst.Throw (disamb_exp e info), info)
    | NAst.SuperCall es ->
        (DAst.SuperCall (disamb_exp_list es info), info)
    | NAst.ThisCall es ->
        (DAst.ThisCall (disamb_exp_list es info), info)
  in 
    ({ DAst.stm_pos = stm.NAst.stm_pos; DAst.stm = stm' }, info')

and disamb_block stms info = match stms with
  | [] -> []
  | stm :: stms' ->
      let (stm', info') = disamb_stm stm info in
        stm' :: disamb_block stms' info'

let disamb_formals fs info = fs

let disamb_body body fs info =
  let info' = { info with lenv = List.map (fun (_,id) -> id.Ast.identifier) fs} in 
  disamb_block body info'

let disamb_body_opt body fs tenv = match body with
  | None -> None
  | Some b -> Some (disamb_body b fs tenv)

let disamb_field_decl d info = 
  let info' = { info with should_check_field_init = true;
	      current_field = Some d} in
  let init = disamb_exp_opt d.NAst.field_init info' in
    { DAst.field_access = d.NAst.field_access;
      DAst.field_static = d.NAst.field_static;
      DAst.field_final  = d.NAst.field_final;
      DAst.field_type   = d.NAst.field_type;
      DAst.field_name   = d.NAst.field_name;
      DAst.field_init   = init }

let disamb_method_decl d info = 
  let formals = disamb_formals d.NAst.method_formals info in
  let body = disamb_body_opt d.NAst.method_body formals info in
    { DAst.method_access   = d.NAst.method_access;
      DAst.method_static   = d.NAst.method_static;
      DAst.method_final    = d.NAst.method_final;
      DAst.method_abstract = d.NAst.method_abstract;
      DAst.method_result   = d.NAst.method_result;
      DAst.method_name     = d.NAst.method_name;
      DAst.method_formals  = formals;
      DAst.method_throws   = d.NAst.method_throws;
      DAst.method_body     = body }

let disamb_constructor_decl d info = 
  let formals = disamb_formals d.NAst.constructor_formals info in
  let body = disamb_body d.NAst.constructor_body formals info in
    { DAst.constructor_access  = d.NAst.constructor_access;
      DAst.constructor_name    = d.NAst.constructor_name;
      DAst.constructor_formals = formals;
      DAst.constructor_throws  = d.NAst.constructor_throws;
      DAst.constructor_body    = body }

let disamb_decls ds info =
  let rec visit ds info =
    match ds with
    | [] -> []
    | d :: ds -> match d.NAst.decl with
        | NAst.Field f ->
	    let field_decl = disamb_field_decl f info in
	    let info' = { info with fields = field_decl  :: info.fields } in 
	    { DAst.decl_pos = d.NAst.decl_pos;
	      DAst.decl = DAst.Field field_decl} :: visit ds info' 
        | NAst.Method m ->
            { DAst.decl_pos = d.NAst.decl_pos;
              DAst.decl = DAst.Method (disamb_method_decl m info) }
            :: visit ds info
        | NAst.Constructor c ->
            { DAst.decl_pos = d.NAst.decl_pos;
              DAst.decl = DAst.Constructor (disamb_constructor_decl c info) }
            :: visit ds info
  in visit ds info

let disamb_class_decl decl info =
  let members = disamb_decls decl.NAst.class_members info in
    { DAst.class_final      = decl.NAst.class_final;
      DAst.class_abstract   = decl.NAst.class_abstract;
      DAst.class_name       = decl.NAst.class_name;
      DAst.class_extends    = decl.NAst.class_extends;
      DAst.class_implements = decl.NAst.class_implements;
      DAst.class_members    = members }

let disamb_interface_decl decl info =
  let members = disamb_decls decl.NAst.interface_members info in
    { DAst.interface_name    = decl.NAst.interface_name;
      DAst.interface_extends = decl.NAst.interface_extends;
      DAst.interface_members = members }

let disamb_type_decl decl info =
  let cname = decl.NAst.type_canonical_name in
    { DAst.type_decl_pos       = decl.NAst.type_decl_pos;
      DAst.type_canonical_name = cname;
      DAst.type_decl           = match decl.NAst.type_decl with
        | NAst.Class d -> DAst.Class (disamb_class_decl d info)
        | NAst.Interface d -> DAst.Interface (disamb_interface_decl d info) }

let disamb_source_file prog sf tenv =
  let info = { program = prog;
               tenv = tenv;
               source_file = sf;
               lenv = [];
	       fields = [];
	       current_field = None;
	       should_check_field_init = false} in
  let decl = disamb_type_decl sf.NAst.source_file_decl info in
    { DAst.source_file_name             = sf.NAst.source_file_name;
      DAst.source_file_package          = sf.NAst.source_file_package;
      DAst.source_file_single_imports   = sf.NAst.source_file_single_imports;
      DAst.source_file_ondemand_imports = sf.NAst.source_file_ondemand_imports;
      DAst.source_file_decl             = decl }

(** Transform a name resolved AST [NAst.program] into a fully disambiguated AST [DAst.program] *)
let disamb_program (prog : NAst.program) (tenv : Types.type_env) : DAst.program =
  List.map (fun sf -> disamb_source_file prog sf tenv) prog
