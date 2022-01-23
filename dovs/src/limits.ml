(** Compiler phase to calculate the maximum number of locals and temporary stack
    locations needed for each method. *)

module CAst = Codegenerationast
module LAst = Limitsast
module Inst = Instruction

module LabelMap = Map.Make(String)

(* ********************************************************************** *)
(* ERROR MESSAGE                                                          *)
(* ********************************************************************** *)

let verify_error message =
  if !Globals.noverify
  then print_endline ("Verify warning: " ^ message)
  else Error.internal_compiler_error message

(* ********************************************************************** *)
(* LIMITS TRAVERSAL                                                       *)
(* ********************************************************************** *)

type info = { formals   : CAst.formal_param list;
	      is_static : bool                  }

type stackinfo = { maxstack : int;
		   stackmap : int LabelMap.t }
    
(*  compute_limits : local_decl list * body * bool -> LAst.body  *)
let limit_body body info = 

  (* find maximum local by iterating over the instructions of the body *)
  let mymax = 
    let rec find_max_local is max = match is with
      | [] -> max
      | i::is -> 
	find_max_local is (match Inst.local_access i with
	  | None -> max
	  | Some l -> if l+1 > max then l+1 else max)
    in 
    find_max_local body (List.length info.formals + (if info.is_static then 0 else 1)) in

  (* label map that associates instruction sequences to labels *)
  let lmap = 
    let rec build_label_map is lmap = match is with
      | [] -> lmap
      | i::is -> build_label_map is (match i with
	  | Inst.Ilabel l -> (LabelMap.add l is lmap)
	  | _ -> lmap)
    in
    build_label_map body LabelMap.empty in

  (* graph traversal of control-flow graph *)
  let rec traverse is stack stackinfo = match is with
    | [] -> stackinfo
    | i::is -> 
(*    let () = Printf.printf "At instruction %s -- stackheight is %i\n" (Inst.to_asm i) stack in *)
      (match i with
	| Inst.Ilabel l ->
	  let stackinfo' = 
	    if LabelMap.mem l stackinfo.stackmap
	    then let height = LabelMap.find l stackinfo.stackmap in
		 (if height != stack
		  then verify_error ("Stack height does not match at " ^ l ^ ": (" ^
 			     (string_of_int height) ^ " != " ^ (string_of_int stack) ^ ")")
		  else (); stackinfo)
	    else { stackinfo with stackmap = LabelMap.add l stack stackinfo.stackmap } in
	  traverse is stack stackinfo'
	| _ -> 
	  let stack' = stack + (Inst.stack_change i) in
	  let stackinfo' =
	    { stackinfo with
	      maxstack = if stack' > stackinfo.maxstack then stack' else stackinfo.maxstack } in

	  (if stack' < 0 
	   then
	      verify_error ("Negative stack height at " ^ (Inst.to_asm i) ^
				 " (" ^ (string_of_int stack') ^ ")")
	   else ();
	   let stackinfo'' = 
	     if Inst.can_fall_through i  (* then continue along path *)
	     then traverse is stack' stackinfo'
	     else stackinfo' in
	   (match Inst.can_jump i with   (* then explore alternative path *)
	     | None -> stackinfo''
	     | Some l ->
	       if LabelMap.mem l stackinfo''.stackmap
	       then
		 let height = LabelMap.find l stackinfo''.stackmap in (* been here: check consistency *)
		 (if height != stack'
		  then verify_error ("Stack height does not match at " ^ l ^ ": (" ^
 			  (string_of_int height) ^ " != " ^ (string_of_int stack') ^ ")")
		  else (); stackinfo'')
	       else
		 let is' = LabelMap.find l lmap in   (* first time here: "color" l *)
		 let stackinfo''' =
		   { stackinfo'' with stackmap = LabelMap.add l stack' stackinfo''.stackmap } in
		 traverse is' stack' stackinfo''')))
  in
  
  let stackinfo = traverse body 0 { maxstack = 0; stackmap = LabelMap.empty } in

  { LAst.body_instructions = body;
    LAst.body_max_stack    = stackinfo.maxstack;
    LAst.body_max_locals   = mymax }

let limit_body_opt body info = match body with
  | None -> None
  | Some body -> Some (limit_body body info)

let limit_field_decl fdecl = (* identity function *sigh* *)
    { LAst.field_access    = fdecl.CAst.field_access;
      LAst.field_static    = fdecl.CAst.field_static;
      LAst.field_final     = fdecl.CAst.field_final;
      LAst.field_type      = fdecl.CAst.field_type;
      LAst.field_name      = fdecl.CAst.field_name;
      LAst.field_signature = fdecl.CAst.field_signature }

let limit_method_decl mdecl =
  let info = { formals   = mdecl.CAst.method_formals;
	       is_static = mdecl.CAst.method_static } in
  let body' = limit_body_opt mdecl.CAst.method_body info in
    { LAst.method_access      = mdecl.CAst.method_access;
      LAst.method_static      = mdecl.CAst.method_static;
      LAst.method_final       = mdecl.CAst.method_final;
      LAst.method_abstract    = mdecl.CAst.method_abstract;
      LAst.method_result      = mdecl.CAst.method_result;
      LAst.method_name        = mdecl.CAst.method_name;
      LAst.method_formals     = mdecl.CAst.method_formals;
      LAst.method_throws      = mdecl.CAst.method_throws;
      LAst.method_body        = body';
      LAst.method_signature   = mdecl.CAst.method_signature }

let limit_constructor_decl cdecl =
  let info = { formals   = cdecl.CAst.constructor_formals;
	       is_static = false } in
  let body' = limit_body cdecl.CAst.constructor_body info in
  { LAst.constructor_access    = cdecl.CAst.constructor_access;
    LAst.constructor_name      = cdecl.CAst.constructor_name;
    LAst.constructor_formals   = cdecl.CAst.constructor_formals;
    LAst.constructor_throws    = cdecl.CAst.constructor_throws;
    LAst.constructor_body      = body';
    LAst.constructor_signature = cdecl.CAst.constructor_signature }

let limit_decl decl = 
  let decl' = match decl.CAst.decl with
    | CAst.Field fdecl       -> LAst.Field (limit_field_decl fdecl)
    | CAst.Method mdecl      -> LAst.Method (limit_method_decl mdecl)
    | CAst.Constructor cdecl -> LAst.Constructor (limit_constructor_decl cdecl) in
  { LAst.decl_pos = decl.CAst.decl_pos; 
    LAst.decl     = decl' }

let limit_decls decls = List.map limit_decl decls

let limit_class_decl cdecl =
  let members' = limit_decls cdecl.CAst.class_members in
  { LAst.class_final      = cdecl.CAst.class_final;
    LAst.class_abstract   = cdecl.CAst.class_abstract;
    LAst.class_name       = cdecl.CAst.class_name;
    LAst.class_extends    = cdecl.CAst.class_extends;
    LAst.class_implements = cdecl.CAst.class_implements;
    LAst.class_members    = members' }

let limit_interface_decl idecl =
  let members' = limit_decls idecl.CAst.interface_members in
  { LAst.interface_name    = idecl.CAst.interface_name;
    LAst.interface_extends = idecl.CAst.interface_extends;
    LAst.interface_members = members' }

let limit_type_decl tdecl =
  let tdecl' = match tdecl.CAst.type_decl with
    | CAst.Class cdecl     -> LAst.Class (limit_class_decl cdecl)
    | CAst.Interface idecl -> LAst.Interface (limit_interface_decl idecl) in
    { LAst.type_decl_pos       = tdecl.CAst.type_decl_pos;
      LAst.type_decl           = tdecl';
      LAst.type_canonical_name = tdecl.CAst.type_canonical_name;
      LAst.type_decl_signature = tdecl.CAst.type_decl_signature }

let limit_source_file src_file =
  let tdecl' = limit_type_decl src_file.CAst.source_file_decl in
  { LAst.source_file_name             = src_file.CAst.source_file_name;
    LAst.source_file_package          = src_file.CAst.source_file_package;
    LAst.source_file_single_imports   = src_file.CAst.source_file_single_imports;
    LAst.source_file_ondemand_imports = src_file.CAst.source_file_ondemand_imports;
    LAst.source_file_decl             = tdecl' }

let limit_program prog = List.map limit_source_file prog
