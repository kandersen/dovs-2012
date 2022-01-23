(** Compiler phase to check reachability of statements and definite return from
    methods and constructors. *)

module TAst = Typecheckingast
module Option = Utils.OptionPlus

(************************************************************************)
(** {2 Error Messages }                                                 *)
(************************************************************************)

(** Reports the error that the method has a missing return statement. *)
let error_missing_return_statement pos =
  Error.error Error.MISSING_RETURN_STATEMENT pos "Missing return statement"

(** Reports the error that a statement is unreachable. *)
let error_unreachable_statement pos =
  Error.error Error.UNREACHABLE_STATEMENT pos "Unreachable statement"

(************************************************************************)
(** {2 Reachability Traversal }                                         *)
(************************************************************************)

let rec reach_stmt s reachable =
  if not reachable
  then error_unreachable_statement s.TAst.stm_pos
  else match s.TAst.stm with
    | TAst.Exp e ->
      reachable
    | TAst.IfThen(_, s) ->
      let _ = reach_stmt s reachable in
      reachable
    | TAst.IfThenElse(_, s1, s2) ->
      let s1_completes = reach_stmt s1 reachable in
      let s2_completes = reach_stmt s2 reachable in
      s1_completes || s2_completes
    | TAst.While({TAst.exp = TAst.BooleanConst true}, s) ->
      let _ = reach_stmt s reachable in
      false
    | TAst.While({TAst.exp = TAst.BooleanConst false}, s) ->
      let _ = reach_stmt s false in
      reachable
    | TAst.While(_, s) ->
      let _ = reach_stmt s reachable in
      reachable
    | TAst.Empty ->
      reachable
    | TAst.Block b ->
      reach_block b reachable
    | TAst.VoidReturn ->
      false
    | TAst.ValueReturn _ ->
      false
    | TAst.LocalDecl(t, id, init) ->
      reachable
    | TAst.Throw _ ->
      false
    | TAst.SuperCall(args, cname, ctype) ->
      reachable
    | TAst.ThisCall(args, cname, ctype) ->
      reachable
	
and reach_block s1s2 reachable =
  match s1s2 with
    | [] ->
      reachable
    | s1 :: s2 -> 
      let s1_completes = reach_stmt s1 reachable in
      reach_block s2 s1_completes
	
let reach_body b =
  reach_block b true
    
let insert_void_return cm =
  let pos = cm.TAst.decl_pos in
  let void_return_stm = { TAst.stm = TAst.VoidReturn; TAst.stm_pos = pos } in
  match cm.TAst.decl with
    | TAst.Field _ -> 
      cm
    | TAst.Constructor cd ->
      { cm with TAst.decl = TAst.Constructor { cd with
	TAst.constructor_body = cd.TAst.constructor_body @ [void_return_stm] }}
    | TAst.Method md ->
      { cm with TAst.decl = TAst.Method { md with
	TAst.method_body = Option.map (fun b -> b @ [void_return_stm]) md.TAst.method_body }}
	
let rec reach_member cm =
  match cm.TAst.decl with
    | TAst.Field fd ->
      cm
    | TAst.Method md ->
      let pos = cm.TAst.decl_pos in
      let void_type = Types.Base Ast.Void in
      let return_type = md.TAst.method_result in
      if md.TAst.method_abstract
      then cm
      else 
	begin
	  match md.TAst.method_body with
	    | None -> Error.internal_compiler_error "reach_member: Non-abstract without body"
	    | Some b ->
	      if (return_type = void_type) && reach_body b
	      then insert_void_return cm
	      else if (return_type <> void_type) && reach_body b
	      then error_missing_return_statement pos
	      else cm
	end
    | TAst.Constructor cd ->
      let _ = reach_body cd.TAst.constructor_body in
      insert_void_return cm
	
let reach_class_decl cd =
  { cd with TAst.class_members = List.map reach_member cd.TAst.class_members }
    
let reach_type_decl td =
  match td.TAst.type_decl with
    | TAst.Class cd ->
      { td with TAst.type_decl = TAst.Class (reach_class_decl cd) }
    | TAst.Interface _ ->
      td
	
let reach_source_file sf =
  { sf with TAst.source_file_decl = reach_type_decl sf.TAst.source_file_decl }
    
let reach_program prog = 
  List.map reach_source_file prog
