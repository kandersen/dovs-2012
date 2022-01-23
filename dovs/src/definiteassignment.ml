(** Compiler phase to check definite assignment of local variables. *)

module TAst = Typecheckingast
module Option = Utils.OptionPlus

let compare_id id1 id2 = String.compare id1.Ast.identifier id2.Ast.identifier 

module Varset = 
  Set.Make(struct 
             type t = TAst.identifier
	     let compare = compare_id
           end)

let verify (b : bool) (thunk : unit -> unit) : unit =
  if b
  then ()
  else thunk ()

let reject (b : bool) (thunk : unit -> unit) : unit =
  if b
  then thunk ()
  else ()

(************************************************************************)
(** {2 Error Messages and Checks }                                      *)
(************************************************************************)

(** If compiling with the [-joos1] option, reports the error that a
    local variable is found in its own initializer.
*)
let check_joos1_local_variable_in_own_initializer pos =
  Error.check_joos1 Error.JOOS1_LOCAL_VARIABLE_IN_OWN_INITIALIZER
    			  	pos "local variable in own initializer"

(** If compiling with the [-joos1] option, reports the error that a
    local variable declaration has no initializer.
*)
let check_joos1_omitted_local_initializer pos =
  Error.check_joos1 Error.JOOS1_OMITTED_LOCAL_INITIALIZER 
                                pos "omitted local initializer"

(** Reports the error that a local variable is not definitely assigned. *)
let error_variable_might_not_have_been_initialized id =
  let pos = id.Ast.identifier_pos in
  Error.error Error.VARIABLE_MIGHT_NOT_HAVE_BEEN_INITIALIZED 
    pos ("Variable " ^ id.Ast.identifier ^ " might not have been initialized")
	

(************************************************************************)
(** {2 Definite Assignment Traversal}                                   *)
(************************************************************************)


(* The vars_used_in family of functions are used in the Joos1 check
   that local variables do not occur in their own initializer. *)

let rec vars_used_in_exps es =
   List.fold_left Varset.union Varset.empty (List.map vars_used_in es)

and vars_used_in_lvalue l = 
  match l.TAst.lvalue with
    | TAst.NonstaticField(e, _, _) ->
      vars_used_in e
    | TAst.Local id ->
      Varset.singleton id
    | TAst.Array (e1, e2) ->
      Varset.union (vars_used_in e1) (vars_used_in e2)
    | TAst.StaticField _ ->
      Varset.empty

and vars_used_in e = 
  match e.TAst.exp with
    | TAst.Binop (e1, _, e2) ->
      Varset.union (vars_used_in e1) (vars_used_in e2)
    | TAst.Instanceof (e, _)
    | TAst.ArrayLength e
    | TAst.ArrayClone e
    | TAst.Cast (_, e)
    | TAst.Unop (_, e) ->
      vars_used_in e
    | TAst.IntConst _
    | TAst.StringConst _
    | TAst.BooleanConst _
    | TAst.Null
    | TAst.This ->
      Varset.empty
    | TAst.New (_, args, _, _)
    | TAst.NewArray (_, args)
    | TAst.StaticInvoke(_, _, args, _, _) ->
      vars_used_in_exps args
    | TAst.NonstaticInvoke(e, _, args, _, _) ->
      Varset.union (vars_used_in e) (vars_used_in_exps args)
    | TAst.IncDec (l, _)
    | TAst.Lvalue l ->
      vars_used_in_lvalue l
    | TAst.Assignment (l, e) ->
      Varset.union (vars_used_in e) (vars_used_in_lvalue l)

let rec defass_lvalue_read before scope l = 
  match l.TAst.lvalue with
    | TAst.Local id ->
      begin
	verify (Varset.mem id before)
	  (fun () -> error_variable_might_not_have_been_initialized id);
	(before, before, before)
      end
    | TAst.Array ({TAst.exp = TAst.Lvalue {TAst.lvalue = TAst.Local id}}, e) ->
      begin 
	verify (Varset.mem id before)
	  (fun () -> error_variable_might_not_have_been_initialized id);
	let (after, _, _) = defass_exp before scope e in
	(after, after, after)
      end
    | TAst.Array (e1, e2) ->
      defass_subexps before scope [e1; e2]
    | TAst.NonstaticField (e, _, _) ->
      let (after, _, _) = defass_exp before scope e in
      (after, after, after)
    | TAst.StaticField _ ->
      (before, before, before)
    
and defass_exp before scope e =
  match e.TAst.exp with
    | TAst.Binop (e1, TAst.LazyAnd, e2) ->
      let (after_e1, after_t_e1, after_f_e1) = defass_exp before scope e1 in
      let (after_e2, after_t_e2, after_f_e2) = defass_exp after_t_e1 scope e2 in
      let after_t = after_t_e2 in
      let after_f = Varset.inter after_f_e1 after_f_e2 in
      let after = Varset.inter after_t after_f in
      (after, after_t, after_f)
    | TAst.Binop (e1, TAst.LazyOr, e2) ->
      let (after_e1, after_t_e1, after_f_e1) = defass_exp before scope e1 in
      let (after_e2, after_t_e2, after_f_e2) = defass_exp after_f_e1 scope e2 in
      let after_t = Varset.inter after_t_e1 after_t_e2 in
      let after_f = after_f_e2 in
      let after = Varset.inter after_t after_f in
      (after, after_t, after_f)
    | TAst.Binop (e1, _, e2) ->
      defass_subexps before scope [e1; e2]
    | TAst.Unop (TAst.Complement, e) ->
      let (after_e, after_t_e, after_f_e) = defass_exp before scope e in
      (after_e, after_f_e, after_t_e)
    | TAst.Unop (_, e) ->
      defass_exp before scope e
    | TAst.IntConst _ ->
      (before, before, before)
    | TAst.StringConst _ ->
      (before, before, before)
    | TAst.BooleanConst true ->
      (before, before, scope)
    | TAst.BooleanConst false ->
      (before, scope, before)
    | TAst.Null ->
      (before, before, before)
    | TAst.This ->
      (before, before, before)
    | TAst.StaticInvoke(_, _, args, _, _) ->
      defass_subexps before scope args
    | TAst.NonstaticInvoke(e, _, args, _, _) ->
      defass_subexps before scope (e :: args)
    | TAst.New (_, args, _, _) ->
      defass_subexps before scope args
    | TAst.NewArray (_, args) ->
      defass_subexps before scope args
    | TAst.Lvalue l ->
      defass_lvalue_read before scope l
    | TAst.Assignment (l, e) ->
      begin
	match l.TAst.lvalue with
	  | TAst.Local id ->
	    let (after_e, _, _) = defass_exp before scope e in
	    let after = Varset.add id after_e in
	    (after, after, after)
	  | TAst.Array ({TAst.exp = TAst.Lvalue {TAst.lvalue = TAst.Local id}}, e1) ->
	    begin
	      verify (Varset.mem id before) 
		(fun () -> error_variable_might_not_have_been_initialized id); 
	      let (after_e1, _, _) = defass_exp before scope e1 in
	      let (after_e, _, _) = defass_exp after_e1 scope e in
	      (after_e, after_e, after_e)
	    end
	  | TAst.Array (e1, e2) ->
	    let (after_e1, _, _) = defass_exp before scope e1 in
	    let (after_e2, _, _) = defass_exp after_e1 scope e2 in
	    let (after_e, _, _) = defass_exp after_e2 scope e in
	    (after_e, after_e, after_e)
	  | TAst.NonstaticField (e1, _, _) ->
	    let (after_e1, _, _) = defass_exp before scope e1 in
	    let (after_e, _, _) = defass_exp after_e1 scope e in
	    (after_e, after_e, after_e)
	  | TAst.StaticField _ ->
	    let (after_e, _, _) = defass_exp before scope e in
	    (after_e, after_e, after_e)
      end
    | TAst.IncDec (l, _) ->
      defass_lvalue_read before scope l
    | TAst.Cast (_, e) ->
      defass_exp before scope e
    | TAst.Instanceof (e, _) ->
      defass_exp before scope e
    | TAst.ArrayLength e ->
      defass_exp before scope e
    | TAst.ArrayClone e ->
      defass_exp before scope e

and defass_subexps before scope es =
  match es with
    | [] ->
      (before, before, before)
    | e :: es' ->
      let (after_e, _, _) = defass_exp before scope e in
      defass_subexps after_e scope es'

let rec defass_stm before scope s =
  match s.TAst.stm with
    | TAst.Exp e ->
      let (after, _, _) = defass_exp before scope e in
      (after, after, after, scope)
    | TAst.IfThen (e, s) ->
      let (after_e, after_t_e, after_f_e) = defass_exp before scope e in
      let (after_s, _, _, _) = defass_stm after_t_e scope s in
      let after = Varset.inter after_s after_f_e in
      (after, after, after, scope)
    | TAst.IfThenElse (e, s1, s2) ->
      let (after_e, after_t_e, after_f_e) = defass_exp before scope e in
      let (after_s1, _, _, _) = defass_stm after_t_e scope s1 in
      let (after_s2, _, _, _) = defass_stm after_f_e scope s2 in
      let after = Varset.inter after_s1 after_s2 in
      (after, after, after, scope)
    | TAst.While (e, s) ->
      let (after_e, after_t_e, after_f_e) = defass_exp before scope e in
      let (_, _, _, _) = defass_stm after_t_e scope s in
      let after = after_f_e in
      (after, after, after, scope)
    | TAst.Empty ->
      (before, before, before, scope)
    | TAst.Block b ->
      let (_, _, _, _) = defass_block before scope b in
      (before, before, before, scope)
    | TAst.VoidReturn ->
      (scope, scope, scope, scope)
    | TAst.ValueReturn e ->
      let (_, _, _) = defass_exp before scope e in
      (scope, scope, scope, scope)
    | TAst.LocalDecl (_, id, Some e) ->
      begin
	reject (!Globals.joos1 && Varset.mem id (vars_used_in e))
	  (fun () -> 
	    let pos = id.Ast.identifier_pos in
	    check_joos1_local_variable_in_own_initializer pos);
	let (after_e, _, _) = defass_exp before scope e in
	let after = Varset.add id after_e in
	let scope' = Varset.add id scope in
	(after, after, after, scope')
      end
    | TAst.LocalDecl (_, id, None) ->
      begin 
	check_joos1_omitted_local_initializer id.Ast.identifier_pos;
	let scope' = Varset.add id scope in
	(before, before, before, scope')
      end
    | TAst.Throw e ->
      let (_, _, _) = defass_exp before scope e in
      (scope, scope, scope, scope)
    | TAst.SuperCall (args, _, _) ->
      let (after, _, _) = defass_subexps before scope args in
      (after, after, after, scope)
    | TAst.ThisCall (args, _, _) ->
      let (after, _, _) = defass_subexps before scope args in
      (after, after, after, scope)

and defass_block before scope b =
  match b with
    | [] ->
      (before, before, before, before)
    | s :: b' ->
      let (after, after_t, after_f, scope') = defass_stm before scope s in
      defass_block after scope' b'

let defass_body params b =
  let _ = defass_block params params b in
  ()

let formal_vars formals = 
  List.fold_right Varset.add (List.map snd formals) Varset.empty

let defass_constructor_decl cd =
  let params = formal_vars cd.TAst.constructor_formals in
  defass_body params cd.TAst.constructor_body

let defass_method_decl md =
  let params = formal_vars md.TAst.method_formals in
  Option.map_default (defass_body params) () md.TAst.method_body

let defass_decl d =
  match d.TAst.decl with
    | TAst.Field _ ->
      ()
    | TAst.Method md ->
      defass_method_decl md
    | TAst.Constructor cd ->
      defass_constructor_decl cd 

let defass_class_decl cd = 
  List.iter defass_decl cd.TAst.class_members

let defass_type_decl td = 
  match td.TAst.type_decl with
    | TAst.Class cd ->
      defass_class_decl cd
    | TAst.Interface _ ->
      ()

let defass_source_file sf = 
  defass_type_decl sf.TAst.source_file_decl

let defass_program prog =
  List.iter defass_source_file prog
  
