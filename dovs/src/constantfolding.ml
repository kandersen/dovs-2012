(** Compiler phase to perform constant folding. *)

module NAst = Nameresolvingast
module TAst = Typecheckingast

open Utils

(************************************************************************)
(** {2 Constant Folding Traversal }                                     *)
(************************************************************************)

type info =
    { tenv : Types.type_env;
      changed: bool;
    }

(*  cfold_lvalue :  TAst.lvalue -> info -> TAst.exp_desc  *)
let rec cfold_lvalue lvalue info = match lvalue.TAst.lvalue with
  | TAst.NonstaticField (e,id,base) ->
    let e' = cfold_exp e info in 
    let lvalue' = { lvalue with TAst.lvalue = TAst.NonstaticField (e',id,base) } in
    TAst.Lvalue lvalue'
  | TAst.Array (e0,e1) ->
    let e0' = cfold_exp e0 info in
    let e1' = cfold_exp e1 info in
    let lvalue' = { lvalue with TAst.lvalue = TAst.Array (e0',e1') } in
    TAst.Lvalue lvalue'
  | TAst.Local _ -> 
    TAst.Lvalue lvalue (* identity *)
  | TAst.StaticField (host,id,base) -> 
    let field_id = id.Ast.identifier in
    (match Hierarchy.lookup_field host field_id info.tenv with
      | None ->
	(Error.internal_compiler_error 
		 ("Field " ^ field_id ^ " not found in " ^ CanonicalName.to_string host))
      | Some (_, f) ->
	if f.Types.field_final (* field is static and final *)
	then 
	  match f.Types.field_constval with (* and constant *)
	    | Some (Types.Int i) -> (TAst.IntConst i)
	    | Some (Types.Bool b) -> (TAst.BooleanConst b)
	    | Some (Types.String s) -> (TAst.StringConst s)
	    | None -> TAst.Lvalue lvalue (* identity *)
	else TAst.Lvalue lvalue (* identity *)
    )
      
and cfold_exp e info =
  { e with TAst.exp =
      match e.TAst.exp with
	| TAst.Binop (e1,op,e2) -> 
	  let e1' = cfold_exp e1 info in
	  let e2' = cfold_exp e2 info in
	  (match e1'.TAst.exp, e2'.TAst.exp with
	    | TAst.IntConst v1, TAst.IntConst v2 ->
	      (match op with
		| TAst.Plus   -> TAst.IntConst (Int32.add v1 v2)
		| TAst.Minus  -> TAst.IntConst (Int32.sub v1 v2)
		| TAst.Times  -> TAst.IntConst (Int32.mul v1 v2)
		| TAst.Divide -> if v2 = 0l
		                 then TAst.Binop (e1',op,e2')
		                 else TAst.IntConst (Int32.div v1 v2)
		| TAst.Modulo -> if v2 = 0l
		                 then TAst.Binop (e1',op,e2')
		                 else TAst.IntConst (Int32.rem v1 v2)
		| TAst.And    -> TAst.IntConst (Int32.logand v1 v2)
		| TAst.Or     -> TAst.IntConst (Int32.logor v1 v2)
		| TAst.Xor    -> TAst.IntConst (Int32.logxor v1 v2)
		| TAst.Eq     -> TAst.BooleanConst (v1 = v2)
		| TAst.Ne     -> TAst.BooleanConst (v1 <> v2)
		| TAst.Lt     -> TAst.BooleanConst ((Int32.compare v1 v2) < 0)
		| TAst.Le     -> TAst.BooleanConst ((Int32.compare v1 v2) <= 0)
		| TAst.Gt     -> TAst.BooleanConst ((Int32.compare v1 v2) > 0)
		| TAst.Ge     -> TAst.BooleanConst ((Int32.compare v1 v2) >= 0)
		| _ -> 
		  (Error.internal_compiler_error "Invalid op for int folding")
	      )
	    | TAst.BooleanConst b1, TAst.BooleanConst b2 ->
	      (match op with
		| TAst.Eq      -> TAst.BooleanConst (b1 = b2)
		| TAst.Ne      -> TAst.BooleanConst (b1 <> b2)
		| TAst.And     -> TAst.BooleanConst (b1 && b2)
		| TAst.Or      -> TAst.BooleanConst (b1 || b2)
		| TAst.Xor     -> TAst.BooleanConst (b1 <> b2)
		| TAst.LazyAnd -> TAst.BooleanConst (b1 && b2)
		| TAst.LazyOr  -> TAst.BooleanConst (b1 || b2)
		| _ -> 
		  (Error.internal_compiler_error "Invalid op for boolean folding")
	      )
	    | TAst.StringConst s1, TAst.StringConst s2 ->
	      (match op with
		| TAst.Concat -> TAst.StringConst (s1 ^ s2)
		| TAst.Aeq    -> TAst.BooleanConst (s1 = s2)
		| TAst.Ane    -> TAst.BooleanConst (s1 <> s2)
		| _ -> 
		  (Error.internal_compiler_error "Invalid op for string folding")
	      )
	    | _,_ -> TAst.Binop (e1',op,e2')
	  )
	| TAst.Unop (op,e) ->
	  let e' = cfold_exp e info in 
	  (match (op, e'.TAst.exp) with
	    | TAst.Negate,          TAst.IntConst i -> 
	      TAst.IntConst (Int32.neg i)
	    | TAst.Complement,      TAst.BooleanConst b -> 
	      TAst.BooleanConst (not b)
	    | TAst.BooleanToString, TAst.BooleanConst b ->
	      TAst.StringConst (string_of_bool b)
	    | TAst.ByteToString,    TAst.IntConst i ->
	      let lowest        = Int32.logand i 127l in
	      let lowest_signed = 
		if lowest = Int32.logand i 255l (* is 8th bit set? *)
		then lowest
		else Int32.sub lowest 128l in
	      TAst.StringConst (Int32.to_string lowest_signed)
	    | TAst.ShortToString,   TAst.IntConst i -> 
	      TAst.StringConst (Int32.to_string i)
	    | TAst.IntToString,     TAst.IntConst i -> 
	      TAst.StringConst (Int32.to_string i)
	    | TAst.CharToString,    TAst.IntConst i -> 
	      let i' = Int32.to_int i in
	      let c = Char.chr i' in
	      let str = Std.string_of_char c in
	      TAst.StringConst str 
	    | TAst.ObjectToString,  TAst.StringConst s -> 
	      e'.TAst.exp
	    | _, _ -> TAst.Unop (op,e')
	  )
	| TAst.IntConst _ 
(*	| TAst.CharConst _  *)
	| TAst.StringConst _
	| TAst.BooleanConst _
	| TAst.Null
	| TAst.This -> e.TAst.exp
	| TAst.StaticInvoke (nt,id,es,base,m) ->
	  let es' = cfold_exp_list es info in TAst.StaticInvoke (nt,id,es',base,m)
	| TAst.NonstaticInvoke (e,id,es,base,m) ->
	  let e' = cfold_exp e info in 
	  let es' = cfold_exp_list es info in TAst.NonstaticInvoke (e',id,es',base,m)
	| TAst.New (t,es,cname,c) ->
	  let es' = cfold_exp_list es info in TAst.New (t,es',cname,c)
	| TAst.NewArray (t,es) ->
	  let es' = cfold_exp_list es info in TAst.NewArray (t,es')
	| TAst.Lvalue lvalue ->
	  let lvalue' = cfold_lvalue lvalue info in (*TAst.Lvalue*) lvalue'
	| TAst.Assignment (lvalue,e) ->
	  let lvalue' = cfold_lvalue lvalue info in
	  let e' = cfold_exp e info in 
	  (match lvalue' with
	    | TAst.Lvalue lvalue'' -> TAst.Assignment (lvalue'',e')
	    | _ -> 
	      (Error.internal_compiler_error "Trying to assign to constant lvalue"))
	| TAst.IncDec (lvalue,op) ->
	  let lvalue' = cfold_lvalue lvalue info in
	  (match lvalue' with
	    | TAst.Lvalue lvalue'' -> TAst.IncDec (lvalue'',op)
	    | _ -> 
	      (Error.internal_compiler_error "Trying to inc/dec constant lvalue"))
	| TAst.Cast (t,e) ->
	  let e' = cfold_exp e info in 
	  if Types.is_string t then
	    match e'.TAst.exp with
	      | TAst.StringConst s -> e'.TAst.exp
	      | _ -> TAst.Cast (t,e')
	  else
	    TAst.Cast (t,e')
	| TAst.Instanceof (e,t) ->
	  let e' = cfold_exp e info in TAst.Instanceof (e',t)
	| TAst.ArrayLength e ->
	  let e' = cfold_exp e info in TAst.ArrayLength e'
	| TAst.ArrayClone e ->
	  let e' = cfold_exp e info in TAst.ArrayClone e'
  }

and cfold_exp_list es info = List.map (fun e -> cfold_exp e info) es

let cfold_exp_opt e info = match e with
  | None -> None
  | Some e -> let e' = cfold_exp e info in Some e'

let rec cfold_stm stm info = 
  { stm with TAst.stm = 
      match stm.TAst.stm with
	| TAst.Exp e ->
	  let e' = cfold_exp e info in TAst.Exp e'
	| TAst.IfThen (e,s) ->
	  let e' = cfold_exp e info in 
	  let s' = cfold_stm s info in TAst.IfThen (e',s')
	| TAst.IfThenElse (e,s1,s2) ->
	  let e' = cfold_exp e info in 
	  let s1' = cfold_stm s1 info in 
	  let s2' = cfold_stm s2 info in TAst.IfThenElse (e',s1',s2')
	| TAst.While (e,s) ->
	  let e' = cfold_exp e info in 
	  let s' = cfold_stm s info in TAst.While (e',s')
	| TAst.Empty -> TAst.Empty
	| TAst.Block b ->
	  let b' = cfold_block b info in TAst.Block b'
	| TAst.VoidReturn -> TAst.VoidReturn
	| TAst.ValueReturn e ->
	  let e' = cfold_exp e info in TAst.ValueReturn e'
	| TAst.LocalDecl (t,id,e) ->
          (match e with
            | None ->
              TAst.LocalDecl (t,id,e)
            | Some e ->
              let e' = cfold_exp e info in 
              TAst.LocalDecl (t,id,Some e'))
	| TAst.Throw e ->
	  let e' = cfold_exp e info in TAst.Throw e'
	| TAst.SuperCall (es,base,c) ->
	  let es' = cfold_exp_list es info in TAst.SuperCall (es',base,c)
	| TAst.ThisCall (es,base,c) ->
	  let es' = cfold_exp_list es info in TAst.ThisCall (es',base,c)
  }

and cfold_block stms info = List.map (fun stm -> cfold_stm stm info) stms

let cfold_body body info = cfold_block body info

let cfold_body_opt body info = match body with
  | None -> None
  | Some body -> Some (cfold_body body info)

let cfold_field_decl fdecl info = 
  { fdecl with TAst.field_init = cfold_exp_opt fdecl.TAst.field_init info}

let cfold_method_decl mdecl info = 
  { mdecl with TAst.method_body = cfold_body_opt mdecl.TAst.method_body info}

let cfold_constructor_decl cdecl info = 
  { cdecl with TAst.constructor_body = cfold_body cdecl.TAst.constructor_body info}

let cfold_decl decl info = 
  { decl with
    TAst.decl = match decl.TAst.decl with
      | TAst.Field fdecl -> 
	let fdecl' = cfold_field_decl fdecl info in
	TAst.Field fdecl'
      | TAst.Method mdecl -> 
	let mdecl' = cfold_method_decl mdecl info in
	TAst.Method mdecl'
      | TAst.Constructor cdecl -> 
	let cdecl' = cfold_constructor_decl cdecl info in
	TAst.Constructor cdecl'
  }

let cfold_decls decls info = List.map (fun decl -> cfold_decl decl info) decls

let cfold_class_decl cdecl info =
  { cdecl with
    TAst.class_members = cfold_decls cdecl.TAst.class_members info }

let cfold_interface_decl idecl info = 
  { idecl with
    TAst.interface_members = cfold_decls idecl.TAst.interface_members info }

let cfold_type_decl tdecl info =
  { tdecl with TAst.type_decl = match tdecl.TAst.type_decl with
    | TAst.Class cdecl -> TAst.Class (cfold_class_decl cdecl info)
    | TAst.Interface idecl -> TAst.Interface (cfold_interface_decl idecl info) 
  }

let cfold_source_file src_file info =
  { src_file with
    TAst.source_file_decl = cfold_type_decl src_file.TAst.source_file_decl info }

(* ********************************************************************** *)
(* CYCLE STATIC FINAL FIELD FOLDING TRAVERSAL                             *)
(* ********************************************************************** *)

let is_constant e = match e.TAst.exp with
  | (TAst.IntConst _)
  | (TAst.StringConst _)
  | (TAst.BooleanConst _)
(*| Some (Ast.CharConst _) *) -> true
  | _ -> false

let cycle_field_decl fdecl info ftdecl = 
  if fdecl.TAst.field_static && fdecl.TAst.field_final
  then
    match fdecl.TAst.field_init with
      | Some e ->
	if is_constant e
	then match ftdecl.Types.field_constval with 
          | Some _ ->
            fdecl, info, ftdecl
          | None ->
            fdecl,
            { info with changed = true },
	    { ftdecl with Types.field_constval = match e.TAst.exp with
	      | TAst.IntConst i -> Some (Types.Int i)
	      | TAst.StringConst s -> Some (Types.String s)
	      | TAst.BooleanConst b -> Some (Types.Bool b)
	      | _ -> assert(false) }
	else 
	  let e' = cfold_exp e info in
	  if is_constant e' (* e was folded: changed from non-constant to constant *)
	  then 
	    { fdecl with TAst.field_init = Some e' }, 
	    { info with changed = true },
	    { ftdecl with Types.field_constval = match e'.TAst.exp with
	      | TAst.IntConst i -> Some (Types.Int i)
	      | TAst.StringConst s -> Some (Types.String s)
	      | TAst.BooleanConst b -> Some (Types.Bool b)
	      | _ -> assert(false)
	    }
	  else
	    { fdecl with TAst.field_init = Some e' }, info, ftdecl
      | None -> fdecl, info, ftdecl
  else
    fdecl, info, ftdecl
      
let cycle_decl decl info tdecl = 
  match decl.TAst.decl, tdecl with
    | TAst.Field fdecl, Types.Field ftdecl -> 
      let fdecl', info, ftdecl' = cycle_field_decl fdecl info ftdecl in
      { decl with TAst.decl = TAst.Field fdecl' }, info, Types.Field ftdecl'
    | _,_ -> (Error.internal_compiler_error "Cycling non-field decl")

let cycle_decls decls info tmembers =
  let field_decls,rest_decls = 
    List.partition (fun e -> match e.TAst.decl with 
                                 | TAst.Field _ -> true
				 | _ -> false) decls in
  let tfield_mems,trest_mems = 
    List.partition (fun e -> match e with 
                                 | Types.Field _ -> true
				 | _ -> false) tmembers in

  let rec visit_decls decls info tdecls = match decls with
    | [] -> [],info,[]
    | (fdecl::fdecls) -> (match fdecl.TAst.decl with
	| TAst.Field field -> 
	  let ftdecl = try List.find (fun f -> match f with
	    | Types.Field f -> f.Types.field_name = field.TAst.field_name.Ast.identifier
	    | _ -> false) tdecls
	    with Not_found ->
	      (Error.internal_compiler_error "Field decl without matching decl in type")
	  in
	  let fdecl,info,ftdecl = cycle_decl fdecl info ftdecl in
	  let fdecls',info,ftdecls' = visit_decls fdecls info tdecls in
	  fdecl::fdecls',info,ftdecl::ftdecls'
	| _ ->
	  (Error.internal_compiler_error "Non field in field list")) in

  let field_decls',info,tfield_mems' = visit_decls field_decls info tfield_mems in
  let decls' = field_decls'@rest_decls in
  let tmembers' = tfield_mems'@trest_mems in
  decls',info,tmembers'

let cycle_class_decl cdecl info cname =
  let ntype = 
    try Types.Env.find cname info.tenv
    with Not_found -> 
      (Error.internal_compiler_error "Class decl without Class type")
  in
  match ntype with
    | Types.Interface _ ->
      (Error.internal_compiler_error "Class decl with Interface type")
    | Types.Class ct -> 
      let tmembers = ct.Types.class_members in
      let decls,info',tmembers' = 
	cycle_decls cdecl.TAst.class_members info tmembers in
      let ntype' = Types.Class { ct with Types.class_members = tmembers' } in
      let info'' = { info' with tenv = Types.Env.add cname ntype' info'.tenv } in
      { cdecl with TAst.class_members = decls }, info''

let cycle_interface_decl idecl info cname = 
  let ntype =
    try Types.Env.find cname info.tenv
    with Not_found ->
      (Error.internal_compiler_error "Interface decl without Interface type")
  in
  match ntype with
    | Types.Class _ ->
      (Error.internal_compiler_error "Interface decl with Class type")
    | Types.Interface it ->
      let tmembers = it.Types.interface_members in
      let decls,info',tmembers' = 
	cycle_decls idecl.TAst.interface_members info tmembers in
      let ntype' = Types.Interface { it with Types.interface_members = tmembers' } in
      let info'' = { info' with tenv = Types.Env.add cname ntype' info'.tenv } in
      { idecl with TAst.interface_members = decls }, info''

let cycle_type_decl tdecl info =
  let cname = tdecl.TAst.type_canonical_name in
  match tdecl.TAst.type_decl with
    | TAst.Class cdecl -> 
      let cdecl',info' = cycle_class_decl cdecl info cname in
      { tdecl with TAst.type_decl = TAst.Class cdecl' }, info'
    | TAst.Interface idecl ->
      let idecl',info' = cycle_interface_decl idecl info cname in
        { tdecl with TAst.type_decl = TAst.Interface idecl' }, info'

let cycle_source_file src_file info =
  let type_decl', info' = cycle_type_decl src_file.TAst.source_file_decl info in
  { src_file with TAst.source_file_decl = type_decl' }, info'

let rec cycle_program prog info = match prog with
  | [] -> [],info
  | src_file::src_files -> 
    let src_file',info' = cycle_source_file src_file info in
    let src_files',info'' = cycle_program src_files info' in
    (src_file'::src_files',info'')

let rec cfold_program prog tenv = 
  let prog',info' = cycle_program prog {tenv = tenv; changed = false} in
  if info'.changed
  then 
    begin
      print_string ("  static final fields changed, iterating");
      print_newline ();
      flush stdout; 
      cfold_program prog' info'.tenv
    end
  else (* No change - preprocessing static-final fields complete *)
    List.map (fun src_file -> cfold_source_file src_file info') prog'
