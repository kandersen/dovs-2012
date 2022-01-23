(** Compiler phase to resolve the uses of all types to their canonical name. *)

(* Locally let-bind module names to shorter ones *)
module WAst = Weedingast
module NAst = Nameresolvingast
module CName = CanonicalName
module ListPlus = Utils.ListPlus
module Option = Utils.OptionPlus

(*************************************************************************)
(** {2 Utilities      }                                                  *)
(*************************************************************************)

type info = { package_prefix : string;
	      type_name : string;
	      named_types : Types.canonical_name list;
	      single_imports : Types.canonical_name StringMap.t;
	      ondemand_imports : Ast.name list }

let is_qualified_cname (cname : CName.t) : bool =
  "" <> CName.package cname

let named_types_not_in_default (info : info) : Types.canonical_name list =
  List.filter is_qualified_cname info.named_types

let verify (b : bool) (thunk : unit -> unit) : unit =
  if b
  then ()
  else thunk ()

let reject (b : bool) (thunk : unit -> unit) : unit =
  if b
  then thunk ()
  else ()

let rec canonical_name_prefixes (cname : CName.t) : string list = 
  let pkg = CName.package cname in
  if pkg = ""
  then []
  else pkg :: (canonical_name_prefixes (CName.make pkg))

let name_prefixes (name : Ast.name) : Ast.name list =
  let pos = name.Ast.name_pos in
  let make_name ids = 
    match ids with
      | [] -> []
      | [n] -> [{ Ast.name_pos = pos; Ast.name = Ast.Simple n }]
      | _ -> [{ Ast.name_pos = pos; Ast.name = Ast.Qualified ids}] in
  match name.Ast.name with
    | Ast.Simple _ -> [name]
    | Ast.Qualified ids -> 
      let pres = ListPlus.prefixes ids in
      let result = List.concat (List.map make_name pres) in
      result


(*************************************************************************)
(** {2 Error Messages }                                                  *)
(*************************************************************************)

(** Reports the error that a simple name type doesn't resolve uniquely to a
    type.
     @param is  the identifier holding the name of the ambiguous class
*)
let error_ambiguous_class_name id =
  Error.error Error.AMBIGUOUS_CLASS_NAME id.Ast.identifier_pos 
    ("Ambiguous class name " ^ id.Ast.identifier)

(** Reports the error that a non-existing package is refered in an
    import-on-demand declaration.
     @param name  the [name] of the import-on-demand declaration
     @param pos   the position at which the error occured
*)
let error_non_existing_package name pos =
  Error.error Error.NON_EXISTING_PACKAGE pos 
    ("Package " ^ (Ast.name_to_string name) ^ " does not exist")

(** Reports the error that a prefix of a package name clashes with the name
    of a type.
     @param name    the package [name]
     @param prefix  the clashing prefix
     @param pos     the position at which the error occured
*)
let error_package_clash_with_type name prefix pos =
  Error.error Error.PACKAGE_CLASH_WITH_TYPE pos
    ("Prefix " ^ prefix ^ " of package "
     ^ name ^ " clashes with type of the same name.")

(** Reports the error that a prefix of a type name resolves to a type.
     @param name    the full type [name]
     @param prefix  the prefix which resolves to a type
*)
let error_prefix_resolves_to_type name prefix =
  Error.error Error.PREFIX_RESOLVES_TO_TYPE name.Ast.name_pos
    ("Prefix " ^ prefix
     ^ " of type name " ^ (Ast.name_to_string name)
     ^ " is itself a valid type")

(** Reports the error that the name of declared class (or interface) clashes
    with a single-type-import declaration.
     @param import_name  the name of the imported type
     @param pos          the position at which the error occured
*)
let error_single_type_import_clash_with_class import_name pos =
  Error.error Error.SINGLE_TYPE_IMPORT_CLASH_WITH_CLASS pos
    ("Name clash with imported type " ^ import_name)

(** Reports the error that two single-type-imports clash.
     @param strname            the name of one of the single-type-import declarations
     @param other_import_name  the name of the other imported type
     @param pos                the position at which the error occured
*)
let error_two_single_type_imports_clash strname other_import_name pos =
  let (name, id) = strname in
  let (oname, oid) = other_import_name in
  let this = (Ast.name_to_string name) ^ "." ^ id.Ast.identifier in
  let other = (Ast.name_to_string oname) ^ "." ^ oid.Ast.identifier in
  Error.error Error.TWO_SINGLE_TYPE_IMPORTS_CLASH pos
    ("Name clash between " ^ this
     ^ " and " ^ other)

(** Reports the error that a named type doesn't resolve to a type defined in
    the program or in the class library.
     @param strname  the named type
     @param pos      the position at which the error occured
*)
let error_unresolved_type strname pos =
  Error.error Error.UNRESOLVED_TYPE pos
    ("Could not find type " ^ strname)

(*************************************************************************)
(** {2 Helper Functions and Values }                                     *)
(*************************************************************************)

(** Dummy name for java.lang (useful for package prefix checking) *)
let java_lang =
  { Ast.name_pos = Lexing.dummy_pos;
    Ast.name = Ast.Qualified [{ Ast.identifier_pos = Lexing.dummy_pos;
                                Ast.identifier = "java" };
                              { Ast.identifier_pos = Lexing.dummy_pos;
                                Ast.identifier = "lang" }] }


let package_exists (n : string) (packages : Ast.name list) : bool = 
  let packages = List.concat (List.map (fun n -> List.map Ast.name_to_string (name_prefixes n)) packages) in
  begin
    (List.exists ((=) n) packages) || (Classenvironment.package_exists n)
  end

(** Convert a package type to its package prefix string *)
let package_to_prefix pkg = match pkg with
  | None -> ""
  | Some name -> Ast.name_to_string name ^ "."

(** Look up a class or interface by searching first the program (the
    named_types) and then the classpath.

    @param name  the fully qualified [name] of the class or interface to look up.
    @return      [Some CanonicalName.t] if found or [None] if [name] could not be resolved.
*)
let lookup_canonical_name name named_types =
  let cname = CanonicalName.make name in
  if List.mem cname named_types
  then Some cname
  else match Classenvironment.lookup_named_type cname with
    | None -> None
    | Some _ -> Some cname

let check_prefixes_do_not_resolve (fullname : string) (name : Ast.name) (info : info) : unit =
  let prefixes = name_prefixes name in
  List.iter (fun n -> 
    let strname = Ast.name_to_string n in
    verify (Option.is_none (lookup_canonical_name strname (named_types_not_in_default info)))
      (fun () -> error_package_clash_with_type fullname strname n.Ast.name_pos)) prefixes
    
(** Find the canonical name of the named type denoted by the given identifier
    according to the rules in Section 6.5.5.1 of the JLS.
    
  @param id               the [identifier] to look up. 
  @param package_prefix   the package prefix of the current type declaration
  @param type_name        the type name of the current type declaration
  @param named_types      the named types of the program
  @param single_imports   the single imports of the current type declaration
  @param ondemand_imports the on-demand imports of the current type declaration
  @return                 [Some cname] for the corresponding type declaration, or
                          [None] if the identifier could not be resolved as a type.
*)
let rec resolve_name_from_identifier
    (id : Ast.identifier)
    (package_prefix : string)
    (type_name : string)
    (named_types : Types.canonical_name list)
    (single_imports : Types.canonical_name StringMap.t)
    (ondemand_imports : Ast.name list) : Types.canonical_name option =
  let id_string = id.Ast.identifier in
  (* 1. Try the enclosing class or interface *)
  if type_name = id_string 
  then Some (CName.make (package_prefix ^ type_name))
  else
    
    (* 2. Try any single type import *)
    if StringMap.mem id_string single_imports 
    then Some (StringMap.find id_string single_imports)
    else 
    
      (* 3. Try the same package *)
      let fully_qualified_string = package_prefix ^ id_string in
      let possible_cname = CName.make fully_qualified_string in
      if Option.is_some (lookup_canonical_name fully_qualified_string named_types)
      then Some possible_cname
      else 
	
	(* 4. Try any import-on-demand package *)
	let make_candidate n = CName.make (Ast.name_to_string n ^ "." ^ id_string) in
	let candidates = List.map make_candidate (java_lang :: ondemand_imports) in
	let lookup_result fulltype = (lookup_canonical_name (CName.to_string fulltype) named_types, fulltype) in
	let lookup_results = ListPlus.zip (List.map lookup_result candidates) (java_lang :: ondemand_imports) in
	let is_result ((lres, _), _) = Option.is_some lres in
	if List.exists is_result lookup_results
	then
	  if ListPlus.exactly_one is_result lookup_results
	  then let ((_, cname), _) = List.find is_result lookup_results in
	       Some cname
	  else error_ambiguous_class_name id
	else None
	  
(** Find the canonical name of named type *)
let resolve_name nt package_prefix type_name named_types single_imports ondemand_imports = 
  match nt.Ast.name with
    | Ast.Qualified _ ->
      lookup_canonical_name (Ast.name_to_string nt) named_types
    | Ast.Simple id ->
      resolve_name_from_identifier id
        package_prefix type_name named_types single_imports ondemand_imports
    
let resolve_name' (info : info) (nt : WAst.namedtype) : CName.t option =   
  let package_prefix = info.package_prefix in
  let type_name = info.type_name in
  let named_types = info.named_types in
  let single_imports = info.single_imports in 
  let ondemand_imports = info.ondemand_imports in
  resolve_name nt package_prefix type_name named_types single_imports ondemand_imports

(*************************************************************************)
(** {2 Initial checks                                                    *)
(*************************************************************************)
      
let check_singles_for_duplicates (singles : (WAst.name * WAst.identifier * Lexing.position) list) : unit =
  let rec check_singles_for_duplicates singles seen =
    match singles with
      | [] -> ()
      | (name, id, pos) :: singles' ->
	let clashes (oname, oid) =
	  let oid = oid.Ast.identifier in
	  let id = id.Ast.identifier in
	  let oname = Ast.name_to_string oname in
	  let name = Ast.name_to_string name in
	  oid = id && oname <> name in
	if List.exists clashes seen
	then let (oname, oid) = List.find clashes seen in
	     error_two_single_type_imports_clash (name, id) (oname, oid) pos
	else check_singles_for_duplicates singles' ((name, id) :: seen)	in
  check_singles_for_duplicates singles []

let check_single_not_clash_with_enclosing (enclosing : string)
                                          (pkg_prefix : string)
					  (single : WAst.name * WAst.identifier * Lexing.position) =
  let (imp_pkg, id, pos) = single in
  let name = id.Ast.identifier in
  let fully_qualified_import = (Ast.name_to_string imp_pkg ^ "." ^ name) in
  let fully_qualified_class = pkg_prefix ^ enclosing in
  reject (name = enclosing && not (fully_qualified_class = fully_qualified_import))
    (fun () -> error_single_type_import_clash_with_class name pos)

let check_demands_exist (packages : Ast.name list) (demand : WAst.name * Lexing.position) : unit =
  let name = fst demand in
  let pos  = snd demand in 
  let package = Ast.name_to_string name in
  verify (package_exists package packages)
    (fun () -> error_non_existing_package name pos)

let check_single_exists (named_types : CName.t list) (name, id, pos : Ast.name * Ast.identifier * Lexing.position) : unit =
  let strname = (Ast.name_to_string name) ^ "." ^ id.Ast.identifier in
  verify (Option.is_some (lookup_canonical_name strname named_types))
    (fun () -> error_unresolved_type strname pos)

let check_prefixes_do_not_resolve (fullname : string) (name : Ast.name) (info : info) : unit =
  let prefixes = name_prefixes name in
  List.iter (fun n -> 
    verify (Option.is_none (resolve_name' info n ))
      (fun () -> error_prefix_resolves_to_type n fullname)) prefixes

let check_prefixes_do_not_resolve_package (fullname : string) (name : Ast.name) (info : info) : unit =
  let prefixes = name_prefixes name in
  List.iter (fun n -> 
    let strname = Ast.name_to_string n in
    verify (Option.is_none (lookup_canonical_name strname (named_types_not_in_default info)))
      (fun () -> error_package_clash_with_type fullname strname name.Ast.name_pos)) prefixes

let check_package_and_prefixes_do_not_resolve (package : Ast.name option) (info : info) : unit =
  Option.map_default (fun n -> check_prefixes_do_not_resolve_package (Ast.name_to_string n) n info) () package

let check_single_prefixes (info : info) (name, id, pos : Ast.name * Ast.identifier * Lexing.position) : unit =
  let fullname = (Ast.name_to_string name) ^ id.Ast.identifier in
  check_prefixes_do_not_resolve_package fullname name info
      
(*************************************************************************)
(** {2 Transform }                                                       *)
(*************************************************************************)

let check_qualified_name_prefixes_and_resolve (nt : Ast.name) (info : info) : CName.t option =
    match nt.Ast.name with
      | Ast.Simple _ -> resolve_name' info nt
      | Ast.Qualified ids ->
	let prefix_ids = ListPlus.all_but_last ids in
	let prefix_name =
	  match prefix_ids with
	    | [x] -> Ast.Simple x
	    | _ -> Ast.Qualified prefix_ids in
	let prefix = { nt with Ast.name = prefix_name } in
	begin
	  check_prefixes_do_not_resolve (Ast.name_to_string nt) prefix info;
	  resolve_name' info nt
	end 

let check_resolved_cnames_prefixes (nt : Ast.name) (n : CName.t) (info : info) : unit =
  let strname = Ast.name_to_string nt in
  let strcname = CName.to_string n in
  let pos = nt.Ast.name_pos in
  let prefixes = canonical_name_prefixes n in
  List.iter (fun p ->
    let res = lookup_canonical_name p (named_types_not_in_default info) in
    match res with
      | None ->
	()
      | Some n ->
	error_package_clash_with_type strname strcname pos) prefixes
  

let transform_name (info : info) (nt : Ast.name) : CName.t =
  let strname = Ast.name_to_string nt in
  let pos = nt.Ast.name_pos in
  let res = check_qualified_name_prefixes_and_resolve nt info in 
  match res with
    | None ->
      error_unresolved_type strname pos
    | Some n -> 
      begin
	check_resolved_cnames_prefixes nt n info;
	n
      end
	

let rec transform_typeexp (info : info) (t : WAst.typeexp) : NAst.typeexp =
  match t.Ast.typeexp with
    | Ast.Base b -> 
      Types.Base b
    | Ast.TArray t ->
      Types.Array (transform_typeexp info t)
    | Ast.Named n ->
      Types.Named (transform_name info n)

let rec transform_expression  (info : info) (e : WAst.exp) : NAst.exp =
  let pos = e.WAst.exp_pos in
  let exp' = 
    match e.WAst.exp with
      | WAst.Binop (l, op, r) ->
	let l' = transform_expression info l in
	let r' = transform_expression info r in
	NAst.Binop (l', op, r')
      | WAst.Unop (op, e) ->
	let e' = transform_expression info e in
	NAst.Unop (op, e')
      | WAst.IntConst i ->
	NAst.IntConst i
      | WAst.CharConst c ->
	NAst.CharConst c 
      | WAst.BooleanConst t ->
	NAst.BooleanConst t
      | WAst.StringConst s ->
	NAst.StringConst s
      | WAst.Null ->
	NAst.Null
      | WAst.This ->
	NAst.This
      | WAst.StaticInvoke (n, id, exps) ->
	let n' = transform_name info n in
	let exps' = List.map (transform_expression info) exps in
	NAst.StaticInvoke (n', id, exps')
      | WAst.NonstaticInvoke (e, id, exps) ->
	let e' = transform_expression info e in
	let exps' = List.map (transform_expression info) exps in
	NAst.NonstaticInvoke (e', id, exps')
      | WAst.SimpleInvoke (n, exps) ->
	let exps' = List.map (transform_expression info) exps in
	NAst.SimpleInvoke (n, exps')
      | WAst.AmbiguousInvoke (n, id, exps) ->
	let exps' = List.map (transform_expression info) exps in
	NAst.AmbiguousInvoke (n, id, exps')
      | WAst.New (t, exps) ->
	let t' = transform_typeexp info t in
	let exps' = List.map (transform_expression info) exps in
	NAst.New (t', exps')
      | WAst.NewArray (t, exps) ->
	let t' = transform_typeexp info t in
	let exps' = List.map (transform_expression info) exps in
	NAst.NewArray (t', exps')
      | WAst.Lvalue l ->
	let l' = transform_lvalue l info in
	NAst.Lvalue l'
      | WAst.Assignment (lhs, rhs) ->
	let lhs' = transform_lvalue lhs info in
	let rhs' = transform_expression info rhs in
	NAst.Assignment (lhs', rhs')
      | WAst.IncDec (l, op) ->
	let l' = transform_lvalue l info in
	NAst.IncDec (l', op)
      | WAst.Cast (t, e) ->
	let t' = transform_typeexp info t in
	let e' = transform_expression info e in
	NAst.Cast (t', e')
      | WAst.Instanceof (e, t) ->
	let e' = transform_expression info e in
	let t' = transform_typeexp info t in
	NAst.Instanceof (e', t') in
  { NAst.exp_pos = pos;
    NAst.exp = exp' }

and transform_lvalue (l : WAst.lvalue) (info : info) : NAst.lvalue =
  let pos = l.WAst.lvalue_pos in
  let lvalue' = match l.WAst.lvalue with
    | WAst.NonstaticField (e, id) ->
      let e' = transform_expression info e in
      NAst.NonstaticField (e', id)
    | WAst.Array (e1, e2) ->
      let e1' = transform_expression info e1 in
      let e2' = transform_expression info e2 in
      NAst.Array (e1', e2')
    | WAst.AmbiguousName n ->
      NAst.AmbiguousName n in
  { NAst.lvalue_pos = pos;
    NAst.lvalue = lvalue' }

let transform_field_decl (f : WAst.field_decl) (info : info) (pos : Lexing.position) : NAst.field_decl =
  let access = f.WAst.field_access in
  let static = f.WAst.field_static in
  let final = f.WAst.field_final in
  let field_type = f.WAst.field_type in
  let name = f.WAst.field_name in
  let init = f.WAst.field_init in
  { NAst.field_access = access;
    NAst.field_static = static;
    NAst.field_final = final;
    NAst.field_type = transform_typeexp info field_type;
    NAst.field_name = name;
    NAst.field_init = Option.map (transform_expression info) init }

let rec transform_stm (info : info) (s : WAst.stm) : NAst.stm =
  let pos = s.WAst.stm_pos in
  let stm' = match s.WAst.stm with
    | WAst.Exp e ->
      let e' = transform_expression info e in
      NAst.Exp e'
    | WAst.IfThen (e, s) ->
      let e' = transform_expression info e in
      let s' = transform_stm info s in
      NAst.IfThen (e', s')
    | WAst.IfThenElse (e, s1, s2) ->
      let e' = transform_expression info e in
      let s1' = transform_stm info s1 in
      let s2' = transform_stm info s2 in      
      NAst.IfThenElse (e', s1', s2')
    | WAst.While (e, s) ->
      let e' = transform_expression info e in
      let s' = transform_stm info s in
      NAst.While (e', s')
    | WAst.Empty ->
      NAst.Empty
    | WAst.Block b ->
      let b' = transform_block info b in
      NAst.Block b'
    | WAst.VoidReturn ->
      NAst.VoidReturn
    | WAst.ValueReturn e ->
      let e' = transform_expression info e in
      NAst.ValueReturn e'
    | WAst.LocalDecl (t, id, init) ->
      let t' = transform_typeexp info t in
      let init' = Option.map (transform_expression info) init in
      NAst.LocalDecl (t', id, init')
    | WAst.Throw e ->
      let e' = transform_expression info e in
      NAst.Throw e'
    | WAst.SuperCall exps ->
      let exps' = List.map (transform_expression info) exps in
      NAst.SuperCall exps'
    | WAst.ThisCall exps ->
      let exps' = List.map (transform_expression info) exps in
      NAst.ThisCall exps' in
  { NAst.stm_pos = pos;
    NAst.stm = stm' }

and transform_block (info : info) (b : WAst.block) : NAst.block =
  List.map (transform_stm info) b

let transform_formal (info : info) (f : WAst.formal_param) : NAst.formal_param = 
  let (t, id) = f in
  let t' = transform_typeexp info t in
  (t', id)

let transform_method_decl (m : WAst.method_decl) (info : info) (pos : Lexing.position) : NAst.method_decl =
  let access = m.WAst.method_access in
  let static = m.WAst.method_static in
  let final = m.WAst.method_final in
  let abstract = m.WAst.method_abstract in
  let result = m.WAst.method_result in
  let name = m.WAst.method_name in
  let formals = m.WAst.method_formals in
  let throws = m.WAst.method_throws in
  let body = m.WAst.method_body in
  { NAst.method_access = access;
    NAst.method_static = static;
    NAst.method_final = final;
    NAst.method_abstract = abstract;
    NAst.method_result = transform_typeexp info result;
    NAst.method_name = name;
    NAst.method_formals = List.map (transform_formal info) formals;
    NAst.method_throws = List.map (transform_name info) throws;
    NAst.method_body = Option.map (transform_block info) body }

let transform_constructor_decl (c : WAst.constructor_decl) (info : info) (pos : Lexing.position) : NAst.constructor_decl =
  let access = c.WAst.constructor_access in
  let name = c.WAst.constructor_name in
  let formals = c.WAst.constructor_formals in
  let throws = c.WAst.constructor_throws in
  let body = c.WAst.constructor_body in
  { NAst.constructor_access = access;
    NAst.constructor_name = name;
    NAst.constructor_formals = List.map (transform_formal info) formals;
    NAst.constructor_throws = List.map (transform_name info) throws;
    NAst.constructor_body = transform_block info body }
    
let transform_member (info : info) (mdec : WAst.decl) : NAst.decl =
  let pos = mdec.WAst.decl_pos in
  let decl' =
    match mdec.WAst.decl with
      | WAst.Field f ->
	NAst.Field (transform_field_decl f info pos)
      | WAst.Method m ->
	NAst.Method (transform_method_decl m info pos)
      | WAst.Constructor c ->
	NAst.Constructor (transform_constructor_decl c info pos) in
  { NAst.decl_pos = pos;
    NAst.decl = decl' }
	    
let transform_class (cd : WAst.class_decl) (info : info) : NAst.class_decl = 
  let final = cd.WAst.class_final in
  let abstract = cd.WAst.class_abstract in
  let name = cd.WAst.class_name in
  let extends = cd.WAst.class_extends in
  let implements = cd.WAst.class_implements in
  let members = cd.WAst.class_members in
  { NAst.class_final = final;
    NAst.class_abstract = abstract;
    NAst.class_name = name;
    NAst.class_extends = transform_name info extends;
    NAst.class_implements = List.map (transform_name info) implements;
    NAst.class_members = List.map (transform_member info) members }

let transform_interface (i : WAst.interface_decl) (info : info) : NAst.interface_decl =
  let name = i.WAst.interface_name in
  let extends = i.WAst.interface_extends in
  let members = i.WAst.interface_members in
  { NAst.interface_name = name;
    NAst.interface_extends = List.map (transform_name info) extends;
    NAst.interface_members = List.map (transform_member info) members }
  
let transform_type_decl_desc (type_desc : WAst.type_decl_desc) (info : info) : NAst.type_decl_desc = 
  match type_desc with 
    | WAst.Class c ->
      NAst.Class (transform_class c info)
    | WAst.Interface i ->
      NAst.Interface (transform_interface i info)

let rec split_imports (imps : WAst.import_decl list) : 
    (WAst.name * Lexing.position) list * (WAst.name * WAst.identifier * Lexing.position) list =
  match imps with
    | [] -> 
      ([],[])
    | imp :: imps' ->
      let (demands, singles) = split_imports imps' in
      let pos = imp.Ast.import_decl_pos in
      match imp.Ast.import_decl with
	| Ast.OnDemand n -> 
	  if List.exists (fun (m,_) -> Ast.name_to_string n = Ast.name_to_string m) demands
	  then (demands, singles)
	  else ((n, pos) :: demands, singles)
	| Ast.Single(n, id) ->
	  (demands, (n, id, pos) :: singles)
	    
let transform_source_file_imports (singles : (Ast.name * Ast.identifier * Lexing.position) list)
                                  (demands : (Ast.name * Lexing.position) list) : (Types.canonical_name StringMap.t * Ast.name list) =
  let singles' =
    List.fold_left
      (fun map (n, id, _) ->
	let id_name = id.Ast.identifier in
	let can_name = CName.make ((Ast.name_to_string n) ^ "." ^ id_name) in
	StringMap.add id_name can_name map)
      StringMap.empty
      singles in
  let demands' = List.filter (fun n -> "java.lang" <> Ast.name_to_string n) (List.map fst demands) in
  (singles', demands')
     
let transform_source_file_decl (sfd : WAst.type_decl) (info : info) : NAst.type_decl =
  let pos = sfd.WAst.type_decl_pos in
  let type_decl_desc = sfd.WAst.type_decl in
  let type_decl_desc' = transform_type_decl_desc type_decl_desc info in
  let cname = CName.make (info.package_prefix ^ info.type_name) in
  { NAst.type_decl_pos = pos;
    NAst.type_decl = type_decl_desc';
    NAst.type_canonical_name = cname }

let transform_source_file (named_types : Types.canonical_name list) (packages : Ast.name list) (sf : WAst.source_file) : NAst.source_file =
  let file_name = sf.WAst.source_file_name in
  let package = sf.WAst.source_file_package in
  let package_prefix = package_to_prefix package in
  let imports = sf.WAst.source_file_imports in
  let decl = sf.WAst.source_file_decl in
  let type_name = WAst.type_decl_name decl in
  let (demands, singles) = split_imports imports in
  let (single_imports, ondemand_imports) = transform_source_file_imports singles demands in
  let info = { package_prefix = package_prefix;
	       type_name = type_name;
	       named_types = named_types;
	       single_imports = single_imports;
	       ondemand_imports = ondemand_imports } in
  begin
    List.iter (check_demands_exist packages) demands;
    List.iter (check_single_not_clash_with_enclosing type_name package_prefix) singles;
    check_singles_for_duplicates singles;
    List.iter (check_single_exists named_types) singles;
    List.iter (check_single_prefixes info) singles;
    check_package_and_prefixes_do_not_resolve package info;
    check_package_and_prefixes_do_not_resolve (Some java_lang) info;
    let decl' = transform_source_file_decl decl info in
    { NAst.source_file_name = file_name;
      NAst.source_file_package = package;
      NAst.source_file_single_imports = single_imports;
      NAst.source_file_ondemand_imports = ondemand_imports;
      NAst.source_file_decl = decl'}
  end
      
let transform_program (sfs : WAst.program) (named_types : Types.canonical_name list) (packages : Ast.name list) : NAst.program =
  List.map (transform_source_file named_types packages) sfs

(*************************************************************************)
(** {2 Name Resolving Traversal }                                        *)
(*************************************************************************)

let extract_types (sfs : WAst.program) : Types.canonical_name list =
  let extract_type (sf : WAst.source_file) : Types.canonical_name =
    let type_name = WAst.type_decl_name sf.WAst.source_file_decl in
    let prefix = package_to_prefix sf.WAst.source_file_package in
    CName.make (prefix ^ type_name) in
  List.map extract_type sfs

let extract_packages (sfs : WAst.program) : Ast.name list =
  let extract_package (sf : WAst.source_file) : Ast.name list =
    Option.map_default (fun n -> [n]) [] sf.WAst.source_file_package in
  ListPlus.concat_map extract_package sfs
      
(** Transform a weeded AST [WAst.program] into a name resolved AST [NAst.program] *)
let nres_program (prog : WAst.program) : NAst.program =
  let named_types = extract_types prog in
  let packages = extract_packages prog in
  let transformed_ast = transform_program prog named_types packages in
  transformed_ast
    
