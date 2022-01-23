(** Compiler phase to resolve class hierarchy relationships, inheritance and
    hiding of fields and inheritance, overriding and hiding of methods according
    to chapters 8 and 9 of the JLS. *)

(* Locally let-bind module names to shorter ones *)
module NAst = Nameresolvingast
module T = Types

module CanonicalNameSet = Set.Make (CanonicalName)
module FieldSet = Set.Make (
  struct
    let compare = compare
    type t = (CanonicalName.t * Types.field_type)
  end)

module MethodSet = Set.Make (
  struct
    let compare = compare
    type t = (CanonicalName.t * Types.method_type)
  end)

(*************************************************************************)
(** {2 Helper Functions }                                                *)
(*************************************************************************)

(** Convert a typeexp to its string representation *)
let rec typeexp_to_string typeexp = match typeexp with
  | T.Base Ast.Void -> "void"
  | T.Base Ast.Byte -> "byte"
  | T.Base Ast.Short -> "short"
  | T.Base Ast.Int -> "int"
  | T.Base Ast.Long -> "long"
  | T.Base Ast.Char -> "char"
  | T.Base Ast.Float -> "float"
  | T.Base Ast.Double -> "double"
  | T.Base Ast.Boolean -> "boolean"
  | T.Array typeexp' -> typeexp_to_string typeexp' ^ "[]"
  | T.Named name -> CanonicalName.to_string name
  | T.Null ->
    Error.internal_compiler_error "found an unexpected null-type"

(** Convert a method specification to its string representation *)
let method_to_string m = 
  m.T.method_name ^
    "(" ^ (String.concat "," (List.map typeexp_to_string m.T.method_formals)) ^ ")"

let method_to_extended_string m = 
  (string_of_bool m.T.method_final) ^ m.T.method_name ^
    "(" ^ (String.concat "," (List.map typeexp_to_string m.T.method_formals)) ^ ")"

(** Equality on fields (name equality) *)
let field_signature_equal f1 f2 =
  f1.T.field_name = f2.T.field_name

(** Equality on methods (name and formal parameters equality) *)
let method_signature_equal m1 m2 =
  m1.T.method_name    = m2.T.method_name &&
  m1.T.method_formals = m2.T.method_formals

(** Equality on constructors (name and formal parameters equality) *)
let constructor_signature_equal c1 c2 =
  c1.T.constructor_name    = c2.T.constructor_name &&
  c1.T.constructor_formals = c2.T.constructor_formals

(** Equality on member declarations (for field or method members only) *)
let decl_signature_equal decl1 decl2 = match decl1, decl2 with
  | T.Field f1,  T.Field f2  -> field_signature_equal f1 f2
  | T.Method m1, T.Method m2 -> method_signature_equal m1 m2
  | _, _ -> false

(************************************************************************)
(** {2 Error Messages}                                                  *)
(************************************************************************)

(** Reports the error that a class or interface depends on itself, i.e.,
    circular inheritance.
*)
let error_circular_inheritance pos =
  Error.error Error.CIRCULAR_INHERITANCE pos "Circular inheritance"

(** Report the error that a class that has abstract methods must itself be
    abstract.
     @param classname    the class name
     @param methd        the method type of the abstract method
     @param pos          the position at which the error occured
*)
let error_class_must_be_abstract classname methd pos =
  Error.error Error.CLASS_MUST_BE_ABSTRACT pos
    ("The class " ^ classname
     ^ " must be declared abstract because of abstract method "
     ^ method_to_string methd)

(** Reports the error that a class must not contain two different methods
    with the same signature but different return types.
     @param replacing_method  the replacing method declaration
     @param replaced_method   the replaced method declaration
     @param pos               the position at which the error occured
*)
let error_different_return_type replacing_method replaced_method pos =
  let override = if not (replacing_method.T.method_static)
                 then "overridden" else "hidden" in
  let replace = if not (replacing_method.T.method_abstract
			&& replaced_method.T.method_abstract)
                then override else "other inherited" in
  Error.error Error.DIFFERENT_RETURN_TYPE pos
    ("The method " ^ method_to_string replacing_method
     ^ " must have same return type as "
     ^ replace
     ^ " method " ^ method_to_string replaced_method)

(** Reports the error that a class must not declare two constructors with the
    same signature.
*)
let error_duplicate_constructor (*node*) pos =
  Error.error Error.DUPLICATE_CONSTRUCTOR pos "Duplicate constructor"

(** Reports the error that a class must not declare two methods with the same
    signature.
     @param methd  one of the method declarations
     @param pos    the position at which the error occured
*)
let error_duplicate_method methd pos =
  Error.error Error.DUPLICATE_METHOD pos
    ("The method " ^ (method_to_string methd)
     ^ " is already declared")

(** Reports the error that a class cannot extend a final class.
     @param classname  the name of the extending class
     @param pos        the position at which the error occured
*)
let error_extends_final_class classname pos =
  Error.error Error.EXTENDS_FINAL_CLASS pos
    ("The class " ^ CanonicalName.to_string classname ^ " cannot extend a final class")

(** Reports the error that a class cannot extend a non-class
     @param ntype  the name of the non-class
*)
let error_extends_non_class ntype pos =
  Error.error Error.EXTENDS_NON_CLASS pos
    (CanonicalName.to_string ntype ^ " is not a class")

(** Reports the error that a replacing method cannot declare a checked
    exception in its throws clause which is not declared by the replaced
    method.
     @param method_decl  the method type
     @param excepname    the [canonical_name] of the exception
     @param pos          the position at which the error occured
*)
let error_illegal_throws_in_replace method_decl excepname pos = 
  let override = if not (method_decl.T.method_static)
                 then "overridden" else "hidden" in
  Error.error Error.ILLEGAL_THROWS_IN_REPLACE pos
    ("The method " ^ (method_to_string method_decl)
     ^ " declares checked exception " ^ CanonicalName.to_string excepname
     ^ " which is not declared by " ^ override ^ " method")

(** Reports the error that a class cannot implement a non-interface.
     @param ntype  the [canonical_name] of the non-interface
     @param pos    the position at which the error occured
*)
let error_implements_non_interface ntype pos =
  Error.error Error.IMPLEMENTS_NON_INTERFACE pos
    (CanonicalName.to_string ntype ^ " is not an interface")

(** Reports the error that two different inherited fields clash.
     @param field_name    the name of the field
     @param pos           the position at which the error occured
*)
let error_inherited_field_clash field_name pos =
  Error.error Error.INHERITED_FIELD_CLASH pos
    ("Multiply inherited field name " ^ field_name)

(** Reports the error that a nonstatic method cannot replace a static method.
     @param method_decl  the method type of the nonstatic method
     @param pos          the position at which the error occured
*)
let error_non_static_replace_static method_decl pos =
  Error.error Error.NONSTATIC_REPLACE_STATIC pos
    ("The nonstatic method " ^ (method_to_string method_decl)
     ^ " cannot replace a static method")

(** Reports the error that a method cannot replace a final method.
     @param method_decl  the method type of the replacing method
     @param pos          the position at which the error occured
*)
let error_replace_final method_decl pos =
  let override = if not (method_decl.T.method_static)
                 then "override" else "hide" in
  Error.error Error.REPLACE_FINAL pos 
    ("The method " ^ (method_to_string method_decl)
     ^ " cannot " ^ override ^ " a final method")

(** Reports the error that a protected method cannot replace a public method.
     @param method_decl  the method type of the protected method
     @param pos          the position at which the error occured
*)
let error_protected_replace_public method_decl pos = 
  let override = if not (method_decl.T.method_static)
                 then "overridden" else "hidden" in
  Error.error Error.PROTECTED_REPLACE_PUBLIC pos
    ("The method " ^ (method_to_string method_decl)
     ^ " cannot have stricter access privileges than " ^ override ^ " method")

(** Reports the error that the same interface is found twice in the
    implements clause of a class or in the extends clause of an interface.
     @param ntype  the [canonical_name] of the interface
     @param pos    the position at which the error occured
*)
let error_repeated_interface ntype pos =
  Error.error Error.REPEATED_INTERFACE pos
    ("Repeated interface " ^ CanonicalName.to_string ntype)

(** Reports the error that a static method cannot replace a nonstatic method.
     @param method_decl  the method type of the static method
     @param pos          the position at which the error occured
*)
let error_static_replace_non_static method_decl pos =
  Error.error Error.STATIC_REPLACE_NONSTATIC pos
    ("The static method " ^ (method_to_string method_decl)
     ^ " cannot hide a nonstatic method")

(************************************************************************)
(** {2 Environment Lookup}                                              *)
(************************************************************************)

(** Look up a class or interface with the given name by searching first the
    program and then the classpath.
    @param cname  the fully qualified name of the class or interface to look
                  for.
    @param tenv   the type environment.
    @return       [Types.named_type] if [cname] resolved, or
                  [Error.INTERNAL_COMPILER_ERROR] if there is no type of [cname].
*)
let lookup_named_type cname tenv =
  match Environment.lookup_named_type cname tenv with
    | Some t -> t
    | None -> (Error.internal_compiler_error
                       ("Unable to resolved type for canonical name: "
                        ^ CanonicalName.to_string cname))
  

(**************************************************************)
(* {2 Set methods }                                     *)
(**************************************************************)

let print_canonical_name_set canset = 
  print_string "{";
  CanonicalNameSet.iter (fun e -> print_string (CanonicalName.to_string e); print_string ";") canset;
  print_endline "}"

let print_canonical_name_list canlist = 
  print_string "{";
  List.iter (fun e -> print_string (CanonicalName.to_string e); print_string ";") canlist;
  print_endline "}"


let print_field_set fieldset = 
  print_string "{";
  FieldSet.iter (fun (can_name,fieldtype) -> print_string ("(" ^ (CanonicalName.to_string can_name) ^ "," ^ (fieldtype.Types.field_name) ^ ")"); print_string ";") fieldset;
  print_endline "}"

let print_method_set methodset = 
  print_string "{";
  MethodSet.iter (fun (can_name,methodtype) -> print_string ("(" ^ (CanonicalName.to_string can_name) ^ "," ^ (method_to_extended_string methodtype) ^ ")"); print_string ";") methodset;
  print_endline "}"

let print_replace_list m =
  print_string "{";
  List.iter (fun (m1,m2) -> 
    let m1_string = (method_to_extended_string m1) in
    let m2_string = (method_to_extended_string m2) in
    print_string ( "(" ^ m1_string  ^ "," ^ m2_string ^ ");"))
    m;
  print_endline ")"

let find_super (tenv : Types.type_env) (can_name : CanonicalName.t) : CanonicalNameSet.t =
  let obj = lookup_named_type can_name tenv in
  match obj with
  | Types.Class c ->
      CanonicalNameSet.union (CanonicalNameSet.singleton c.Types.class_extends)
	(List.fold_left CanonicalNameSet.union CanonicalNameSet.empty (List.map CanonicalNameSet.singleton c.Types.class_implements))
  | Types.Interface i ->
    (List.fold_left CanonicalNameSet.union (CanonicalNameSet.singleton (CanonicalName.make "java.lang.Object")) (List.map CanonicalNameSet.singleton i.Types.interface_extends))

let rec fixpoint (can_set : CanonicalNameSet.t) (find_super : CanonicalName.t -> CanonicalNameSet.t)  : CanonicalNameSet.t =
  let next_view = CanonicalNameSet.union can_set (List.fold_left CanonicalNameSet.union CanonicalNameSet.empty (List.map find_super (CanonicalNameSet.elements can_set))) in
  if CanonicalNameSet.equal can_set next_view 
  then next_view
  else fixpoint next_view find_super
    
let create_super_set (can_name : CanonicalName.t) (tenv : Types.type_env) : CanonicalNameSet.t =
  fixpoint (find_super tenv can_name) (find_super tenv)

let super_members named_type can_name =
  match named_type with
  | Types.Class c -> 
    if (can_name = c.Types.class_extends)
    then []
    else c.Types.class_extends :: c.Types.class_implements
  | Types.Interface i -> i.Types.interface_extends 

let create_declare_field_set (can_name : CanonicalName.t) (tenv : Types.type_env) : FieldSet.t =
  let named_type = lookup_named_type can_name tenv in
  let member_fields = Types.member_fields named_type in
  List.fold_left (fun prev item -> FieldSet.add item prev) FieldSet.empty (List.map (fun a -> (can_name,a)) member_fields)  


let rec create_inherit_field_set (can_name : CanonicalName.t) (tenv : Types.type_env) : FieldSet.t = 
  let named_type = lookup_named_type can_name tenv in
  let supers = super_members named_type can_name in
  let inherit_with_all = 
    List.fold_left (fun prev item -> FieldSet.union prev (create_contains_field_set item tenv)) FieldSet.empty supers in
  let declare_set = create_declare_field_set can_name tenv in
  let declare_list = List.map snd (FieldSet.elements declare_set) in
  FieldSet.filter (fun (_,f) ->
    let found = List.exists (fun f' -> field_signature_equal f f') declare_list in
    not found) inherit_with_all

and create_contains_field_set (can_name : CanonicalName.t) (tenv : Types.type_env) : FieldSet.t =
  let declare_set = create_declare_field_set can_name tenv in
  let inherit_set = create_inherit_field_set can_name tenv in
  FieldSet.union declare_set inherit_set

let create_inherit_field_set_without_yourself (can_name : CanonicalName.t) (tenv : Types.type_env) : FieldSet.t =
  let named_type = lookup_named_type can_name tenv in
  let supers = super_members named_type can_name in
  let inherit_with_all = 
    List.fold_left (fun prev item -> FieldSet.union prev (create_contains_field_set item tenv)) FieldSet.empty supers in
  inherit_with_all

let method_set_from_list (can_name : CanonicalName.t) (mlist : Types.method_type list) : MethodSet.t =
  List.fold_left (fun prev item -> MethodSet.add item prev) MethodSet.empty (List.map (fun a -> (can_name,a)) mlist)

let create_abstract_methods_for_all_methods (method_set : MethodSet.t) : MethodSet.t =
  let lang_obj = CanonicalName.make "java.lang.Object" in
  let can_method_list = MethodSet.elements (MethodSet.filter (fun (_,m) -> Ast.is_public m.Types.method_access) method_set) in
  let method_list = List.map snd can_method_list in
  let abstract_method_list = List.map (fun el -> if el.Types.method_final 
    then el 
    else {el with Types.method_abstract=true; Types.method_final=false}) method_list in
  method_set_from_list lang_obj abstract_method_list

let create_declare_method_set (can_name : CanonicalName.t) (tenv : Types.type_env) : MethodSet.t =
  let named_type = lookup_named_type can_name tenv in
  let member_methods = Types.member_methods named_type in
  method_set_from_list can_name member_methods

let nodecl_lookup (m : Types.method_type) ( declare_list : Types.method_type list) : bool =
  not (List.exists (fun m' -> method_signature_equal m m') declare_list)

let rec create_inherit_method_set (can_name : CanonicalName.t) (tenv : Types.type_env) : MethodSet.t =
  let named_type = lookup_named_type can_name tenv in
  let supers = super_members named_type can_name in
  let declare_set = create_declare_method_set can_name tenv in
  let inherit_with_all = 
    List.fold_left (fun prev item -> MethodSet.union prev (create_contains_method_set item tenv)) MethodSet.empty supers in
  let declare_list = List.map snd (MethodSet.elements declare_set) in
  MethodSet.filter (fun (_,m1) -> 
    let nodecl = nodecl_lookup m1 declare_list in
    let is_abstract = m1.Types.method_abstract in
    let con_1 = nodecl && (not is_abstract) in
    let con_2 = nodecl && is_abstract && (allabs can_name m1 tenv) in
    con_1 || con_2
  ) inherit_with_all
    
and create_contains_method_set (can_name : CanonicalName.t) (tenv : Types.type_env) : MethodSet.t =
  let declare_set = create_declare_method_set can_name tenv in

  (* We need to handle abstract imports of java.lang.object *)
  let named_type = lookup_named_type can_name tenv in
  let is_interface = Types.is_interface named_type in
  let no_extends = (super_members named_type can_name) = [] in
  let inherit_set = 
    (if is_interface && no_extends
     then (
       let lang_object = (CanonicalName.make "java.lang.Object") in
       let obj_declare_set = create_declare_method_set lang_object tenv in
       create_abstract_methods_for_all_methods obj_declare_set)
     else create_inherit_method_set can_name tenv) in
  MethodSet.union declare_set inherit_set

and allabs (can_name : CanonicalName.t) (m : Types.method_type) (tenv : Types.type_env) : bool =
  let named_type = lookup_named_type can_name tenv in
  let supers = super_members named_type can_name in
  List.for_all (fun super ->
    let contain_set = create_contains_method_set super tenv in
    MethodSet.for_all (fun (_,m') -> 
      if (method_signature_equal m m')
      then m'.Types.method_abstract
      else true) contain_set) supers

let create_replace_method_list (can_name : CanonicalName.t) (tenv : Types.type_env) : (Types.method_type * Types.method_type) list = 
  let named_type = lookup_named_type can_name tenv in
  let declares = create_declare_method_set can_name tenv in
  let declare_list = MethodSet.elements declares in
  let supers = super_members named_type can_name in
  let is_interface = Types.is_interface named_type in
  let contain_set =  
    (if is_interface && (supers = [])
     then 
	let lang_object = (CanonicalName.make "java.lang.Object") in
	let obj_declare_set = create_declare_method_set lang_object tenv in
	create_abstract_methods_for_all_methods obj_declare_set
     else List.fold_left (fun prev item -> MethodSet.union prev (create_contains_method_set item tenv)) MethodSet.empty supers) in
  let contain_list = MethodSet.elements contain_set in
  let local_replace = List.concat (List.map (fun m -> List.map (fun m' -> 
    if method_signature_equal (snd m) (snd m')
    then [((snd m),(snd m'))]
    else []) contain_list) declare_list) in
  let super_replace = List.concat (List.map (fun m -> 
    List.map ( fun m' ->
    if (m = m')
    then []
    else
      let m_type = (snd m) in
      let m'_type = (snd m') in
      let sig_equal = method_signature_equal m_type m'_type in
      let nodecl = nodecl_lookup m_type (List.map snd declare_list) in
      let m_abstract = m_type.Types.method_abstract in
      let m'_abstract = m'_type.Types.method_abstract in
      if sig_equal && nodecl && (not m_abstract) && m'_abstract
      then [(m_type,m'_type)]
      else []) contain_list) contain_list) in
  (List.concat local_replace) @ (List.concat super_replace) 
 
(**************************************************************)
(* {2 Hierarchy Methods }                                     *)
(**************************************************************)


(** Query the type hierarchy relation <= as described on slide 25
    of Scopes and Environments.
     @param sup   the [canonical_name] of the possible supertype
     @param sub   the [canonical_name] of the possible subtype
     @param tenv  the type environment
     @return [true] if [sup] is a supertype of [sub], including [sub] itself.
*)

let rec is_super_type
    (sup : Types.canonical_name)
    (sub : Types.canonical_name)
    (tenv : Types.type_env) : bool =
  if (CanonicalName.compare sup sub) = 0
  then true
  else 
    let super_set = create_super_set sub tenv in
    let result = CanonicalNameSet.exists ((=) sup) super_set in
    result
  
(** Look up a method by name in a class or interface, taking inheritance into
    account.
     @param host  the [canonical_name] of the receiver type
     @param name  the name of the method
     @param tenv  the type environment
     @return      all methods of the given name that are declared by or inherited
                  by the given receiver type paired with their declaring type.
*)
let lookup_method
    (host : Types.canonical_name)
    (name : string)
    (tenv : Types.type_env) : (Types.canonical_name * Types.method_type) list =
  let contain_set = create_contains_method_set host tenv in
  MethodSet.elements (MethodSet.filter (fun (_,m) -> m.Types.method_name = name) contain_set)


(** Look up a field by name in a class or interface, taking inheritance into
    account.
    @param host  the [canonical_name] of the receiver type
    @param name  the name of the field
    @param tenv  the type environment
    @return      [Some field] declared by or inherited by the given receiver type
                              paired with its declaring type, or
                 [None] if no such field exists.
*)
let lookup_field
    (host : Types.canonical_name)
    (name : string)
    (tenv : Types.type_env) : (Types.canonical_name * Types.field_type) option =
  let contain_set = create_contains_field_set host tenv in
  let found = FieldSet.filter (fun (can_name,field) -> field.Types.field_name = name) contain_set in
  if (FieldSet.cardinal found) = 0
  then None
  else Some (FieldSet.choose found)  

(**************************************************************)
(** set methods **)
(**************************************************************)
let contain_set (name : CanonicalName.t) (env : Types.type_env) : Types.member_type list = 
[]

(**************************************************************)
(** {Checks }                     *)
(**************************************************************)

let check_class_extends_class_and_not_final (class_decl : NAst.class_decl) (tenv : Types.type_env) : unit =
  let pos = class_decl.NAst.class_name.Ast.identifier_pos in
  let extended_name = class_decl.NAst.class_extends in
  let extended_type = lookup_named_type extended_name tenv in
  match extended_type with
  | Types.Interface i -> error_extends_non_class extended_name pos
  | Types.Class c ->
    if c.Types.class_final
    then error_extends_final_class extended_name pos
    else ()
    
let check_implements_must_be_interfaces (class_decl : NAst.class_decl) (tenv : Types.type_env) : unit =
  let pos = class_decl.NAst.class_name.Ast.identifier_pos in
  let rec traverse_implemented_interfaces (interfaces : NAst.namedtype list) (seen : CanonicalName.t list) : unit =
    match interfaces with
    | [] -> ()
    | i::interfaces' -> 
      match (lookup_named_type i tenv) with
      | Types.Class c -> error_implements_non_interface i pos
      | _ -> 
	if List.exists ((=) i)  seen
	then error_repeated_interface i pos
	else traverse_implemented_interfaces interfaces' (i :: seen)
  in
  traverse_implemented_interfaces class_decl.NAst.class_implements []
      
let rec traverse_members_for_constructors (members : Types.constructor_type list) (seen : Types.constructor_type list) (pos : Lexing.position) : unit =
  match members with
  | [] -> ()
  | member::members' -> 
    if List.exists (constructor_signature_equal member) seen
    then error_duplicate_constructor pos
    else traverse_members_for_constructors members' (member::seen) pos
     
let check_interface_extends_must_be_unique_and_interface (interface_decl : NAst.interface_decl) (tenv : Types.type_env) : unit = 
  let pos = interface_decl.NAst.interface_name.Ast.identifier_pos in
  let rec traverse_extended_interfaces (interfaces : NAst.namedtype list) (seen : CanonicalName.t list) : unit =
    match interfaces with
    | [] -> ()
    | i::interfaces' -> 
      match (lookup_named_type i tenv) with
      | Types.Class c -> error_implements_non_interface i pos
      | _ -> 
	if List.mem i  seen
	then error_repeated_interface i pos
	else traverse_extended_interfaces interfaces' (i :: seen)
  in
  traverse_extended_interfaces interface_decl.NAst.interface_extends []

 
let check_for_simple_constraints_in_class (class_decl : NAst.class_decl) (type_decl : NAst.type_decl) (tenv : Types.type_env) : unit = 
  let pos = type_decl.NAst.type_decl_pos in
  let named_type = lookup_named_type type_decl.NAst.type_canonical_name tenv in
  begin
    check_class_extends_class_and_not_final class_decl tenv;
    check_implements_must_be_interfaces class_decl tenv;
    traverse_members_for_constructors (Types.member_constructors named_type) [] pos
  end  

let check_for_simple_constraints (decl : NAst.type_decl) (tenv : Types.type_env) : unit =
  let desc = decl.NAst.type_decl in
  match desc with
  | NAst.Class c -> check_for_simple_constraints_in_class c decl tenv
  | NAst.Interface i -> check_interface_extends_must_be_unique_and_interface i tenv

(**************************************************************)
(** { Wellformedness checks 4 the win }                     *)
(**************************************************************)

let check_class_must_not_depend_on_itself (class_decl : NAst.class_decl) (type_decl : NAst.type_decl) (tenv : Types.type_env) : unit =
  let pos = class_decl.NAst.class_name.Ast.identifier_pos in
  let can_name_this = type_decl.NAst.type_canonical_name in
  let super_name = class_decl.NAst.class_extends in
  begin
    if is_super_type can_name_this super_name tenv
    then error_circular_inheritance pos
    else ()
  end

let check_interface_must_not_depend_on_itself (interface_decl : NAst.interface_decl) (type_decl : NAst.type_decl) (tenv : Types.type_env) : unit =
  let pos = interface_decl.NAst.interface_name.Ast.identifier_pos in
  let can_name_this = type_decl.NAst.type_canonical_name in
  let super_names = interface_decl.NAst.interface_extends in
  List.iter (fun super_name ->
    if is_super_type can_name_this super_name tenv
    then error_circular_inheritance pos
    else ()
  ) super_names

let check_constraint_1_must_not_depend_on_itself (type_decl : NAst.type_decl) (tenv : Types.type_env) : unit =
  match type_decl.NAst.type_decl with
  | NAst.Class c -> check_class_must_not_depend_on_itself c type_decl tenv
  | NAst.Interface i -> check_interface_must_not_depend_on_itself i type_decl tenv
      
let rec check_method_list_for_duplicates (method_types : Types.method_type list) (seen : Types.method_type list) (pos : Lexing.position) : unit =
  match method_types with
  | [] -> ()
  | x::xs -> 
    if List.exists (method_signature_equal x) seen
    then error_duplicate_method x pos
    else check_method_list_for_duplicates xs (x::seen) pos

let rec check_constraint_2_duplicate_methods (type_decl : NAst.type_decl) (tenv : Types.type_env) : unit =
  let pos = type_decl.NAst.type_decl_pos in
  let named_type = lookup_named_type type_decl.NAst.type_canonical_name tenv in
  check_method_list_for_duplicates (Types.member_methods named_type) [] pos

let check_constraint_3_two_methods_different_return (contain_set : MethodSet.t) (pos : Lexing.position) : unit =
  MethodSet.iter (fun (c1,m1) -> 
    MethodSet.iter (fun (c2,m2) -> 
      let equal_sig = method_signature_equal m1 m2 in
      let same_result_type = m1.Types.method_result = m2.Types.method_result in
      if equal_sig && not same_result_type
      then error_different_return_type m1 m2 pos
      else ()) contain_set) contain_set

let check_constraint_4_abstract (class_type : Types.class_type) (contain_set : MethodSet.t) (pos : Lexing.position) : unit =
  if class_type.Types.class_abstract
  then ()
  else
    MethodSet.iter (fun (c,m) -> 
      if m.Types.method_abstract
      then error_class_must_be_abstract class_type.Types.class_name m pos 
      else ()) contain_set
      
let check_constraint_5_static_non_static (replace_list : (Types.method_type * Types.method_type) list) (pos : Lexing.position) : unit =
  List.iter (fun (m,m') -> 
    let m_is_static = m.Types.method_static in
    let m'_is_static = m'.Types.method_static in
    if m_is_static && not m'_is_static
    then error_static_replace_non_static m pos
    else
      if not m_is_static && m'_is_static
      then error_non_static_replace_static m pos
      else ()) replace_list

let check_constraint_6_two_methods_different_return (replace_list : (Types.method_type * Types.method_type) list) (pos : Lexing.position) : unit =
  List.iter (fun (m,m') -> 
    let same_result_type = m.Types.method_result = m'.Types.method_result in
    if not same_result_type
    then error_different_return_type m m' pos
    else ()) replace_list

let check_constraint_7_protected (replace_list : (Types.method_type * Types.method_type) list) (pos : Lexing.position) : unit =
  List.iter (fun (m,m') -> 
    let m_protected = Ast.is_protected m.Types.method_access in
    let m'_public = Ast.is_public m'.Types.method_access in
    if (m'_public && m_protected)
    then error_protected_replace_public m pos
    else ()) replace_list

let check_constraint_8_throws (replace_list : (Types.method_type * Types.method_type) list) (tenv : Types.type_env) (pos : Lexing.position) : unit =
  let error = (CanonicalName.make "java.lang.Error") in
  let runtime = (CanonicalName.make "java.lang.RuntimeException") in
  List.iter (fun (m,m') -> 
    List.iter (fun e ->
      let found = List.exists (fun e' -> is_super_type e' e tenv) m'.Types.method_throws in
      let error = is_super_type error e tenv in
      let runtime = is_super_type runtime e tenv in
      if (found ||  error || runtime)
      then ()
      else error_illegal_throws_in_replace m e pos) m.Types.method_throws) replace_list

let check_constraint_9_final (replace_list : (Types.method_type * Types.method_type) list) (pos : Lexing.position) : unit =
  List.iter (fun (m,m') -> 
    let orig_is_final = m'.Types.method_final in
    if (orig_is_final)
    then error_replace_final m pos
    else ()) replace_list

let check_constraint_11_two_fields_inherited (host : CanonicalName.t) (fields : FieldSet.t) (pos : Lexing.position) : unit =
  FieldSet.iter (fun (c,f) -> 
    FieldSet.iter (fun (c',f') -> 
      let equal_sig = field_signature_equal f f' in
      let decl_same = c = c' in
      if (not equal_sig) || (equal_sig && decl_same)
      then ()
      else error_inherited_field_clash f.Types.field_name pos) fields) fields

let check_for_wellformedness_constraints (type_decl : NAst.type_decl) (tenv : Types.type_env) : unit = 
  let pos = type_decl.NAst.type_decl_pos in
  let can_name_this = type_decl.NAst.type_canonical_name in
  let named_type = lookup_named_type can_name_this tenv in
  begin
    check_constraint_1_must_not_depend_on_itself type_decl tenv;
    let field_inherit_set = create_inherit_field_set_without_yourself can_name_this tenv in
    let method_declare_set = create_declare_method_set can_name_this tenv in
    let method_inherit_set = create_inherit_method_set can_name_this tenv in
    let method_contain_set = create_contains_method_set can_name_this tenv in
    let replace_list = create_replace_method_list can_name_this tenv in
    print_endline (CanonicalName.to_string can_name_this);
    check_constraint_2_duplicate_methods type_decl tenv;
    check_constraint_3_two_methods_different_return method_contain_set pos;
    (match named_type with
    | Types.Class c -> 
      check_constraint_4_abstract c method_contain_set pos
    | _ -> ());
      check_constraint_5_static_non_static replace_list pos;
      check_constraint_6_two_methods_different_return replace_list  pos;
      check_constraint_7_protected replace_list pos;
      check_constraint_8_throws replace_list tenv pos;
      check_constraint_9_final replace_list pos;
      check_constraint_11_two_fields_inherited can_name_this field_inherit_set pos
  end

(**************************************************************)
(** {2 Hierarchy Well-formedness Checks }                     *)
(**************************************************************)

let rec check_source_file (sfs : NAst.source_file list) (tenv : Types.type_env) : unit =
  match sfs with
  | [] -> ()
  | x :: xs ->
    let type_decl = x.NAst.source_file_decl in
    begin
      check_for_simple_constraints type_decl tenv;
      check_for_wellformedness_constraints type_decl tenv;
      check_source_file xs tenv
    end
let add_abstract_object_to_env (tenv : Types.type_env) : unit = 
()

(** Check the class hierarchy of a program [NAst.program] under a type environment [Types.type_env] *)
let hier_program (prog : NAst.program) (tenv : Types.type_env) : unit =  
  add_abstract_object_to_env tenv;
  check_source_file prog tenv
