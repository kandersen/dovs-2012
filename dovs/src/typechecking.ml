(** Compiler phase to perform type checking of the program and resolve
    the uses of fields and methods to their definitions.
    It also performs type coercions and other transformations
    based on the types. *)

module NAst = Nameresolvingast
module DAst = Disambiguationast
module TAst = Typecheckingast
module T = Types
module Option = Utils.OptionPlus
module ListPlus = Utils.ListPlus

module NameSet = Set.Make (CanonicalName)

let nameset_from_list ls = 
  List.fold_left (fun acc e -> NameSet.add e acc) NameSet.empty ls

open Utils

let rec typeexp_to_string typeexp = match typeexp with
  | T.Base Ast.Void  -> "void"
  | T.Base Ast.Byte  -> "byte"
  | T.Base Ast.Short -> "short"
  | T.Base Ast.Int   -> "int"
  | T.Base Ast.Long  -> "long"
  | T.Base Ast.Char  -> "char"
  | T.Base Ast.Float -> "float"
  | T.Base Ast.Double -> "double"
  | T.Base Ast.Boolean -> "boolean"
  | T.Array typeexp' -> typeexp_to_string typeexp' ^ "[]"
  | T.Named name -> CanonicalName.to_string name
  | T.Null -> "null"

let binop_to_string binop = match binop with
  | Ast.Plus   -> "plus"
  | Ast.Minus  -> "minus"
  | Ast.Times  -> "times"
  | Ast.Divide -> "divide"
  | Ast.Modulo -> "modulo"
  | Ast.Eq -> "eq"
  | Ast.Ne -> "ne"
  | Ast.Lt -> "lt"
  | Ast.Le -> "le"
  | Ast.Gt -> "gt"
  | Ast.Ge -> "ge"
  | Ast.And -> "and"
  | Ast.Or  -> "or"
  | Ast.Xor -> "xor"
  | Ast.LazyAnd -> "lazyand"
  | Ast.LazyOr  -> "lazyor"

let unop_to_string unop = match unop with
  | Ast.Negate     -> "negate"
  | Ast.Complement -> "complement"

let cname_of_type t = 
  CanonicalName.make (typeexp_to_string t)
(************************************************************************)
(** {2 Error Messages }                                                 *)
(************************************************************************)

(** Reports the error that an invocation does not resolve uniquely to a closest
    matching method or constructor.
 *)
let error_ambiguous_overloading pos =
  Error.error Error.AMBIGUOUS_OVERLOADING pos
    "Ambiguous overloading"

(** Reports the error that array length cannot be assigned to. *)
let error_assign_to_array_length pos =
  Error.error Error.ASSIGN_TO_ARRAY_LENGTH pos
    "Array length cannot be assigned to"

(** Reports the error that a final field cannot be assigned to.
     @param field  the field type of the final field
     @param pos    the position at which the error occured
*)
let error_assign_to_final_field field pos =
  Error.error Error.ASSIGN_TO_FINAL_FIELD pos
    ("Final field " ^ field.T.field_name ^ " cannot be assigned to")

(** Reports the error that two types are assignment incompatible.
     @param tt   the type which was assigned to
     @param ft   the type which was assigned from
     @param pos  the position at which the error occured
*)
let error_assign_type tt ft pos =
  Error.error Error.ASSIGN_TYPE pos
    ("The type " ^ typeexp_to_string ft
     ^ " cannot be assigned to the type " ^ typeexp_to_string tt)

(** Reports the error that a binary operator cannot be used with the types of
    its subexpressions.
     @param t1   the left hand side type
     @param op   the binary operator
     @param t2   the right hand side type
     @param pos  the position at which the error occured
*)
let error_binop_type t1 op t2 pos =
  Error.error Error.BINOP_TYPE pos 
    ("The operator " ^ binop_to_string op
     ^ " cannot be used with the types "
     ^ typeexp_to_string t1
     ^ " and "
     ^ typeexp_to_string t2)

(** Reports the error that a constructor invocation is circular. *)
let error_circular_constructor_invocation pos =
  Error.error Error.CIRCULAR_CONSTRUCTOR_INVOCATION pos
    "Circular constructor invocation"

(** Reports the error that a field could not be found on a host type.
     @param host      the [canonical_name] of the host type
     @param field_id  the identifier of the field
*)
let error_field_not_found host field_id =
  Error.error Error.FIELD_NOT_FOUND field_id.Ast.identifier_pos
    ("Could not find field " ^ field_id.Ast.identifier
     ^ " on type " ^ CanonicalName.to_string host)

(** Reports the error that the field cannot be accessed on an array. *)
let error_field_on_array pos =
  Error.error Error.FIELD_ON_ARRAY pos
    "Cannot access field on array"

(** Reports the error that a field cannot be accessed on a non-reference type.
     @param typ  the non-reference type
     @param pos  the position at which the error occured
*)
let error_field_on_non_reference typ pos =
  Error.error Error.FIELD_ON_NON_REFERENCE pos
    ("Cannot access field on " ^ typeexp_to_string typ)

(** Reports the error that a checked exception cannot be thrown from the
    current method context.
     @param t    the type of the exception
     @param pos  the position at which the error occured
*)
let error_illegal_throws t pos =
  Error.error Error.ILLEGAL_THROWS pos
    ("The type " ^ typeexp_to_string t
     ^ " cannot be thrown by the current context")

(** Reports the error that an abstract class cannot be instantiated.
     @param cname  the [canonical_name] of the class
     @param pos    the position at which the error occured
*)
let error_instantiate_abstract_class cname pos =
  Error.error Error.INSTANTIATE_ABSTRACT_CLASS pos
    ("Abstract class " ^ CanonicalName.to_string cname
     ^ " cannot be instantiated")

(** Reports the error that an interface cannot be instantiated.
     @param cname  the [canonical_name] of the interface
     @param pos    the position at which the error occured
*)
let error_instantiate_interface cname pos =
  Error.error Error.INSTANTIATE_INTERFACE pos
    ("Interface " ^ CanonicalName.to_string cname
     ^ " cannot be instantiated")

(** Reports the error that a cast expression is invalid.
     @param t1   the type of the casted expression
     @param t2   the target type of the cast
     @param pos  the position at which the error occured
*)
let error_invalid_cast t1 t2 pos =
  Error.error Error.INVALID_CAST pos
  ("Cannot cast " ^ typeexp_to_string t1
   ^ " to " ^ typeexp_to_string t2)

(** Reports the error that an instanceof expression is invalid.
     @param t1   the type of the queried expression
     @param t2   the target type of the query
     @param pos  the position at which the error occured
*)
let error_invalid_instanceof t1 t2 pos =
  Error.error Error.INVALID_INSTANCEOF pos
    ("Cannot check if " ^ typeexp_to_string t2
     ^ " is an instance of " ^ typeexp_to_string t1)

(** If compiling with the [-joos1] option, reports the error that array
    method call is used.
*)
let check_joos1_array_method_call pos =
  Error.check_joos1 Error.JOOS1_ARRAY_METHOD_CALL pos
    "array method call"

(** If compiling with the [-joos1] option, reports the error that
    bitwise operation is used.
*)
let check_joos1_bitwise_operations pos =
  Error.check_joos1 Error.JOOS1_BITWISE_OPERATIONS pos
    "bitwise operations"

(** If compiling with the [-joos1] option, reports the error that
    closest method overloading is used.
*)
let check_joos1_closest_match_overloading pos =
  Error.check_joos1 Error.JOOS1_CLOSEST_MATCH_OVERLOADING pos
    "closest match overloading"

(** If compiling with the [-joos1] option, reports the error that
    implicit this class for a static method invocation is used.
*)
let check_joos1_implicit_this_class_static_method pos =
  Error.check_joos1 Error.JOOS1_IMPLICIT_THIS_CLASS_STATIC_METHOD pos
    "implicit this class for static method"

(** Reports the error that no matching constructor was found for the
    constructor invocation.
*)
let error_no_matching_constructor_found pos =
  Error.error Error.NO_MATCHING_CONSTRUCTOR_FOUND pos
    "Could not find a matching constructor"

(** Reports the error that no matching method was found for the method
    invocation.
*)
let error_no_matching_method_found name pos =
  Error.error Error.NO_MATCHING_METHOD_FOUND pos
    ("Could not find a matching method for " ^ name)

(** Reports the error that an array lvalue does not have an array base type. *)
let error_non_array_type_array_base pos =
  Error.error Error.NON_ARRAY_TYPE_ARRAY_BASE pos
    "Array base must have array type"

(** Reports the error that a condition does not have a boolean type. *)
let error_non_boolean_condition pos =
  Error.error Error.NON_BOOLEAN_CONDITION pos
    "Condition must have type boolean"

(** Reports the error that a field has a non-Joos type. 
     @param field  the field type of the field with non-Joos type
     @param pos    the position at which the error occured
*)
let error_non_joos_field_type field pos =
  Error.error Error.NON_JOOS_FIELD_TYPE pos
    ("The field " ^ field.T.field_name ^ " has non-Joos type")

(** Reports the error that a parameter on a called constructor or method has
    a non-Joos type.
     @param invoke   the name of the called method
     @param pos      the position at which the error occured
*)
let error_non_joos_parameter_type invoke pos =
  Error.error Error.NON_JOOS_PARAMETER_TYPE pos
    ("Called " ^ invoke ^ " has non-Joos parameter type")

(** Reports the error that the return type on a called method has a non-Joos
    type.
     @param invoke   the name of the called method
     @param pos      the position at which the error occured
*)
let error_non_joos_return_type invoke pos =
  Error.error Error.NON_JOOS_RETURN_TYPE pos
    ("Called " ^ invoke ^ " has non-Joos return type")

(** Reports the error that an array lvalue has a non-numeric index type. *)
let error_non_numeric_array_index pos =
  Error.error Error.NON_NUMERIC_ARRAY_INDEX pos
    "Array index must have numeric type"

(** Reports the error that an new array expression has a non-numeric size type. *)
let error_non_numeric_array_size pos =
  Error.error Error.NON_NUMERIC_ARRAY_SIZE pos
    "Array size must have numeric type"

(** Reports the error that an inc/dec expression has a non-numeric type. *)
let error_non_numeric_inc_dec pos =
  Error.error Error.NON_NUMERIC_INC_DEC pos
    "Inc/dec needs a numeric type"

(** Reports the error that a receiver of a nonstatic invoke expression has a
    non-reference type.
*)
let error_non_reference_receiver pos = 
  Error.error Error.NON_REFERENCE_RECEIVER pos
    "Only class types can be method invocation receivers"

(** Reports the error that a nonstatic field is linked as static.
     @param field  the field type of the static field
     @param pos    the position at which the error occured
*)
let error_nonstatic_field_linked_as_static field pos =
  Error.error Error.NONSTATIC_FIELD_LINKED_AS_STATIC pos
    ("The field " ^ field.T.field_name ^ " is not static")

(** Reports the error that a nonstatic method is linked as static.
     @param meth  the name of the static method
     @param pos   the position at which the error occured
*)
let error_nonstatic_method_linked_as_static meth pos =
  Error.error Error.NONSTATIC_METHOD_LINKED_AS_STATIC pos
    ("The method " ^ meth ^ " is not static")

(** Reports the error that a protected constructor has been invoked from
    outside its package.
*)
let error_protected_constructor_invocation pos =
  Error.error Error.PROTECTED_CONSTRUCTOR_INVOCATION pos
    ("Invocation of protected constructors using new is "
     ^ " only allowed from the same package")

(** Reports the error that a protected member has been accessed illegally. *)
let error_protected_member_access pos =
  Error.error Error.PROTECTED_MEMBER_ACCESS pos
    ("Protected members can only be accessed from the same "
     ^ "package or from a subclass")

(** Reports the error that a qualified protected member has been accessed
    illegally.
*)
let error_qualified_protected_member_access pos =
  Error.error Error.PROTECTED_MEMBER_ACCESS pos
    ("Qualified access to protected members must be on a subtype "
     ^ "of the current class")

(** Reports the error that a static field is linked as nonstatic.
     @param field  the field type of the static field
     @param pos    the position at which the error occured
*)
let error_static_field_linked_as_nonstatic field pos =
  Error.error Error.STATIC_FIELD_LINKED_AS_NONSTATIC pos
    ("The field " ^ field.T.field_name ^ " is static")

(** Reports the error that a static method is linked as nonstatic.
     @param meth   the name of the static method
     @param pos    the position at which the error occured
*)
let error_static_method_linked_as_nonstatic meth pos =
  Error.error Error.STATIC_METHOD_LINKED_AS_NONSTATIC pos
    ("The method " ^ meth ^ " is static")

(** Reports the error that this is used before a super constructor call.
    @param exp  the this expression
*)
let error_this_before_super_call exp =
  Error.error Error.THIS_BEFORE_SUPER_CALL exp.DAst.exp_pos
    "Cannot refer to this before super constructor call"

(** Reports the error that this is used in a static context.
     @param exp the this expression
*)
let error_this_in_static_context exp =
  Error.error Error.THIS_IN_STATIC_CONTEXT exp.DAst.exp_pos
    "Cannot refer to this in a static context"

(** Reports the error that a named type in a throws clause is not throwable.
     @param exc  the not throwable named type
*)
let error_non_throwable_in_throws exc pos =
  Error.error Error.NON_THROWABLE_IN_THROWS pos
    ("Thrown class must be throwable: " ^ CanonicalName.to_string exc)

(** Reports the error that a unary operator cannot be used with the type of
    its subexpression.
     @param op   the unary operation
     @param exp  the type of the subexpression
     @param pos  the position at which the error occured
*)
let error_unop_type op typ pos =
  Error.error Error.UNOP_TYPE pos
    ("The operator " ^ unop_to_string op
     ^ " cannot be used with the type " ^ typeexp_to_string typ)

(** Reports the error that a non-void method has void return statement. *)
let error_void_return_in_non_void_method pos =
  Error.error Error.VOID_RETURN_IN_NON_VOID_METHOD pos
    "A non-void method must return a value"
  

(************************************************************************)
(** {2 Helpers Functions }                                              *)
(************************************************************************)

let rec assignable' (to_type : T.typeexp) (from_type : T.typeexp) tenv : bool =
  match to_type, from_type with
    | sigma, sigma' when sigma = sigma' -> true
    | T.Array sigma, T.Array tau when assignable' sigma tau tenv -> true
    | T.Named n, T.Array _ when CanonicalName.to_string n = "java.lang.Cloneable" -> true
    | T.Named n, T.Array _ when CanonicalName.to_string n = "java.io.Serializable" -> true
    | T.Named n, T.Array _ when CanonicalName.to_string n = "java.lang.Object" -> true
    | T.Named c, T.Named d when Hierarchy.is_super_type c d tenv -> true
    | _ -> false

(** Check if a value of one type can be assigned to a variable of another.
     @param to_type    the type that was expected
     @param from_type  the type that was given
     @param tenv       the type environment
     @return [true] if the assignment is legal,
             [false] otherwise.
*)
let assignable (to_type : T.typeexp) (from_type : T.typeexp) tenv : bool =
  match to_type, from_type with
    | sigma, sigma' when sigma = sigma' -> true
    | T.Base Ast.Short, T.Base Ast.Byte

    | T.Base Ast.Int, T.Base Ast.Byte
    | T.Base Ast.Int, T.Base Ast.Short
    | T.Base Ast.Int, T.Base Ast.Char

    | T.Base Ast.Long, T.Base Ast.Byte
    | T.Base Ast.Long, T.Base Ast.Short
    | T.Base Ast.Long, T.Base Ast.Char
    | T.Base Ast.Long, T.Base Ast.Int

    | T.Base Ast.Float, T.Base Ast.Byte
    | T.Base Ast.Float, T.Base Ast.Short
    | T.Base Ast.Float, T.Base Ast.Char
    | T.Base Ast.Float, T.Base Ast.Int
    | T.Base Ast.Float, T.Base Ast.Long
    
    | T.Base Ast.Double, T.Base Ast.Byte
    | T.Base Ast.Double, T.Base Ast.Short
    | T.Base Ast.Double, T.Base Ast.Char
    | T.Base Ast.Double, T.Base Ast.Int
    | T.Base Ast.Double, T.Base Ast.Long
    | T.Base Ast.Double, T.Base Ast.Float

    | T.Named _, T.Null
    | T.Array _, T.Null -> true
    | T.Named n, T.Array _ when CanonicalName.to_string n = "java.lang.Object" -> true
    | T.Named n, T.Array _ when CanonicalName.to_string n = "java.lang.Cloneable" -> true
    | T.Named n, T.Array _ when CanonicalName.to_string n = "java.io.Serializable" -> true
    | T.Array sigma, T.Array tau when assignable' sigma tau tenv -> true
    | T.Named c, T.Named d when Hierarchy.is_super_type c d tenv -> true
    | _ -> false

let is_joos_type typ = match typ with
  | T.Base Ast.Long
  | T.Base Ast.Float
  | T.Base Ast.Double -> false
  | _ -> true

let make_type (n : string) =
  T.Named (CanonicalName.make n)

(** Check whether type [from_type] is assignable to the type [to_type] and
    report an error if this is not the case.
     @param pos        the position at which the error occured
     @param to_type    the target type of the assignment
     @param from_type  the original type of the assigned expression
     @param tenv       the type environment
*)
let check_assignable pos to_type from_type tenv =
  let assignable = assignable to_type from_type tenv in
  if assignable
  then ()
  else error_assign_type to_type from_type pos

(************************************************************************)
(** {2 Type Checking Traversal }                                        *)
(************************************************************************)

type info =
    { tenv : Types.type_env;
      type_cname : T.canonical_name;
      lenv : T.typeexp StringMap.t; 
      exceptions : NameSet.t;
      return : T.typeexp;
      is_field_init: bool;
      is_lv_assignment: bool;
      in_static_method: bool;
      in_super_constructor: bool;
      is_static_field_assign: bool;
      in_this_constructor: bool}

(** Insert a conversion to string of the given expression.
     @param e    the expression to coerce
     @param err  the error function to call if the conversion is not possible
*)
let coerce_to_string e err =
(*  let dummy_exp =
    { TAst.exp_pos = Lexing.dummy_pos;
      TAst.exp_type = T.Null;
      TAst.exp = TAst.Null } in *)
  let mkunop uop = { e with TAst.exp = TAst.Unop (uop, e) } in
    if T.is_string e.TAst.exp_type
    then match e.TAst.exp with
      | TAst.StringConst _ | TAst.Binop _ | TAst.Unop _ -> e
      | _ -> mkunop TAst.ObjectToString
    else match e.TAst.exp_type with
      | T.Base Ast.Byte    -> mkunop TAst.ByteToString
      | T.Base Ast.Short   -> mkunop TAst.ShortToString
      | T.Base Ast.Int     -> mkunop TAst.IntToString
      | T.Base Ast.Char    -> mkunop TAst.CharToString
      | T.Base Ast.Boolean -> mkunop TAst.BooleanToString
      | T.Null | T.Named _ | T.Array _ -> mkunop TAst.ObjectToString
      | _ -> err ()

let tcheck_binop op = match op with
  | Ast.Plus   -> TAst.Plus
  | Ast.Minus  -> TAst.Minus
  | Ast.Times  -> TAst.Times
  | Ast.Divide -> TAst.Divide
  | Ast.Modulo -> TAst.Modulo
  | Ast.Eq -> TAst.Eq
  | Ast.Ne -> TAst.Ne
  | Ast.Lt -> TAst.Lt
  | Ast.Le -> TAst.Le
  | Ast.Gt -> TAst.Gt
  | Ast.Ge -> TAst.Ge
  | Ast.And -> TAst.And
  | Ast.Or  -> TAst.Or
  | Ast.Xor -> TAst.Xor
  | Ast.LazyAnd -> TAst.LazyAnd
  | Ast.LazyOr  -> TAst.LazyOr

let tcheck_unop op = match op with
  | Ast.Negate -> TAst.Negate
  | Ast.Complement -> TAst.Complement

let check_return_type ret_type name pos = 
  if not (is_joos_type ret_type)
  then error_non_joos_return_type name pos
  else ()
    
let valid_protected_nonstatic_access info cname declared =
  (Hierarchy.is_super_type declared info.type_cname info.tenv
   && Hierarchy.is_super_type info.type_cname cname info.tenv) 
  || CanonicalName.package info.type_cname = CanonicalName.package cname

let valid_protected_static_access info cname =
  Hierarchy.is_super_type cname info.type_cname info.tenv
  || CanonicalName.package info.type_cname = CanonicalName.package cname

let is_primitiv_type typ = 
  match typ with 
    | T.Base b -> true
    | _ -> false


 let rec pointwise_supertypes sups subs info =
    match sups, subs with
      | [], [] ->
	true
      | [], _
      | _, [] ->
	false
      | sup :: sups', sub :: subs' when assignable sup sub info.tenv ->
	pointwise_supertypes sups' subs' info
      | _ -> 
	false

and throwok a b pos info =
  let tenv = info.tenv in
  let error = T.Named (CanonicalName.make "java.lang.Error") in
  let runtime_exception = T.Named (CanonicalName.make "java.lang.RuntimeException") in
  let list_of_exceptions_in_a = List.map (fun n -> T.Named n) (NameSet.elements a) in
  if info.is_field_init && not (ListPlus.is_empty (List.filter (fun name -> not (assignable runtime_exception name tenv)) list_of_exceptions_in_a))
  then error_illegal_throws (T.Named (NameSet.choose a)) pos
  else true &&
    List.for_all (fun alpha -> 
      assignable error alpha tenv
      || assignable runtime_exception alpha tenv
      || if List.exists (fun beta -> 
	assignable beta alpha tenv)
	  (List.map (fun n -> T.Named n) (NameSet.elements b))
	then true
	else error_illegal_throws alpha pos)
    list_of_exceptions_in_a


let match_closest_constructor arg_types all_possibilities info pos =
  let candidates = List.filter (fun con -> pointwise_supertypes con.T.constructor_formals arg_types info) all_possibilities in
  if List.length candidates = 0
  then error_no_matching_constructor_found pos info.type_cname
  else ();
  let closest = List.filter (fun con -> List.for_all (fun con' -> pointwise_supertypes con'.T.constructor_formals con.T.constructor_formals info) candidates) candidates in
  let closest_count = List.length closest in
  begin    
    if closest_count = 0
    then error_ambiguous_overloading pos
    else ();
    if List.for_all (fun con -> List.for_all is_joos_type con.T.constructor_formals) closest
    then ()
    else error_non_joos_parameter_type (CanonicalName.to_string info.type_cname) pos;
    let con = List.hd closest in
    throwok (nameset_from_list con.T.constructor_throws) info.exceptions pos info;
    if con.T.constructor_formals <> arg_types
    then check_joos1_closest_match_overloading pos
    else ();
    con
  end    

let rec tcheck_lvalue lv info =
  let pos = lv.DAst.lvalue_pos in
  let mklv lv' typ =
    { TAst.lvalue_pos = pos; TAst.lvalue_type = typ; TAst.lvalue = lv' }
  in match lv.DAst.lvalue with
    | DAst.NonstaticField (e, id) ->
      let e' = tcheck_exp e info in
      begin
	match e'.TAst.exp_type with
	  | T.Named cname ->
	    begin 
	      match Hierarchy.lookup_field cname id.Ast.identifier info.tenv with
		| None ->
		  error_field_not_found cname id
		| Some (field_cname, field_type) ->
		  if field_type.T.field_static
		  then error_static_field_linked_as_nonstatic field_type pos
		  else if info.is_lv_assignment && field_type.T.field_final
		  then error_assign_to_final_field field_type pos
		  else if field_type.T.field_access = Ast.Protected
		       && not (valid_protected_nonstatic_access info cname field_cname)
		  then error_protected_member_access pos
		  else if not (is_joos_type field_type.T.field_type)
		  then error_non_joos_field_type field_type pos
		  else mklv (TAst.NonstaticField (e', id, field_cname)) field_type.T.field_type
	    end
	  | T.Array a ->
	    if info.is_lv_assignment
	    then  error_assign_to_array_length pos
	    else mklv (TAst.NonstaticField (e', id, CanonicalName.make "This is")) (make_type "a Hack!")
	  | _ -> error_field_on_non_reference e'.TAst.exp_type pos
      end
    | DAst.Array (base, idx) ->
        let base' = tcheck_exp base info in
        let idx' = tcheck_exp idx info in
        let typ = match base'.TAst.exp_type with
          | T.Array t -> t
          | _ -> error_non_array_type_array_base pos
        in
          if T.is_numeric idx'.TAst.exp_type
          then mklv (TAst.Array (base', idx')) typ
          else error_non_numeric_array_index pos
    | DAst.Local id ->
        mklv (TAst.Local id)
          (match StringMap.lookup id.Ast.identifier info.lenv with
             | Some t -> t
             | None -> (Error.internal_compiler_error "unbound local"))
    | DAst.StaticField (host, id) ->
      match Hierarchy.lookup_field host id.Ast.identifier info.tenv with
	| None ->
	  error_field_not_found host id
	| Some (field_cname, field_type) ->
	  if (not field_type.T.field_static)
	  then error_nonstatic_field_linked_as_static field_type pos
	  else if field_type.T.field_final && info.is_lv_assignment
	  then error_assign_to_final_field field_type pos
	  else if field_type.T.field_access = Ast.Protected
	       && not (valid_protected_static_access info field_cname)
	  then error_protected_member_access pos
	  else if not (is_joos_type field_type.T.field_type)
	  then error_non_joos_field_type field_type pos
	  else mklv (TAst.StaticField (host, id, field_cname)) field_type.T.field_type
	    

(*does the exact same thing as throwok execpt it does not throw an error it simply returns false*)
and throw_clear_bool a b info =
  let tenv = info.tenv in
  let error = T.Named (CanonicalName.make "java.lang.Error") in
  let runtime_exception = T.Named (CanonicalName.make "java.lang.RuntimeException") in
  List.for_all (fun alpha -> 
      assignable error alpha tenv
      || assignable runtime_exception alpha tenv
      || List.exists (fun beta -> 
	assignable beta alpha tenv)
	  (List.map (fun n -> T.Named n) (NameSet.elements b)))
    (List.map (fun n -> T.Named n) (NameSet.elements a))


(*the special set intersection use to create the intersection between exception sets on abstract methods*)
and special_set_inter info pos s1 s2 = 
  let s1_inter_s2 = NameSet.filter (fun el -> throw_clear_bool (NameSet.singleton el) s2 info) s1 in
  let s2_inter_s1 = NameSet.filter (fun el -> throw_clear_bool (NameSet.singleton el) s1 info) s2 in
  NameSet.union s1_inter_s2 s2_inter_s1


(*returns a single method pair (canonicalname * method_type) from the host type with the correct formals and checks the exceptions or an error if no such method is found.*)
and single_method_from_name (name : string) (host : T.canonical_name) (formals : TAst.exp list) (info : info) (pos : Lexing.position): (Types.canonical_name * Types.method_type) = 
  let all_methods = Hierarchy.lookup_method host name info.tenv in  
  let candidates =
    List.filter
      (fun (cname, mt) ->
	(if mt.T.method_access = Ast.Protected
	 then Hierarchy.is_super_type cname info.type_cname info.tenv || CanonicalName.package cname = CanonicalName.package info.type_cname
	 else true) && pointwise_supertypes mt.T.method_formals (List.map (fun f -> f.TAst.exp_type) formals) info) all_methods in
  if List.length candidates = 0
  then error_no_matching_method_found name pos
  else ();
  let closest =
    List.filter
      (fun (cname, mt) ->
	List.for_all
	  (fun (cname', mt') ->
	    (*assignable (T.Named cname') (T.Named cname) info.tenv &&*) pointwise_supertypes mt'.T.method_formals mt.T.method_formals info)
	  candidates)
      candidates in
  let closest_count = List.length closest in
  begin    
    if closest_count = 0
    then error_ambiguous_overloading pos
    else ();
    let (cname, mt) = List.hd closest in
    if closest_count = 1
    then
      (throwok (nameset_from_list mt.T.method_throws) info.exceptions pos info;
       if mt.T.method_formals <> (List.map (fun f -> f.TAst.exp_type) formals)
       then check_joos1_closest_match_overloading pos
       else ();
       (cname, mt))
    else let all_exceptions =
	   List.fold_left
	     (special_set_inter info pos)
	     (nameset_from_list mt.T.method_throws)
	     (List.map
		(fun (_, mt) -> nameset_from_list mt.T.method_throws) closest) in
      throwok all_exceptions info.exceptions pos info;
      if mt.T.method_formals <> (List.map (fun f -> f.TAst.exp_type) formals)
      then check_joos1_closest_match_overloading pos
      else ();
      (cname, mt)
  end    
    
and tcheck_exp e info =
  let pos = e.DAst.exp_pos in
  let mkexp exp typ = 
    { TAst.exp_pos = pos; TAst.exp_type = typ; TAst.exp = exp }
  in match e.DAst.exp with
    | DAst.Binop (e1, op, e2) ->
        let e1' = tcheck_exp e1 info in
        let e2' = tcheck_exp e2 info in
        let e1_t = e1'.TAst.exp_type in
        let e2_t = e2'.TAst.exp_type in
        let op' = tcheck_binop op in
        let binop = TAst.Binop (e1', op', e2') in
        let err () = error_binop_type e1_t op e2_t pos in
          (match op with
             | Ast.Plus ->
                 if T.is_string e1_t || T.is_string e2_t
                 then mkexp
                   (TAst.Binop (coerce_to_string e1' err,
                                TAst.Concat,
                                coerce_to_string e2' err))
                   (make_type "java.lang.String")
                 else if T.is_numeric e1_t && T.is_numeric e2_t
                 then mkexp
                   (TAst.Binop (e1', op', e2'))
                   (T.Base Ast.Int)
                 else err ()
             | Ast.Minus
             | Ast.Times
             | Ast.Divide
             | Ast.Modulo ->
                 if T.is_numeric e1_t && T.is_numeric e2_t
                 then mkexp binop (T.Base Ast.Int)
                 else err ()
             | Ast.Eq
             | Ast.Ne ->
                 if (T.is_numeric e1_t && T.is_numeric e2_t)
                 || (T.is_boolean e1_t && T.is_boolean e2_t)
                 then mkexp binop (T.Base Ast.Boolean)
                 else if T.is_reference e1_t
                      && T.is_reference e2_t
                      && (assignable e1_t e2_t info.tenv ||
                          assignable e2_t e1_t info.tenv)
                 then let op' = match op with
                   | Ast.Eq -> TAst.Aeq
                   | Ast.Ne -> TAst.Ane
                   | _ ->  (Error.internal_compiler_error
                                   "unexpected equality operator")
                 in mkexp (TAst.Binop (e1', op', e2')) (T.Base Ast.Boolean)
                 else err ()
            | Ast.Lt
            | Ast.Le
            | Ast.Gt
            | Ast.Ge ->
                if T.is_numeric e1_t && T.is_numeric e2_t
                then mkexp binop (T.Base Ast.Boolean)
                else err ()
            | Ast.And
            | Ast.Or
            | Ast.Xor ->
                if T.is_boolean e1_t && T.is_boolean e2_t
                then mkexp binop (T.Base Ast.Boolean)
                else if T.is_numeric e1_t && T.is_numeric e2_t
                then (check_joos1_bitwise_operations pos;
                      mkexp binop (T.Base Ast.Int))
                else err ()
            | Ast.LazyAnd
            | Ast.LazyOr ->
                if T.is_boolean e1_t && T.is_boolean e2_t
                then mkexp binop (T.Base Ast.Boolean)
                else err ())
    | DAst.Unop (op, e) ->
        let op' = tcheck_unop op in
        let e'  = tcheck_exp e info in
        let typ = match op with
          | Ast.Negate ->
              if T.is_numeric e'.TAst.exp_type
              then T.Base Ast.Int
              else error_unop_type op e'.TAst.exp_type pos
          | Ast.Complement ->
              if T.Base Ast.Boolean = e'.TAst.exp_type
              then T.Base Ast.Boolean
              else error_unop_type op e'.TAst.exp_type pos
        in mkexp (TAst.Unop (op', e')) typ
    | DAst.IntConst i ->
        mkexp (TAst.IntConst i) (T.Base Ast.Int)
    | DAst.CharConst c ->
        mkexp (TAst.IntConst (Int32.of_int (Char.code c))) (T.Base Ast.Char) (* OBS: no more CharConsts *)
    | DAst.StringConst s ->
        mkexp (TAst.StringConst s) (make_type "java.lang.String")
    | DAst.BooleanConst b ->
        mkexp (TAst.BooleanConst b) (T.Base Ast.Boolean)
    | DAst.Null ->
        mkexp TAst.Null T.Null
    | DAst.StaticInvoke (nt, id, exp_list) ->
      let exp_list' = tcheck_exp_list exp_list info in
      let method_name = id.Ast.identifier in
      let (cname, mt) = single_method_from_name method_name nt exp_list' info pos in
      begin
	check_return_type mt.T.method_result method_name pos;
	if not (mt.T.method_static)
	then error_nonstatic_method_linked_as_static method_name pos
	else if mt.T.method_access = Ast.Protected && not (valid_protected_static_access info cname)
	then error_protected_member_access pos
	else mkexp (TAst.StaticInvoke (nt, id, exp_list', cname, mt)) mt.T.method_result
      end 
    | DAst.NonstaticInvoke (exp, id, exp_list) ->
      let exp_list' = tcheck_exp_list exp_list info in
      let exp' = tcheck_exp exp info in
      let method_name = id.Ast.identifier in
      let typ = exp'.TAst.exp_type in
      begin
	match typ with 
	  | T.Named name -> 
	    let (cname, mt) = single_method_from_name method_name name exp_list' info pos in
	    begin
	      check_return_type mt.T.method_result method_name pos;
	      if mt.T.method_static
	      then error_static_method_linked_as_nonstatic method_name pos
	      else if mt.T.method_access = Ast.Protected &&
		     not (valid_protected_nonstatic_access info name cname)
	      then error_protected_member_access pos
	      else mkexp (TAst.NonstaticInvoke (exp', id, exp_list', cname, mt)) mt.T.method_result
	    end
	  | T.Array a ->
            begin 
	      check_joos1_array_method_call pos;
	      match method_name, exp_list' with
		| "clone", [] -> 
		  mkexp (TAst.ArrayClone exp') (T.Named (CanonicalName.make "java.lang.Object"))
		| _ -> 
		  let res = Hierarchy.lookup_method (CanonicalName.make "java.lang.Object") method_name info.tenv in
		  begin
		    match res with
		      |	[(cname, mt)] when mt.T.method_formals = List.map (fun arg -> arg.TAst.exp_type) exp_list' -> 
			mkexp (TAst.NonstaticInvoke (exp', id, exp_list', cname, mt)) mt.T.method_result
		      | _ -> error_no_matching_method_found method_name pos
		  end
	    end
	  | _ -> error_non_reference_receiver pos 
      end
    | DAst.SimpleInvoke (id, exp_list) ->
      let exp_list' = tcheck_exp_list exp_list info in
      let method_name = id.Ast.identifier in
      let (cname, mt) = single_method_from_name method_name info.type_cname exp_list' info pos  in
      let exp = mkexp TAst.This (T.Named info.type_cname) in
      begin
	check_return_type mt.T.method_result method_name pos;
	if mt.T.method_static
	then 
	  (check_joos1_implicit_this_class_static_method pos;
	   mkexp (TAst.StaticInvoke(cname, id, exp_list', cname, mt)) (mt.T.method_result))
	else if info.in_static_method
	then error_this_in_static_context e
	else mkexp (TAst.NonstaticInvoke (exp, id , exp_list', cname, mt)) (mt.T.method_result)
      end
    | DAst.NewArray (t, exp_list) ->
      let exp_list' = tcheck_exp_list exp_list info in
      begin
	match t with 
	  | T.Array a ->
	    if List.for_all (fun e -> T.is_numeric e.TAst.exp_type) exp_list'
	    then mkexp (TAst.NewArray (t, exp_list')) t
	    else error_non_numeric_array_size pos
	  | _ -> error_non_array_type_array_base pos
      end
    | DAst.Assignment (lv, e) ->
      let lv' = tcheck_lvalue lv {info with is_lv_assignment = true} in
      let e' =
	match lv'.TAst.lvalue with 
	  |TAst.StaticField _ -> tcheck_exp e { info with is_static_field_assign = true }
	  |_ -> tcheck_exp e info in
      let lv_t = lv'.TAst.lvalue_type in
      let e_t = e'.TAst.exp_type in
      check_assignable e'.TAst.exp_pos lv_t e_t info.tenv;
      mkexp (TAst.Assignment (lv', e')) lv_t
    | DAst.Cast (t, e) ->
      let e' = tcheck_exp e info in
      let e_t = e'.TAst.exp_type in
      if not ((T.is_numeric t && T.is_numeric e_t)
              || assignable t e_t info.tenv
              || assignable e_t t info.tenv)
      then error_invalid_cast e_t t pos
      else if T.is_boolean t
      then
            (* replace boolean cast: JLS ??15.16, JLS ??16.1 *)
        mkexp e'.TAst.exp t
      else mkexp (TAst.Cast (t, e')) t
    | DAst.Lvalue lv ->          
      let lv' = tcheck_lvalue lv info in
      begin
	match lv'.TAst.lvalue with
	  | TAst.NonstaticField (exp, id, cname) ->
	    begin
	      match exp.TAst.exp_type with
		| T.Array a -> 
		  if id.Ast.identifier = "length"
		  then mkexp (TAst.ArrayLength exp) (T.Base Ast.Int)
		  else error_field_on_array pos
		| _ -> mkexp (TAst.Lvalue lv') (lv'.TAst.lvalue_type)
	    end 
	  |_ -> mkexp (TAst.Lvalue lv') (lv'.TAst.lvalue_type)
      end	  
    | DAst.IncDec (l, op) ->
      let l' = tcheck_lvalue l {info with is_lv_assignment = true} in
      let type_of_l = l'.TAst.lvalue_type in
      if T.is_numeric type_of_l
      then mkexp (TAst.IncDec (l', op)) type_of_l
      else error_non_numeric_inc_dec pos
    | DAst.Instanceof (e, target) ->
      let e' = tcheck_exp e info in
      let source= e'.TAst.exp_type in
      if (assignable source target info.tenv || assignable target source info.tenv) && not (is_primitiv_type target)
      then mkexp (TAst.Instanceof (e', target)) (T.Base Ast.Boolean)
      else error_invalid_instanceof source target pos
    | DAst.This ->
      if info.in_static_method || info.is_static_field_assign
      then error_this_in_static_context e
      else if info.in_super_constructor || info.in_this_constructor 
      then error_this_before_super_call e
      else mkexp (TAst.This) (T.Named info.type_cname)
    | DAst.New (t, args) ->
      let args' = tcheck_exp_list args info in
      match t with
	| T.Named cname ->
	  begin
	    match Environment.lookup_named_type cname info.tenv with
	      | Some t_desc ->
		begin 
		  let valid_protected_constructor_invocation cname info =
		    CanonicalName.package cname = CanonicalName.package info.type_cname in
		  match t_desc with
		    | T.Interface _ -> error_instantiate_interface cname pos
		    | T.Class c ->
		      if c.T.class_abstract 
		      then error_instantiate_abstract_class cname pos
		      else 
			let constructors = T.constructors c.T.class_members in
			let arg_types = List.map (fun arg -> arg.TAst.exp_type) args' in
			let con = match_closest_constructor arg_types constructors info pos in
			if con.T.constructor_access = Ast.Protected && not (valid_protected_constructor_invocation cname info)
			then error_protected_constructor_invocation pos
			else mkexp (TAst.New (t, args', cname, con)) t
		end
	      | None -> Error.internal_compiler_error "Instantiating unknown type"
	  end 
	| _ -> Error.internal_compiler_error "Attempting to instantiate non-reference type"
	  
and tcheck_exp_list es info =
  List.map (fun e -> tcheck_exp e info) es

let rec tcheck_stm stm info =
  let pos = stm.DAst.stm_pos in
  let lenv = info.lenv in
  let mkstm stm' lenv = ({ TAst.stm_pos = pos; TAst.stm = stm' }, lenv)
  in match stm.DAst.stm with
    | DAst.Exp e ->
        mkstm (TAst.Exp (tcheck_exp e info)) lenv
    | DAst.IfThen (e, s) ->
        let e' = tcheck_exp e info in
        let (s', _) = tcheck_stm s info in
          if T.is_boolean e'.TAst.exp_type
          then mkstm (TAst.IfThen (e', s')) lenv
          else error_non_boolean_condition pos
    | DAst.IfThenElse (e, st, sf) ->
        let e' = tcheck_exp e info in
        let (st', _) = tcheck_stm st info in
        let (sf', _) = tcheck_stm sf info in
          if T.is_boolean e'.TAst.exp_type
          then mkstm (TAst.IfThenElse (e', st', sf')) lenv
          else error_non_boolean_condition pos
      | DAst.While (e, s) ->
          let e' = tcheck_exp e info in
          let (s', _) = tcheck_stm s info in
            if T.is_boolean e'.TAst.exp_type
            then mkstm (TAst.While (e', s')) lenv
            else error_non_boolean_condition pos
      | DAst.Empty ->
          mkstm TAst.Empty lenv
      | DAst.VoidReturn ->
	if info.return = T.Base Ast.Void
	then mkstm TAst.VoidReturn lenv
	else error_void_return_in_non_void_method pos
      | DAst.ValueReturn e ->
	let e' = tcheck_exp e info in
	let () = check_assignable pos info.return e'.TAst.exp_type info.tenv in
	if info.return = T.Base Ast.Void
	then error_assign_type info.return e'.TAst.exp_type pos
	else mkstm (TAst.ValueReturn e') lenv
      | DAst.Throw e ->
	let e_checked = tcheck_exp e info in
	check_assignable pos (T.Named (CanonicalName.make "java.lang.Throwable")) e_checked.TAst.exp_type info.tenv;
	if throwok (NameSet.singleton (cname_of_type e_checked.TAst.exp_type)) info.exceptions pos info
	then mkstm (TAst.Throw e_checked) lenv 
	else error_illegal_throws e_checked.TAst.exp_type pos
      | DAst.LocalDecl (t, id, init) -> 
	let info' = { info with lenv = StringMap.add id.Ast.identifier t lenv } in
	let init' = Option.map (fun i -> tcheck_exp i info') init in
	Option.map_default (fun i -> check_assignable pos t i.TAst.exp_type info.tenv) () init';
	mkstm (TAst.LocalDecl (t, id, init'))
	  (StringMap.add id.Ast.identifier t lenv)
      | DAst.Block b ->
	let b' = tcheck_block b info in
	mkstm (TAst.Block b') lenv
      | DAst.ThisCall exp_list -> 
	let exp_list' = tcheck_exp_list exp_list { info with in_this_constructor = true } in
	begin
	  match Environment.lookup_named_type info.type_cname info.tenv with
	    | Some t ->
	      begin
		match t with
		  | T.Class ct ->
		    let constructors = T.constructors ct.T.class_members in
		    let arg_types = List.map (fun arg -> arg.TAst.exp_type) exp_list' in
		    let con = match_closest_constructor arg_types constructors info pos in
		    mkstm (TAst.ThisCall (exp_list', info.type_cname, con)) lenv
		  | _ -> Error.internal_compiler_error "ThisCall:Implementation in interface"
	      end
	    | None -> Error.internal_compiler_error "ThisCall:Unknown this-class"
	end
      | DAst.SuperCall exp_list ->
	let exp_list' = tcheck_exp_list exp_list {info with in_super_constructor = true} in
	begin
	  match Environment.lookup_named_type info.type_cname info.tenv with
	    | Some t ->
	      begin
		match t with 
		  | T.Class ct -> 
		    begin
		      match Environment.lookup_named_type ct.T.class_extends info.tenv with
			  | Some t -> 
			    begin
			      match t with
				| T.Class c ->
				  let constructors = T.constructors c.T.class_members in
				  let arg_types = List.map (fun arg -> arg.TAst.exp_type) exp_list' in
				  let con = match_closest_constructor arg_types constructors info pos in
				  mkstm (TAst.SuperCall (exp_list', ct.T.class_extends, con)) lenv
				| _ -> Error.internal_compiler_error "Class extends interface"
			    end
			  | None -> Error.internal_compiler_error "unknown class"
		    end
		  | T.Interface i -> Error.joos_not_implemented_yet "src/typechecking.ml" "super call in interface!!"
	      end
	    | None -> Error.internal_compiler_error "unknown class"
	end

and tcheck_block stms info = match stms with
  | [] -> []
  | stm :: stms ->
      let (stm', lenv) = tcheck_stm stm info in
        stm' :: tcheck_block stms { info with lenv = lenv }

let tcheck_formals fs info = fs

let tcheck_body body fs info =
  tcheck_block body
    { info with lenv =
        fold fs StringMap.empty
          (fun (t, id) lenv ->
             StringMap.add id.Ast.identifier t lenv) }

let tcheck_body_opt body fs info = match body with
  | None -> None
  | Some body -> Some (tcheck_body body fs info)

let tcheck_field_decl d pos info =
  { TAst.field_access = d.DAst.field_access;
    TAst.field_static = d.DAst.field_static;
    TAst.field_final  = d.DAst.field_final;
    TAst.field_type   = d.DAst.field_type;
    TAst.field_name   = d.DAst.field_name;
    TAst.field_init   = match d.DAst.field_init with 
      | None -> None
      | Some e ->
          let field_type = d.DAst.field_type in
          let init = tcheck_exp e {info with is_field_init = true; is_static_field_assign = d.DAst.field_static} in
            check_assignable pos field_type init.TAst.exp_type info.tenv;
            Some init }

let check_throws_are_sub_of_throwable throw_set pos info = 
  let throwable = CanonicalName.make "java.lang.Throwable" in
  List.iter (fun cname -> if not (Hierarchy.is_super_type throwable cname info.tenv)
    then error_non_throwable_in_throws cname pos
    else ()) throw_set

let tcheck_method_decl d pos info =
  let formals = tcheck_formals d.DAst.method_formals info in
  let return_type = d.DAst.method_result in
  let method_throws = d.DAst.method_throws in
  let method_static = d.DAst.method_static in
  let body = tcheck_body_opt d.DAst.method_body formals
    {info with return = return_type;
      exceptions = nameset_from_list method_throws;
      in_static_method = method_static } in
  check_throws_are_sub_of_throwable method_throws pos info;
    { TAst.method_access   = d.DAst.method_access;
      TAst.method_static   = method_static;
      TAst.method_final    = d.DAst.method_final;
      TAst.method_abstract = d.DAst.method_abstract;
      TAst.method_result   = d.DAst.method_result;
      TAst.method_name     = d.DAst.method_name;
      TAst.method_formals  = formals;
      TAst.method_throws   = method_throws;
      TAst.method_body     = body }

let tcheck_constructor_decl d pos info =
  let formals = tcheck_formals d.DAst.constructor_formals info in
  let constructor_throws = d.DAst.constructor_throws in
  let body = tcheck_body d.DAst.constructor_body formals
    { info with exceptions = nameset_from_list constructor_throws;
      return = T.Base Ast.Void} in
  check_throws_are_sub_of_throwable constructor_throws pos info;
  { TAst.constructor_access  = d.DAst.constructor_access;
    TAst.constructor_name    = d.DAst.constructor_name;
    TAst.constructor_formals = formals;
    TAst.constructor_throws  = constructor_throws;
    TAst.constructor_body    = body }
    
let tcheck_decl d info = 
  let pos = d.DAst.decl_pos in
  { TAst.decl_pos = pos;
    TAst.decl = match d.DAst.decl with
      | DAst.Field f -> TAst.Field (tcheck_field_decl f d.DAst.decl_pos info)
      | DAst.Method m -> TAst.Method (tcheck_method_decl m pos info)
      | DAst.Constructor c -> TAst.Constructor (tcheck_constructor_decl c pos info) }

let tcheck_decls ds info =
  List.map (fun d -> tcheck_decl d info) ds

let check_constructor_circularity decls = 
  let constructor decl = match decl.TAst.decl with
    | TAst.Constructor c ->
      [c]
    | _ -> [] in
  let constructors = ListPlus.concat_map constructor decls in
  let find_code cons_type = 
    List.find
      (fun tast_cons -> 
	cons_type.T.constructor_access = tast_cons.TAst.constructor_access
	&& cons_type.T.constructor_name = tast_cons.TAst.constructor_name.Ast.identifier
	  && cons_type.T.constructor_formals = List.map fst tast_cons.TAst.constructor_formals
      && cons_type.T.constructor_throws = tast_cons.TAst.constructor_throws) constructors in
  let edges = 
    let rec visit conss = 
      match conss with
	| [] -> []
	| cons :: conss' ->
	  begin
	    match cons.TAst.constructor_body with
	      | { TAst.stm = TAst.ThisCall(_, _, target) } :: _ ->
		(cons, find_code target) :: visit conss'
	      | _ -> visit conss'
	  end in
    visit constructors in
  let nodes : TAst.constructor_decl list = List.map fst edges @ List.map snd edges in
  let succ node : TAst.constructor_decl list = List.map snd (List.filter (fun (from, _) -> from = node) edges) in
  let detect_cycle start =
    let rec visit visited node =
      if List.mem node visited
      then error_circular_constructor_invocation node.TAst.constructor_name.Ast.identifier_pos
      else ListPlus.concat_map (visit (node :: visited)) (succ node) in
    visit [] start in
  let _ = List.map detect_cycle nodes in
  ()

let tcheck_class_decl d info =
  let decls' = tcheck_decls d.DAst.class_members info in
  let _ = check_constructor_circularity decls' in
  { TAst.class_final      = d.DAst.class_final;
    TAst.class_abstract   = d.DAst.class_abstract;
    TAst.class_name       = d.DAst.class_name;
    TAst.class_extends    = d.DAst.class_extends;
    TAst.class_implements = d.DAst.class_implements;
    TAst.class_members    = decls' }

let tcheck_interface_decl d info =
  { TAst.interface_name    = d.DAst.interface_name;
    TAst.interface_extends = d.DAst.interface_extends;
    TAst.interface_members = tcheck_decls d.DAst.interface_members info }

let tcheck_type_decl tdecl tenv =
  let info =
    { tenv = tenv;
      type_cname = tdecl.DAst.type_canonical_name;
      lenv = StringMap.empty; 
      exceptions = NameSet.empty;
      return = T.Null;
      is_field_init = false;
      is_lv_assignment = false;
      in_static_method = false;
      in_super_constructor = false;
      is_static_field_assign = false;
      in_this_constructor = false}
  in
    { TAst.type_decl_pos       = tdecl.DAst.type_decl_pos;
      TAst.type_canonical_name = tdecl.DAst.type_canonical_name;
      TAst.type_decl = match tdecl.DAst.type_decl with
        | DAst.Class d -> TAst.Class (tcheck_class_decl d info)
        | DAst.Interface d -> TAst.Interface (tcheck_interface_decl d info) }

let tcheck_source_file sf tenv =
  { TAst.source_file_name             = sf.DAst.source_file_name;
    TAst.source_file_package          = sf.DAst.source_file_package;
    TAst.source_file_single_imports   = sf.DAst.source_file_single_imports;
    TAst.source_file_ondemand_imports = sf.DAst.source_file_ondemand_imports;
    TAst.source_file_decl = tcheck_type_decl sf.DAst.source_file_decl tenv }

let tcheck_program prog tenv =
  List.map (fun sf -> tcheck_source_file sf tenv) prog


