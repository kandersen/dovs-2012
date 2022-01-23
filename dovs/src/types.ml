(* Structure/signature of Java types. *)

type canonical_name = CanonicalName.t

type identifier = string

type base_type = Ast.base_type

type typeexp =
  | Base  of base_type
  | Array of typeexp
  | Named of canonical_name
  | Null (* the null expression is special *)

type access = Ast.access

type constval =
  | Int of int32
  | Bool of bool
  | String of string

type field_type =
    { field_access   : access;
      field_static   : bool;
      field_final    : bool;
      field_type     : typeexp;
      field_name     : identifier;
      field_constval : constval option }

type method_type =
    { method_access   : access;
      method_static   : bool;
      method_final    : bool;
      method_abstract : bool;
      method_result   : typeexp;
      method_name     : identifier;
      method_formals  : typeexp list;
      method_throws   : canonical_name list }

type constructor_type =
    { constructor_access  : access;
      constructor_name    : identifier;
      constructor_formals : typeexp list;
      constructor_throws  : canonical_name list }

type member_type =
  | Field       of field_type
  | Method      of method_type
  | Constructor of constructor_type

type class_type =
    { class_final      : bool;
      class_abstract   : bool;
      class_name       : identifier;
      class_extends    : canonical_name;(*Note:is itself for java.lang.Object*)
      class_implements : canonical_name list;
      class_members    : member_type list }

type interface_type =
    { interface_name    : identifier;
      interface_extends : canonical_name list;
      interface_members : member_type list }

type named_type =
  | Class     of class_type
  | Interface of interface_type

(******************** Type Environment ************************)

(* map from canonical_name to named_type *)
module Env = Map.Make (CanonicalName)
type type_env = named_type Env.t

(**************************************************************)
(** {2 Helper functions for types and the type environment}   *)
(**************************************************************)

(** {3 Predicate helpers} *)

(* member_type -> bool *)
let is_field mt = match mt with Field _ -> true | _ -> false
let is_method mt = match mt with Method _ -> true | _ -> false
let is_constructor mt = match mt with Constructor _ -> true | _ -> false

(* named_type -> bool *)
let is_class nt = match nt with Class _ -> true |  _ -> false
let is_interface nt = match nt with Interface _ -> true |  _ -> false

(* typeexp -> bool *)
let is_base_type t = match t with Base _ -> true | _ -> false
let is_array t = match t with Array _ -> true | _ -> false
let is_named_type t = match t with Named _ -> true | _ -> false
let is_void t = match t with Base Ast.Void -> true | _ -> false
let is_byte t = match t with Base Ast.Byte -> true | _ -> false
let is_short t = match t with Base Ast.Short -> true | _ -> false
let is_int t = match t with Base Ast.Int -> true | _ -> false
let is_long t = match t with Base Ast.Long -> true | _ -> false
let is_char t = match t with Base Ast.Char -> true | _ -> false
let is_float t = match t with Base Ast.Float -> true | _ -> false
let is_double t = match t with Base Ast.Double -> true | _ -> false
let is_boolean t = match t with Base Ast.Boolean -> true | _ -> false
let is_string t = match t with
  | Named cname -> CanonicalName.to_string cname = "java.lang.String"
  | _ -> false

(** true if the argument is a joos type and a numeric type (ie. byte, short, int, or char) **)
let is_numeric t = match t with
  | Base Ast.Byte
  | Base Ast.Short
  | Base Ast.Int
  | Base Ast.Char -> true
  | _ -> false

let is_reference t = match t with
  | Array _ 
  | Named _ 
  | Null -> true
  | Base _ -> false

(** {3 Accessor helpers} *)

(* XXXs : members list -> XXX_type list *)
(* where XXX = field|method|constructor *)
let rec fields ms = match ms with
  | [] -> []
  | Field f :: ms -> f :: fields ms
  | _ :: ms -> fields ms

let rec methods ms = match ms with
  | [] -> []
  | Method m :: ms -> m :: methods ms
  | _ :: ms -> methods ms

let rec constructors ms : constructor_type list = match ms with
  | [] -> []

  | Constructor c :: ms -> c :: constructors ms
  | _ :: ms -> constructors ms

(* XXXs : named_type -> XXX_type list *)
(* where XXX = member|_field|_method|_constructor *)
let members nt = match nt with
  | Class d -> d.class_members
  | Interface d -> d.interface_members

let member_fields nt = fields (members nt)
let member_methods nt = methods (members nt)
let member_constructors nt = constructors (members nt)

(** {3 Type Signatures} *)

let dots_to_slashes s =
  let dotregexp = Str.regexp "\\." in
  let ss = Str.split dotregexp s in
  String.concat "/" ss

let basetype_to_sig bt = match bt with
  | Ast.Void    -> "V"
  | Ast.Byte    -> "B"
  | Ast.Short   -> "S"
  | Ast.Int     -> "I"
  | Ast.Char    -> "C"
  | Ast.Boolean -> "Z"
  | Ast.Long 
  | Ast.Float
  | Ast.Double  -> 
    Error.internal_compiler_error "Unknown type in typeexp_to_sig"

let cname_to_sig cn = 
  dots_to_slashes (CanonicalName.to_string cn)

let rec typeexp_to_desc t = match t with
  | Base bt -> basetype_to_sig bt
  | Array t -> "[" ^ (typeexp_to_desc t)
  | Named cn -> "L" ^ (cname_to_sig cn) ^ ";"
  | Null ->
    Error.internal_compiler_error "Unknown type in typeexp_to_desc"
