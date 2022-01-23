(************************************************************************)
(** AST type produced by the parser                                     *)
(************************************************************************)

type identifier = { identifier_pos : Lexing.position; identifier : string } 

type name = { name_pos: Lexing.position; name: name_desc }
and name_desc =
  | Simple of identifier
  | Qualified of identifier list

(* *************** Types *************** *)

type namedtype = name

type base_type =
  | Void
  | Byte
  | Short
  | Int
  | Long
  | Char
  | Float
  | Double
  | Boolean

type typeexp = { typeexp_pos : Lexing.position; typeexp : typeexp_desc }
and typeexp_desc =
  | Base   of base_type
  | TArray of typeexp
  | Named  of namedtype

(* *************** Operators *************** *)
type binop =
  | Plus
  | Minus
  | Times
  | Divide
  | Modulo
  | Eq
  | Ne
  | Lt
  | Le
  | Gt
  | Ge
  | And
  | Or
  | Xor
  | LazyAnd
  | LazyOr

type unop =
  | Negate (* minus*)
  | Complement (*complement*)

type inc_dec_op =
  | PreInc (*plus_plus*)
  | PreDec (*minus_minus*)
  | PostInc (*plus_plus*)
  | PostDec (*minus_minus*)

(* *************** Expressions *************** *)

type lvalue = { lvalue_pos: Lexing.position; lvalue: lvalue_desc }
and lvalue_desc =
  | NonstaticField of exp * identifier
  | Array of exp * exp
  | AmbiguousName of name

(* ********************************************************************* *)
(* Note: You may add data constructors to the *exp_desc* data type       *)
(* ********************************************************************* *)
and exp = { exp_pos: Lexing.position; exp: exp_desc }
and exp_desc =
  | Binop of exp * binop * exp
  | Unop of unop * exp
  | IntConst of string
  | CharConst of string
  | StringConst of string
  | BooleanConst of bool
  | Null
  | This
  | StaticInvoke of namedtype * identifier * exp list
  | NonstaticInvoke of exp * identifier * exp list
  | SimpleInvoke of identifier * exp list
  | AmbiguousInvoke of name * identifier * exp list
  | New of typeexp * exp list
  | NewArray of typeexp * exp list
  | Lvalue of lvalue
  | Assignment of lvalue * exp
  | IncDec of lvalue * inc_dec_op
  | Cast of typeexp * exp
  | Instanceof of exp * typeexp
  (* ... your data constructors go here ... *)
  | PossiblyCast of exp * exp
  | InnerExp of exp

(* *************** Blocks and statements *************** *)

type block = stm list
and stm = { stm_pos: Lexing.position; stm: stm_desc }
and stm_desc =
  | Exp of exp
  | IfThen of exp * stm
  | IfThenElse of exp * stm * stm
  | While of exp * stm
  | Empty
  | Block of block
  | VoidReturn
  | ValueReturn of exp
  | LocalDecl of typeexp * identifier * exp option
  | Throw of exp
  | SuperCall of exp list
  | ThisCall of exp list

type body = block

(* *************** Package and imports **************** *)

type package_decl = name

type import_decl = { import_decl_pos: Lexing.position; 
		     import_decl: import_decl_desc }
and import_decl_desc =
  | OnDemand of name
  | Single of name * identifier

(* *************** Field and method declarations *************** *)

type access =
  | Public
  | Protected

type formal_param = typeexp * identifier

(* access static? final? type name = init *)
type field_decl =
    { field_access : access;
      field_static : bool;
      field_final  : bool;
      field_type   : typeexp;
      field_name   : identifier;
      field_init   : exp option }

(* access static? final? abstract?
   return_t name(formal = init, ...) throws body? *)
type method_decl =
    { method_access   : access;
      method_static   : bool;
      method_final    : bool;
      method_abstract : bool;
      method_result   : typeexp;
      method_name     : identifier;
      method_formals  : formal_param list;
      method_throws   : namedtype list;
      method_body     : body option }

(* access name(type formal = init, ...) throws body? *)
type constructor_decl =
    { constructor_access  : access;
      constructor_name    : identifier;
      constructor_formals : formal_param list;
      constructor_throws  : namedtype list;
      constructor_body    : body }

type decl = { decl_pos: Lexing.position; decl: decl_desc }
and decl_desc =
  | Field of field_decl
  | Method of method_decl
  | Constructor of constructor_decl

(* *************** Type declarations **************** *)

type class_decl
  = { class_final      : bool;
      class_abstract   : bool;
      class_name       : identifier;
      class_extends    : namedtype;
      class_implements : namedtype list;
      class_members    : decl list }

type interface_decl
  = { interface_name    : identifier;
      interface_extends : namedtype list;
      interface_members : decl list }

type type_decl = { type_decl_pos: Lexing.position; type_decl: type_decl_desc }
and type_decl_desc =
  | Class of class_decl
  | Interface of interface_decl

(* *************** Programs **************** *)

type source_file
  = { source_file_name    : string;
      source_file_package : package_decl option;
      source_file_imports : import_decl list;
      source_file_decl    : type_decl }

type program = source_file list

(************************************************************************)
(** {2 Helper functions for the AST}                                    *)
(************************************************************************)

(** {3 Converstion helpers} *)

(** Convert a name to its string representation *)
let name_to_string name =
  let identifiers = match name.name with
    | Simple id -> [id]
    | Qualified ids -> ids in
  let strs = List.map (fun id -> id.identifier) identifiers in
  let str = String.concat "." strs in str

(** {3 Predicate helpers} *)

(* name -> bool *)
let is_simple n = match n.name with Simple _ -> true | _ -> false

(* typeexp -> bool *)
let is_base_type t = match t.typeexp with Base _ -> true | _ -> false
let is_array t = match t.typeexp with TArray _ -> true | _ -> false
let is_named_type t = match t.typeexp with Named _ -> true | _ -> false
let is_void t = match t.typeexp with Base Void -> true | _ -> false
let is_byte t = match t.typeexp with Base Byte -> true | _ -> false
let is_short t = match t.typeexp with Base Short -> true | _ -> false
let is_int t = match t.typeexp with Base Int -> true | _ -> false
let is_long t = match t.typeexp with Base Long -> true | _ -> false
let is_char t = match t.typeexp with Base Char -> true | _ -> false
let is_float t = match t.typeexp with Base Float -> true | _ -> false
let is_double t = match t.typeexp with Base Double -> true | _ -> false
let is_boolean t = match t.typeexp with Base Boolean -> true | _ -> false

(* stm -> bool *)
let is_supercall s = match s.stm with SuperCall _ -> true | _ -> false
let is_thiscall s = match s.stm with ThisCall _ -> true | _ -> false

(* exp -> bool *)
let is_false_const e = match e.exp with
  | BooleanConst b -> not b
  | _ -> false

let is_true_const e = match e.exp with
  | BooleanConst b -> b
  | _ -> false

(* access -> bool *)
let is_public a = match a with Public -> true | _ -> false
let is_protected a = match a with Protected -> true | _ -> false

(* decl -> bool *)
let is_field mt = match mt.decl with Field _ -> true | _ -> false
let is_method mt = match mt.decl with Method _ -> true | _ -> false
let is_constructor mt = match mt.decl with Constructor _ -> true | _ -> false

(* type_decl -> bool *)
let is_class nt = match nt.type_decl with Class _ -> true |  _ -> false
let is_interface nt = match nt.type_decl with Interface _ -> true |  _ -> false

(** {3 Accessor helpers}  *)

let type_decl_name td = match td.type_decl with
  | Class d -> d.class_name.identifier
  | Interface d -> d.interface_name.identifier

let type_decl_kind td = 
  match td.type_decl with
    | Class _ -> "class"
    | Interface _ -> "interface"
