(************************************************************************)
(** AST type produced by the environment phase                          *)
(************************************************************************)

type identifier = Ast.identifier
type name = Ast.name

(* *************** Types *************** *)
type namedtype = CanonicalName.t        (* NEW: Changed from Ast.name *)

type base_type = Ast.base_type

type typeexp = Types.typeexp            (* NEW: Changed from Ast.typeexp
                                           Types.Named now contains canonical names *)

(* *************** Operators *************** *)
type binop = Ast.binop
type unop = Ast.unop
type inc_dec_op = Ast.inc_dec_op

(* *************** Expressions *************** *)

type lvalue = { lvalue_pos: Lexing.position; lvalue: lvalue_desc }
and lvalue_desc =
  | NonstaticField of exp * identifier
  | Array of exp * exp
  | AmbiguousName of name

and exp = { exp_pos: Lexing.position; exp: exp_desc }
and exp_desc =
  | Binop of exp * binop * exp
  | Unop of unop * exp
  | IntConst of int32
  | CharConst of char
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

type package_decl = Ast.package_decl
type import_decl = Ast.import_decl

(* *************** Field and method declarations *************** *)

type access = Ast.access

type formal_param = typeexp * identifier

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

type type_decl =
    { type_decl_pos       : Lexing.position;
      type_decl           : type_decl_desc;
      type_canonical_name : Types.canonical_name } (* NEW: Added *)

and type_decl_desc =
  | Class of class_decl
  | Interface of interface_decl

(* *************** Programs **************** *)

type source_file =
    { source_file_name    : string;
      source_file_package : package_decl option;
   (* NEW: source_file_imports split in two *)
      source_file_single_imports : Types.canonical_name StringMap.t;
      source_file_ondemand_imports : Ast.name list;
      source_file_decl    : type_decl }

type program = source_file list

(************************************************************************)
(** {2 Helper functions for the AST}                                    *)
(************************************************************************)

(** {3 Converstion helpers} *)

(** Convert a name to its string representation *)
let name_to_string name =
  let identifiers = match name.Ast.name with
    | Ast.Simple id -> [id]
    | Ast.Qualified ids -> ids in
  let strs = List.map (fun id -> id.Ast.identifier) identifiers in
  let str = String.concat "." strs in str

(** {3 Predicate helpers} *)

(* exp -> bool *)
let is_false_const e = match e.exp with
  | BooleanConst b -> not b
  | _ -> false

let is_true_const e = match e.exp with
  | BooleanConst b -> b
  | _ -> false

(* access -> bool *)
let is_public a = match a with Ast.Public -> true | _ -> false
let is_protected a = match a with Ast.Protected -> true | _ -> false

(* decl -> bool *)
let is_field mt = match mt.decl with Field _ -> true | _ -> false
let is_method mt = match mt.decl with Method _ -> true | _ -> false
let is_constructor mt = match mt.decl with Constructor _ -> true | _ -> false

(* type_decl -> bool *)
let is_class nt = match nt.type_decl with Class _ -> true |  _ -> false
let is_interface nt = match nt.type_decl with Interface _ -> true |  _ -> false

(** {3 Accessor helpers}  *)

let type_decl_name td = match td.type_decl with
  | Class d -> d.class_name.Ast.identifier
  | Interface d -> d.interface_name.Ast.identifier
