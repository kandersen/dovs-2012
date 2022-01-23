(************************************************************************)
(** AST type produced by the code generation phase                      *)
(************************************************************************)

module NAst = Nameresolvingast
module Inst = Instruction

type identifier = Ast.identifier

(* *************** Types *************** *)

type namedtype = NAst.namedtype

type typeexp = NAst.typeexp

(* *************** Body *************** *)

type body = Inst.instruction list

(* *************** Package and imports **************** *)

type package_decl = Ast.package_decl
type import_decl = Ast.import_decl

(* *************** Field and method declarations *************** *)

type access = Ast.access

type formal_param = Resourceast.formal_param

type field_decl = (* TAst.field_decl *)
    { field_access    : access;
      field_static    : bool;
      field_final     : bool;
      field_type      : typeexp;
      field_name      : identifier;
      (*field_init    : exp option*)
      field_signature : string }

type method_decl =
    { method_access      : access;
      method_static      : bool;
      method_final       : bool;
      method_abstract    : bool;
      method_result      : typeexp;
      method_name        : identifier;
      method_formals     : formal_param list;
      method_throws      : namedtype list;
      method_body        : body option;
      method_signature   : string }

type constructor_decl =
    { constructor_access    : access;
      constructor_name      : identifier;
      constructor_formals   : formal_param list;
      constructor_throws    : namedtype list;
      constructor_body      : body;
      constructor_signature : string }

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
      type_canonical_name : Types.canonical_name;   (*unused*)
      type_decl_signature : string }

and type_decl_desc =
  | Class of class_decl
  | Interface of interface_decl

(* *************** Programs **************** *)

type source_file =
    { source_file_name             : string;
      source_file_package          : package_decl option;                   (*unused*)
      source_file_single_imports   : Types.canonical_name StringMap.t;      (*unused*)
      source_file_ondemand_imports : Ast.name list;                         (*unused*)
      source_file_decl             : type_decl }

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
