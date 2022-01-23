(** Pretty printer for the AST *)

open Printf
open Ast

let pp_identifier id = printf "%s" id.Ast.identifier

let pp_name name = match name.Ast.name with
  | Ast.Simple id
    -> pp_identifier id
  | Ast.Qualified ids
    -> let _ = 
	 List.fold_left (fun sep id -> printf "%s" sep; 
	                               pp_identifier id;
	                               ".") "" ids in
       ()

let pp_name_list names = 
  let _ = List.fold_left (fun sep name -> printf "%s" sep; pp_name name; ",") "" names in ()

let pp_namedtype ntype = pp_name ntype

let pp_namedtype_opt ntype prefix = match ntype with
  | None
    -> ()
  | Some ntype
    -> printf "%s" prefix;
       pp_namedtype ntype

let pp_base_type t = match t with
  | Void -> printf "void"
  | Byte -> printf "byte"
  | Short -> printf "short"
  | Int -> printf "int"
  | Long -> printf "long"
  | Char -> printf "char"
  | Float -> printf "float"
  | Double -> printf "double"
  | Boolean -> printf "boolean"

let rec pp_typeexp t = match t.typeexp with
  | Base base       -> pp_base_type base
  | TArray typeexp' -> pp_typeexp typeexp'; printf "[]"
  | Named name      -> pp_namedtype name

let rec pp_array_elemtype t = match t.typeexp with
  | Base base       -> pp_base_type base
  | TArray typeexp' -> pp_array_elemtype typeexp'
  | Named name      -> pp_namedtype name

let pp_typeexp_opt type_opt prefix = match type_opt with
  | None
    -> ()
  | Some typeexp
    -> printf "%s" prefix;
       pp_typeexp typeexp

let pp_binop binop = match binop with
  | Ast.Plus -> printf "+"
  | Ast.Minus -> printf "-"
  | Ast.Times -> printf "*"
  | Ast.Divide -> printf "/"
  | Ast.Modulo -> printf "%%"
  | Ast.Eq -> printf "=="
  | Ast.Ne -> printf "!="
  | Ast.Lt -> printf "<"
  | Ast.Le -> printf "<="
  | Ast.Gt -> printf ">"
  | Ast.Ge -> printf ">="
  | Ast.And -> printf "&"
  | Ast.Or -> printf "|"
  | Ast.Xor -> printf "^"
  | Ast.LazyAnd -> printf "&&"
  | Ast.LazyOr -> printf "||"

let pp_unop unop = match unop with
  | Ast.Negate -> printf "-"
  | Ast.Complement -> printf "!"

let pp_inc_dec_op idop = match idop with
  | Ast.PreInc -> printf "++"
  | Ast.PreDec -> printf "--"
  | Ast.PostInc -> printf "++"
  | Ast.PostDec -> printf "--"

let rec pp_lvalue lvalue = match lvalue.Ast.lvalue with
  | Ast.NonstaticField (e,id) -> pp_exp e; printf "."; pp_identifier id
  | Ast.Array (e0,e1) -> pp_exp e0; printf "["; pp_exp e1; printf "]" 
  | Ast.AmbiguousName name -> pp_name name

and pp_exp exp = match exp.Ast.exp with
  | Ast.Binop (e0,binop,e1)
    -> printf "(";
       pp_exp e0;
       printf " ";
       pp_binop binop;
       printf " ";
       pp_exp e1;
       printf ")"
  | Ast.InnerExp e -> printf "("; pp_exp e; printf ")"
  | Ast.Unop (unop,e) -> printf ""; pp_unop unop; pp_exp e; printf ""
  | Ast.IntConst s -> printf "%s" s
  | Ast.CharConst s -> printf "%s" s
  | Ast.StringConst s -> printf "%s" s
  | Ast.BooleanConst b -> if b then printf "true" else printf "false"
  | Ast.Null -> printf "null"
  | Ast.This -> printf "this"
  | Ast.StaticInvoke (ntype,id,exps)
      -> pp_namedtype ntype;
	 printf ".";
	 pp_identifier id;
	 printf "(";
	 pp_exp_list exps ",";
	 printf ")";
  | Ast.NonstaticInvoke (e,id,exps)
      -> pp_exp e;
	 printf ".";
	 pp_identifier id;
	 printf "(";
	 pp_exp_list exps ",";
	 printf ")";
  | Ast.SimpleInvoke (id,exps)
      -> pp_identifier id;
	 printf "(";
	 pp_exp_list exps ",";
	 printf ")";
  | Ast.AmbiguousInvoke (name,id,exps)
      -> pp_name name;
	 printf ".";
	 pp_identifier id;
	 printf "(";
	 pp_exp_list exps ",";
	 printf ")";
  | Ast.New (typeexp,exps)
      -> printf "new ";
	 pp_typeexp typeexp;
	 printf "(";
	 pp_exp_list exps ",";
	 printf ")";
  | Ast.NewArray (typeexp,exps)
      -> printf "new ";
	 pp_array_elemtype typeexp;
	 printf "[";
	 pp_exp_list exps "][";
	 printf "]";
  | Ast.Lvalue lvalue -> pp_lvalue lvalue
  | Ast.Assignment (lvalue,e)
      -> pp_lvalue lvalue;
	 printf " = ";
	 pp_exp e
  | Ast.IncDec (lvalue,idop)
      -> (match idop with
	  | Ast.PreInc | Ast.PreDec
	    -> pp_inc_dec_op idop; pp_lvalue lvalue
	  | Ast.PostInc | Ast.PostDec
	    -> pp_lvalue lvalue; pp_inc_dec_op idop)
  | Ast.Cast (typeexp,e)
      -> printf "(";
	 pp_typeexp typeexp;
	 printf ") ";
         pp_exp e
  | Ast.Instanceof (e,typeexp)
      -> pp_exp e;
	 printf " instanceof ";
         pp_typeexp typeexp
  (* Expression types added by dOvs students can be handled here ... *)
  | Ast.PossiblyCast (e1,e2)
      -> printf "(PossiblyCast: ";
         pp_exp e1;
	 printf ") ";
	 pp_exp e2
	
  (* Note: Writing a working pretty-printer is not mandatory.
           You may choose to ignore compiler warnings about unexhaustive pattern matches here. *)

and pp_exp_list exps sep = 
  let _ = List.fold_left (fun sep' exp -> printf "%s" sep'; pp_exp exp; sep) "" exps in ()

let pp_exp_opt exp_opt prefix = match exp_opt with
  | None -> ()
  | Some e -> printf "%s" prefix; pp_exp e

let pp_local_decl ldecl =
  let (typeexp,id,exp_opt) = ldecl in
  pp_typeexp typeexp;
  printf " ";
  pp_identifier id;
  pp_exp_opt exp_opt " = "

let pp_formal_param (typeexp,id) =
  pp_typeexp typeexp;
  printf " ";
  pp_identifier id

let pp_formal_param_list formals =
  let _ = List.fold_left (fun sep formal -> printf "%s" sep; pp_formal_param formal; ",") "" formals in ()
  
let indent = "   "

let rec pp_block block prefix =
  printf "%s{" prefix;
  print_newline();
  pp_stm_list block (indent ^ prefix);
  printf "%s}" prefix;
  print_newline()

(* Invariant: pp_stm prints a final newline *)
and pp_stm stm prefix = 
  match stm.Ast.stm with
    | Ast.Exp e
      -> printf "%s" prefix;
	 pp_exp e;
	 printf ";";
	 print_newline()
    | Ast.IfThen (e,s)
      -> printf "%sif (" prefix;
	 pp_exp e;
	 printf ")";
	 print_newline();
	 pp_stm s (indent ^ prefix);
    | Ast.IfThenElse (e,s1,s2)
      -> printf "%sif (" prefix;
	 pp_exp e;
	 printf ")";
	 print_newline();
	 pp_stm s1 (indent ^ prefix);
	 printf "%selse" prefix;
	 print_newline();
	 pp_stm s2 (indent ^ prefix);
    | Ast.While (e,s)
      -> printf "%swhile (" prefix; 
	 pp_exp e;
	 printf ")";
	 print_newline();
	 pp_stm s (indent ^ prefix);
    | Ast.Empty
      -> printf "%s;" prefix;
	 print_newline()
    | Ast.Block block
      -> pp_block block prefix
    | Ast.VoidReturn
      -> printf "%sreturn;" prefix;
	 print_newline()
    | Ast.ValueReturn e
      -> printf "%sreturn " prefix;
	 pp_exp e;
	 printf ";";
	 print_newline()
    | Ast.LocalDecl (typ,id,init)
      -> printf "%s" prefix;
         pp_local_decl (typ,id,init);
	 printf ";";
	 print_newline()
    | Ast.Throw e
      -> printf "%sthrow " prefix;
	 pp_exp e;
	 printf ";";
	 print_newline()
    | Ast.SuperCall exps
      -> printf "%ssuper(" prefix;
	 pp_exp_list exps ",";
	 printf ");";
	 print_newline()
    | Ast.ThisCall exps
      -> printf "%sthis(" prefix;
	 pp_exp_list exps ",";
	 printf ");";
	 print_newline()

and pp_stm_list stms prefix = List.iter (fun s -> pp_stm s prefix) stms

let pp_body body prefix = pp_block body prefix

let pp_body_opt body_opt prefix = match body_opt with
  | None
    -> printf ";"; print_newline()
  | Some body
    -> pp_body body prefix

let pp_package_decl name =
  printf "package ";
  pp_name name;
  printf ";";
  print_newline()

let pp_package_decl_opt name_opt = match name_opt with
  | None -> ()
  | Some name -> pp_package_decl name

let pp_import_decl impdecl =
  printf "import ";
  match impdecl.import_decl with
    | Ast.OnDemand name
      -> pp_name name;
	 printf ".*;";
	 print_newline()
    | Ast.Single (name,id)
      -> pp_name name;
	 printf ".";
	 pp_identifier id;
	 printf ";";
	 print_newline()

let pp_import_decl_list imp_decls = List.iter pp_import_decl imp_decls

let pp_exceptions names = match names with
  | [] -> ()
  | _ -> printf " throws ";
         pp_name_list names

let pp_access access = match access with
  | Public    -> printf "public "
  | Protected -> printf "protected "

let pp_abstract abs = match abs with
    | false -> ()
    | true  -> printf "abstract "

let pp_final final = match final with
    | false -> ()
    | true  -> printf "final "

let pp_static static = match static with
    | false -> ()
    | true  -> printf "static "

let pp_field_decl fdecl prefix = 
  printf "%s" prefix;
  pp_access fdecl.field_access;
  pp_static fdecl.field_static;
  pp_final fdecl.field_final;
  pp_typeexp fdecl.field_type;
  pp_exp_opt fdecl.field_init " = ";
  printf ";";
  print_newline()

let pp_method_decl mdecl prefix = 
  printf "%s" prefix;
  pp_access mdecl.method_access;
  pp_static mdecl.method_static;
  pp_final mdecl.method_final;
  pp_abstract mdecl.method_abstract;
  pp_typeexp mdecl.method_result;
  printf " ";
  pp_identifier mdecl.method_name;
  printf "(";
  pp_formal_param_list mdecl.method_formals;
  printf ")";
  pp_exceptions mdecl.method_throws;
  print_newline();
  pp_body_opt mdecl.Ast.method_body prefix
    
let pp_constructor_decl cdecl prefix = 
  printf "%s" prefix;
  pp_access cdecl.constructor_access;
  pp_identifier cdecl.constructor_name;
  printf "(";
  pp_formal_param_list cdecl.constructor_formals;
  printf ")";
  pp_exceptions cdecl.constructor_throws;
  print_newline();
  pp_body cdecl.constructor_body prefix

let pp_decl decl prefix = match decl.Ast.decl with
  | Ast.Field fdecl -> pp_field_decl fdecl prefix
  | Ast.Method mdecl -> pp_method_decl mdecl prefix
  | Ast.Constructor cdecl -> pp_constructor_decl cdecl prefix

let pp_decl_list decls =
  List.iter (fun decl -> pp_decl decl indent) decls
       
let pp_class_decl cdecl =
  printf "public ";
  pp_final cdecl.Ast.class_final;
  pp_abstract cdecl.Ast.class_abstract;
  printf "class ";
  pp_identifier cdecl.Ast.class_name;
  printf " extends ";
  pp_namedtype cdecl.Ast.class_extends;
  (match cdecl.Ast.class_implements with
    | [] -> ()
    | _ -> printf " implements ";
           pp_name_list cdecl.Ast.class_implements);
  print_newline();
  printf "{";
  print_newline();
  pp_decl_list cdecl.Ast.class_members;
  printf "}"

let pp_interface_decl idecl =
  printf "public interface ";
  (match idecl.Ast.interface_extends with
    | [] -> ()
    | _ -> printf "extends ";
           pp_name_list idecl.Ast.interface_extends);
  print_newline();
  printf "{";
  print_newline();
  pp_decl_list idecl.Ast.interface_members;
  printf "}"

let pp_type_decl type_decl = match type_decl.Ast.type_decl with
  | Ast.Class cdecl -> pp_class_decl cdecl
  | Ast.Interface idecl -> pp_interface_decl idecl

let pp_source_file sf =
  pp_package_decl_opt sf.source_file_package;
  pp_import_decl_list sf.source_file_imports;
  pp_type_decl sf.source_file_decl;
  print_newline()

let pp_program sfs = List.iter pp_source_file sfs
