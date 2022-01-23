open Resourceast
module Option = Utils.OptionPlus

let space () = print_string " "

let print_access =
  Tastpp.print_access

let rec string_of_typeexp =
  Tastpp.string_of_typeexp

let print_inc_dec_op =
  Tastpp.print_inc_dec_op

let print_unop =
  Tastpp.print_unop
    
let print_binop = 
  Tastpp.print_binop

let rec print_array_args args =
  List.iter (fun arg -> print_string "["; print_exp arg; print_string "]") args

and print_lvalue l =
  match l.lvalue with
    | NonstaticField (e, id, _) ->
      print_exp e;
      print_string ("." ^ id.Ast.identifier)
    | Array(e, e2) ->
      print_exp e;
      print_string "[";
      print_exp e2;
      print_string "]"
    | Local (id, reg) ->
      print_string id.Ast.identifier;
      print_string "{";
      print_string (string_of_int reg);
      print_string "}"
    | StaticField(cn, id, _) ->
      print_string (CanonicalName.to_string cn);
      print_string ("." ^ id.Ast.identifier)
      

and print_exp e =
  match e.exp with
    | Binop(l, op, r) ->
      print_exp l;
      space ();
      print_binop op;
      space ();
      print_exp r
    | Unop(op, e) ->
      print_unop op;
      print_string "(";
      print_exp e;
      print_string ")"
    | IntConst i -> 
      print_string (Int32.to_string i)
    | StringConst s ->
      print_string ("\"" ^ s ^ "\"")
    | BooleanConst b ->
      print_string (string_of_bool b)
    | Null ->
      print_string "null"
    | This ->
      print_string "this"
    | StaticInvoke(t, id, args,_, _) ->
      print_string (CanonicalName.to_string t);
      print_string ("." ^ id.Ast.identifier ^ "(");
      print_args args;
      print_string ")"
    | NonstaticInvoke(e, id, args, _, _) ->
      print_exp e;
      print_string ("." ^ id.Ast.identifier ^ "(");
      print_args args;
      print_string ")"
    | New(t, args, _, _) ->
      print_string "new ";
      print_string (string_of_typeexp t);
      print_string "(";
      print_args args;
      print_string ")"
    | NewArray(t, args) ->
      print_string "new ";
      print_string (string_of_typeexp t);
      print_array_args args
    | Lvalue l ->
      print_lvalue l
    | Assignment(l, e) ->
      print_lvalue l;
      print_string " = ";
      print_exp e
    | IncDec (l, op) ->
      print_lvalue l;
      print_inc_dec_op op
    | Cast(t, e) ->
      print_string "(";
      print_string (string_of_typeexp t);
      print_string ")";
      print_exp e
    | Instanceof(e, t) ->
      print_exp e;
      print_string " instanceof ";
      print_string (string_of_typeexp t)
    | ArrayLength e ->
      print_exp e;
      print_string ".length"
    | ArrayClone e ->
      print_exp e;
      print_string ".clone()"

and print_args args =
  match args with
    | [] ->
      ()
    | [a] ->
      print_exp a
    | a :: args' ->
      print_exp a;
      print_string ", ";
      print_args args'

let rec print_stm indent s = 
  match s.stm with
    | Exp e ->
      print_string indent;
      print_exp e;
      print_endline ";"
    | While(e, s) ->
      print_string (indent ^ "while (");
      print_exp e;
      print_endline ")";
      print_stm (indent ^ "") s
    | Empty ->
      print_endline (indent ^ "<empty>")
    | IfThen(e, s) ->
      print_string (indent ^ "if (");
      print_exp e;
      print_endline ")";
      print_stm (indent ^ "") s
    | IfThenElse(e, s1, s2) ->
      print_string (indent ^ "if (");
      print_exp e;
      print_endline ")";
      print_stm indent s1;
      print_endline "else";
      print_stm indent s2;
    | LocalDecl(t, id, None, register) ->
      print_string indent;
      Typespp.pp_typeexp t;
      space ();
      print_string id.Ast.identifier;
      print_string "{";
      print_string (string_of_int register);
      print_endline "};"
    | LocalDecl(t, id, Some e, register) ->
      print_string indent;
      Typespp.pp_typeexp t;
      space ();
      print_string id.Ast.identifier;
      print_string "{";
      print_string (string_of_int register);
      print_string "} = ";
      print_exp e;
      print_endline ";"
    | Block b ->
      print_endline (indent ^ "{");
      print_block (indent ^ "  ") b;
      print_endline (indent ^ "}")
    | VoidReturn ->
      print_endline (indent ^ "return;")
    | ValueReturn e ->
      print_string (indent ^ "return ");
      print_exp e;
      print_endline ";"
    | Throw e ->
      print_string (indent ^ "throw ");
      print_exp e;
      print_endline ";"
    | ThisCall (args, _, _)
    | SuperCall (args, _, _) ->
      print_string (indent ^ "super(");
      print_args args;
      print_endline ");";

and print_block indent b = 
  List.iter (print_stm (indent ^ "  ")) b

let print_body b =
  print_block "  " b

let print_formals fs =
  print_string (String.concat
		  ", "
		  (List.map
		     (fun (t, id, reg) ->
		       (string_of_typeexp t)
		       ^ " "
		       ^ id.Ast.identifier
		       ^ "{"
		       ^ (string_of_int reg)
		       ^ "}")
		     fs))
  
let print_method md =
  print_string "  ";
  print_access md.method_access;
  print_string " ";
  if md.method_static then print_string "static " else ();
  if md.method_final then print_string "final " else ();
  if md.method_abstract then print_string "abstract " else ();
  Typespp.pp_typeexp md.method_result;
  print_string " ";
  print_string md.method_name.Ast.identifier;
  print_string "(";
  print_formals md.method_formals;
  print_string ")";
  (if Option.is_none md.method_body then print_endline ";"
   else
      begin
	print_endline " {";
	Option.map_default print_body () md.method_body;
	print_endline "  }"
      end)

let print_constructor cd = 
  print_string "  ";
  print_access cd.constructor_access;
  space ();
  print_string cd.constructor_name.Ast.identifier;
  print_string "(";
  print_formals cd.constructor_formals;
  print_string ")";
  print_endline " {";
  print_body cd.constructor_body;
  print_endline "  }"

let print_field fd = 
  print_string "  ";
  print_access fd.field_access;
  space();
  if fd.field_static then print_string "static " else ();
  if fd.field_final then print_string "final " else ();  
  Typespp.pp_typeexp fd.field_type;
  space();
  print_string fd.field_name.Ast.identifier;
  (if Option.is_none fd.field_init then print_endline ";"
   else
      begin
	print_string " = ";
	Option.map_default print_exp () fd.field_init;
	print_endline ";"
      end)
  
let print_decl { decl = d } =
  print_endline "";
  match d with
    | Field fd ->
      print_field fd
    | Method md ->
      print_method md
    | Constructor cd ->
      print_constructor cd

let print_class cd =
  print_string "public ";
  if cd.class_abstract then print_string "abstract " else ();
  if cd.class_final then print_string "final " else ();
  print_string "class ";
  print_string cd.class_name.Ast.identifier;
  print_string " extends ";
  print_string (CanonicalName.to_string cd.class_extends);
  (match cd.class_implements with
    | [] ->
      ()
    | _ ->
      print_string " implements ";
      print_string (String.concat ", " (List.map CanonicalName.to_string cd.class_implements)));
  print_endline " {";
  List.iter print_decl cd.class_members;
  print_endline "}"

let print_interface id = 
  print_string "public interface ";
  print_string id.interface_name.Ast.identifier;
  (match id.interface_extends with
    | [] ->
      ()
    | _ ->
      print_string " extends ";
      print_string (String.concat ", " (List.map CanonicalName.to_string id.interface_extends)));
  print_endline " {";
  List.iter print_decl id.interface_members;
  print_endline "}"

let print_source_file sf =
  match sf.source_file_decl.type_decl with
    | Class cd ->
      print_class cd
    | Interface id ->
      print_interface id

let print_program =
  List.iter print_source_file
