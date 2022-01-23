open Typecheckingast
module Option = Utils.OptionPlus

let space () = print_string " "

let print_access a =
  match a with
    | Ast.Public ->
      print_string "public"
    | Ast.Protected ->
      print_string "protected"

let string_of_base_type bt =
  match bt with
    | Ast.Void -> "void"
    | Ast.Byte -> "byte"
    | Ast.Short -> "short"
    | Ast.Int -> "int"
    | Ast.Long -> "long"
    | Ast.Char -> "char"
    | Ast.Float -> "float"
    | Ast.Double -> "double"
    | Ast.Boolean -> "boolean"

let rec string_of_typeexp t =
  match t with
    | Types.Base b ->
      string_of_base_type b
    | Types.Array t ->
      (string_of_typeexp t) ^ "[]"
    | Types.Named cn ->
      CanonicalName.to_string cn
    | Types.Null ->
      "null"

let print_inc_dec_op i =
  print_string 
    (match i with
      | Ast.PreInc
      | Ast.PostInc -> "++"
      | Ast.PostDec
      | Ast.PreDec -> "--")

let print_unop u =
  print_string 
    (match u with
      | Negate -> "-"
      | Complement -> "!"
      | BooleanToString
      | ByteToString
      | ShortToString
      | IntToString
      | CharToString
      | ObjectToString -> "")

let print_binop b = 
  print_string
    (match b with
      | Plus -> "+"
      | Minus -> "-"
      | Times -> "*"
      | Divide -> "/"
      | Modulo -> "%"
      | Eq -> "=="
      | Ne -> "!="
      | Lt -> "<"
      | Le -> "<="
      | Gt -> ">"
      | Ge -> ">="
      | And -> "&"
      | Or -> "|"
      | Xor -> "^"
      | LazyAnd -> "&&"
      | LazyOr -> "||"
      | Aeq -> "=="
      | Ane -> "!="
      | Concat -> "+")

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
    | Local id ->
      print_string id.Ast.identifier
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
    | IncDec (l, (Ast.PreInc as op))
    | IncDec (l, (Ast.PreDec as op)) ->
      print_inc_dec_op op;
      print_lvalue l;
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
    | LocalDecl(t, id, None) ->
      print_string indent;
      Typespp.pp_typeexp t;
      print_endline (" " ^ id.Ast.identifier ^ ":" ^ (string_of_int id.Ast.identifier_pos.Lexing.pos_lnum) ^ ";")
    | LocalDecl(t, id, Some e) ->
      print_string indent;
      Typespp.pp_typeexp t;
      print_string (" " ^ id.Ast.identifier ^ ":" ^ (string_of_int id.Ast.identifier_pos.Lexing.pos_lnum) ^ " = ");
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
      print_endline ");"

and print_block indent b = 
  List.iter (print_stm (indent ^ "  ")) b

let print_body b =
  print_block "  " b

let print_formals fs =
  print_string (String.concat
		  ", "
		  (List.map
		     (fun (t, id) ->
		       (string_of_typeexp t) ^ " " ^ id.Ast.identifier ^ ":" ^ (string_of_int id.Ast.identifier_pos.Lexing.pos_lnum))
		     fs))
  
let print_method md =
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
	print_endline " }"
      end)
