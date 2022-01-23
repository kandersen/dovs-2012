(** Pretty printer for types *)

open Printf
open Types

let pp_identifier id = printf "%s" id

let pp_base_type = Astpp.pp_base_type

let rec pp_typeexp t  = match t with
  | Base  base     -> pp_base_type base
  | Array typeexp' -> pp_typeexp typeexp'; printf "[]"
  | Named name     -> printf "%s" (CanonicalName.to_string name)
  | Null           -> printf "null"

let pp_access = Astpp.pp_access

let pp_abstract abs = match abs with
    | false -> ()
    | true  -> printf "abstract "

let pp_final final = match final with
    | false -> ()
    | true  -> printf "final "

let pp_static static = match static with
    | false -> ()
    | true  -> printf "static "

let pp_field_type t  =
  pp_access t.field_access;
  pp_static t.field_static;
  pp_final t.field_final;
  pp_typeexp t.field_type 

let pp_cname cname =
  printf "%s" (CanonicalName.to_string cname)

let pp_formals formals = 
  let _ = List.fold_left (fun sep formal -> printf "%s" sep; pp_typeexp formal; ",") "" formals in ()

let pp_throws throws =
  if [] <> throws then printf " throws ";
  let _ = List.fold_left (fun sep t -> printf "%s" sep; pp_cname t; ",") "" throws in ()

let pp_method_type t  =
  pp_access t.method_access;
  pp_static t.method_static;
  pp_final t.method_final;
  pp_abstract t.method_abstract;
  pp_typeexp t.method_result ;
  printf " ";
  pp_identifier t.method_name;
  printf "(";
  pp_formals t.method_formals ;
  printf ")";
  pp_throws t.method_throws

let pp_constructor_type t =
  pp_access t.constructor_access;
  pp_identifier t.constructor_name;
  printf "(";
  pp_formals t.constructor_formals;
  printf ")";
  pp_throws t.constructor_throws

let pp_member_type t = match t with
  | Field f -> pp_field_type f
  | Method m -> pp_method_type m
  | Constructor c -> pp_constructor_type c

let pp_member_type_list ts =
  List.iter (fun mem -> printf "    "; pp_member_type mem; print_newline()) ts

let pp_cname_list cns =
  let _ = List.fold_left (fun sep cn -> printf "%s" sep; pp_cname cn; ",") "" cns in ()

let pp_class_implements is =
  if [] <> is then printf " implements ";
  pp_cname_list is

let pp_class_type c =
  printf "public ";
  pp_final c.class_final;
  pp_abstract c.class_abstract;
  printf "class ";
  pp_identifier c.class_name;
  printf " extends ";
  pp_cname c.class_extends;
  if [] <> c.class_implements then printf " implements ";
  pp_cname_list c.class_implements;
  printf " {";
  print_newline();
  pp_member_type_list c.class_members;
  printf "}"

let pp_interface_type i =
  printf "public interface ";
  pp_identifier i.interface_name;
  if [] <> i.interface_extends then printf " extends ";
  pp_cname_list i.interface_extends;
  printf " {";
  print_newline();
  pp_member_type_list i.interface_members;
  printf "}"

let pp_named_type nt = match nt with
  | Class c -> pp_class_type c
  | Interface i -> pp_interface_type i

let pp_type_env tenv = 
  Env.iter (fun cname nt -> pp_cname cname; printf ":"; print_newline(); pp_named_type nt; print_newline()) tenv
