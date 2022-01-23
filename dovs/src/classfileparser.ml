(** *)

(**/**) (*/*)

module WAst = Weedingast
module StringMap = Map.Make (String)

open Javalib_pack

let make_identifier id = id
let make_name name cname = cname
let make_typeexp te = te

(* merges makeType:String and makeName *)
let make_named_type jb_clname = 
  let strname = JBasics.cn_name jb_clname in
  let strs = Str.split (Str.regexp_string ".") strname in
  let _ids = List.map make_identifier strs in
  make_name ((* Ast.Qualified ids *)) (CanonicalName.make strname)

(*  make_basictype : JBasics.java_basic_type -> Ast.typeexp_desc *)
let make_basictype (t:JBasics.java_basic_type) =
  match t with
    | `Bool -> Ast.Boolean
    | `Byte -> Ast.Byte
    | `Char -> Ast.Char
    | `Double -> Ast.Double
    | `Float -> Ast.Float
    | `Int -> Ast.Int
    | `Long -> Ast.Long
    | `Short -> Ast.Short

(*  make_objecttype : JBasics.java_object_type -> Ast.typeexp_desc *)
let rec make_objecttype t =
  match t with
    | JBasics.TClass cname -> 
(*      let clname = JBasics.cn_name cname in*)
      let named_type = make_named_type cname in
      Types.Named named_type
    | JBasics.TArray vtype -> 
      let elemtype = make_type vtype in
      Types.Array elemtype

(*  make_type : JBasics.value_type -> Ast.typeexp *)
and make_type t =
  let texp_desc = match t with
    | JBasics.TBasic btype -> Types.Base (make_basictype btype)
    | JBasics.TObject otype -> make_objecttype otype
  in
  make_typeexp texp_desc

(* The original make_identifier is handled by JBasics.cn_simple_name *)

exception NothingCreated

let make_access ac = match ac with
    | `Protected -> Ast.Protected
    | `Public -> Ast.Public
    | _ -> raise NothingCreated

let make_initval_opt cval typ = match cval with
  | None -> None
  | Some cval -> match cval with
      | JBasics.ConstString s -> Some (Types.String (JBasics.jstr_raw s))
	(* Do other types beside String have ConstString initializers? *)
      | JBasics.ConstInt i32 -> 
	(match typ with
	  | Types.Base (Ast.Boolean) -> Some (Types.Bool (i32 != 0l))
	  | Types.Base (Ast.Int) -> Some (Types.Int i32)
	  | _ -> None)
      | JBasics.ConstFloat _ -> None
      | JBasics.ConstLong _ -> None
      | JBasics.ConstDouble _ -> None
      | JBasics.ConstClass _ -> None

let make_field fieldsig afield =
  let access = make_access (Javalib.get_field_visibility afield) in
  let static = Javalib.is_static_field afield in
  let final = Javalib.is_final_field afield in
(*  let fieldsig = Javalib.get_field_signature afield in *)
  let typ = make_type (JBasics.fs_type fieldsig) in
  let fname = JBasics.fs_name fieldsig in
(*  let () = Printf.printf "Field name: %s\n" fname in *)
  let id = make_identifier fname in
  let jlibval_opt = match afield with
    | Javalib.InterfaceField ifield -> ifield.Javalib.if_value
    | Javalib.ClassField cfield -> cfield.Javalib.cf_value in
  let constval_opt = make_initval_opt jlibval_opt typ in
  let fdecl = { Types.field_access = access;
                Types.field_static = static;
                Types.field_final = final;
                Types.field_type = typ;
                Types.field_name = id;
		Types.field_constval = constval_opt
	      }
  in 
    (Types.Field fdecl,fname,fdecl)

let make_field_decls clss fieldmap accum =
  JBasics.FieldMap.fold
    (fun fieldsig anyfield (acc,fenv) -> 
      try 
	let decl,fname,fdecl = make_field fieldsig anyfield in
	(decl::acc, StringMap.add fname fdecl fenv)
      with NothingCreated -> (acc,fenv)) fieldmap (accum,StringMap.empty)

let make_method clss methodsig jmethod =
  let access = make_access (Javalib.get_method_visibility jmethod) in
  let argtypes = JBasics.ms_args methodsig in
  let formals = 
 (* List.map (fun at -> (make_type at,make_dummy_name(),None)) argtypes *)
    List.map make_type argtypes in
  let jexceptions = match jmethod with
    | Javalib.AbstractMethod am -> am.Javalib.am_exceptions
    | Javalib.ConcreteMethod cm -> cm.Javalib.cm_exceptions in
  let exceptions = 
    List.map (fun cn -> make_named_type cn) jexceptions in
  match JBasics.ms_name methodsig with
    | "<clinit>"
      -> (* Static initializer *)
         raise NothingCreated
    | "<init>"
      -> (* Constructor *)
         let id_desc = JBasics.cn_simple_name (Javalib.get_name clss) in
	 let cname = make_identifier id_desc in
	 let cons_decl = { Types.constructor_access = access;
                           Types.constructor_name = cname;
                           Types.constructor_formals = formals;
                           Types.constructor_throws = exceptions }
         in (Types.Constructor cons_decl, None)
    | mid
      -> (* Ordinary method *)
         let static = Javalib.is_static_method jmethod in
	 let final = Javalib.is_final_method jmethod in
	 let abstract = match jmethod with
	   | Javalib.AbstractMethod _ -> true
	   | Javalib.ConcreteMethod _ -> false in
	 let rettype = match JBasics.ms_rtype methodsig with
	   | Some typ -> make_type typ
	   | None -> Types.Base (make_typeexp Ast.Void) in
	 let method_id = make_identifier mid in
	 let method_decl = { Types.method_access = access;
                             Types.method_static = static;
                             Types.method_final = final;
                             Types.method_abstract = abstract;
			     Types.method_result = rettype;
                             Types.method_name = method_id;
                             Types.method_formals = formals;
                             Types.method_throws = exceptions } in
	 (Types.Method method_decl, Some (mid,method_decl))

let make_method_decls clss methodmap accum =
  JBasics.MethodMap.fold
    (fun methodsig jmethod acc ->
      try 
	let decl, method_opt = make_method clss methodsig jmethod in
	(* match method_opt with *)
	(*   | None -> (decl::acc,menv) (\* adding constructor decl *\) *)
	(*   | Some (mid,mdecl) -> *)
	(*     let old_set =  *)
	(*       if StringMap.mem mid menv *)
	(*       then StringMap.find mid menv *)
	(*       else MethodDeclSet.empty in *)
	(*     let new_set = MethodDeclSet.add mdecl old_set in *)
	(*     let menv' = StringMap.add mid new_set menv in *)
	(*     (decl::acc,menv') (\* adding method decl to both decl list and menv *\) *)
          decl :: acc
      with NothingCreated -> acc) methodmap accum

let make_decls clss = 
  let fieldmap = Javalib.get_fields clss in
  let field_decls,fenv = make_field_decls clss fieldmap [] in
  let methodmap = Javalib.get_methods clss in
  let field_and_method_decls (*,menv *) = make_method_decls clss methodmap field_decls in
  (field_and_method_decls(* ,fenv,menv *))
(*  let methods = Javalib.get_methods clss in *)

let make_interface_list jb_names = List.map make_named_type jb_names

let make_interface_decl clss ji =
  let jb_name = Javalib.get_name clss in
  let iname_simpl = make_identifier (JBasics.cn_simple_name jb_name) in
  let iname = JBasics.cn_name jb_name in
  let named_interfaces = make_interface_list ji.Javalib.i_interfaces in
  let decls(* ,fenv,menv *) = make_decls clss in
  let idecl = { Types.interface_name = iname_simpl;
                Types.interface_extends = named_interfaces;
                Types.interface_members = decls } in
  (Types.Interface idecl, iname(* , fenv, menv *))

let make_class_decl clss jc =
  let jb_name = Javalib.get_name clss in
  let cname_simpl = make_identifier (JBasics.cn_simple_name jb_name) in
  let cname = JBasics.cn_name jb_name in
  let final = jc.Javalib.c_final in
  let abstract = jc.Javalib.c_abstract in
  let jsupercl_opt = jc.Javalib.c_super_class in
  let supercl = match jsupercl_opt with
    | None -> CanonicalName.make "java.lang.Object"
    | Some jscl -> make_named_type jscl in
  let named_interfaces = make_interface_list jc.Javalib.c_interfaces in
  let decls(* ,fenv,menv *) = make_decls clss in
  let cdecl = { Types.class_final = final;
                Types.class_abstract = abstract;
                Types.class_name = cname_simpl;
                Types.class_extends = supercl;
                Types.class_implements = named_interfaces;
                Types.class_members = decls } in
  (Types.Class cdecl, cname(* , fenv, menv *))

(**/**) (*/*)

exception Non_Public

(** Parses and builds a [type_decl] from a given classpath and filename
     @param class_path  the classpath to search
     @param filename    the filename containing the type
     @raise Non_Public  if the resulting [type_decl] is not [public].
 *)
let make_type_decl class_path filename =
  let class_path = Javalib.class_path class_path in (* open cp *)
  try
    let class_name = JBasics.make_cn filename in
    let clss = Javalib.get_class class_path class_name in
    let () = Javalib.close_class_path class_path in   (* close cp *)
    match Javalib.get_access clss with
      | `Default -> raise Non_Public 
      | `Public ->
	let type_decl_desc,cname(* ,fenv,menv *) = match clss with
	  | Javalib.JInterface ji -> make_interface_decl clss ji
	  | Javalib.JClass jc -> make_class_decl clss jc in
	type_decl_desc
  (* { Types.type_decl_pos = dummy_pos; *)
  (*   Types.type_decl = type_decl_desc; *)
  (*   Types.canonical_name = Types.CanonicalName cname(\* ; *\) *)
  (*     (\* Types.field_env = fenv; *\) *)
  (*     (\* Types.method_env = menv *\) } *)
  with (JBasics.No_class_found _) -> 
  let () = Javalib.close_class_path class_path in
  raise Not_found
