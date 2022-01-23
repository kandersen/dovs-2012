(** Compiler phase to peephole optimize instruction sequences *)

module CAst = Codegenerationast
module Inst = Instruction

let apply_pattern p ops is = (snd p) ops is

let rec apply_patterns_here ps ops is = match ps with
  | [] -> None (* no more patterns to apply *)
  | p::ps' ->
    (match apply_pattern p ops is with
      | None -> apply_patterns_here ps' ops is (* no click *)
      | Some is' -> 
	begin

	  Printf.printf "Pattern %s clicked\n" (fst p);

	  Some is' (*optimize allocation?*)
	end)

let rec apply_patterns ps ops is = match is with
  | [] -> None
  | i::is' -> 
    (match apply_patterns_here ps ops is with
      | None -> 
	(match apply_patterns ps ops is' with (* no change, try next position *)
	  | None -> None
	  | Some is'' -> Some (i::is'')) (* propagate click *)
      | Some is -> Some is)

module TargetMap = StringMap
module DegreeMap = StringMap

let rec opt_body body = 
  (* build maps --- naive approach: rebuild after every click *)
  let lmap,dmap = 
    let rec build_label_map is lmap dmap = match is with
      | [] -> lmap, dmap
      | i::is -> (match i with
	  | Inst.Ilabel l -> 
	    build_label_map is (TargetMap.add l is lmap) dmap
	  | _ -> 
	    (match Inst.can_jump i with
	      | Some l -> 
		let newcount = (match DegreeMap.lookup l dmap with
		                 | Some c -> c+1
				 | None -> 1) in
		build_label_map is lmap (DegreeMap.add l newcount dmap)
	      | None -> build_label_map is lmap dmap))
    in
    build_label_map body TargetMap.empty DegreeMap.empty
  in
  (* build record of operations *)
  let ops = 
    { Peephole.degree  = (fun lab -> try DegreeMap.find lab dmap with Not_found -> 0);
      Peephole.target  = (fun lab -> TargetMap.find lab lmap);
      Peephole.formals = (fun msig -> msig.Inst.method_nargs);
      Peephole.returns = (fun msig -> msig.Inst.method_nreturns);
      Peephole.negate  = Inst.negate;
      Peephole.commute = Inst.commute; }
  in
  match apply_patterns Peephole.mypatterns ops body with (* optimize *)
    | None -> body
    | Some body' -> opt_body body' (* rebuild maps *)

let opt_body_opt body = match body with
  | None -> None
  | Some body ->
    let body' = opt_body body in
    Some body'

let opt_method_decl mdecl =
  let body' = opt_body_opt mdecl.CAst.method_body in
  { mdecl with CAst.method_body = body' }

let opt_constructor_decl cdecl = 
  let body' = opt_body cdecl.CAst.constructor_body in
  { cdecl with CAst.constructor_body = body' }

let opt_decl decl = match decl.CAst.decl with
  | CAst.Field _ -> decl
  | CAst.Method mdecl ->
    let mdecl' = opt_method_decl mdecl in
    { decl with CAst.decl = CAst.Method mdecl' }
  | CAst.Constructor cdecl ->
    let cdecl' = opt_constructor_decl cdecl in
    { decl with CAst.decl = CAst.Constructor cdecl' }

let opt_decls decls = List.map opt_decl decls

let opt_class_decl cdecl =
  let decls' = opt_decls cdecl.CAst.class_members in
  { cdecl with CAst.class_members = decls' }

let opt_type_decl tdecl = match tdecl.CAst.type_decl with
  | CAst.Class cdecl -> 
    let cdecl' = opt_class_decl cdecl in
    { tdecl with CAst.type_decl = CAst.Class cdecl' }
  | CAst.Interface _ -> tdecl

let opt_source_file src_file =
  let tdecl' = opt_type_decl src_file.CAst.source_file_decl in
  { src_file with CAst.source_file_decl = tdecl' }

let opt_program prog = List.map opt_source_file prog
