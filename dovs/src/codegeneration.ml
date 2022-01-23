(** Compiler phase to generate Java bytecode for all method bodies. *)

module NAst = Nameresolvingast
module TAst = Typecheckingast
module RAst = Resourceast
module CAst = Codegenerationast
module Inst = Instruction
module ListPlus = Utils.ListPlus
module T = Types

(************************************************************************)
(** {2 Helper Functions }                                               *)
(************************************************************************)

let is_constant e = match e.RAst.exp with
  | RAst.IntConst _ 
  | RAst.StringConst _
  | RAst.BooleanConst _ -> true
  | _ -> false

let has_some a : bool =
  match a with
  | None -> false
  | Some a' -> true

let is_constant_option exp_option : bool =
  match exp_option with
  | None -> false
  | Some exp -> is_constant exp

let sig_of_cname = Types.cname_to_sig

let desc_of_typeexp = Types.typeexp_to_desc
    
let create_if_compare c true_list false_list =
  let end_lbl = Inst.make_label "end" in
  match false_list with
  | [] ->
    List.concat [[Inst.Iifcmp (c,end_lbl)];
		 true_list;
		 [Inst.Ilabel end_lbl];
		 [Inst.Inop]]
  | _ ->
    let false_lbl = Inst.make_label "false" in
    List.concat [[Inst.Iifcmp (c,false_lbl)];
		 true_list;
		 [Inst.Igoto end_lbl];
		 [Inst.Ilabel false_lbl];
		 false_list;
		 [Inst.Ilabel end_lbl];
		 [Inst.Inop]]
      
let sig_of_type texp =
  match texp with
  | Types.Named cn -> sig_of_cname cn
  | _ -> Types.typeexp_to_desc texp

let is_named_type_interface nt info : bool =
  let named_type = Hierarchy.lookup_named_type nt info in
  Types.is_interface named_type
      
let codegen_field_sig base id =
    (sig_of_cname base) ^ "/" ^ id.Ast.identifier
 
let codegen_method_sig_known msig numargs numreturns =
  { Inst.method_sig      = msig;
    Inst.method_nargs    = numargs;
    Inst.method_nreturns = numreturns
  }

let codegen_method_sig id base m =
  let mname   = id.Ast.identifier in
  let basesig = sig_of_cname base in
  let argsigs = List.map desc_of_typeexp m.Types.method_formals in
  let ressig  = desc_of_typeexp m.Types.method_result in
  let numargs = List.length m.Types.method_formals in
  let numreturns = if m.Types.method_result = Types.Base Ast.Void then 0 else 1 in
  let msig =
    basesig ^ "/" ^ mname ^ "(" ^ (String.concat "" argsigs) ^ ")" ^ ressig in
  codegen_method_sig_known msig numargs numreturns

let codegen_constructor_sig base c =
    let basesig = sig_of_cname base in
    let argsigs = List.map desc_of_typeexp c.Types.constructor_formals in
    let numargs = List.length c.Types.constructor_formals in
    let msig    = basesig ^ "/<init>(" ^ (String.concat "" argsigs) ^ ")V" in
    codegen_method_sig_known msig numargs 0

let codegen_cond op = match op with 
  | TAst.Eq -> Inst.Eq
  | TAst.Ne -> Inst.Ne
  | TAst.Lt -> Inst.Lt
  | TAst.Le -> Inst.Le
  | TAst.Gt -> Inst.Gt
  | TAst.Ge -> Inst.Ge
  | TAst.Aeq -> Inst.Aeq
  | TAst.Ane -> Inst.Ane
  | _ -> (Error.internal_compiler_error "Illegal cond in binary operation")      

(************************************************************************)
(** {2 Code Generation Traversal }                                      *)
(************************************************************************)

type info = { tenv  : Types.type_env;
	      fields : RAst.field_decl list;
	      stringbuffer : bool;
	      true_label : Inst.label option;
	      false_label : Inst.label option}

let rec codegen_exp exp (info : info) : Inst.instruction list = match exp.RAst.exp with
  | RAst.IntConst i ->
    [Inst.Ildc_int i]
  | RAst.BooleanConst b ->
    (match b,info.true_label,info.false_label with
    | true, Some l, _ -> [Inst.Igoto l]
    | false, _, Some l -> [Inst.Igoto l]
    | _ -> [Inst.Ildc_int (if b then 1l else 0l)])
  | RAst.Null ->
    [Inst.Iaconst_null]
  | RAst.This ->
    [Inst.Iaload 0]
  | RAst.StaticInvoke (nt,id,es,base,m) ->
    let ies = List.concat (List.map (fun e -> codegen_exp e info) es) in
    List.concat [ies;
		 [Inst.Iinvokestatic (codegen_method_sig id base m)]]
  | RAst.NonstaticInvoke (exp,id,es,base,m) ->
    let ie = codegen_exp exp info in
    let ies = List.concat (List.map (fun e -> codegen_exp e info) es) in
    let instruction = if is_named_type_interface base info.tenv 
      then Inst.Iinvokeinterface (codegen_method_sig id base m)
      else Inst.Iinvokevirtual (codegen_method_sig id base m) in
    List.concat [ie;
		 ies; 
		 [instruction]]
  | RAst.New (type_exp, es, base, ct) ->
    let sig_of_name = sig_of_cname base in
    let ies = List.concat (List.map (fun e -> codegen_exp e info) es) in
    List.concat [[Inst.Inew sig_of_name];
		 [Inst.Idup];
		 ies;
		 [Inst.Iinvokespecial (codegen_constructor_sig base ct)]]
  | RAst.NewArray (type_exp, es) ->
    let dims = List.length es in
    let ies = List.concat (List.map (fun e -> codegen_exp e info) es) in
    let desc = desc_of_typeexp type_exp in
    List.concat [ies;
		[Inst.Imultianewarray (desc,dims)]]
  | RAst.Lvalue lval ->
    codegen_lval lval info
  | RAst.Assignment (lval, exp) ->
    let lval_type = lval.RAst.lvalue_type in
    let desc = desc_of_typeexp lval_type in
    let codegen_exp_assign = codegen_exp exp info in
    begin
      match lval.RAst.lvalue with
      | RAst.Local (id,reg) ->
	List.concat [codegen_exp_assign;
		     [Inst.Idup];
		     if Types.is_reference lval_type
		     then [Inst.Iastore reg]
		     else [Inst.Iistore reg]]
      | RAst.NonstaticField (exp1,id,base) ->
	let codegen_exp1 = codegen_exp exp1 info in
	List.concat [codegen_exp1;
		     codegen_exp_assign;
		     [Inst.Idup_x1];
		     [Inst.Iputfield ((codegen_field_sig base id) ^ " " ^ desc)]]
      | RAst.StaticField (nt,id,base) ->
	List.concat [codegen_exp_assign;
		     [Inst.Idup];
		     [Inst.Iputstatic ((codegen_field_sig base id) ^ " " ^ desc)]]
      | RAst.Array (exp1, exp2) ->
	List.concat [codegen_exp exp1 info;
		     codegen_exp exp2 info;
		     codegen_exp_assign;
		     [Inst.Idup_x2];
		     [match lval_type with
		     | Types.Base Ast.Int -> Inst.Iiastore
		     | Types.Base Ast.Boolean
		     | Types.Base Ast.Byte -> Inst.Ibastore
		     | Types.Base Ast.Short -> Inst.Isastore
		     | Types.Base Ast.Char -> Inst.Icastore
		     | _ -> Inst.Iaastore]]
    end
  | RAst.Cast (type_exp, exp) ->    
    let exp = codegen_exp exp info in
    List.concat [exp;
		 [match type_exp with
		 | Types.Base Ast.Char -> Inst.Ii2c
		 | Types.Base Ast.Short -> Inst.Ii2s
		 | Types.Base Ast.Byte -> Inst.Ii2b
		 | Types.Base Ast.Int -> Inst.Inop
		 | _ -> Inst.Icheckcast (sig_of_type type_exp)]]
  | RAst.Instanceof (exp, type_exp) ->    
    let exp = codegen_exp exp info in
    List.concat [exp;
		 [Inst.Iinstanceof (sig_of_type type_exp)]]
  | RAst.ArrayLength exp ->
    let exp = codegen_exp exp info in
    List.concat [exp;
		 [Inst.Iarraylength]]
  | RAst.ArrayClone exp ->
    let e = codegen_exp exp info in
    let sig_type = sig_of_type exp.RAst.exp_type in
    List.concat [e;
		 [Inst.Iinvokevirtual (codegen_method_sig_known (sig_type ^ "/clone()LJava/lang/Object;") 0 1)]]
  | RAst.IncDec (lval,op) ->
    let lval_load = codegen_lval lval info in
    let lval_type = lval.RAst.lvalue_type in
    let desc = desc_of_typeexp lval_type in
    let is_pre = match op with
      | Ast.PostInc
      | Ast.PostDec -> false
      | Ast.PreInc 
      | Ast.PreDec -> true in
    let incDecInstructions = [Inst.Ildc_int 1l;
			      (match op with
			      | Ast.PostInc
			      | Ast.PreInc -> Inst.Iiadd
			      | Ast.PostDec
			      | Ast.PreDec -> Inst.Iisub)] in
    (match lval.RAst.lvalue with
    | RAst.Local (id,reg) ->
      List.concat [lval_load;
		   if is_pre then [] else [Inst.Idup];
		   incDecInstructions;
		   if is_pre then [Inst.Idup] else [];
		   if Types.is_reference lval_type
		   then [Inst.Iastore reg]
		   else [Inst.Iistore reg]]
    | RAst.NonstaticField (exp1,id,base) ->
      let codegen_exp = codegen_exp exp1 info in
      List.concat [codegen_exp;
		   [Inst.Idup];
		   [Inst.Igetfield ((codegen_field_sig base id) ^ " " ^ desc)];
		   if is_pre then [] else [Inst.Idup_x1];
		   incDecInstructions;
		   if is_pre then [Inst.Idup_x1] else [];
		   [Inst.Iputfield ((codegen_field_sig base id) ^ " " ^ desc)]]
    | RAst.StaticField (nt,id,base) ->
      List.concat [lval_load;
		   if is_pre then [] else [Inst.Idup];
		   incDecInstructions;
		   if is_pre then [Inst.Idup] else [];
		   [Inst.Iputstatic ((codegen_field_sig base id) ^ " " ^ desc)]]
    | RAst.Array (exp1, exp2) ->
      List.concat [codegen_exp exp1 info;
		   codegen_exp exp2 info;
		   [Inst.Idup2];
		   [match lval_type with
		   | Types.Base Ast.Int -> Inst.Iiaload
		   | Types.Base Ast.Boolean
		   | Types.Base Ast.Byte -> Inst.Ibaload
		   | Types.Base Ast.Short -> Inst.Isaload
		   | Types.Base Ast.Char -> Inst.Icaload
		   | _ -> Inst.Iaaload];
		   if is_pre then [] else [Inst.Idup_x2];
		   incDecInstructions;
		   if is_pre then [Inst.Idup_x2] else [];
		   [match lval_type with
		   | Types.Base Ast.Int -> Inst.Iiastore
		   | Types.Base Ast.Boolean
		   | Types.Base Ast.Byte -> Inst.Ibastore
		   | Types.Base Ast.Short -> Inst.Isastore
		   | Types.Base Ast.Char -> Inst.Icastore
		   | _ -> Inst.Iaastore]])
  | RAst.StringConst str ->
    [Inst.Ildc_string str]
  | RAst.Unop (unop, exp) -> 
    codegen_unop unop exp info
  | RAst.Binop (exp1,binop,exp2) ->
    codegen_binop binop exp1 exp2 info
      
and codegen_lval lval (info : info) : Inst.instruction list =
  let lval_type = lval.RAst.lvalue_type in
  let desc = desc_of_typeexp lval_type in
  match lval.RAst.lvalue with
  | RAst.Local (id,reg) ->
	if Types.is_reference lval_type
	then [Inst.Iaload reg]
	else [Inst.Iiload reg]
  | RAst.NonstaticField (exp,id,base) ->
    let codegen_exp = codegen_exp exp info in
    List.concat [codegen_exp;
		 [Inst.Igetfield ((codegen_field_sig base id) ^ " " ^ desc)]]
  | RAst.StaticField (nt,id,base) ->
    [Inst.Igetstatic ((codegen_field_sig base id) ^ " " ^ desc)]
  | RAst.Array (exp1, exp2) ->
    List.concat [codegen_exp exp1 info;
		 codegen_exp exp2 info;
		 [match lval_type with
		 | Types.Base Ast.Int -> Inst.Iiaload
		 | Types.Base Ast.Boolean
		 | Types.Base Ast.Byte -> Inst.Ibaload
		 | Types.Base Ast.Short -> Inst.Isaload
		 | Types.Base Ast.Char -> Inst.Icaload
		 | _ -> Inst.Iaaload]]
      

and codegen_binop binop exp1 exp2 (info : info) : Inst.instruction list =
   match binop with
      | TAst.Plus   ->
	let ie1 = codegen_exp exp1 info in
	let ie2 = codegen_exp exp2 info in
	List.concat [ie1;ie2;[Inst.Iiadd]]
      | TAst.Minus  -> 
	let ie1 = codegen_exp exp1 info in
	let ie2 = codegen_exp exp2 info in
	List.concat [ie1;ie2;[Inst.Iisub]]
      | TAst.Times  -> 
	let ie1 = codegen_exp exp1 info in
	let ie2 = codegen_exp exp2 info in
	List.concat [ie1;ie2;[Inst.Iimul]]
      | TAst.Divide -> 
	let ie1 = codegen_exp exp1 info in
	let ie2 = codegen_exp exp2 info in
	List.concat [ie1;ie2;[Inst.Iidiv]]
      | TAst.Modulo -> 
	let ie1 = codegen_exp exp1 info in
	let ie2 = codegen_exp exp2 info in
	List.concat [ie1;ie2;[Inst.Iirem]]
      | TAst.And    -> 
	let ie1 = codegen_exp exp1 info in
	let ie2 = codegen_exp exp2 info in
	List.concat [ie1;ie2;[Inst.Iiand]]
      | TAst.Or     -> 
	let ie1 = codegen_exp exp1 info in
	let ie2 = codegen_exp exp2 info in
	List.concat [ie1;ie2;[Inst.Iior]]
      | TAst.Xor    -> 
	let ie1 = codegen_exp exp1 info in
	let ie2 = codegen_exp exp2 info in
	List.concat [ie1;ie2;[Inst.Iixor]]
      | TAst.Eq    
      | TAst.Ne    
      | TAst.Lt
      | TAst.Le
      | TAst.Gt
      | TAst.Ge
      | TAst.Aeq
      | TAst.Ane    ->
	let ie1 = codegen_exp exp1 info in
	let ie2 = codegen_exp exp2 info in
	List.concat [ie1;ie2;
		     create_if_compare (codegen_cond binop) [Inst.Ildc_int 0l] [Inst.Ildc_int 1l]]
      | TAst.Concat ->
	if !Globals.joos1
	then (let msig = "java/lang/String/concat(Ljava/lang/String;)Ljava/lang/String;" in
	      let ie1 = codegen_exp exp1 info in
	      let ie2 = codegen_exp exp2 info in
	      List.concat [ie1;ie2;
			   [Inst.Iinvokevirtual (codegen_method_sig_known msig 1 1)]])
	else (let class_sig = "java/lang/StringBuffer" in
	      let construct_sig = codegen_method_sig_known "java/lang/StringBuffer/<init>()V" 0 0 in
	      let toString = codegen_method_sig_known "java/lang/StringBuffer/toString()Ljava/lang/String;" 0 1  in
	      let ie1_appended = match exp1.RAst.exp with
		| RAst.StringConst _ ->
		  let ie1 = codegen_exp exp1 info in
		  List.concat [ie1;
				[Inst.Iinvokevirtual (codegen_method_sig_known "java/lang/StringBuffer/append(Ljava/lang/String;)Ljava/lang/StringBuffer;" 1 1)]]
		| RAst.Binop (_,TAst.Concat,_) -> codegen_exp exp1 {info with stringbuffer = true}		  
		| _ -> codegen_exp exp1 {info with stringbuffer = false} in
	      let ie2_appended = match exp2.RAst.exp with
		| RAst.StringConst _ ->
		  let ie2 = codegen_exp exp2 info in
		  List.concat [ie2;
			       [Inst.Iinvokevirtual (codegen_method_sig_known "java/lang/StringBuffer/append(Ljava/lang/String;)Ljava/lang/StringBuffer;" 1 1)]]
		| RAst.Binop (_,TAst.Concat,_) -> codegen_exp exp2 {info with stringbuffer = true}		  
		| _ -> codegen_exp exp2 {info with stringbuffer = false} in
	      List.concat [(if info.stringbuffer
		then []
		else [Inst.Inew class_sig;Inst.Idup;Inst.Iinvokespecial construct_sig]);
			   ie1_appended;
			   ie2_appended;
			   (if info.stringbuffer
			    then [] 
			    else [Inst.Iinvokevirtual toString])])
      | TAst.LazyAnd ->
	let true_lbl = Inst.make_label "true" in
	let false_lbl = Inst.make_label "false" in
	let end_lbl = Inst.make_label "end" in
	let firsttrue_lbl = Inst.make_label "firsttrue" in
	let ie1 = codegen_exp exp1 info in
	let ie2 = codegen_exp exp2 info in
	List.concat [ie1;
		     [Inst.Iif (Inst.Ne, firsttrue_lbl);
		      Inst.Igoto false_lbl;
		      Inst.Ilabel firsttrue_lbl];
		     ie2;
		     [Inst.Iif (Inst.Ne, true_lbl);
		      Inst.Igoto false_lbl;
		      Inst.Ilabel true_lbl;
		      Inst.Ildc_int 1l;
		      Inst.Igoto end_lbl;
		      Inst.Ilabel false_lbl;
		      Inst.Ildc_int 0l;
		      Inst.Ilabel end_lbl]]
      | TAst.LazyOr ->
	let true_lbl = Inst.make_label "true" in
	let false_lbl = Inst.make_label "false" in
	let end_lbl = Inst.make_label "end" in
	let firstfalse_lbl = Inst.make_label "firstfalse" in
	let ie1 = codegen_exp exp1 info in
	let ie2 = codegen_exp exp2 info in
	List.concat [ie1;
		     [Inst.Iif (Inst.Ne, true_lbl);
		      Inst.Igoto firstfalse_lbl;
		      Inst.Ilabel firstfalse_lbl];
		     ie2;
		     [Inst.Iif (Inst.Ne, true_lbl);
		      Inst.Igoto false_lbl;
		      Inst.Ilabel true_lbl;
		      Inst.Ildc_int 1l;
		      Inst.Igoto end_lbl;
		      Inst.Ilabel false_lbl;
		      Inst.Ildc_int 0l;
		      Inst.Ilabel end_lbl]]
	  
and codegen_unop unop exp (info : info) : Inst.instruction list =
  match unop with
  | TAst.Negate -> 
    let ie = codegen_exp exp info in
    List.concat [ie;
		 [Inst.Iineg]]
  | TAst.Complement -> 
    let true_lbl = Inst.make_label "true" in
    let false_lbl = Inst.make_label "false" in
    let end_lbl = Inst.make_label "endif" in
    let ie = codegen_exp exp info in
    List.concat [ie;
		 [Inst.Iif (Inst.Ne, false_lbl);
		  Inst.Igoto true_lbl;
		  Inst.Ilabel true_lbl;
		  Inst.Ildc_int 1l;
		  Inst.Igoto end_lbl;
		  Inst.Ilabel false_lbl;
		  Inst.Ildc_int 0l;
		  Inst.Ilabel end_lbl]]
   | TAst.BooleanToString ->
     let ie = codegen_exp exp info in
     List.concat [ie;
		  if !Globals.joos1
		  then [Inst.Iinvokestatic (codegen_method_sig_known "java/lang/String/valueOf(Z)Ljava/lang/String;" 1 1)]
		  else [Inst.Iinvokevirtual (codegen_method_sig_known "java/lang/StringBuffer/append(Z)Ljava/lang/StringBuffer;" 1 1)]]
   | TAst.ByteToString
   | TAst.ShortToString
   | TAst.IntToString ->
     let ie = codegen_exp exp info in
     List.concat [ie;
		  if !Globals.joos1
		  then [Inst.Iinvokestatic (codegen_method_sig_known "java/lang/String/valueOf(I)Ljava/lang/String;" 1 1)]
		  else [Inst.Iinvokevirtual (codegen_method_sig_known "java/lang/StringBuffer/append(I)Ljava/lang/StringBuffer;" 1 1)]]
   | TAst.CharToString ->
     let ie = codegen_exp exp info in
     List.concat [ie;
		  if !Globals.joos1
		  then [Inst.Iinvokestatic (codegen_method_sig_known "java/lang/String/valueOf(C)Ljava/lang/String;" 1 1)]
		  else [Inst.Iinvokevirtual (codegen_method_sig_known "java/lang/StringBuffer/append(C)Ljava/lang/StringBuffer;" 1 1)]]
     | TAst.ObjectToString ->
       let ie = codegen_exp exp info in
       List.concat [ie;
		    if !Globals.joos1
		    then [Inst.Iinvokestatic (codegen_method_sig_known "java/lang/String/valueOf(Ljava/lang/Object;)Ljava/lang/String;" 1 1)]
		    else [Inst.Iinvokevirtual (codegen_method_sig_known "java/lang/StringBuffer/append(Ljava/lang/Object;)Ljava/lang/StringBuffer;" 1 1)]]

and create_if_negated (exp : RAst.exp) (stm1 : Inst.instruction list) (stm2 : Inst.instruction list option) info : Inst.instruction list =
  let end_lbl = Inst.make_label "endif" in
  match exp.RAst.exp with
  | RAst.BooleanConst b ->
    if b
    then stm1
    else (match stm2 with
    | None -> [] 
    | Some stm_else -> stm_else) 
  | _ ->  match stm2 with
    | None ->
      let true_lbl = Inst.make_label "true" in
      let code_exp = codegen_exp exp {info with true_label = (Some true_lbl); false_label = None} in
      List.concat [code_exp;
		   [Inst.Iif (Inst.Ne, true_lbl)];
		   [Inst.Igoto end_lbl];
		   [Inst.Ilabel true_lbl];
		   stm1;
		   [Inst.Ilabel end_lbl];
		   [Inst.Inop]]
    | Some stm_else ->
      let true_lbl = Inst.make_label "true" in
      let false_lbl = Inst.make_label "false" in
      let code_exp = codegen_exp exp {info with true_label = (Some true_lbl); false_label = (Some false_lbl)} in
      List.concat [code_exp;
		   [Inst.Iif (Inst.Ne, true_lbl)];
		   [Inst.Igoto false_lbl];
		   [Inst.Ilabel true_lbl];
		   stm1;
		   [Inst.Igoto end_lbl];
		   [Inst.Ilabel false_lbl];
		   stm_else;
		   [Inst.Ilabel end_lbl];
		   [Inst.Inop]]

    
let rec codegen_stm stm (info : info) : Inst.instruction list = match stm.RAst.stm with
  | RAst.Exp e ->
    let ie = codegen_exp e info in
    (match e.RAst.exp_type with
      | Types.Base Ast.Void -> ie
      | _ -> List.concat [ie;
			  [Inst.Ipop]] )
  | RAst.IfThen (exp,stm) ->
    let code_stm = codegen_stm stm info in
    create_if_negated exp code_stm None info
  | RAst.IfThenElse (exp,stm1,stm2) ->
    let stm1_code = codegen_stm stm1 info in
    let stm2_code = codegen_stm stm2 info in
    create_if_negated exp stm1_code (Some stm2_code) info
  | RAst.While (e,s) ->
    let loopl = Inst.make_label "loop" in
    let is = codegen_stm s info in
    (match e.RAst.exp with
    | RAst.BooleanConst true ->
      List.concat
	[[Inst.Ilabel loopl];
	 is;
	 [Inst.Igoto loopl]]     
    | _ ->
      let condl = Inst.make_label "cond" in
      let end_lbl = Inst.make_label "end" in
      let ie = codegen_exp e {info with true_label = Some loopl; false_label = (Some end_lbl)} in
      List.concat
	[[Inst.Igoto condl;
	  Inst.Ilabel loopl];
	 is;
	 [Inst.Ilabel condl];
	 ie;
	 [Inst.Iif (Inst.Ne,loopl);
	 Inst.Igoto end_lbl;
	 Inst.Ilabel end_lbl]])
  | RAst.Empty -> []
  | RAst.Block b ->
    List.concat (List.map (fun b_single -> codegen_stm b_single info) b) 
  | RAst.VoidReturn ->
    [Inst.Ireturn;]
  | RAst.ValueReturn exp ->
    let ie = codegen_exp exp info in
    List.concat [ie;
		 [if Types.is_base_type exp.RAst.exp_type
		   then Inst.Iireturn
		   else Inst.Iareturn]]
  | RAst.LocalDecl (typ,id,exp_opt,offset) ->
    (match exp_opt with
      | None -> []
      | Some e ->
	let ie = codegen_exp e info in
	(match typ with
	  | Types.Base Ast.Byte
	  | Types.Base Ast.Short
	  | Types.Base Ast.Int
	  | Types.Base Ast.Char
	  | Types.Base Ast.Boolean -> 
	    List.concat [ie;
			 [Inst.Iistore offset]]
	  | Types.Base _ ->
	    (Error.internal_compiler_error "Illegal type of local initializer")
	  | _ -> 
	    List.concat [ie;
			 [Inst.Iastore offset]]
	))
  | RAst.Throw exp ->
    let ie = codegen_exp exp info in
    List.concat [ie;
		 [Inst.Iathrow]]
  | RAst.ThisCall (exp_list,base,ct) ->
    let ies = List.concat (List.map (fun e -> codegen_exp e info) exp_list) in
    List.concat [[Inst.Iaload 0];
		 ies;
		 [Inst.Iinvokespecial (codegen_constructor_sig base ct)]]
  | RAst.SuperCall (exp_list,base,ct) ->
    let ies = List.concat (List.map (fun e -> codegen_exp e info) exp_list) in
    let super_call = List.concat [[Inst.Iaload 0];
				  ies;
				  [Inst.Iinvokespecial (codegen_constructor_sig base ct)]] in
    let (final,not_final) = List.partition 
      (fun f -> not f.RAst.field_static && 
	f.RAst.field_final &&
	has_some f.RAst.field_init &&
	is_constant_option f.RAst.field_init) info.fields in 
    let all_fields = List.concat [final;
				  not_final] in
    let field_init = List.fold_right (fun fdecl flist ->
      match fdecl.RAst.field_init with
      | None -> flist
      | Some e -> 
	let ie = codegen_exp e info in
	let field_type_sig = desc_of_typeexp fdecl.RAst.field_type in
	let fieldsig = fdecl.RAst.field_signature
	  ^ " " ^ field_type_sig in
	List.concat [[Inst.Iaload 0];
		     ie;
		     [Inst.Iputfield fieldsig];
		     flist]) all_fields [] in
    List.concat [super_call;
		 field_init]

let init_static_fields_ordered_list info : RAst.field_decl list =
  let (final_const,other) = List.partition 
    (fun fd -> has_some fd.RAst.field_init && 
      fd.RAst.field_final && 
      fd.RAst.field_static && 
      is_constant_option fd.RAst.field_init) info.fields in
  let only_static_fields = 
    List.filter 
      (fun fd -> has_some fd.RAst.field_init && 
      fd.RAst.field_static) other in
  List.concat [final_const;
	       only_static_fields]

let codegen_static_init_for_class c pos info : CAst.method_decl =
  let ordered_fields_list = init_static_fields_ordered_list info in
  let field_list_instructions = List.concat 
    (List.map 
       (fun fd -> 
	 match fd.RAst.field_init with
	 | None -> []
	 | Some exp -> 
	   let desc = desc_of_typeexp fd.RAst.field_type in
	   List.concat [codegen_exp exp info;
			[Inst.Iputstatic (fd.RAst.field_signature ^ " " ^ desc)]]) ordered_fields_list) in
  let body_option = match field_list_instructions with
    | [] -> Some [Inst.Ireturn]
    | _ -> Some (field_list_instructions @ [Inst.Ireturn]) in
  { CAst.method_access = Ast.Public;
    CAst.method_static = true;
    CAst.method_final = false;
    CAst.method_abstract = false;
    CAst.method_result = T.Base Ast.Void;
    CAst.method_name = { Ast.identifier_pos = pos;
			 Ast.identifier = "<clinit>" };
    CAst.method_formals = [];
    CAst.method_throws = [];
    CAst.method_body = body_option;
    CAst.method_signature = "<clinit>()V"}

let codegen_constructor c (info : info) : CAst.constructor_decl =
  let body = List.concat (List.map (fun stm -> codegen_stm stm info) c.RAst.constructor_body) in
  { CAst.constructor_access = c.RAst.constructor_access;
    CAst.constructor_name = c.RAst.constructor_name;
    CAst.constructor_formals = c.RAst.constructor_formals;
    CAst.constructor_throws = c.RAst.constructor_throws;
    CAst.constructor_body = body;
    CAst.constructor_signature = c.RAst.constructor_signature }

let rec extract_local_numbers_stm stm =
  match stm.RAst.stm with
    | RAst.IfThenElse (_,s1,s2) ->
      extract_local_numbers_stm s1 @ extract_local_numbers_stm s2
    | RAst.While (_,s)
    | RAst.IfThen (_,s) ->
      extract_local_numbers_stm s
    | RAst.Block b ->
      extract_local_numbers b
    | RAst.LocalDecl (_, _, _, offset) ->
      [offset]
    | _ -> []
  
and extract_local_numbers block =
  ListPlus.concat_map extract_local_numbers_stm block

let codegen_method (m : RAst.method_decl) (info : info) : CAst.method_decl =
  let body_option = match m.RAst.method_body with
    | None -> None
    | Some stm_list -> 
      let local_numbers = extract_local_numbers stm_list in
      let inits = ListPlus.concat_map (fun loc -> [Inst.Ildc_int Int32.zero; Inst.Iistore loc]) local_numbers in
      Some (inits @ (ListPlus.concat_map (fun stm -> codegen_stm stm info) stm_list)) in
  { CAst.method_access = m.RAst.method_access;
    CAst.method_static = m.RAst.method_static;
    CAst.method_final = m.RAst.method_final;
    CAst.method_abstract = m.RAst.method_abstract;
    CAst.method_result = m.RAst.method_result;
    CAst.method_name = m.RAst.method_name;
    CAst.method_formals = m.RAst.method_formals;
    CAst.method_throws = m.RAst.method_throws;
    CAst.method_body = body_option;
    CAst.method_signature = m.RAst.method_signature}

let codegen_field f : CAst.field_decl =
  { CAst.field_access = f.RAst.field_access;
    CAst.field_static = f.RAst.field_static;
    CAst.field_final = f.RAst.field_final;
    CAst.field_type = f.RAst.field_type;
    CAst.field_name = f.RAst.field_name;
    CAst.field_signature = f.RAst.field_signature }

let codegen_members (members : RAst.decl list) info : CAst.decl list=
  List.map (fun m -> 
    { CAst.decl_pos = m.RAst.decl_pos;
      CAst.decl = match m.RAst.decl with
      | RAst.Field f -> CAst.Field (codegen_field f)
      | RAst.Method m -> CAst.Method (codegen_method m info)
      | RAst.Constructor c -> CAst.Constructor (codegen_constructor c info)}) members

let codegen_class_decl cdecl pos tenv =
  let fields = List.concat (List.map (fun member -> 
    match member.RAst.decl with
    | RAst.Field f -> [f]
    | _ -> []) cdecl.RAst.class_members) in
  let info = { tenv = tenv; fields = fields; stringbuffer = false; true_label = None; false_label = None } in
  let members = codegen_members cdecl.RAst.class_members info in
  let init = {CAst.decl_pos = pos;
	      CAst.decl = CAst.Method (codegen_static_init_for_class cdecl pos info)} in
  { CAst.class_final = cdecl.RAst.class_final;
    CAst.class_abstract = cdecl.RAst.class_abstract; 
    CAst.class_name  = cdecl.RAst.class_name;
    CAst.class_extends = cdecl.RAst.class_extends;
    CAst.class_implements = cdecl.RAst.class_implements;
    CAst.class_members  = members @ [init] }

let codegen_interface_decl i tenv =
  let info = { tenv = tenv; fields = []; stringbuffer = false; true_label = None; false_label = None } in
  let members = codegen_members i.RAst.interface_members info in  
  { CAst.interface_name = i.RAst.interface_name;
    CAst.interface_extends = i.RAst.interface_extends;
    CAst.interface_members = members }
      
let codegen_type_decl t tenv = 
  let type_decl_desc = match t.RAst.type_decl with
    | RAst.Interface i ->
      CAst.Interface (codegen_interface_decl i tenv)
    | RAst.Class c ->
      CAst.Class (codegen_class_decl c t.RAst.type_decl_pos tenv) in
  {CAst.type_decl_pos = t.RAst.type_decl_pos;
   CAst.type_decl = type_decl_desc;
   CAst.type_canonical_name = t.RAst.type_canonical_name;
   CAst.type_decl_signature = t.RAst.type_decl_signature}
    
let codegen_source_file sf tenv =
  let type_decl = codegen_type_decl sf.RAst.source_file_decl tenv in
  { CAst.source_file_name = sf.RAst.source_file_name;
    CAst.source_file_package = sf.RAst.source_file_package;
    CAst.source_file_single_imports = sf.RAst.source_file_single_imports;
    CAst.source_file_ondemand_imports = sf.RAst.source_file_ondemand_imports;
    CAst.source_file_decl = type_decl }

let codegen_program program tenv = 
  List.map (fun sf -> codegen_source_file sf tenv) program 
