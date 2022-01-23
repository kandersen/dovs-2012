(** Compiler phase to calculate resource information,
    such as JVM signatures and local variable indices. *)

module NAst = Nameresolvingast
module TAst = Typecheckingast
module RAst = Resourceast

module ListPlus = Utils.ListPlus
module Option = Utils.OptionPlus

(************************************************************************)
(** {2 Liveness Analysis}                                               *)
(************************************************************************)
  
(************************************************************************)
(** {3 Step 1: AST -> CFG}                                              *)
(************************************************************************)

type program_point_desc = 
  | Read of TAst.identifier
  | Write of TAst.identifier
  | True 
  | False
  | Return

type program_point = { index : int;
		       desc  : program_point_desc }

let print_pp pp = 
  print_string ("  " ^ (string_of_int pp.index) ^ ": ");
  print_endline
    (match pp.desc with
      | Read id ->
	"READ " ^ id.Ast.identifier ^":"^ (string_of_int id.Ast.identifier_pos.Lexing.pos_lnum)
      | Write id ->
	"WRITE " ^ id.Ast.identifier ^":"^ (string_of_int id.Ast.identifier_pos.Lexing.pos_lnum)
      | True ->
	"TRUE"
      | False ->
	"FALSE"
      | Return ->
	"RETURN")

module PPSet = Set.Make(struct
  type t = program_point
  let compare pp1 pp2 = compare pp1.index pp2.index
end)

type edge = int * int

let print_edge (f, t) =
  print_endline ("  " ^ (string_of_int f) ^ " ---> " ^ (string_of_int t))

module EdgeSet = Set.Make(struct
  type t = edge
  let compare = compare
end)

type cfg = PPSet.t * EdgeSet.t

type scope = Ast.identifier list

let empty_scope = []

let add id scope : scope =
  id :: scope

let rec lookup id scope = 
  match scope with
    | [] ->
      Error.internal_compiler_error
	("Resources: AST->CFG: lookup: Unbound variable " ^ id.Ast.identifier)
    | id' :: scope' ->
      if id.Ast.identifier = id'.Ast.identifier
      then id'
      else lookup id scope'

type arg_record = { next_index : int;
		    nodes      : PPSet.t;
		    edges      : EdgeSet.t;
		    scope      : scope }

type result_record = { res_next_index : int; 
		       res_nodes      : PPSet.t;
		       res_edges      : EdgeSet.t;
		       res_exits      : PPSet.t;
		       res_exits_t    : PPSet.t;
		       res_exits_f    : PPSet.t;
		       res_scope      : scope }
    
let drop_exits res =
  { next_index = res.res_next_index;
    nodes = res.res_nodes;
    edges = res.res_edges;
    scope = res.res_scope }

let make_write i id =
  { index = i; desc = Write id }

let make_read i id =
  { index = i; desc = Read id }

let make_return i =
  { index = i; desc = Return }

let make_true i =
  { index = i; desc = True }

let make_false i =
  { index = i; desc = False }

let make_exit_edges exits index =
  List.fold_left 
    (fun acc edge -> EdgeSet.add edge acc)
    EdgeSet.empty
    (List.map (fun pp -> pp.index, index) (PPSet.elements exits))
    
let rec build_pps_exp e args =
  match e.TAst.exp with
    | TAst.Binop(l, TAst.LazyAnd, r) ->
      let res_l = build_pps_exp l args in
      if not (PPSet.is_empty res_l.res_exits_f)
      then res_l
      else build_pps_exp r (drop_exits res_l)
    | TAst.Binop(l, TAst.LazyOr, r) ->
      let res_l = build_pps_exp l args in
      if not (PPSet.is_empty res_l.res_exits_t)
      then res_l
      else build_pps_exp r (drop_exits res_l)
    | TAst.Binop(l, _, r) ->
      let res_l = build_pps_exp l args in
      let res_r = build_pps_exp r (drop_exits res_l) in
      if PPSet.is_empty res_r.res_exits
      then {res_r with res_exits = res_l.res_exits }
      else res_r
    | TAst.Unop(TAst.Complement, e) ->
      let res = build_pps_exp e args in
      { res with
	res_exits_t = res.res_exits_f;
	res_exits_f = res.res_exits_t }
    | TAst.Unop(_, e) ->
      build_pps_exp e args
    | TAst.BooleanConst true ->
      let pp = make_true args.next_index in
      { res_nodes = PPSet.add pp args.nodes;
	res_edges = args.edges;
	res_next_index = args.next_index + 1;
	res_exits = PPSet.singleton pp;
	res_exits_t = PPSet.singleton pp;
	res_exits_f = PPSet.empty;
	res_scope = args.scope }
    | TAst.BooleanConst false ->
      let pp = make_false args.next_index in
      { res_nodes = PPSet.add pp args.nodes;
	res_edges = args.edges;
	res_next_index = args.next_index + 1;
	res_exits = PPSet.singleton pp;
	res_exits_t = PPSet.empty;
	res_exits_f = PPSet.singleton pp;
	res_scope = args.scope }
    | TAst.IntConst _
    | TAst.StringConst _ 
    | TAst.Null
    | TAst.This ->
      { res_next_index = args.next_index;
	res_nodes = args.nodes;
	res_edges = args.edges;
	res_scope = args.scope;
	res_exits = PPSet.empty;
	res_exits_t = PPSet.empty;
	res_exits_f = PPSet.empty }
    | TAst.StaticInvoke(_, _, arg_list, _, _) ->
      build_pps_exp_list arg_list args
    | TAst.NonstaticInvoke(e, _, arg_list, _, _) ->
      let res_e = build_pps_exp e args in
      build_pps_exp_list arg_list (drop_exits res_e)
    | TAst.New(_, arg_list,_, _) ->
      build_pps_exp_list arg_list args
    | TAst.NewArray(_, arg_list) ->
      build_pps_exp_list arg_list args
    | TAst.Lvalue { TAst.lvalue = l } ->
      begin 
	match l with
	  | TAst.Local id ->
	    let id = lookup id args.scope in
	    let pp = make_read args.next_index id in
	    { res_nodes = PPSet.add pp args.nodes;
	      res_edges = EdgeSet.add (args.next_index, args.next_index + 1) args.edges;
	      res_next_index = args.next_index + 1;
	      res_exits = PPSet.singleton pp;
	      res_exits_t = PPSet.empty;
	      res_exits_f = PPSet.empty;
	      res_scope = args.scope }
	  | TAst.Array(e1, e2) ->
	    let res_e1 = build_pps_exp e1 args in
	    build_pps_exp e2 (drop_exits res_e1)
	  | TAst.NonstaticField (e1, _, _) ->
	    build_pps_exp e1 args
	  | TAst.StaticField _ ->
	    { res_nodes = args.nodes;
	      res_edges = args.edges;
	      res_next_index = args.next_index;
	      res_exits = PPSet.empty;
	      res_exits_t = PPSet.empty;
	      res_exits_f = PPSet.empty;
	      res_scope = args.scope }
      end
    | TAst.Assignment ({TAst.lvalue = l }, e) ->
      begin
	match l with
	  | TAst.Local id ->
	    let id = lookup id args.scope in
	    let res_e = build_pps_exp e args in
	    let args' = drop_exits res_e in
	    let pp_write = make_write args'.next_index id in
	    let pp_read = make_read (args'.next_index + 1) id in
	    let ass_edge = (args'.next_index, args'.next_index + 1) in
	    let next_edge = (args'.next_index + 1, args'.next_index + 2) in
	    { res_nodes = PPSet.add pp_read (PPSet.add pp_write args'.nodes);
	      res_edges = EdgeSet.add next_edge (EdgeSet.add ass_edge args'.edges);
	      res_next_index = args'.next_index + 2;
	      res_exits = PPSet.singleton pp_read;
	      res_exits_t = PPSet.empty;
	      res_exits_f = PPSet.empty;
	      res_scope = args'.scope }
	  | TAst.Array (e1, e2) ->
	    let res_e1 = build_pps_exp e1 args in
	    let res_e2 = build_pps_exp e2 (drop_exits res_e1) in
	    build_pps_exp e (drop_exits res_e2)
	  | TAst.NonstaticField(e1, _, _) ->
	    let res_e1 = build_pps_exp e1 args in
	    build_pps_exp e (drop_exits res_e1)
	  | TAst.StaticField _ ->
	    build_pps_exp e args
      end
    | TAst.IncDec ({TAst.lvalue = l}, _) ->
      begin
	match l with
	  | TAst.Local id ->
	    let pp_read1 = make_read (args.next_index) id in
	    let pp_write = make_write (args.next_index + 1) id in
	    let pp_read2 = make_read (args.next_index + 2) id in
	    let edge1 = (args.next_index, args.next_index + 1) in
	    let edge2 = (args.next_index + 1, args.next_index + 2) in
	    let edge3 = (args.next_index + 2, args.next_index + 3) in
	    { res_nodes = PPSet.add pp_read1 (PPSet.add pp_write (PPSet.add pp_read2 args.nodes));
	      res_edges = EdgeSet.add edge1 (EdgeSet.add edge2 (EdgeSet.add edge3 args.edges));
	      res_exits = PPSet.singleton pp_read2;
	      res_exits_t = PPSet.empty;
	      res_exits_f = PPSet.empty;
	      res_next_index = args.next_index + 3;
	      res_scope = args.scope }
	  | TAst.Array (e1, e2) ->
	    let res_e1 = build_pps_exp e1 args in
	    build_pps_exp e2 (drop_exits res_e1)
	  | TAst.NonstaticField(e1, _, _) ->
	    build_pps_exp e1 args
	  | TAst.StaticField _ ->
	    { res_nodes = args.nodes;
	      res_edges = args.edges;
	      res_next_index = args.next_index;
	      res_exits = PPSet.empty;
	      res_exits_t = PPSet.empty;
	      res_exits_f = PPSet.empty;
	      res_scope = args.scope }
      end
    | TAst.Cast(_, e)
    | TAst.Instanceof(e, _)
    | TAst.ArrayLength e
    | TAst.ArrayClone e ->
      build_pps_exp e args

and build_pps_exp_list es args =
  match es with
    | [] -> 
      { res_next_index = args.next_index;
	res_nodes = args.nodes;
	res_edges = args.edges;
	res_exits = PPSet.empty;
	res_exits_t = PPSet.empty;
	res_exits_f = PPSet.empty;
	res_scope = args.scope }
    | [e] ->
      build_pps_exp e args
    | e :: es' ->
      let res_e = build_pps_exp e args in
      build_pps_exp_list es' 
	{ nodes = res_e.res_nodes;
	  edges = res_e.res_edges;
	  next_index = res_e.res_next_index;
	  scope = res_e.res_scope }

let rec build_pps_stm s args =
  match s.TAst.stm with
    | TAst.Exp { TAst.exp = TAst.Assignment ({TAst.lvalue = l}, e)} ->
      begin 
	match l with
	  | TAst.Local id ->
	    let id = lookup id args.scope in
	    let res_e = build_pps_exp e args in
	    let pp_write = make_write res_e.res_next_index id in
	    let from_exp_to_write = make_exit_edges res_e.res_exits res_e.res_next_index in
	    { res_e with
	      res_nodes = PPSet.add pp_write res_e.res_nodes;
	      res_edges = EdgeSet.union from_exp_to_write res_e.res_edges;
	      res_next_index = pp_write.index + 1;
	      res_exits = PPSet.singleton pp_write;
	      res_exits_t = PPSet.empty;
	      res_exits_f = PPSet.empty; }
	  | TAst.Array (e1, e2) ->
	    let res_e1 = build_pps_exp e1 args in
	    let res_e2 = build_pps_exp e2 (drop_exits res_e1) in
	    build_pps_exp e (drop_exits res_e2)
	  | TAst.NonstaticField(e1, _, _) ->
	    let res_e1 = build_pps_exp e1 args in
	    build_pps_exp e (drop_exits res_e1)
	  | TAst.StaticField _ ->
	    build_pps_exp e args
      end
    | TAst.Exp e ->
      build_pps_exp e args
    | TAst.IfThen(e, s) ->
      let res_e = build_pps_exp e args in
      let always_true = not (PPSet.is_empty res_e.res_exits_t) in
      let always_false = not (PPSet.is_empty res_e.res_exits_f) in
      begin
	match always_true, always_false with
	  | true, true ->
	    Error.internal_compiler_error "Expression is always both false and true!"
	  | true, false ->
	    let res_s = build_pps_stm s (drop_exits res_e) in
	    let t = PPSet.choose res_e.res_exits_t in
	    { res_s with
	      res_edges = EdgeSet.add (t.index, res_e.res_next_index) res_s.res_edges }
	  | false, true ->
	    let f = PPSet.choose res_e.res_exits_f in
	    { res_e with
	      res_edges = EdgeSet.add (f.index, res_e.res_next_index) res_e.res_edges;
	      res_exits = res_e.res_exits_f }
	  | false, false ->
	    let res_s = build_pps_stm s (drop_exits res_e) in
	    let true_edges = make_exit_edges res_e.res_exits res_e.res_next_index in
	    let false_edges = make_exit_edges res_e.res_exits res_s.res_next_index in
	    { res_s with
	      res_edges = EdgeSet.union true_edges (EdgeSet.union false_edges res_s.res_edges);
	      res_exits = PPSet.union res_e.res_exits res_s.res_exits }
      end
    | TAst.IfThenElse(e, s1, s2) ->
      let res_e = build_pps_exp e args in
      let always_true = not (PPSet.is_empty res_e.res_exits_t) in
      let always_false = not (PPSet.is_empty res_e.res_exits_f) in
      begin
	match always_true, always_false with
	  | true, true ->
	    Error.internal_compiler_error "Expression is always both false and true!"
	  | true, false ->
	    let res_s1 = build_pps_stm s1 (drop_exits res_e) in
	    let t = PPSet.choose res_e.res_exits_t in
	    let link = (t.index, res_e.res_next_index) in
	    { res_s1 with
	      res_edges = EdgeSet.add link res_s1.res_edges }
	  | false, true ->
	    let res_s2 = build_pps_stm s2 (drop_exits res_e) in
	    let f = PPSet.choose res_e.res_exits_f in
	    let link = (f.index, res_e.res_next_index) in
	    { res_s2 with
	      res_edges = EdgeSet.add link res_s2.res_edges }
	  | false, false ->
	    let res_s1 = build_pps_stm s1 (drop_exits res_e) in
	    let res_s2 = build_pps_stm s2 (drop_exits res_s1) in
	    let true_links = make_exit_edges res_e.res_exits res_e.res_next_index in
	    let false_links = make_exit_edges res_e.res_exits res_s1.res_next_index in
	    let jump_endif = make_exit_edges res_s1.res_exits res_s2.res_next_index in
	    { res_s2 with
	      res_edges =
		EdgeSet.union true_links
		  (EdgeSet.union false_links
		     (EdgeSet.union jump_endif res_s2.res_edges));
	      res_exits = PPSet.union res_s1.res_exits res_s2.res_exits}
      end
    | TAst.While(e, s) ->
      let res_e = build_pps_exp e args in
      let always_true = not (PPSet.is_empty res_e.res_exits_t) in
      let always_false = not (PPSet.is_empty res_e.res_exits_f) in
      begin
	match always_true, always_false with
	  | true, true ->
	    Error.internal_compiler_error "Expression is always both false and true!"
	  | true, false ->
	    let res_s = build_pps_stm s (drop_exits res_e) in
	    let t = PPSet.choose res_e.res_exits_t in
	    if PPSet.is_empty res_s.res_exits
	    then let loop = (t.index, t.index) in
		 { res_e with
		   res_edges = EdgeSet.add loop res_s.res_edges }
	    else let link = (t.index, res_e.res_next_index) in
		 let loops = make_exit_edges res_s.res_exits t.index in
		 { res_s with
		   res_edges = EdgeSet.add link (EdgeSet.union loops res_s.res_edges);
		   res_exits = PPSet.empty }
	  | false, true ->
	    { res_e with
	      res_exits = res_e.res_exits_f }
	  | false, false ->
	    let res_s = build_pps_stm s (drop_exits res_e) in
	    if PPSet.is_empty res_s.res_exits
	    then let loop = make_exit_edges res_e.res_exits args.next_index in
		 let break = make_exit_edges res_e.res_exits res_s.res_next_index in
		 { res_s with
		   res_edges = EdgeSet.union loop (EdgeSet.union break res_s.res_edges) }
	    else let loop = make_exit_edges res_s.res_exits args.next_index in
		 let break = make_exit_edges res_e.res_exits res_s.res_next_index in
		 { res_s with
		   res_edges = EdgeSet.union loop (EdgeSet.union break res_s.res_edges);
		   res_exits = res_e.res_exits;
		   res_exits_t = PPSet.empty;
		   res_exits_f = PPSet.empty }
      end
    | TAst.Empty ->
      { res_next_index = args.next_index;
	res_edges = args.edges;
	res_nodes = args.nodes;
	res_exits = PPSet.empty;
	res_exits_t = PPSet.empty;
	res_exits_f = PPSet.empty;
	res_scope = args.scope }
    | TAst.Block b ->
      { (build_pps_block b args) with
	res_scope = args.scope }
    | TAst.VoidReturn ->
      let pp = make_return args.next_index in
      { res_nodes = PPSet.add pp args.nodes;
	res_edges = args.edges;
	res_next_index = args.next_index + 1;
	res_exits = PPSet.empty;
	res_exits_t = PPSet.empty;
	res_exits_f = PPSet.empty;
	res_scope = args.scope }
    | TAst.Throw e 
    | TAst.ValueReturn e ->
      let res_e = build_pps_exp e args in
      let pp = make_return res_e.res_next_index in
      let links = make_exit_edges res_e.res_exits res_e.res_next_index in
      { res_next_index = res_e.res_next_index + 1;
	res_edges = EdgeSet.union links res_e.res_edges;
	res_nodes = PPSet.add pp res_e.res_nodes;
	res_exits = PPSet.empty;
	res_exits_t = PPSet.empty;
	res_exits_f = PPSet.empty;
	res_scope = res_e.res_scope }
    | TAst.LocalDecl (_, id, None) ->
      { res_next_index = args.next_index;
	res_edges = args.edges;
	res_nodes = args.nodes;
	res_exits = PPSet.empty;
	res_exits_t = PPSet.empty;
	res_exits_f = PPSet.empty;
	res_scope = add id args.scope }
    | TAst.LocalDecl (_, id, Some e) ->
      let res_e = build_pps_exp e { args with scope = add id args.scope } in
      let pp = make_write res_e.res_next_index id in
      let from_e_to_pp = make_exit_edges res_e.res_exits pp.index in
      { res_next_index = pp.index + 1;
	res_edges = EdgeSet.union from_e_to_pp res_e.res_edges;
	res_nodes = PPSet.add pp res_e.res_nodes;
	res_exits = PPSet.singleton pp;
	res_exits_t = PPSet.empty;
	res_exits_f = PPSet.empty;
	res_scope = res_e.res_scope }
    | TAst.ThisCall(arg_list, _, _)
    | TAst.SuperCall(arg_list, _, _) ->
      build_pps_exp_list arg_list args
	
and build_pps_block b args =
  match b with
    | [] ->
      { res_next_index = args.next_index;
	res_edges = args.edges;
	res_nodes = args.nodes;
	res_exits = PPSet.empty;
	res_exits_t = PPSet.empty;
	res_exits_f = PPSet.empty;
	res_scope = args.scope }
    | [s] ->
      build_pps_stm s args
    | s :: b ->
      let res_s = build_pps_stm s args in
      let res_b = build_pps_block b (drop_exits res_s) in
      if res_s.res_next_index = res_b.res_next_index
      then res_b
      else let links = make_exit_edges res_s.res_exits res_s.res_next_index in
	   { res_b with
	     res_edges = EdgeSet.union links res_b.res_edges }

let build_arg_writes ids =
  let rec iter ids index =
    match ids with
      | [] ->
	(PPSet.empty, index)
      | id::ids' ->
	let (rest, next) = iter ids' (index + 1) in
	(PPSet.add (make_write index id) rest, next) in
  iter ids 0

let build_arg_edges last_write =
  let rec iter i =
    if i = last_write
    then EdgeSet.empty
    else EdgeSet.add (i, i+1) (iter (i + 1)) in
  iter 0

let build_cfg formals body : cfg =
  let (init_nodes, init_index) = build_arg_writes (List.map snd formals) in
  let init_edges = build_arg_edges init_index in
  let init_cfg =
    { nodes = init_nodes;
      edges = init_edges;
      next_index = init_index;
      scope = List.map snd formals } in
  let result = build_pps_block body init_cfg in
  (result.res_nodes, result.res_edges)

let print_cfg (nodes, edges) = 
  List.iter print_pp (PPSet.elements nodes);
  print_endline "";
  List.iter print_edge (EdgeSet.elements edges)

(************************************************************************)
(** {3 Step 2: Liveness Analysis Proper (CFG -> Liveness Sets)          *)
(************************************************************************)

module VarSet = Set.Make(struct
  type t = Ast.identifier
  let compare = compare
end)

let succ (nodes, edges) pp : PPSet.t =
  let is_outgoing_edge (origin, destination) =
    pp.index = origin in
  let succ_indices = 
    List.map
      snd
      (EdgeSet.elements (EdgeSet.filter is_outgoing_edge edges)) in
  PPSet.filter (fun pp -> List.mem pp.index succ_indices) nodes

let uses pp = 
  match pp.desc with
    | Read id ->
      VarSet.singleton id
    | _ -> 
      VarSet.empty

let defs pp =
  match pp.desc with
    | Write id ->
      VarSet.singleton id
    | _ ->
      VarSet.empty

let rec empty_sets n =
  match n with
    | 0 ->
      []
    | n ->
      VarSet.empty :: empty_sets (n - 1)

let live_set_equals a b = 
  let id x = x in
  let rec iter_vars v1s v2s =
    match v1s, v2s with
      | [], [] -> true
      | [], _
      | _, [] -> false
      | v1 :: v1s, v2 :: v2s ->
	v1 = v2 && iter_vars v1s v2s in
  List.for_all id (ListPlus.zipWith (fun x y -> iter_vars (VarSet.elements x) (VarSet.elements y)) a b)

let rec fixpoint f old =
  begin
    let next = f old in
    if live_set_equals old next 
    then next
    else fixpoint f next
  end

let pick indices es =
  let rec iter i es =
    match es with
      | [] ->
	[]
      | e :: es' when List.mem i indices ->
	e :: iter (i + 1) es'
      | _ :: es' ->
	iter (i + 1) es' in
  iter 0 es

let after cfg vector pp =
  let successors = succ cfg pp in
  let succ_indices = List.map (fun pp -> pp.index) (PPSet.elements successors) in
  let before_succs = pick succ_indices vector in
  List.fold_left VarSet.union VarSet.empty before_succs

let before cfg vector pp =
  VarSet.union (uses pp) (VarSet.diff (after cfg vector pp) (defs pp))

let make_stepper ((nodes, edges) as cfg) =
  let program_points = PPSet.elements nodes in
  fun vector ->
    let rec iter sets pps =
      match sets, pps with
	| [], [] ->
	  []
	| _ :: sets', pp :: pps' ->
	  (before cfg vector pp) :: (iter sets' pps')
	| _ -> Error.internal_compiler_error "Not enough program points compared to vector entries" in
    iter vector program_points

let compute_live_sets (nodes, edges) =
  let init_vector = empty_sets (PPSet.cardinal nodes) in
  let step = make_stepper (nodes, edges) in
  fixpoint step init_vector

let print_live_sets sets =
  let rec iter sets index =
    match sets with
      | [] -> ()
      | s :: sets' -> 
	print_string "  ";
	print_int index;
	print_string ": { ";
	List.iter (fun v -> print_string (v.Ast.identifier ^":"^ (string_of_int v.Ast.identifier_pos.Lexing.pos_lnum) ^ ", ")) (VarSet.elements s);
	print_endline "}";
	iter sets' (index + 1) in
  iter sets 0
    

(************************************************************************)
(** {3 Step 3: Live Sets -> Coloring                                    *)
(************************************************************************)

module VarMap = Map.Make(struct
  type t = Ast.identifier
  let compare = compare
end)

module VarVarMap = Map.Make(struct
  type t = Ast.identifier * Ast.identifier
  let compare = compare
end)

type type_map = Types.typeexp VarMap.t

module IntSet = Set.Make(struct
  type t = int
  let compare = compare
end)

module IntMap = Map.Make(struct
  type t = int
  let compare = compare
end)

type coloring = int VarMap.t

let build_init_coloring formals static = 
  let rec iter formals i = 
    match formals with
      | [] -> 
	(i, VarMap.empty)
      | (_, id) :: formals' ->
	let (next_index, coloring) = iter formals' (i + 1) in
	next_index, VarMap.add id i coloring in
  iter formals (if static then 0 else 1)
 
let color_graph vars live_sets formals static (type_map : type_map) program_points : coloring = 
  let highest_to_lowest a b = -1 * (compare a b) in

  let live_sets_map =
    List.fold_left
      (fun acc (k, v) -> IntMap.add k v acc)
      IntMap.empty
      (ListPlus.zip
	 (ListPlus.range 0 ((List.length live_sets) - 1))
	 live_sets) in
      
  let writes = 
    List.fold_left
      (fun acc (k, v) -> VarMap.add k v acc)
      VarMap.empty
      (ListPlus.zip
	 (VarSet.elements vars)
	 (List.map (fun id -> List.map (fun pp -> pp.index) (PPSet.elements (PPSet.filter (fun pp -> match pp.desc with Write id' -> id = id' | _ -> false) program_points))) (VarSet.elements vars))) in

  let written_while_live a b = 
    let writes_to_a = VarMap.find a writes  in
    List.exists (fun i -> VarSet.mem b (IntMap.find i live_sets_map)) writes_to_a in
    
  let neighbours = 
    List.fold_left
      (fun acc (k, v) -> VarMap.add k v acc)
      VarMap.empty 
      (ListPlus.zip
	 (VarSet.elements vars)
	 (List.map (fun id -> VarSet.remove id (List.fold_left VarSet.union VarSet.empty (List.filter (VarSet.mem id) live_sets))) (VarSet.elements vars))) in

  let live_at_same_time a b =    
    VarSet.mem b (VarMap.find a neighbours) in

  let degrees = 
    List.fold_left
      (fun acc (k, v) -> VarMap.add k v acc)
      VarMap.empty
      (ListPlus.zip
	 (VarSet.elements vars)
	 (List.map (fun id -> VarSet.cardinal (VarMap.find id neighbours)) (VarSet.elements vars))) in
  let degree id = VarMap.find id degrees in

  let type_of id = VarMap.find id type_map in
  
  let color_compatible a b =
    type_of a = type_of b
    && not (written_while_live a b)
    && not (written_while_live b a)
    && not (live_at_same_time a b) in
  
  let color_compatability_map = 
    let pairs = ListPlus.pairs (VarSet.elements vars) in
    List.fold_left
      (fun acc ((a, b), v) -> VarVarMap.add (a, b) v (VarVarMap.add (b, a) v acc))
      VarVarMap.empty
      (ListPlus.zip
	 pairs
	 (List.map (fun (a, b) -> color_compatible a b) pairs)) in

  let color_compatible a b = VarVarMap.find (a, b) color_compatability_map in

  let find_candidate_colors (id : Ast.identifier) (coloring : int VarMap.t) : int list =
    let rec iter (bindings : (VarMap.key * int) list) (candidates : IntSet.t) rejected_colors : int list =
	match bindings with
	  | [] ->
	    IntSet.elements candidates
	  | (id', color) :: bindings' when color_compatible id id' && not (IntSet.mem color rejected_colors) ->
	    iter bindings' (IntSet.add color candidates) rejected_colors
	  | (_, color) :: bindings' ->
	    iter bindings' (IntSet.remove color candidates) (IntSet.add color rejected_colors) in
    iter (VarMap.bindings coloring) IntSet.empty IntSet.empty in
	    
  let next_available_color, init_coloring = build_init_coloring formals static in
  let vars_to_alloc = VarSet.elements (VarSet.diff vars (List.fold_right VarSet.add (List.map snd formals) VarSet.empty)) in
  let work_list = List.map snd (List.sort highest_to_lowest (List.map (fun v -> (degree v, v)) vars_to_alloc)) in
  let rec color_graph work_list next_available_color coloring =
    match work_list with
      | [] ->
	coloring
      | next :: work_list' ->
	let candidates = find_candidate_colors next coloring in
	begin
	  match candidates with
	    | [] -> 
	      color_graph work_list' (next_available_color + 1) (VarMap.add next next_available_color coloring)
	    | color :: _ ->
	      color_graph work_list' next_available_color (VarMap.add next color coloring)
	end in
  color_graph work_list next_available_color init_coloring
        
(************************************************************************)
(** {3 Interface to Liveness Analysis }                                 *)
(************************************************************************)

let merge_type_maps =
  VarMap.merge
    (fun k in_a in_b ->
      match in_a, in_b with
	| Some t, None
	| None, Some t -> Some t
	| _ -> Error.internal_compiler_error "Shouldn't happen")

let rec vars_decled_stm s : VarSet.t * type_map = 
  match s.TAst.stm with
    | TAst.LocalDecl(t, id, _) ->
      VarSet.singleton id, VarMap.singleton id t
    | TAst.While(_, s)
    | TAst.IfThen(_, s) ->
      vars_decled_stm s
    | TAst.IfThenElse(_, s1, s2) ->
      let vars_s1, type_s1 = vars_decled_stm s1 in
      let vars_s2, type_s2 = vars_decled_stm s2 in
      VarSet.union vars_s1 vars_s2,
      merge_type_maps type_s1 type_s2
    | TAst.Block b ->
      vars_decled_block b
    | _ -> 
      VarSet.empty, VarMap.empty

and vars_decled_block b : VarSet.t * type_map =
  match b with
    | [] ->
      VarSet.empty, VarMap.empty
    | s :: b' ->
      let vars_s, type_s = vars_decled_stm s in
      let vars_b', type_b' = vars_decled_block b' in
      (VarSet.union vars_s vars_b'),
      (merge_type_maps type_s type_b')

let rec vars_decled_formals formals : VarSet.t * type_map = 
  match formals with
    | [] ->
      VarSet.empty, VarMap.empty
    | (t, id) :: formals' ->
      let (vars', types') = vars_decled_formals formals' in
      VarSet.add id vars', VarMap.add id t types'

let vars_decled formals body : VarSet.t * type_map =
  let vars_f, types_f = vars_decled_formals formals in
  let vars_b, types_b = vars_decled_block body in
  VarSet.union vars_f vars_b,
  merge_type_maps types_f types_b

type allocation = coloring

let allocate formals body static = 
  let cfg = build_cfg formals body in
  let live_sets = compute_live_sets cfg in
  let var_set, type_map = vars_decled formals body in
  color_graph var_set live_sets formals static type_map (fst cfg)

let build_abstract_allocation formals static = 
  let rec iter formals index =
    match formals with
      | [] ->
	VarMap.empty
      | (_, v) :: formals' ->
	VarMap.add v index (iter formals' (index + 1)) in
  iter formals (if static then 0 else 1)

let allocate_constructor (cd : TAst.constructor_decl) : allocation =
  allocate cd.TAst.constructor_formals cd.TAst.constructor_body false
    
let allocate_method (md : TAst.method_decl) : allocation =
  let static = md.TAst.method_static in
  match md.TAst.method_body with
    | None ->
      build_abstract_allocation md.TAst.method_formals static
    | Some body ->
      allocate md.TAst.method_formals body static

	
(************************************************************************)
(** {2 Resource Traversal }                                             *)
(************************************************************************)

type local_scope = Ast.identifier list

let empty_scope = []

let add_to_scope id scope = id :: scope

let rec find id scope = 
  match scope with
    | [] ->
      Error.internal_compiler_error "resource.ml/find: Unbound variable!"
    | id' :: scope when id.Ast.identifier = id'.Ast.identifier ->
      id'
    | _ :: scope -> find id scope

type info = { allocation : allocation;
	      scope : scope }

let rec res_lvalue lvalue info = 
  let pos = lvalue.TAst.lvalue_pos in
  let lvalue_type = lvalue.TAst.lvalue_type in
  let mklvalue lvalue' = { RAst.lvalue_pos = pos;
			   RAst.lvalue = lvalue';
			   lvalue_type = lvalue_type } in
  match lvalue.TAst.lvalue with
    | TAst.NonstaticField (e,id,base) ->
      let e' = res_exp e info in
      mklvalue (RAst.NonstaticField (e',id,base))
    | TAst.Array (e1,e2) ->
      let e1' = res_exp e1 info in
      let e2' = res_exp e2 info in
      mklvalue (RAst.Array (e1',e2'))
    | TAst.Local id ->
      let i = find id info.scope in
      let r = VarMap.find i info.allocation in
      mklvalue (RAst.Local (id, r))
    | TAst.StaticField (nt,id,base) -> 
      mklvalue (RAst.StaticField (nt,id,base))

and res_exp exp info =
  let pos = exp.TAst.exp_pos in
  let exp_type = exp.TAst.exp_type in
  let mkexp exp' = { RAst.exp_pos = pos; 
		     RAst.exp = exp'; 
		     exp_type = exp_type } in
  match exp.TAst.exp with
    | TAst.Binop (e1,op,e2) ->
      let e1' = res_exp e1 info in
      let e2' = res_exp e2 info in
      mkexp (RAst.Binop (e1',op,e2'))
    | TAst.Unop (op,e) ->
      let e' = res_exp e info in
      mkexp (RAst.Unop (op,e'))
    | TAst.IntConst i -> mkexp (RAst.IntConst i)
    | TAst.StringConst s -> mkexp (RAst.StringConst s)
    | TAst.BooleanConst b -> mkexp (RAst.BooleanConst b)
    | TAst.Null -> mkexp RAst.Null
    | TAst.This -> mkexp RAst.This
    | TAst.StaticInvoke (nt,id,es,base,m) ->
      let es' = res_exp_list es info in
      mkexp (RAst.StaticInvoke (nt,id,es',base,m))
    | TAst.NonstaticInvoke (e,id,es,base,m) ->
      let e' = res_exp e info in
      let es' = res_exp_list es info in
      mkexp (RAst.NonstaticInvoke (e',id,es',base,m))
    | TAst.New (t,es,cname,c) ->
      let es' = res_exp_list es info in
      mkexp (RAst.New (t,es',cname,c))
    | TAst.NewArray (t,es) ->
      let es' = res_exp_list es info in
      mkexp (RAst.NewArray (t,es'))
    | TAst.Lvalue lvalue ->
      let lvalue' = res_lvalue lvalue info in
      mkexp (RAst.Lvalue lvalue')
    | TAst.Assignment (lvalue,e) ->
      let lvalue' = res_lvalue lvalue info in
      let e' = res_exp e info in
      mkexp (RAst.Assignment (lvalue',e'))
    | TAst.IncDec (lvalue,op) ->
      let lvalue' = res_lvalue lvalue info in
      mkexp (RAst.IncDec (lvalue',op))
    | TAst.Cast (t,e) ->
      let e' = res_exp e info in
      mkexp (RAst.Cast (t,e'))
    | TAst.Instanceof (e,t) ->
      let e' = res_exp e info in
      mkexp (RAst.Instanceof (e',t))
    | TAst.ArrayLength e ->
      let e' = res_exp e info in
      mkexp (RAst.ArrayLength e')
    | TAst.ArrayClone e ->
      let e' = res_exp e info in
      mkexp (RAst.ArrayClone e')

and res_exp_list exps info =
  List.map (fun e -> res_exp e info) exps

let res_exp_opt exp info = match exp with
  | None -> None
  | Some e -> let e' = res_exp e info in Some e'

let rec res_stm stm info = 
  let pos = stm.TAst.stm_pos in
  let mkstm stm' = { RAst.stm_pos = pos;
		     RAst.stm = stm' } in
  match stm.TAst.stm with
    | TAst.Exp e -> 
      let e' = res_exp e info in
      info, mkstm (RAst.Exp e')
    | TAst.IfThen (e,s) -> 
      let e' = res_exp e info in
      let _, s' = res_stm s info in
      info, mkstm (RAst.IfThen (e',s'))
    | TAst.IfThenElse (e,s1,s2) -> 
      let e' = res_exp e info in
      let _, s1' = res_stm s1 info in
      let _, s2' = res_stm s2 info in
      info,mkstm (RAst.IfThenElse (e',s1',s2'))
    | TAst.While (e,s) -> 
      let e' = res_exp e info in
      let _, s' = res_stm s info in
      info, mkstm (RAst.While (e',s'))
    | TAst.Empty -> 
      info,mkstm (RAst.Empty)
    | TAst.VoidReturn ->
      info,mkstm (RAst.VoidReturn)
    | TAst.ValueReturn e ->
      let e' = res_exp e info in
      info,mkstm (RAst.ValueReturn e')
    | TAst.LocalDecl (t,id,e) ->
      let info' = { info with scope = add_to_scope id info.scope } in
      let e' = res_exp_opt e info' in (* local visible in own initializer *)
      let r = VarMap.find id info.allocation in
      info',mkstm (RAst.LocalDecl (t,id,e',r))
    | TAst.Throw e ->
      let e' = res_exp e info in
      info,mkstm (RAst.Throw e')
    | TAst.SuperCall (es,base,c) ->
      let es' = res_exp_list es info in
      info,mkstm (RAst.SuperCall (es',base,c))
    | TAst.ThisCall (es,base,c) ->
      let es' = res_exp_list es info in
      info,mkstm (RAst.ThisCall (es',base,c))
    | TAst.Block b ->
      let info',b' = res_block b info in
      info, mkstm (RAst.Block b')

and res_block block info = match block with
  | [] -> info,[]
  | stm::stms ->
    let info',stm' = res_stm stm info in
    let info'',stms' = res_block stms info' in
    info'',stm'::stms'

let res_body body info = 
  let _,body' = res_block body info in
  body'

let res_body_opt body info = match body with
  | None ->
    None
  | Some body -> 
    let body' = res_body body info in
    Some body'

type tdinfo = { type_sig : string } (* OBS: type decl info *)

let res_field_decl fdecl tdinfo =
  let field_sig = tdinfo.type_sig ^ "/" ^ fdecl.TAst.field_name.Ast.identifier in
  let info = { allocation = VarMap.empty; (* Note: no locals in opt. field initializer *)
	       scope = empty_scope } in
  let field_init' = res_exp_opt fdecl.TAst.field_init info in
  { RAst.field_access    = fdecl.TAst.field_access;
    RAst.field_static    = fdecl.TAst.field_static;
    RAst.field_final     = fdecl.TAst.field_final; 
    RAst.field_type      = fdecl.TAst.field_type;  
    RAst.field_name      = fdecl.TAst.field_name;  
    RAst.field_init      = field_init';
    RAst.field_signature = field_sig
  }

let make_method_sig tdinfo name formals return_sig =
  let formal_sigs = List.map (fun (t,_) -> Types.typeexp_to_desc t) formals in
  tdinfo.type_sig ^ "/" ^ name ^ "(" ^ (String.concat "" formal_sigs) ^ ")" ^ return_sig 

let rec res_formals fs info =
  match fs with
    | [] -> info,[]
    | (x,id)::fs ->
      let r = VarMap.find id info.allocation in
      let f' = (x,id,r) in
      let info' = { info with
	scope = add_to_scope id info.scope } in
      let info'',fs' = res_formals fs info' in
      info'',f'::fs'

let res_method_decl mdecl tdinfo =
  let allocation = allocate_method mdecl in
  let info = { allocation = allocation;
	       scope = empty_scope } in
  let info',formals' = res_formals mdecl.TAst.method_formals info in
  let body' = res_body_opt mdecl.TAst.method_body info' in
  let name = mdecl.TAst.method_name in
  let return_sig = Types.typeexp_to_desc mdecl.TAst.method_result in
  let method_sig = 
    make_method_sig tdinfo name.Ast.identifier mdecl.TAst.method_formals return_sig in
  { RAst.method_access    = mdecl.TAst.method_access;
    RAst.method_static    = mdecl.TAst.method_static;
    RAst.method_final     = mdecl.TAst.method_final;
    RAst.method_abstract  = mdecl.TAst.method_abstract;
    RAst.method_result    = mdecl.TAst.method_result;
    RAst.method_name      = name;
    RAst.method_formals   = formals';
    RAst.method_throws    = mdecl.TAst.method_throws;
    RAst.method_body      = body';
    RAst.method_signature = method_sig
  }

let res_constructor_decl cdecl tdinfo = 
  let allocation = allocate_constructor cdecl in
  let info = { allocation = allocation;
	       scope = empty_scope } in
  let info', formals' = res_formals cdecl.TAst.constructor_formals info in
  let body' = res_body cdecl.TAst.constructor_body info' in
  let name = cdecl.TAst.constructor_name in
  let return_sig = "V" in
  let constructor_sig = 
    make_method_sig tdinfo name.Ast.identifier cdecl.TAst.constructor_formals return_sig in
  { RAst.constructor_access    = cdecl.TAst.constructor_access;
    RAst.constructor_name      = cdecl.TAst.constructor_name;
    RAst.constructor_formals   = formals';
    RAst.constructor_throws    = cdecl.TAst.constructor_throws;
    RAst.constructor_body      = body';
    RAst.constructor_signature = constructor_sig
  }

let res_decl decl tdinfo = 
  { RAst.decl_pos = decl.TAst.decl_pos;
    RAst.decl     = match decl.TAst.decl with
      | TAst.Field fdecl ->
	let fdecl' = res_field_decl fdecl tdinfo in
	RAst.Field fdecl'
      | TAst.Method mdecl ->
	let mdecl' = res_method_decl mdecl tdinfo in
	RAst.Method mdecl'
      | TAst.Constructor cdecl ->
	let cdecl' = res_constructor_decl cdecl tdinfo in
	RAst.Constructor cdecl'
  }

let res_decls decls tdinfo =
  List.map (fun decl -> res_decl decl tdinfo) decls

let res_class_decl cdecl tdinfo =
  let members' = res_decls cdecl.TAst.class_members tdinfo in
  { RAst.class_final      = cdecl.TAst.class_final;
    RAst.class_abstract   = cdecl.TAst.class_abstract;
    RAst.class_name       = cdecl.TAst.class_name;
    RAst.class_extends    = cdecl.TAst.class_extends;
    RAst.class_implements = cdecl.TAst.class_implements;
    RAst.class_members    = members'
  }

let res_interface_decl idecl tdinfo =
  let members' = res_decls idecl.TAst.interface_members tdinfo in
  { RAst.interface_name    = idecl.TAst.interface_name;
    RAst.interface_extends = idecl.TAst.interface_extends;
    RAst.interface_members = members'
  }

let res_type_decl tdecl =
  let canonical_name = tdecl.TAst.type_canonical_name in
  let slashed_name = Types.cname_to_sig canonical_name in
  let tdinfo = { type_sig = slashed_name } in
  { RAst.type_decl_pos       = tdecl.TAst.type_decl_pos;
    RAst.type_canonical_name = canonical_name;
    RAst.type_decl_signature = slashed_name;
    RAst.type_decl = match tdecl.TAst.type_decl with
      | TAst.Class cdecl ->
	let cdecl' = res_class_decl cdecl tdinfo in RAst.Class cdecl'
      | TAst.Interface idecl ->
	let idecl' = res_interface_decl idecl tdinfo in RAst.Interface idecl'
  }

let res_source_file src_file =
  let type_decl' = res_type_decl src_file.TAst.source_file_decl in
  { RAst.source_file_name             = src_file.TAst.source_file_name;
    RAst.source_file_package          = src_file.TAst.source_file_package;
    RAst.source_file_single_imports   = src_file.TAst.source_file_single_imports;
    RAst.source_file_ondemand_imports = src_file.TAst.source_file_ondemand_imports;
    RAst.source_file_decl             = type_decl'
  }

let res_program prog =
  let res =  List.map res_source_file prog in
(*  Rastpp.print_program res;*)
  res

