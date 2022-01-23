(** Contains the peephole optimizations *)
open Instruction

(** A record type of handy operations. The record a given as first argument
    to each peephole pattern function. *)
type operations =
    { degree : label -> int;
      target : label -> instruction list;
      formals : method_signature -> int;
      returns : method_signature -> int;
      negate : condition -> condition;
      commute : condition -> condition }

(** A peephole pattern function (maps input instructions to output instructions).
    @return Some inst if the peephole clicked and None otherwise *)
type peephole_pattern = 
    operations -> Instruction.instruction list -> Instruction.instruction list option

(**/**) (*/*)

let myVar = ref 0

let inst_list_to_string is =
  print_string "{";
  List.iter (fun i -> print_string ((to_asm i) ^ ";")) is;
  print_endline "}"

let dup_store_pop ops is = match is with
  | Idup::
    Iistore i0::
    Ipop::
    is' -> Some (Iistore i0::
		 is')
  | Idup::
    Iastore a0::
    Ipop::
    is' -> Some (Iastore a0::is')
  | Idup_x2::
    Iiastore::
    Ipop::
    is' -> Some (Iiastore::is')
  | Idup_x2::
    Iaastore::
    Ipop::
    is' -> Some (Iaastore::is')
  | _ -> None

let goto_label ops is = match is with
  | Igoto l1::
    Ilabel l2::
    is' when l1=l2 ->
    Some (Ilabel l2::
	  is')
  | _ -> None

let constant_iadd_residue ops is = match is with
  | Ildc_int i0::
    Iiadd::
    Ildc_int i1::
    Iiadd::
    is' -> Some (Ildc_int (Int32.add i0 i1)::
		 Iiadd::
		 is')
  | _ -> None

let goto_goto ops is = match is with
  | Igoto l0::
    is' -> 
    (match ops.target l0 with
      | Igoto l1::
        is'' -> 
	(match ops.target l1 with
	  | Igoto _ :: _
	  | Ilabel _ :: _ -> None
	  | _ -> Some (Igoto l1::
		       is'))
      | _ -> None)
  | _ -> None

let kill_nops ops is = 
  match is with
  | i :: Inop :: is' ->
    (match i, is' with
    | Ilabel l1,[] -> None
    | _ -> 
	Some (i::is'))
  | _ -> None

let goto_return ops is = match is with
  | Igoto l0::
    is' -> 
    (match ops.target l0 with
      | Iireturn::is'' -> Some (Iireturn::is')
      | Iareturn::is'' -> Some (Iareturn::is')
      | Ireturn::is'' -> Some (Ireturn::is')
      | _ -> None)
  | _ -> None
(*what the fuck is fucking wrong here stupid shit *)
let goto_target_is_label ops is = 
  match is with 
    | Igoto g1::is' ->
      (match ops.target g1 with
	| Ilabel l1::xs -> Some (Igoto l1::is')
	| _ -> None)
    | _ -> None


let remove_unreachable_instructions_after_goto ops is = match is with
  | (Igoto l1)::i::is' -> 
    (match i with
    | Ilabel l2 -> None
    | _ -> Some ((Igoto l1)::is'))
  | _ -> None

let remove_unreachable_instructions_after_return ops is = match is with
  | Iireturn::i::is' -> (match i with
    | Ilabel _ -> None
    | _ -> Some (Iireturn::is'))
  | Iareturn::i::is' -> (match i with
    | Ilabel _ -> None
    | _ -> Some (Iareturn::is'))
  | Ireturn::i::is' -> (match i with
    | Ilabel _ -> None
    | _ -> Some (Ireturn::is'))
  | _ -> None

let remove_labels_with_zero_indegree ops is = match is with
  | Ilabel l0::is' -> 
    (match ops.degree l0 with
    | 0 -> Some is'
    | _ -> None)
  | _ -> None


let remove_redundant_ifcmp_branch ops is = 
  match is with
  | Iifcmp (cond0,l0)::Ildc_int 0l::Igoto gl0::
    Ilabel l0'::Ildc_int 1l::Ilabel lend::Iif (cond1,l1)::is' -> 
    if (l0 = l0' && gl0 = lend)
    then (match cond1 with 
    | Eq -> Some (Iifcmp (ops.negate cond0,l1)::is')
    | Ne -> Some (Iifcmp (cond0,l1)::is')
    | _ -> None)
    else None
  | _ -> None

let remove_redundant_if_branch ops is = 
  match is with
  | Iif (cond0,l0)::Ildc_int 0l::Igoto gl0::
    Ilabel l0'::Ildc_int 1l::Ilabel lend::Iif (cond1,l1)::is' -> 
    if (l0 = l0' && gl0 = lend)
    then (match cond1 with 
    | Eq -> Some (Iif (ops.negate cond0,l1)::is')
    | Ne -> Some (Iif (cond0,l1)::is')
    | _ -> None)
    else None
  | _ -> None


(* why this is a problem
            
   bipush 2     1
   idup         2
   iadd         1
   invokevirtuel (something that takes an argument and returns a value) 1
   pop          0
   pop har ikke fjerne dup værdien. Jeg kan sku ikke rigtig gennemskue den her tbh.

let dup_will_be_eaten_by_pop height is =
  match is with 
    | [] -> height = 1
    | i::is' ->
      let new_height = height + (stack_change i)

let dup_eliminator ops is = 
  match is with
    | Idup::is'::Ipop::is'' ->
      if dup_will_be_eaten_by_pop 2 is'
      then Some (is'::is'')
      else None
    | _ -> None
*)
let rec eaten_by height is =
  match is with
  | [] -> None
  | (Igoto lbl)::is' -> Some (Igoto lbl)
  | i::is' ->
    let new_height = height + (stack_change i) in
    if (new_height <= 0)
    then Some i
    else eaten_by new_height is'

(* very unsound atm!!! I Don't really know bro. *)
(* does not work simply removing the dup is not enough *)
let remove_instructions_who_pushes_unused_variables_if_possible ops is =
  match is with
  | Idup::is' ->
    (match eaten_by 2 is' with
    | Some Ipop -> Some is'
    | _ -> None)
  | _ -> None

let load_followed_by_store ops is =
  match is with
  | Iiload id1 :: Iistore id2::is' -> 
    if id1 = id2
    then Some is'
    else None
  | _ -> None

let convert_number_to_bool (a : int32) : bool  = 
  if a > 0l then true else false

let convert_bool_to_number l =
  if l
  then 1l
  else 0l

let evaluate_boolean_exp a1 a2 op =
  let l1 = convert_number_to_bool a1 in
  let l2 = convert_number_to_bool a2 in
  let result = op a1 a2 in
  convert_bool_to_number result

(*
let constant_arithmetic_expression ops is =
  match is with
  | Iildc_int a1 :: Iildc_int a2 :: i :: is' ->
    (match i with
    | Iimul -> Some (Iildc_int (a1 * a2)::is')
    | Iisub -> hmm, ordnung?
    | Iiadd -> Some (Iildc_int (a1 + a2)::is')
    | Iidiv -> hmm, ??
    | Iirem -> sds
    | Iiand -> Some (Iildc_int (evaluate_boolean_exp a1 a2 (&&))::is')
    | Iior  -> Some (Iildc_int (evaluate_boolean_exp a1 a2 (||))::is')
    | Iixor  -> 
    | _ -> None)
  | _ -> None *)

let simple_negate ops is = 
  match is with
  | Ildc_int a1 :: Iineg :: is' ->
    let bool_of_number = convert_number_to_bool a1 in
    let const_to_add = convert_bool_to_number (not bool_of_number) in
    Some ((Ildc_int const_to_add)::is')
  | _ -> None

let remove_dup_x1_from_putfield ops is = 
  match is with
    | Idup_x1 :: Iputfield f1 :: Ipop::is' ->
      Some (Iputfield f1 :: is')
    | _ -> None
    
let replace_simple_increments_with_iinc ops is = 
  match is with 
    | Iiload s1 :: Ildc_int il :: Iiadd::Iistore s2::is' ->
      if il <= 127l && il >= -128l
      then Some (Iiinc (s2,il) ::is')
      else None
    (* i = i + 1 is the same as i = 1 + i so check the other case aswell *)
    | Ildc_int il :: Iiload s1 :: Iiadd::Iistore s2::is' ->
      if il <= 127l && il >= -128l
      then Some (Iiinc (s2,il) :: is')
      else None
    (* we can also handle i = i - 1 and i = 1 - i *)
    | Iiload s1 :: Ildc_int il :: Iisub::Iistore s2::is' ->
      if il <= 128l && il >= -127l
      then 
	let il_negated = Int32.neg il in
	Some (Iiinc (s2, il_negated) :: is')
      else None
    | Ildc_int il :: Iiload s1 :: Iisub::Iistore s2::is' ->
      if il <= 128l && il >= -127l
      then 
	let il_negated = Int32.neg il in
	Some (Iiinc (s2, il_negated) :: is')
      else None
    | _ -> None

(* null is not an instance of anything *)
let null_instance_of_always_false ops is = 
  match is with
    | Iaconst_null::Iinstanceof i1::is' ->
      Some (Ildc_int 0l :: is')
    | _ -> None

(**/**) (*/*)

(** The list of patterns. Each pattern is represented as a pair: 
    [(name, function)] with the [name] being a descriptive string naming the pattern 
    and the [function] actually implementing the pattern. *)
let mypatterns = [
  (* Initial patterns from slides *)
  ("dup_store_pop",dup_store_pop);
  ("goto_label",goto_label);
  ("constant_iadd_residue",constant_iadd_residue);
  ("goto_goto",goto_goto);
  ("kill_nops",kill_nops);
  ("goto_return",goto_return);
  ("remove_unreachable_instructions_after_return",remove_unreachable_instructions_after_return);
  ("remove_labels_with_zero_indegree",remove_labels_with_zero_indegree);
  ("remove_redundant_if_branch", remove_redundant_if_branch);
  ("remove_redundant_ifcmp_branch", remove_redundant_ifcmp_branch);
  ("remove_dup_x1_from_putfield",remove_dup_x1_from_putfield);
  ("replace_simple_increments_with_iinc",replace_simple_increments_with_iinc);
  ("remove_unreachable_instructions_after_goto",remove_unreachable_instructions_after_goto);
  ("load_followed_by_store",load_followed_by_store);
  ("null_instance_of_always_false",null_instance_of_always_false);
]

