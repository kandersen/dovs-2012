(** Various utility modules and functions.

    To avoid prefixing functions with [Util] you can open the module
    in your source code: {[ open Util ]} *)

(**************************************************************)
(** {2 List Processing}                                      

These list processing functions provide general abstractions
for some commonly used patterns on lists.

{i Note to functional programmers: The arguments are in a non-standard
order make for a more readable inline usage.}

{i Please don't change the functions already implemented here.}
*)
(**************************************************************)

(** Iterate over a list of x's, [xs],
    executing a command, [f], on each x.

    For example:
{[# iter [1; 2; 3] (fun x ->
    print_int x;
    print_string " ")
  ;;
1 2 3 - : unit = () ]}
*)
let iter xs f = List.iter f xs

(** Iterate over the product of two lists: Xs * Ys.

    For example:
{[# iter_prod [1; 2] [3; 4] (fun x y ->
    print_int (x + y);
    print_string " ")
  ;;
4 5 5 6 - : unit = ()
]} *)
let iter_prod xs ys f =
  (* fold_prod xs ys () (fun x y () -> f x y) *)
  iter xs (fun x -> iter ys (fun y -> f x y))

(** Iterate over the product of a single list with itself: Xs * Xs,
    excluding pairs of the same entry (ie, the diagonal).

    For example:
{[# iter_pow2 [1; 2] (fun x y ->
    print_int (x + y);
    print_string " ")
  ;;
3 3 - : unit = ()
]} *)
let iter_pow2 xs f =
  iter_prod xs xs (fun x1 x2 -> if x1 == x2 then () else f x1 x2)

(** Fold over a list of x's, [xs],
    changing the previous result (or accumulator), [a],
    with the function, [f], for each x.

    For example:
{[# fold [1; 2; 3] 0 (fun x a ->
    x + a)
  ;;
-: int = 6 ]}
    Here [0] is the initial value of the accumulator [a].
    In (semi-valid) Java, this example could be translated as:
{[
int a = 0;
for (int x : List(1, 2, 3))
  a = x + a;
]}
*)
let fold xs a f = List.fold_left (fun a x -> f x a) a xs

(** Fold over the product of two lists: Xs * Ys.

    For example:
{[# fold_prod [1; 2] [3; 4] [] (fun x y zs ->
    (x,y) :: zs)
  ;;
-: (int * int) list = [(2, 4); (2, 3); (1, 4); (1, 3)]
]} *)
(* 'x list -> 'y list -> 'a -> ('x -> 'y -> 'a -> 'a) -> 'a *)
let fold_prod xs ys a f =
  fold xs a (fun x a -> fold ys a (fun y a -> f x y a))

(** Fold over the product of a single list with itself: Xs * Xs,
    excluding pairs of the same entry (ie, the diagonal).

    For example:
{[# fold_pow2 [1; 2] [] (fun x y zs ->
    (x,y) :: zs)
  ;;
-: (int * int) list = [(2, 1); (1, 2)]
]} *)
(* 'x list -> 'a -> ('a -> 'x -> 'x -> 'a) -> 'a *)
let fold_pow2 xs a f =
  fold_prod xs xs a (fun x1 x2 a -> if x1 == x2 then a else f x1 x2 a)

let rec fixpoint (eq : 'a -> 'a -> bool)  (f : 'a -> 'a) (a : 'a) : 'a =
  let next = f a in
  if eq a next
  then next
  else fixpoint eq f next 

let compose f g = (fun n -> (f (g n)))

module type LIST =
  sig
    val all_but_last : 'a list -> 'a list
    val exactly_one  : ('a -> bool) -> 'a list -> bool
    val zip          : 'a list -> 'b list -> ('a * 'b) list
    val zipWith      : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
    val prefixes     : 'a list -> 'a list list
    val suffixes     : 'a list -> 'a list list
    val sep_by       : 'a -> 'a list -> 'a list
    val concat_map   : ('a -> 'b list) -> 'a list -> 'b list
    val is_empty     : 'a list -> bool
    val range        : int -> int -> int list
    val pairs        : 'a list -> ('a * 'a) list
  end

module ListPlus : LIST =
  struct
    let rec pairs ls =
      match ls with
	| [] -> []
	| e :: ls' ->
	  (List.map (fun f -> (e, f)) ls') @ (pairs ls')

    let rec range first last = 
      if first = last
      then [last]
      else if (first < last)
      then first :: range (first + 1) last
      else first :: range (first - 1) last

    let is_empty list = 
      match list with 
	| [] -> true
	| _ -> false

    let cons x xs = x :: xs

    let concat_map f =
      compose List.concat (List.map f)

    let rec sep_by sep xs =
      match xs with
	| []
	| [_] -> xs
	| x :: xs -> x :: sep :: sep_by sep xs


    let suffixes input =
      let rec aux input = 
	match input with
	  | [] -> []
	  | x :: xs -> input :: aux xs in
      [] :: (List.rev (aux input))

    let prefixes input = List.map List.rev (suffixes (List.rev input))
      
    let rec all_but_last = function
      | [] -> []
      | [x] -> []
      | x :: xs -> x :: all_but_last xs

    let exactly_one p xs =
      let rec found_one xs =
	match xs with
	  | [] -> true
	  | x :: xs' when p x ->
	    false
	  | _ :: xs' ->
	    found_one xs' in
      let rec found_none xs =
	match xs with
	  | [] -> false
	  | x :: xs' when p x ->
	    found_one xs'
	  | _ :: xs' ->
	    found_none xs' in
      found_none xs

    let rec zip xs ys =
      match xs, ys with
	| [] , _
	| _ , [] -> []
	| (x :: xs'), (y :: ys') ->
	  (x , y) :: zip xs' ys'

    let rec zipWith f xs ys =
      match xs, ys with
	| [], _
	| _ , [] -> []
	| (x :: xs'), (y :: ys') ->
	  (f x y) :: zipWith f xs' ys'
  end

module type OPTION =
  sig
    val is_some : 'a option -> bool
    val is_none : 'a option -> bool
    val map_default : ('a -> 'b) -> 'b -> 'a option -> 'b
    val map : ('a -> 'b) -> 'a option -> 'b option 
  end

module OptionPlus : OPTION =
  struct
    let is_some o = 
      match o with
	| Some _ -> true
	| None -> false
	  
    let is_none o =
      match o with
	| None -> true
	| Some _ -> false
	  
    let map_default f d o =
      match o with
	| None -> d
	| Some v -> f v	  

    let map f o  =
      match o with
	| None -> None
	| Some v -> Some (f v)
  end

module type STRING =
  sig
    val list_of_string : string -> char list
    val string_of_list : char list -> string
  end

module StringPlus : STRING =
  struct	
    let list_of_string s = 
      let rec aux i =
	if i < String.length s
	then s.[i] :: aux (i + 1)
	else [] in
      aux 0
	
    let string_of_list cs = 
      let result = String.create (List.length cs) in
      let index = ref 0 in
      begin
	List.iter (fun c -> begin String.set result !index c; incr index end) cs;
	result
      end
  end

module type INT =
  sig
    val pow : int -> int -> int
  end

module IntPlus : INT =
  struct
    let pow a n =
      let rec aux p x = function
	| 0 -> x
	| i ->
	  aux (p * p) (if i mod 2 = 1 then p * x else x) (i/2)
      in
      aux a 1 n 
  end

module type CHAR =
  sig
    val is_octal : char -> bool
    val int_of_octs : char list -> int
    val int_of_oct : char -> int
    val escape_oct : char -> char
  end

module CharPlus : CHAR =
  struct
    let is_octal = function
      | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' -> true
      | _ -> false

    let int_of_oct o = (int_of_char o) - 48

    let escape_oct o = Char.chr (int_of_oct o)

    let int_of_octs =
      let rec aux i octs = 
	match octs with
	  | [] -> 0
	  | o :: os -> 
	    (int_of_oct o) * (IntPlus.pow 8 i) + (aux (i + 1)) os in
      aux 0
  end
