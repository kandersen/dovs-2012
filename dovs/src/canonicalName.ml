(** Canonical Names.

    This module creates a unique type for canonical names
    [CanonicalName.t] which can not be confused with plain strings.

*)

(** The type of canonical names *)
(* This is representation is opaque
   (ie, the signature does not mention string) *)
type t = string

(** Make a canonical name from a string which is (supposedly) a fully
    qualified name *)
let make str = str

(** Convert a canonical name to its string representation *)
let to_string str = str

(** Get the class part of a canonical name.
    Fx: [java.lang.Object -> Object] *)
let class_name str =
  try
    let idx = String.rindex str '.' + 1 in
      String.sub str idx (String.length str - idx)
  with Not_found -> str

(** Get the package part of a canonical name.
    Fx: [java.lang.Object -> java.lang] *)
let package str =
  try
    let idx = String.rindex str '.' in
      String.sub str 0 idx
  with Not_found -> ""

(** Compare to canonical names for equality *)
(* Needed to satify Map module requirements *)
let compare c1 c2 = String.compare c1 c2
