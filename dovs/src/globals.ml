(** Global command line parameters *)

let joos1 = ref false
let peephole = ref false
let noverify = ref false 
let classpath = ref (try Sys.getenv "CLASSPATH"
                     with Not_found -> "")
