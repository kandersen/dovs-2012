(** Custom printers for the toplevel and debugging environments *)

let pp_canonical_name fmt cname =
  Format.pp_print_string fmt (CanonicalName.to_string cname)

