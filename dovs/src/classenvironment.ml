(** An interface to the class library. This class generates class and interface
    declaration nodes from class files in the classpath.

    Classes and interfaces loaded from class files will contain the
    declarations of all declared public or protected fields,
    constructors and methods. The fields do not have initializers, and
    the methods and constructors do not have bodies. All types will be
    refer to their declarations, which are loaded transitively as
    necessary.
*)

(**/**) (*/*)
let lookup_java13_home () =
  try Sys.getenv "JAVA13_HOME"
  with Not_found -> 
    try match (Unix.stat "lib/jdk13").Unix.st_kind with
      | Unix.S_DIR -> "lib/jdk13"
      | _ -> raise Not_found
    with _ ->
      begin
        print_string "JAVA13_HOME is not set and could not be found in ./lib/jdk13";
        print_newline ();
        ""
      end

let compose_classpath () =
  let java13_home = lookup_java13_home () in
  let classpath = match !Globals.classpath with
    | "" -> ""
    | _ -> !Globals.classpath ^ Javalib_pack.JFile.sep in
  classpath ^ java13_home ^ Filename.dir_sep ^ "jre/lib/rt.jar"

module TEnv = Types.Env
module StringSet = Set.Make(String)
let class_cache : Types.type_env ref = ref (TEnv.empty)
let negative_class_cache = ref (StringSet.empty)

(**/**) (*/*)

(** Lookup package name in classpath and in java13_home runtime.
     @param full_name the  [CanonicalName] of the requested type *)
let lookup_named_type full_name =
  (* Printf.printf "Classenvironment.lookup_named_type: %s" (CanonicalName.to_string full_name); flush stdout; *)
  let sname = CanonicalName.to_string full_name in
  if TEnv.mem full_name !class_cache
  then 
(*    let () = Printf.printf " -- cached\n" in *)
    Some (TEnv.find full_name !class_cache)
  else
    if StringSet.mem sname !negative_class_cache
    then
(*      let () = Printf.printf " -- cached\n" in *)
      None 
    else
(*    let () = Printf.printf "\n" in*)
    let classpath = compose_classpath () in
(*    let () = Printf.printf "classpath: %s\n" classpath in *)
    let cp_entries = Str.split (Str.regexp_string Javalib_pack.JFile.sep) classpath in

    let rec search_classpath cpentries = match cpentries with
    | cpentry::cpentries
      -> (try 
	    let type_decl = Classfileparser.make_type_decl cpentry sname in
	    class_cache := TEnv.add full_name type_decl !class_cache;
	    Some type_decl
 	  with Not_found
	      -> search_classpath cpentries
            | (Sys_error s)
	      -> Printf.printf "Sys_error: %s\n" s;
	         search_classpath cpentries)
    | []
      -> negative_class_cache := StringSet.add sname !negative_class_cache;
 	 None
  in
    try
      search_classpath cp_entries
    with
	Classfileparser.Non_Public -> None
(*
  let () = Printf.printf " classpath: %s\n" classpath; flush stdout in
  try 
    let type_decl = Classfileparser.make_type_decl classpath full_name in
    Some type_decl
  with Not_found -> None
    | (Sys_error s) -> Printf.printf "Sys_error: %s\n" s; None *)

(** Checks for package name occurrence in classpath and in java13_home runtime 
     @param pkg_name  the package name to query
*)
let package_exists pkg_name =
(*  Printf.printf "Classenvironment.package_exists: %s\n" pkg_name; flush stdout;*)
  let pkg_name_slashed = 
    Str.global_replace (Str.regexp_string ".") Filename.dir_sep pkg_name in
  let classpath = compose_classpath () in
(*  let () = Printf.printf " classpath: %s\n" classpath; flush stdout in*)
  let cp_entries = Str.split (Str.regexp_string Javalib_pack.JFile.sep) classpath in

  let ends_with str suffix = 
    let suffixlength = String.length suffix in
    String.length str > suffixlength && (Str.last_chars str suffixlength) = suffix in

  let rec scan_zentries zentries = match zentries with (*search zip file *)
    | [] -> false
    | zentry::zentries ->
      let filename = zentry.Zip.filename in
      if Str.string_match (Str.regexp_string pkg_name_slashed) filename 0
	&& String.length filename > (String.length pkg_name_slashed)
	&& String.get filename (String.length pkg_name_slashed) == '/' 
	&& ends_with filename ".class" 
      then (Printf.printf " Prefix,length,slash,ending ok: %s\n" filename; 
	    true)
      else scan_zentries zentries
  in

  let rec scan_entries entries = match entries with (*search class path *)
    | entry::entries ->
      if 
	(try ends_with entry ".jar"
	 with (Invalid_argument _) -> false)
      then
	let zf = Zip.open_in entry in
	let zentries = Zip.entries zf in
	let pkgmatch = scan_zentries zentries in
	let () = Zip.close_in zf in
	if pkgmatch (* pkg exists if jar/zip file contains appropriate prefix dir *)
	then (Printf.printf " Package %s exists\n" pkg_name_slashed; true)
	else scan_entries entries
      else scan_entries entries	  
    | [] ->
      try
	Sys.is_directory pkg_name_slashed
      with 
	  (Sys_error _) -> false
  in
  scan_entries cp_entries
