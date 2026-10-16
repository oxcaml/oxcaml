(* What distinguishes one library from another, as far as building it for
   JSIR goes. Paths are dune paths relative to the build directory;
   "installed" ones are in the compiler's installation, [%{ocaml_where}]. *)

type t =
  { lib : string;
    tree_dir : string;  (* the library's sources in the tree *)
    installed_dir : string;
    install_dir : string;  (* where to install the JSIR artifacts *)
    archive : string;  (* the installed native archive *)
    extra : string list;  (* installed .cmx of units outside the archive *)
    flags : [ `Stdlib | `File ];
        (* [`File]: the flags.sexp that the library's dune file includes *)
    unit_flags : string -> string list;  (* more flags, per compilation unit *)
    requires : string list
  }

(* The flags dune adds to every module of the compiler's libraries: the
   (env (main (flags ...))) of the top-level dune file and the env of the main
   context in Makefile.common-ox, minus -directory. Keep in sync. *)
let common_flags =
  [ "-g"; "-principal"; "-warn-error"; "+A";
    "-w"; "+a-4-9-40-41-42-44-45-48-66-67-70";
    "-alert"; "-unsafe_multidomain"; "-alert"; "-unsafe_effects" ]

let sprintf = Printf.sprintf

let stdlib =
  { lib = "stdlib";
    tree_dir = "../../stdlib";
    installed_dir = "%{ocaml_where}";
    install_dir = "ocaml";
    archive = "%{ocaml_where}/stdlib.cmxa";
    extra = [ "%{ocaml_where}/std_exit.cmx" ];
    flags = `Stdlib;
    unit_flags =
      (function
      (* stdlib.ml gets its module aliases from this script. *)
      | "Stdlib" ->
        [ "-pp"; "\"awk -f %{dep:../../stdlib/expand_module_aliases.awk}\"" ]
      | _ -> []);
    requires = []
  }

let otherlib lib =
  { lib;
    tree_dir = "../../otherlibs/" ^ lib;
    installed_dir = "%{ocaml_where}/" ^ lib;
    install_dir = "ocaml/" ^ lib;
    archive = sprintf "%%{ocaml_where}/%s/%s.cmxa" lib lib;
    extra = [];
    flags = `File;
    unit_flags = (fun _ -> []);
    requires = [ "stdlib" ]
  }

let of_name = function "stdlib" -> stdlib | lib -> otherlib lib

(* The actual path a dune path under [%{ocaml_where}] stands for. *)
let expand ~ocaml_where path =
  let prefix = "%{ocaml_where}" in
  if String.starts_with ~prefix path
  then
    ocaml_where
    ^ String.sub path (String.length prefix)
        (String.length path - String.length prefix)
  else path

(* Libraries built by dune install their sources, including the alias module
   dune generates, which only exists there; libraries with a hand-written
   install stanza (unix, str) do not, and come from the tree. The installed
   sources are the ones the installed .cmi were compiled from, so they are
   preferred. *)
let source t ~ocaml_where file =
  let installed = sprintf "%s/%s" t.installed_dir file in
  if Sys.file_exists (expand ~ocaml_where installed) then installed
  else sprintf "%s/%s" t.tree_dir file

(* [flags.sexp] holds a single list of atoms, as (:include ...) expects,
   possibly with ; comments. *)
let read_flags_file filename =
  let s = In_channel.with_open_text filename In_channel.input_all in
  let buf = Buffer.create 16 and atoms = ref [] in
  let state = ref `Atoms in
  let flush () =
    if Buffer.length buf > 0 then begin
      atoms := Buffer.contents buf :: !atoms;
      Buffer.clear buf
    end
  in
  String.iter
    (fun c ->
      match !state, c with
      | `Comment, '\n' -> state := `Atoms
      | `Comment, _ -> ()
      | `String, '"' -> flush (); state := `Atoms
      | `String, c -> Buffer.add_char buf c
      | `Atoms, ';' -> flush (); state := `Comment
      | `Atoms, '"' -> flush (); state := `String
      | `Atoms, ('(' | ')' | ' ' | '\t' | '\n' | '\r') -> flush ()
      | `Atoms, c -> Buffer.add_char buf c)
    s;
  flush ();
  List.rev !atoms

let flags t ~flags_file =
  match t.flags, flags_file with
  | `Stdlib, _ -> Stdlib_flags.flags
  | `File, Some file -> common_flags @ read_flags_file file
  | `File, None -> failwith ("no flags.sexp given for " ^ t.lib)
