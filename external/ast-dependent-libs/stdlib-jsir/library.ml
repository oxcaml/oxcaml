(* A library to compile for ocamlopt -target js_of_ocaml, described the way
   dune built it natively: the same units, in the same link order, each
   compiled from the same source with the same flags, against the installed
   .cmi files. *)

type unit_ =
  { name : string;  (* compilation unit, e.g. Stdlib_stable__Float32 *)
    basename : string;  (* of its artifacts, e.g. stdlib_stable__Float32 *)
    source : string;  (* dune path of its .ml *)
    imports : string list;
        (* [basename]s of the units of this library it may need the .cmjx of:
           the interfaces and implementations the native build imported,
           restricted to the units linked before it (sorted) *)
    flags : string list  (* the complete flags for compiling it *)
  }

type t =
  { name : string;
    installed_dir : string;  (* where the native build installed the .cmi *)
    requires : string list;  (* names of the libraries this one links with *)
    required_units : string list;
        (* [basename]s of all their units: which of their .cmjx a unit needs
           depends on what gets inlined, which the native build does not
           predict *)
    archive : unit_ list;  (* link order *)
    extra : unit_ list;  (* compiled but not archived, e.g. std_exit *)
    linkall : bool
  }

(* dune's conventions for a wrapped library [lib]: the modules other than the
   main one are compiled as [Lib__Module] and see each other through the
   alias module [Lib__], which it generates. *)
let alias_module lib = String.capitalize_ascii lib ^ "__"

let basename name = String.uncapitalize_ascii name

let source_file ~lib name =
  let prefix = alias_module lib in
  let unprefixed =
    let n = String.length prefix in
    if String.starts_with ~prefix name && String.length name > n
    then String.sub name n (String.length name - n)
    else name
  in
  basename unprefixed ^ ".ml"

(* How dune compiles alias modules (see the .cmt of an installed one). *)
let alias_module_flags =
  [ "-nopervasives"; "-nostdlib"; "-w"; "-53"; "-w"; "-49"; "-no-alias-deps" ]

let units t = t.archive @ t.extra

(* [source] gives the dune path of a source file of the library; [requires]
   are the libraries it links with, already converted. *)
let create ~name ~source ~installed_dir ~(requires : t list) ~flags
    ~unit_flags ~(archive : Objinfo.unit_ list) ~(extra : Objinfo.unit_ list)
    =
  let alias = alias_module name in
  let has_alias =
    List.exists (fun (u : Objinfo.unit_) -> u.name = alias) archive
  in
  let flags_of_unit (u : Objinfo.unit_) =
    if u.name = alias then alias_module_flags
    else
      flags
      @ (if List.mem "-no-alias-deps" flags then [] else [ "-no-alias-deps" ])
      @ (if has_alias then [ "-open"; alias ] else [])
      @ unit_flags u.name
  in
  let convert ~linked_before (u : Objinfo.unit_) =
    { name = u.name;
      basename = basename u.name;
      source = source (source_file ~lib:name u.name);
      imports =
        List.filter (fun n -> n <> u.name && List.mem n linked_before)
          (u.interfaces_imported @ u.implementations_imported)
        |> List.map basename |> List.sort_uniq compare;
      flags = flags_of_unit u
    }
  in
  let archive_names = List.map (fun (u : Objinfo.unit_) -> u.name) archive in
  (* -linkall at archive creation sets force_link on every unit. *)
  let linkall =
    archive <> []
    && List.for_all (fun (u : Objinfo.unit_) -> u.force_link) archive
  in
  let converted, _ =
    List.fold_left
      (fun (acc, linked_before) (u : Objinfo.unit_) ->
        convert ~linked_before u :: acc, u.name :: linked_before)
      ([], []) archive
  in
  { name;
    installed_dir;
    requires = List.map (fun r -> r.name) requires;
    required_units =
      List.concat_map
        (fun r -> List.map (fun (u : unit_) -> u.basename) (units r))
        requires;
    archive = List.rev converted;
    extra = List.map (convert ~linked_before:archive_names) extra;
    linkall
  }
