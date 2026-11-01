(* The dune rules, all in one directory. *)

let fprintf = Printf.fprintf
let sprintf = Printf.sprintf
let words = String.concat " "

(* ocamlopt -target js_of_ocaml runs js_of_ocaml, the one built here. *)
let ocamlopt_jsir args =
  sprintf
    "(setenv OXCAML_JS_OF_OCAML %%{bin:js_of_ocaml}\n\
    \   (run %%{ocamlopt} -target js_of_ocaml %s))"
    (words args)

let rule ppf ~targets ~deps ~action =
  fprintf ppf "(rule\n (targets %s)\n (deps\n  %s)\n (action\n  %s))\n\n"
    (words targets) (String.concat "\n  " deps) action

let archive_name lib = lib ^ ".cmjxa"

(* A .cmjx and .cmjo per unit, then the archive. All the libraries share
   the directory, so the .cmjx of other libraries are found there too, as
   long as the rule depends on them: dune runs it in a sandbox holding only
   its dependencies, and the compiler would otherwise fall back to whatever
   .cmjx the installation has.

   The source is compiled from a copy named after the unit: libraries share
   the directory (the stdlib and stdlib_stable both have an int.ml), and the
   paths of installed sources would otherwise end up in the generated code. *)
let library ppf (lib : Library.t) =
  let installed file = sprintf "%s/%s" lib.installed_dir file in
  let unit_ (u : Library.unit_) =
    let cmi = installed (u.basename ^ ".cmi") and src = u.basename ^ ".ml" in
    fprintf ppf "(rule (target %s) (action (copy %s %%{target})))\n\n" src
      u.source;
    rule ppf
      ~targets:[ u.basename ^ ".cmjx"; u.basename ^ ".cmjo" ]
      ~deps:
        ((sprintf "(:src %s)" src :: cmi
          :: List.map (fun dep -> installed (dep ^ ".cmi")) u.imports)
        @ List.map
            (fun dep -> dep ^ ".cmjx")
            (u.imports @ lib.required_units))
      ~action:
        (ocamlopt_jsir
           (u.flags
           @ [ "-I"; lib.installed_dir; "-cmi-file"; cmi;
               "-o"; u.basename ^ ".cmjx"; "-c"; "%{src}" ]))
  in
  List.iter unit_ lib.archive;
  List.iter unit_ lib.extra;
  if lib.archive <> [] then
    rule ppf
      ~targets:[ archive_name lib.name; lib.name ^ ".cmja" ]
      ~deps:
        (List.map (fun (u : Library.unit_) -> u.basename ^ ".cmjo") lib.archive)
      ~action:
        (ocamlopt_jsir
           ([ "-g"; "-nostdlib" ]
           @ (if lib.linkall then [ "-linkall" ] else [])
           @ [ "-a"; "-o"; archive_name lib.name ]
           @ List.map
               (fun (u : Library.unit_) -> sprintf "%%{dep:%s.cmjx}" u.basename)
               lib.archive))

let libraries ppf libs = List.iter (fun (_, lib) -> library ppf lib) libs

(* The (files (include ...)) of the install stanza: the metadata and
   archives, plus the objects of the units outside archives. [build_dir] is
   where the rules above run, relative to the install stanza. *)
let install ppf ~build_dir (libs : (Spec.t * Library.t) list) =
  fprintf ppf "(";
  List.iter
    (fun ((spec : Spec.t), (lib : Library.t)) ->
      let file f =
        fprintf ppf "\n (%s/%s as %s/%s)" build_dir f spec.install_dir f
      in
      List.iter
        (fun (u : Library.unit_) -> file (u.basename ^ ".cmjx"))
        lib.archive;
      List.iter
        (fun (u : Library.unit_) ->
          file (u.basename ^ ".cmjx");
          file (u.basename ^ ".cmjo"))
        lib.extra;
      if lib.archive <> [] then begin
        file (archive_name lib.name);
        file (lib.name ^ ".cmja")
      end)
    libs;
  fprintf ppf ")\n"
