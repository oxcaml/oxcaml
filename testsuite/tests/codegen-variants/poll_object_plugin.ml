(* TEST
 native-compiler;
 set OCAMLLIB = "${ocamlsrcdir}/stdlib";
 codegen_variant = "+poll";
 variant_available;
 shared-libraries;
 native-dynlink;
 readonly_files = "poll_object_unit.ml";
 setup-ocamlopt.byte-build-env;
 script = "sh ${test_source_directory}/poll_object_plugin.sh";
 script;
 check-program-output;
*)

(* The header of a plugin says which of its units have polls, and a fully
   polled program that may run more than one domain loads the plugin on
   that word (see poll_object.ml for the units of a program). So a plugin
   whose header says that every unit has polls, or names no unit, must not
   contain an object file compiled without them. An object file without
   polls refers to a symbol that a plugin defines, for itself, when its
   header says that it has a unit without polls. A plugin whose header says
   that every unit has polls says that nothing outside it may define the
   symbol, and its link fails; where the object format cannot say that,
   and for a plugin without units, the dynamic linker refuses the plugin
   in a fully polled program when no definition is visible to the plugin. A
   plugin whose header says the truth is refused by Dynlink, in its own
   words, and loads when the program runs one domain. A plugin without
   units links whatever its object files are, and a program that is not
   fully polled loads it.

   The script makes the plugins; this program loads those that it is
   given. The first one is the truthful plugin without polls: what it
   defines for itself must not serve the others. *)

let mentions ~part string =
  let length = String.length part in
  let rec from position =
    position + length <= String.length string
    && (String.equal (String.sub string position length) part
       || from (position + 1))
  in
  from 0

let load file =
  match Dynlink.loadfile file with
  | () -> Printf.printf "%s: loaded\n" file
  | exception Dynlink.Error (Library_not_fully_polled _) ->
    Printf.printf "%s: refused by Dynlink, as not fully polled\n" file
  | exception Dynlink.Error (Cannot_open_dynamic_library reason) ->
    let reason = Printexc.to_string reason in
    if mentions ~part:"caml_code_not_fully_polled" reason
    then Printf.printf "%s: refused, as an object file without polls\n" file
    else Printf.printf "%s: refused for another reason: %s\n" file reason
  | exception Dynlink.Error error ->
    Printf.printf "%s: %s\n" file (Dynlink.error_message error)

let () = Array.iteri (fun i file -> if i > 0 then load file) Sys.argv
