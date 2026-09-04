(* TEST
 modules = "mtf_facts.ml";
 readonly_files = "mtf_aux.mli mtf_aux.ml mtf_broken.ml \
                   mtf_gate_cmt.ml mtf_gate_cms.ml mtf_gate_none.ml";
 include ocamlcommon;
 setup-ocamlc.byte-build-env;
 flags = "-bin-annot -bin-annot-cms";
 module = "mtf_aux.mli";
 ocamlc.byte;
 module = "mtf_aux.ml";
 ocamlc.byte;
 module = "mtf_broken.ml";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 ocamlc_byte_exit_status = "0";
 flags = "-bin-annot";
 module = "mtf_gate_cmt.ml";
 ocamlc.byte;
 flags = "-bin-annot-cms";
 module = "mtf_gate_cms.ml";
 ocamlc.byte;
 flags = "";
 module = "mtf_gate_none.ml";
 ocamlc.byte;
 module = "";
 program = "${test_build_directory}/mtf_artifacts.byte";
 all_modules = "mtf_facts.ml mtf_artifacts.ml";
 ocamlc.byte;
 run;
 check-program-output;
*)

(* The plumbing of module-type facts through build artifacts: the facts of a
   unit travel in its [.cmt]/[.cmti] and [.cms]/[.cmsi] when and only when
   those artifacts are written, and identically in all of them.  See
   [mtf_facts.ml] for where the contents of the facts are tested. *)

open Mtf_facts

let () = heading "facts read back from the .cmt and the .cmti of a unit"

let () =
  let cmt = Cmt_format.read_cmt "mtf_aux.cmt" in
  let cmti = Cmt_format.read_cmt "mtf_aux.cmti" in
  Printf.printf "present: cmt %b cmti %b\n"
    (Option.is_some cmt.cmt_module_implementation_facts)
    (Option.is_some cmti.cmt_module_implementation_facts)

let () = heading ".cms and .cmsi hold the same facts as .cmt and .cmti"

let () =
  let cms = Cms_format.read "mtf_aux.cms" in
  let cmsi = Cms_format.read "mtf_aux.cmsi" in
  let cmt = Cmt_format.read_cmt "mtf_aux.cmt" in
  let cmti = Cmt_format.read_cmt "mtf_aux.cmti" in
  Printf.printf "present: cms %b cmsi %b\n"
    (Option.is_some cms.cms_module_implementation_facts)
    (Option.is_some cmsi.cms_module_implementation_facts);
  Printf.printf "cms = cmt: %b\ncmsi = cmti: %b\n"
    (Option.equal equal_facts cms.cms_module_implementation_facts
       cmt.cmt_module_implementation_facts)
    (Option.equal equal_facts cmsi.cms_module_implementation_facts
       cmti.cmt_module_implementation_facts)

let () = heading "facts are only extracted for the artifacts that are written"

(* Each artifact is written, and hence carries facts, only if the flag that
   asks for it was passed. *)
let () =
  let report unit_ =
    let exists extension = Sys.file_exists (unit_ ^ extension) in
    let facts_present extension read present =
      if exists extension
      then Printf.sprintf "%b" (present (read (unit_ ^ extension)))
      else "no artifact"
    in
    Printf.printf "%s: cmt %s, cms %s\n" unit_
      (facts_present ".cmt" Cmt_format.read_cmt (fun cmt ->
           Option.is_some cmt.Cmt_format.cmt_module_implementation_facts))
      (facts_present ".cms" Cms_format.read (fun cms ->
           Option.is_some cms.Cms_format.cms_module_implementation_facts))
  in
  report "mtf_gate_cmt";
  report "mtf_gate_cms";
  report "mtf_gate_none"

let () = heading "a partial artifact carries no facts"

(* The facts of a unit that failed to typecheck are not extracted at all. *)
let () =
  let cmt = Cmt_format.read_cmt "mtf_broken.cmt" in
  Printf.printf "partial implementation: %b\n"
    (match cmt.cmt_annots with
     | Partial_implementation _ -> true
     | Implementation _ | Interface _ | Packed _ | Partial_interface _
     | Functorize ->
         false);
  Printf.printf "present: %b\n"
    (Option.is_some cmt.cmt_module_implementation_facts)
