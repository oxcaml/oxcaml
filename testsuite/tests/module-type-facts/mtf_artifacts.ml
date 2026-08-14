(* TEST
 modules = "mtf_facts.ml";
 readonly_files = "mtf_aux.mli mtf_aux.ml mtf_client.ml mtf_broken.ml \
                   mtf_gate_cmt.ml mtf_gate_cms.ml mtf_gate_none.ml \
                   mtf_param.mli mtf_argument.ml";
 include ocamlcommon;
 setup-ocamlc.byte-build-env;
 flags = "-bin-annot -bin-annot-cms";
 module = "mtf_aux.mli";
 ocamlc.byte;
 module = "mtf_aux.ml";
 ocamlc.byte;
 module = "mtf_client.ml";
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
 flags = "-bin-annot -as-parameter";
 module = "mtf_param.mli";
 ocamlc.byte;
 flags = "-bin-annot -as-argument-for Mtf_param";
 module = "mtf_argument.ml";
 ocamlc.byte;
 flags = "";
 module = "";
 program = "${test_build_directory}/mtf_artifacts.byte";
 all_modules = "mtf_facts.ml mtf_artifacts.ml";
 ocamlc.byte;
 run;
 check-program-output;
*)

(* Module-type facts as they survive in the artifacts of separately compiled
   units: the [.cmt] and [.cmti] of an implementation and its interface, the
   [.cms] and [.cmsi] holding the same facts, and the facts of a unit whose
   expectations all come from another unit's [.cmi].  See [mtf_facts.ml] for
   the output format. *)

open Mtf_facts

let read_cmt file = Cmt_format.read_cmt file

(* The uids of [Mtf_aux] appear in the facts of both its own artifacts and
   those of [Mtf_client]; the interface ones are marked [[intf]], as
   [ocamlobjinfo -uid-deps] does. *)
let aux_labels () =
  let implementation = read_cmt "mtf_aux.cmt" in
  let interface = read_cmt "mtf_aux.cmti" in
  labels_of_annots implementation.cmt_annots
  @ labels_of_annots ~prefix:"[intf]" interface.cmt_annots

let () = heading "facts of an implementation, read back from its .cmt"

let () =
  let cmt = read_cmt "mtf_aux.cmt" in
  Printf.printf "present: %b\n" cmt.cmt_module_implementation_facts_present;
  print_facts ~sites:false
    (printer (aux_labels ()))
    cmt.cmt_module_implementation_facts

let () = heading "facts of an interface, read back from its .cmti"

let () =
  let cmti = read_cmt "mtf_aux.cmti" in
  Printf.printf "present: %b\n" cmti.cmt_module_implementation_facts_present;
  print_facts ~sites:false
    (printer (labels_of_annots cmti.cmt_annots))
    cmti.cmt_module_implementation_facts

let () = heading "the interface pairs of the unit are directional"

(* The [Interface] dependencies whose two ends are named declarations are the
   pairs of the [.ml] and the [.mli] of the unit; each pair is recorded in one
   direction only. *)
let () =
  let cmt = read_cmt "mtf_aux.cmt" in
  let labels = aux_labels () in
  print_interface_pairs (printer labels) cmt.cmt_module_implementation_facts

let () = heading ".cms and .cmsi hold the same facts as .cmt and .cmti"

let () =
  let cms = Cms_format.read "mtf_aux.cms" in
  let cmsi = Cms_format.read "mtf_aux.cmsi" in
  let cmt = read_cmt "mtf_aux.cmt" in
  let cmti = read_cmt "mtf_aux.cmti" in
  Printf.printf "present: cms %b cmsi %b\n"
    cms.cms_module_implementation_facts_present
    cmsi.cms_module_implementation_facts_present;
  Printf.printf "cms = cmt: %b\ncmsi = cmti: %b\n"
    (Facts.compare cms.cms_module_implementation_facts
       cmt.cmt_module_implementation_facts
     = 0)
    (Facts.compare cmsi.cms_module_implementation_facts
       cmti.cmt_module_implementation_facts
     = 0);
  (* All four lists are printed from the [.cms] itself, so that the test would
     notice a list that is dropped or reordered by the serialization of either
     artifact. *)
  print_facts ~sites:false
    (printer (aux_labels ()))
    cms.cms_module_implementation_facts

let () = heading "facts of a unit whose expectations come from a .cmi"

let () =
  let cmt = read_cmt "mtf_client.cmt" in
  let signature, _staticity =
    (Cmi_format.read_cmi "mtf_aux.cmi").cmi_sign
  in
  let labels =
    labels_of_annots cmt.cmt_annots
    @ signature_labels ~prefix:"Mtf_aux." signature
  in
  Printf.printf "present: %b\n" cmt.cmt_module_implementation_facts_present;
  print_facts ~sites:false (printer labels)
    cmt.cmt_module_implementation_facts

let () = heading "an ascription against another unit's signature is no pair"

(* Only the inclusion check of the [.ml] of a unit against its own [.mli]
   yields [Interface] facts.  [Mtf_client] has no [.mli], and the members of
   [Mtf_aux.Container] that its last ascription is checked against are declared
   in [mtf_aux.mli]; pairing them with the client's own declarations would
   claim an interface the client does not have.  Those members are still
   checked, as [Ascription] checks against the foreign expectation, in the
   facts printed above. *)
let () =
  let report description file =
    let facts = (read_cmt file).cmt_module_implementation_facts in
    Printf.printf "%s: interface checks %d, interface pairs %d\n" description
      (List.length (interface_checks facts))
      (List.length (interface_pairs facts))
  in
  report "mtf_aux, which has an .mli" "mtf_aux.cmt";
  report "mtf_client, which has none" "mtf_client.cmt"

let () = heading "the parameter expectation of a functor loaded from a .cmi"

(* [Types.Named] carries the uid of the module type expected of the parameter.
   It has to survive being written to and read back from the [.cmi], being
   substituted into, and being strengthened, or an application of [Mtf_aux.F]
   would have no expectation to check its argument against. *)
let () =
  Load_path.init ~auto_include:Load_path.no_auto_include
    ~visible:[ { Clflags.path = "."; cmx_guaranteed = false } ] ~hidden:[];
  let env = Lazy.force Env.initial in
  let aux = Path.Pident (Ident.create_persistent "Mtf_aux") in
  let path = Path.Pdot (aux, "F") in
  let signature, _staticity =
    (Cmi_format.read_cmi "mtf_aux.cmi").cmi_sign
  in
  let labels = signature_labels ~prefix:"Mtf_aux." signature in
  let print description module_type =
    match (module_type : Types.module_type) with
    | Mty_functor (Named (_, _, expectation, _), _, _) ->
        Printf.printf "%s: %s\n" description
          (match expectation with
           | Some uid -> uid_name (printer labels) uid
           | None -> "none")
    | Mty_functor (Unit, _, _)
    | Mty_ident _ | Mty_signature _ | Mty_alias _ | Mty_strengthen _ ->
        Printf.printf "%s: not a named functor\n" description
  in
  let module_type = (Env.find_module path env).md_type in
  print "loaded from the .cmi" (Mtype.scrape_alias env module_type);
  print "after substitution"
    (Subst.modtype Keep Subst.identity (Mtype.scrape_alias env module_type));
  print "after strengthening"
    (Mtype.scrape_alias env
       (Mtype.strengthen ~aliasable:true (Mtype.scrape_alias env module_type)
          path))

let () = heading "the interface a unit is an argument for"

(* [-as-argument-for] checks the unit against the signature of a parameter
   unit rather than against an [.mli] of its own; the expectation of that
   check is the parameter unit itself. *)
let () =
  let cmt = read_cmt "mtf_argument.cmt" in
  print_facts ~sites:false
    (printer (labels_of_annots cmt.cmt_annots))
    cmt.cmt_module_implementation_facts

let () = heading "a partial artifact carries no facts"

let () =
  let cmt = read_cmt "mtf_broken.cmt" in
  Printf.printf "partial implementation: %b\n"
    (match cmt.cmt_annots with
     | Partial_implementation _ -> true
     | Implementation _ | Interface _ | Packed _ | Partial_interface _ ->
         false);
  Printf.printf "present: %b\n" cmt.cmt_module_implementation_facts_present;
  print_digest cmt.cmt_module_implementation_facts

let () = heading "facts are only extracted for the artifacts that are written"

let () =
  let report unit_ =
    let exists extension = Sys.file_exists (unit_ ^ extension) in
    let facts_present extension read present =
      if exists extension
      then Printf.sprintf "%b" (present (read (unit_ ^ extension)))
      else "no artifact"
    in
    Printf.printf "%s: cmt %s, cms %s\n" unit_
      (facts_present ".cmt" read_cmt (fun cmt ->
           cmt.Cmt_format.cmt_module_implementation_facts_present))
      (facts_present ".cms" Cms_format.read (fun cms ->
           cms.Cms_format.cms_module_implementation_facts_present))
  in
  report "mtf_gate_cmt";
  report "mtf_gate_cms";
  report "mtf_gate_none"
