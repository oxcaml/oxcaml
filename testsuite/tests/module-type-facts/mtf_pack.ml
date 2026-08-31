(* TEST
 modules = "mtf_facts.ml";
 readonly_files = "mtf_packed.mli mtf_pack_a.ml mtf_pack_b.ml";
 include ocamlcommon;
 setup-ocamlc.byte-build-env;
 flags = "-bin-annot";
 module = "mtf_packed.mli";
 ocamlc.byte;
 flags = "-bin-annot -for-pack Mtf_packed";
 module = "mtf_pack_a.ml";
 ocamlc.byte;
 module = "mtf_pack_b.ml";
 ocamlc.byte;
 module = "";
 flags = "-bin-annot -pack";
 program = "mtf_packed.cmo";
 all_modules = "mtf_pack_a.cmo mtf_pack_b.cmo";
 ocamlc.byte;
 flags = "";
 program = "${test_build_directory}/mtf_pack.byte";
 all_modules = "mtf_facts.ml mtf_pack.ml";
 ocamlc.byte;
 run;
 check-program-output;
*)

(* A packed unit is checked against its [.mli] like any other unit, so its
   [.cmt] must carry the facts of that interface check.  See [mtf_facts.ml]
   for where the contents of the facts are tested. *)

open Mtf_facts

let () = heading "a packed unit stores the facts of its interface check"

let () =
  let cmt = Cmt_format.read_cmt "mtf_packed.cmt" in
  Printf.printf "packed: %b\n"
    (match cmt.cmt_annots with
     | Packed _ -> true
     | Implementation _ | Interface _ | Partial_implementation _
     | Partial_interface _ | Functorize -> false);
  Printf.printf "present: %b\n"
    (Option.is_some cmt.cmt_module_implementation_facts)
