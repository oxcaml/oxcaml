(* TEST
   flags = "-extension layout_poly_alpha";
   readonly_files = "layout_sigs.mli";
   setup-ocamlc.byte-build-env;
   module = "layout_sigs.mli";
   ocamlc.byte;
   module = "import_layout_poly.ml";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

(* CR-soon layouts: update these tests as soon as instantiation works. *)

let _ = fun (module M : Layout_sigs.Sig_with_repr) -> M.foo

let _ = fun (module M : Layout_sigs.Sig_with_layout_1) -> ()

let _ = fun (module M : Layout_sigs.Sig_with_layout_2) -> ()
