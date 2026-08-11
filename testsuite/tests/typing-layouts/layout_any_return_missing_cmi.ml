(* TEST
 readonly_files = "layout_any_return_missing_cmi_m.ml layout_any_return_missing_cmi_b.ml layout_any_return_missing_cmi_d.ml";
 setup-ocamlopt.opt-build-env;
 module = "layout_any_return_missing_cmi_m.ml";
 ocamlopt.opt;
 module = "layout_any_return_missing_cmi_b.ml";
 ocamlopt.opt;
 script = "mv layout_any_return_missing_cmi_m.cmi hidden_m_cmi";
 script;
 module = "layout_any_return_missing_cmi_d.ml";
 ocamlopt.opt;
 script = "mv hidden_m_cmi layout_any_return_missing_cmi_m.cmi";
 script;
 module = "layout_any_return_missing_cmi.ml";
 ocamlopt.opt;
 module = "";
 all_modules = "layout_any_return_missing_cmi_m.cmx layout_any_return_missing_cmi_b.cmx layout_any_return_missing_cmi_d.cmx layout_any_return_missing_cmi.cmx";
 ocamlopt.opt;
 run;
 check-program-output;
*)

(* A unit compiled without some cmi sees types from it at layout [any], so
   its functions returning such types become unknown-result forwarders,
   while callers holding the cmi compile concrete-result calls against the
   same functions.  The two conventions must agree at runtime. *)

let () =
  print_int
    (Layout_any_return_missing_cmi_m.get (Layout_any_return_missing_cmi_d.f ()));
  print_newline ()
