(* TEST
 {
   set c_or_opt = "c";
   setup-ocaml${c_or_opt}.byte-build-env;
   ocaml${c_or_opt}.byte;
   file = "${program}";
   file-exists;
 }{
   set t0 = "code";
   set t1 = "byte${t0}";
   ${t1};
   file = "${program}";
   file-exists;
 }{
   set act = "pass";
   ${act};
   (* CR-someday lmaurer: When we have [not], use that to make sure that nothing
      has run. *)
   act = "bytecode";
   ${act};
   file = "${program}";
   file-exists;
 }
*)

let () = print_endline "ran a test via an expanded action name"
