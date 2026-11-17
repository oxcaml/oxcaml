(* TEST
 no-frame_pointers;
 no-stack-checks;
 no-address-sanitizer;
 no-poll-insertion;
 shared-libraries;
 setup-ocamlopt.byte-build-env;
 set OCAMLPARAM = "_,poll-insertion=1";
 flags = "-shared";
 all_modules = "poll_ocamlparam.ml";
 program = "poll_ocamlparam.cmxs";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
 program = "-no-code -no-approx poll_ocamlparam.cmx";
 ocamlobjinfo;
 check-program-output;
*)

(* poll-insertion=1 in OCAMLPARAM ENABLES poll insertion (the key used to
   be inverted: it set the old [disable_poll_insertion] ref, so =1
   disabled insertion).  With no poll flag on the command line, the
   OCAMLPARAM key alone must yield a unit recorded as polled, which
   ocamlobjinfo prints as "Polled: true".  Gated to configurations where
   the rest of the pinned output is known (all axes off).  The
   installation need not have a library tree with poll insertion: the
   unit is then compiled against the default one. *)
let answer () = 42
