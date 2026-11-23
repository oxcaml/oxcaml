(* TEST
 setup-ocamlopt.byte-build-env;
 flags = "-for-pack Pack";
 module = "packed.ml";
 ocamlopt.byte;
 module = "";
 flags = "-pack";
 program = "pack.cmx";
 all_modules = "packed.cmx";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
 {
   program = "-no-code -no-approx pack.cmx";
   ocamlobjinfo;
   check-program-output;
 }{
   program = "pack.cmx";
   ocamlobjinfo;
   (* As in question.ml, the full output varies too much to check. *)
 }
*)

(* The static data of a packed .cmx must be in the section that the .cmx
   records for it: [ocamlobjinfo] used to crash reading it. [answer] is a
   function so that the pack also has a code section. *)
let answer () = 42
