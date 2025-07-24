(* TEST_BELOW
   (* Blank lines added here to preserve locations. *)
*)

let f = fun [@inline] [@inline never] x -> x

let g = fun [@inline] [@something_else] [@ocaml.inline] x -> x

let h x = (g [@inlined] [@ocaml.inlined never]) x

let v = (fun [@inline] [@inlined] x -> x) 1 (* accepted *)

let i = fun [@inline] x -> x [@@inline]

(* TEST
   flags = "-w +A-70";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)
