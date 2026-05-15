(* TEST
 flags = "-w +A-70";
 setup-ocamlc.byte-build-env;
 compile_only = "true";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

let[@zero_alloc] w = 42

let x : int = 42 [@@zero_alloc]

let[@zero_alloc assume] foo =
  let y = 42 in
  fun z -> z + y

let[@zero_alloc] bar =
  let y = 42 in
  fun z -> z + y

let[@zero_alloc] (x as y) = 42
