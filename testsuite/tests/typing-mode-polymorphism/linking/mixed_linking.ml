(* TEST
 readonly_files = "lib.ml";
 setup-ocamlc.byte-build-env;
 flags += "-extension mode_polymorphism_alpha";
 module = "lib.ml";
 ocamlc.byte;
 flags += "-no-extension mode_polymorphism_alpha -I ocamlc.byte \
   ocamlc.byte/lib.cmo";
 expect;
*)

let x = Lib.id 42
[%%expect{|
Uncaught exception: File "typing/typedtree.ml", line 160, characters 2-8: Assertion failed

|}]

let y = Lib.apply (fun n -> n + 1) 41
[%%expect{|
Uncaught exception: File "typing/typedtree.ml", line 150, characters 2-8: Assertion failed

|}]

let f = Lib.id
let z = f 42
[%%expect{|
val f : 'a -> 'a = <fun>
Uncaught exception: File "typing/typedtree.ml", line 160, characters 2-8: Assertion failed

|}]
