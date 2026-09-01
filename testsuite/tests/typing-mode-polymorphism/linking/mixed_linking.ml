(* TEST
 readonly_files = "lib.ml";
 setup-ocamlc.byte-build-env;
 flags += "-extension mode_polymorphism_alpha";
 module = "lib.ml";
 ocamlc.byte;
 flags += " -no-extension mode_polymorphism_alpha -I ocamlc.byte \
   ocamlc.byte/lib.cmo";
 expect;
*)

let x = Lib.id 42
[%%expect{|
val x : int = 42
|}]

let y = Lib.apply (fun n -> n + 1) 41
[%%expect{|
val y : int = 42
|}]

let f = Lib.id
let z = f 42
[%%expect{|
val f : 'a -> 'a = <fun>
val z : int = 42
|}]
