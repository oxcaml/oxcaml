(* TEST
 include stdlib_stable;
 include stdlib_upstream_compatible;
 flambda2;
 {
 native;
 } {
 flags = "-O3";
 native;
 } {
 flags = "-Oclassic";
 native;
 } {
 bytecode;
 }
*)
open Stdlib_stable

let ints = Array.init 256 Fun.id
let chars = Array.init 256 Char.chr

let () =
  let f c = Char_u.to_char (Char_u.of_char c) in
  Array.iter (fun c -> assert (Char.equal c (f c))) chars

let () =
  let f i = Char_u.to_int8_u (Char_u.of_int8_u i) in
  Array.iter
    (fun i -> let i = Int8_u.of_int i in assert (Int8_u.equal i (f i))) ints

let test_unary f f_u eq =
  Array.iter (fun c -> assert (eq (f c) (f_u (Char_u.of_char c)))) chars

let test_binary f f_u eq =
  Array.iter (fun c -> test_unary (f c) (f_u (Char_u.of_char c)) eq) chars

let () = test_unary Char.code Char_u.code Int.equal

let () = test_unary Char.escaped Char_u.escaped String.equal

let () =
  test_unary Char.lowercase_ascii
    (fun c -> Char_u.to_char (Char_u.lowercase_ascii c)) Char.equal

let () =
  test_unary Char.uppercase_ascii
    (fun c -> Char_u.to_char (Char_u.uppercase_ascii c)) Char.equal

let () = test_binary Char.equal Char_u.equal Bool.equal

let () = test_binary Char.compare Char_u.compare (fun a b -> a * b > 0 || a = b)
