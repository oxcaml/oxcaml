(* TEST
 modules = "run_phrases.ml";
 flags = "-I +compiler-libs -I +unix -I +dynlink -I +ocaml-jit -linkall -w -58";
 libraries = "unix dynlink ocamlcommon ocamlfrontend ocamloptcomp";
 libraries += " ocamlopttoplevel jit";
 native;
*)

(* Phrases that need generic functions which the program does not contain:
   [caml_curry25] and its companions for a function of 25 parameters, and
   [caml_apply25] for an application of an unknown function to 25 arguments.
   The JIT generates them with the first phrase that needs them, and later
   phrases use those. Should the program come to contain them, the test says
   so and needs another number of parameters.

   The phrases print without formats, so that this test does not depend on
   what static_data.ml tests.

   Warning 58 is disabled because the [.cmx] files of some modules of the
   libraries are not installed. *)

let () =
  List.iter
    (fun name ->
      if Option.is_some (Jit.jit_lookup_symbol name)
      then Printf.printf "the program contains %s\n" name)
    ["caml_curry25"; "caml_apply25"];
  Run_phrases.run
    {|
let sum25 a b c d e f g h i j k l m n o p q r s t u v w x y =
  a + b + c + d + e + f + g + h + i + j + k + l + m + n + o + p + q + r + s
  + t + u + v + w + x + y;;
let () =
  print_string "sum25 = ";
  print_int
    ((Sys.opaque_identity sum25)
       1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25);
  print_newline ();;
let alt25 a b c d e f g h i j k l m n o p q r s t u v w x y =
  a - b + c - d + e - f + g - h + i - j + k - l + m - n + o - p + q - r + s
  - t + u - v + w - x + y;;
let () =
  print_string "alt25 = ";
  print_int
    ((Sys.opaque_identity alt25)
       1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25);
  print_newline ();;
let () =
  let partial =
    (Sys.opaque_identity sum25)
      1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24
  in
  print_string "partial sum25 = ";
  print_int (partial 0);
  print_newline ();;
|}
