(* TEST
 readonly_files = "\
   unboxed_product_monoid.mli \
   monoid.mli \
   unboxed_product_monoid_utils.ml unboxed_product_monoid_utils.mli \
   unboxed_product_monoid_utils_mixed.ml unboxed_product_monoid_utils_mixed.mli \
   mixed_monoid.ml mixed_monoid.mli \
   string_monoid.ml string_monoid.mli \
   mixed_with_monoid.ml mixed_with_monoid.mli \
 ";

 setup-ocamlopt.byte-build-env;

 flags = "-as-parameter";
 module = "unboxed_product_monoid.mli";
 ocamlopt.byte;

 flags = "-as-parameter";
 module = "monoid.mli";
 ocamlc.byte;

 flags = "-as-argument-for Unboxed_product_monoid";
 module = "mixed_monoid.mli mixed_monoid.ml";
 ocamlopt.byte;

 flags = "-as-argument-for Monoid";
 module = "string_monoid.mli string_monoid.ml";
 ocamlopt.byte;

 flags = "-parameter Unboxed_product_monoid";
 module = "unboxed_product_monoid_utils.mli unboxed_product_monoid_utils.ml";
 ocamlopt.byte;

 flags = "-parameter Unboxed_product_monoid";
 module = "unboxed_product_monoid_utils_mixed.mli unboxed_product_monoid_utils_mixed.ml";
 ocamlopt.byte;

 flags = "-parameter Monoid";
 module = "mixed_with_monoid.mli mixed_with_monoid.ml";
 ocamlopt.byte;

 flags = "-instantiate";
 module = "";
 program = "unboxed_product_monoid_utils-Mixed_monoid.cmx";
  (* Value-only module that takes a mixed module as a parameter. *)
 all_modules = "unboxed_product_monoid_utils.cmx mixed_monoid.cmx";
 ocamlopt.byte;

 flags = "-instantiate";
 module = "";
 program = "unboxed_product_monoid_utils_mixed-Mixed_monoid.cmx";
  (* Mixed module that takes a mixed module as a parameter. *)
 all_modules = "unboxed_product_monoid_utils_mixed.cmx mixed_monoid.cmx";
 ocamlopt.byte;

 flags = "-instantiate";
 module = "";
 program = "mixed_with_monoid-String_monoid.cmx";
  (* Mixed module that takes a value-only module as a parameter. *)
 all_modules = "mixed_with_monoid.cmx string_monoid.cmx";
 ocamlopt.byte;

 flags = "-w -misplaced-attribute";
 module = "test_mixed_monoid.ml";
 program = "";
 all_modules = "";
 ocamlopt.byte;

 flags = "";
 module = "";
 program = "test_mixed_monoid.exe";
 all_modules = "\
   mixed_monoid.cmx \
   string_monoid.cmx \
   unboxed_product_monoid_utils.cmx \
   unboxed_product_monoid_utils_mixed.cmx \
   mixed_with_monoid.cmx \
   unboxed_product_monoid_utils-Mixed_monoid.cmx \
   unboxed_product_monoid_utils_mixed-Mixed_monoid.cmx \
   mixed_with_monoid-String_monoid.cmx \
   test_mixed_monoid.cmx \
 ";
 ocamlopt.byte;

 program = "${test_build_directory}/test_mixed_monoid.exe";
 run;

 check-program-output;
*)

external to_float : float# -> float = "%float_of_float#"
external to_int64 : int64# -> int64 = "%box_int64"

module M_mixed =
  Unboxed_product_monoid_utils (Unboxed_product_monoid) (Mixed_monoid)
[@jane.non_erasable.instances]

module M_mixed_utils_mixed =
  Unboxed_product_monoid_utils_mixed (Unboxed_product_monoid) (Mixed_monoid)
[@jane.non_erasable.instances]

module M_with_string =
  Mixed_with_monoid (Monoid) (String_monoid)
[@jane.non_erasable.instances]

let () = print_endline "Test: Mixed monoid with utils"

let () =
  let #(a, b, c, _void) =
    M_mixed.pow #(#2L, #3.0, "x", Mixed_monoid.void ()) 4
  in
  print_endline "Expected: 16 81.0 xxxx";
  Printf.printf "Actual:   %s %.1f %s\n\n"
    (Int64.to_string (to_int64 a)) (to_float b) c
;;

let () = print_endline "Test: Mixed monoid with utils_mixed"

let () =
  let #(a, b, c, _void) =
    M_mixed_utils_mixed.pow #(#2L, #2.0, "a", Mixed_monoid.void ()) 5
  in
  print_endline "Expected: 32 32.0 aaaaa";
  Printf.printf "Actual:   %s %.1f %s\n"
    (Int64.to_string (to_int64 a)) (to_float b) c
;;

let () =
  let #(a, b, c, _void) = M_mixed_utils_mixed.one in
  print_endline "Expected: 1 1.0 (empty string)";
  Printf.printf "Actual:   %s %.1f %s\n\n"
    (Int64.to_string (to_int64 a))
    (to_float b)
    (if c = "" then "(empty string)" else c)
;;

let () = print_endline "Test: Mixed with string monoid"

let () =
  let #(a, b, c) =
    M_with_string.append
      #(#2.0, "hello ", #2L) #(#4.0, "world", #4L)
  in
  print_endline "Expected: 8.0 hello world 8";
  Printf.printf "Actual:   %.1f %s %s\n"
    (to_float a)
    b
    (Int64.to_string (to_int64 c))
;;
