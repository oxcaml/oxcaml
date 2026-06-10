(* TEST

 readonly_files = "\
   monoid.mli \
   unboxed_product_monoid.mli \
   string_monoid.ml string_monoid.mli \
   mixed_monoid_of_monoid.ml mixed_monoid_of_monoid.mli \
   unboxed_product_monoid_utils.ml unboxed_product_monoid_utils.mli \
   test_param_arg_mixed.ocamlobjinfo.reference \
 ";

 setup-ocamlopt.byte-build-env;

 flags = "-as-parameter";
 module = "monoid.mli";
 ocamlopt.byte;

 flags = "-as-parameter";
 module = "unboxed_product_monoid.mli";
 ocamlopt.byte;

 flags = "-as-argument-for Monoid";
 module = "string_monoid.mli string_monoid.ml";
 ocamlopt.byte;

 flags = "-parameter Monoid -as-argument-for Unboxed_product_monoid";
 module = "mixed_monoid_of_monoid.mli mixed_monoid_of_monoid.ml";
 ocamlopt.byte;

 flags = "-parameter Unboxed_product_monoid";
 module = "unboxed_product_monoid_utils.mli unboxed_product_monoid_utils.ml";
 ocamlopt.byte;

 flags = "-instantiate";
 module = "";
 program = "mixed_monoid_of_monoid-String_monoid.cmx";
  (* Parameterised module with a mixed module block, used as an argument. *)
 all_modules = "mixed_monoid_of_monoid.cmx string_monoid.cmx";
 ocamlopt.byte;

 flags = "-instantiate";
 module = "";
 program = "unboxed_product_monoid_utils-Mixed_monoid_of_monoid--String_monoid.cmx";
  (* Instantiation whose argument is itself an instance with a mixed module
     block; the argument block is projected out of that mixed block. *)
 all_modules = "\
   unboxed_product_monoid_utils.cmx \
   mixed_monoid_of_monoid-String_monoid.cmx \
 ";
 ocamlopt.byte;

 flags = "-w -misplaced-attribute";
 module = "test_param_arg_mixed.ml";
 program = "";
 all_modules = "";
 ocamlopt.byte;

 flags = "";
 module = "";
 program = "test_param_arg_mixed.exe";
 all_modules = "\
   string_monoid.cmx \
   mixed_monoid_of_monoid.cmx \
   mixed_monoid_of_monoid-String_monoid.cmx \
   unboxed_product_monoid_utils.cmx \
   unboxed_product_monoid_utils-Mixed_monoid_of_monoid--String_monoid.cmx \
   test_param_arg_mixed.cmx \
 ";
 ocamlopt.byte;

 program = "${test_build_directory}/test_param_arg_mixed.exe";
 run;

 check-program-output;

 program = "\
   -no-code -no-approx \
   mixed_monoid_of_monoid.cmx \
   mixed_monoid_of_monoid-String_monoid.cmx \
 ";
  (* Check the recorded argument descriptions: the argument block index must
     be paired with the representation of the block it indexes into (the
     block returned by the instantiating functor, which is mixed here). *)
 output = "test_param_arg_mixed.ocamlobjinfo.output";
 ocamlobjinfo;

 reference = "test_param_arg_mixed.ocamlobjinfo.reference";
 check-program-output;
*)

external to_float : float# -> float = "%float_of_float#"
external to_int64 : int64# -> int64 = "%box_int64"

module Mixed_string_monoid =
  Mixed_monoid_of_monoid (Monoid) (String_monoid)
[@jane.non_erasable.instances]

module M =
  Unboxed_product_monoid_utils
    (Unboxed_product_monoid)
    (Mixed_monoid_of_monoid (Monoid) (String_monoid))
[@jane.non_erasable.instances]

let () = print_endline "Test: parameterised argument module with mixed block"

let () =
  let #(a, b, c, _void) =
    Mixed_string_monoid.append
      #(#2L, #2.0, "a", Mixed_string_monoid.void ())
      #(#4L, #3.0, "b", Mixed_string_monoid.void ())
  in
  print_endline "Expected: 8 6.0 ab";
  Printf.printf "Actual:   %s %.1f %s\n\n"
    (Int64.to_string (to_int64 a)) (to_float b) c
;;

let () =
  let #(a, b, c, _void) =
    M.pow #(#2L, #3.0, "x", Mixed_string_monoid.void ()) 4
  in
  print_endline "Expected: 16 81.0 xxxx";
  Printf.printf "Actual:   %s %.1f %s\n"
    (Int64.to_string (to_int64 a)) (to_float b) c
;;
