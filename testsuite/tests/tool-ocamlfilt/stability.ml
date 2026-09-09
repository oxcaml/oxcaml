(* TEST
 native-compiler;
 readonly_files = "stability_lib.ml stability_lib_edited.ml \
                   stability_edited.ml";
 setup-ocamlopt.byte-build-env;
 flags = "-name-mangling-scheme structured -O3 -c";
 module = "stability_lib.ml";
 ocamlopt_byte_exit_status = "0";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
 module = "stability_lib_edited.ml";
 ocamlopt_byte_exit_status = "0";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
 module = "stability_edited.ml";
 ocamlopt_byte_exit_status = "0";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
 module = "";
 ocamlopt_byte_exit_status = "0";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
 output = "stability.output";
 script = "sh ${test_source_directory}/stability.sh";
 script;
 reference = "${test_source_directory}/stability.reference";
 check-program-output;
*)

(* The point of the structured name-mangling scheme is that demangled names
   do not change when unrelated code changes. This unit and [stability_lib]
   are compiled twice: as written, and as [stability_edited] and
   [stability_lib_edited], which add a comment and unrelated top-level
   functions. [stability.sh] checks that the demangled names of every other
   function are the same in both compilations, including the closure of
   [make_adder] that gets copied into this unit by inlining. The reference
   lists those names. *)

let adder = Stability_lib.make_adder (Sys.opaque_identity 10)

let[@inline never] double_even numbers =
  let even = List.filter (fun[@cold] x -> x mod 2 = 0) numbers in
  List.map (fun[@cold] x -> x * 2) even

let[@inline never] labelled ~a ~b = a - b

let[@inline never] omit_first x = labelled ~b:x

let curried = labelled ~a:(Sys.opaque_identity 1)

let thunk = lazy (Sys.opaque_identity 41 + 1)

let[@inline never] use_lib xs =
  Stability_lib.twice
    (fun[@cold] z -> z * 3)
    (List.length (Stability_lib.scale_all 2 xs))

let () =
  ignore (adder 1);
  ignore (double_even [ 1; 2; 3; 4 ]);
  ignore (omit_first 1 ~a:2);
  ignore (curried ~b:3);
  ignore (Lazy.force thunk);
  ignore (use_lib [ 5; 6 ])
