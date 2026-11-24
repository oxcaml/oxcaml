(* TEST
 flambda2;
 stack-allocation;
 readonly_files = "escape_result_types_lib.ml";
 setup-ocamlopt.byte-build-env;
 flags = "-O3 -flambda2-reaper -X reaper-escape=1 \
          -flambda2-result-types-all-functions";
 module = "escape_result_types_lib.ml";
 ocamlopt.byte;
 module = "escape_result_types.ml";
 ocamlopt.byte;
 module = "";
 program = "${test_build_directory}/escape_result_types.exe";
 all_modules = "escape_result_types_lib.cmx escape_result_types.cmx";
 ocamlopt.byte;
 run;
 check-program-output;
*)

(* See [escape_result_types_lib.ml]. *)
let[@inline never] sum7 (t : int * int * int * int * int * int * int) =
  let a, b, c, d, e, f, g = t in
  a + b + c + d + e + f + g

let[@inline never] test n =
  let r = Escape_result_types_lib.run n in
  let big = n + 1, n + 2, n + 3, n + 4, n + 5, n + 6, n + 7 in
  let s = sum7 big in
  r + s + Escape_result_types_lib.run (n + 1)

let () = (Printf.printf [@inlined never]) "%d\n" (test 10)
