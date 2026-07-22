(* TEST
 stack-allocation;
 flambda2;
 setup-ocamlopt.byte-build-env;
 unset OCAMLPARAM;
 ocamlopt_byte_exit_status = "2";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

let[@unboxable] ret_local : unit -> float @ local = fun () -> exclave_ 1.0
let[@unboxable] ret_heap : unit -> float = fun () -> 2.0

let () =
  let a = int_of_float (ret_local ()) in
  let b = int_of_float (ret_heap ()) in
  assert (a = 1);
  assert (b = 2);
  print_endline "ok"
