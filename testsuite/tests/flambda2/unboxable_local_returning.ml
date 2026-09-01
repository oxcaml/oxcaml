(* TEST
 stack-allocation;
 flambda2;
 setup-ocamlopt.byte-build-env;
 unset OCAMLPARAM;
 ocamlopt.byte with dump-simplify;
 check-ocamlopt.byte-output;
 check-fexpr-dump;
 run;
 check-program-output;
*)

let[@unboxable] ret_local : unit -> float @ local = fun () -> exclave_ 1.0
let[@unboxable] ret_heap : unit -> float = fun () -> 2.0

let () =
  let a = int_of_float (ret_local ()) in
  let b = int_of_float (ret_heap ()) in
  assert (a = 1);
  assert (b = 2);
  (* [@inlined never] keeps stdlib internals (e.g. the [stdout] value slot
     projection) out of the fexpr dump; their names depend on the flags the
     stdlib was compiled with (e.g. reaper renames value slots). *)
  (print_endline [@inlined never]) "ok"
