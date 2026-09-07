(* TEST
 flambda2;
 ocamlopt_flags += " -O3 -no-flambda2-reaper";
 ocamlopt_flags += " -zero-alloc-check all -dflambda-invariants";
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte;
 run;
 check-program-output;
*)

[@@@ocaml.flambda_o3]

type cell = { mutable value : int }

(* Inlining exposes the allocation to flow's mutable unboxing, without any
   specialisation sites. Keeping this separate prevents Lambda unboxing it. *)
let[@inline always] accumulate cell n =
  for i = 1 to n do
    cell.value <- cell.value + i
  done;
  cell.value

let[@zero_alloc] sum n =
  let cell = { value = 0 } in
  accumulate cell n

let () =
  assert (sum (-1) = 0);
  assert (sum 0 = 0);
  assert (sum 10 = 55)
