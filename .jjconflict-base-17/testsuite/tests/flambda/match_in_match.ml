(* TEST
 flambda;
 ocamlopt_flags = "-flambda2-match-in-match";
 native;
*)

(* This test checks the application of the match-in-match optimization. The optimization
   requires joins to be computed, which only happens at O2 and O3 levels, therefore we
   enforce O3 *)
[@@@ocaml.flambda_o3]

(* Code tested : the goal is to eliminate the allocation of the intermediate
   `Ok x` in the [g] function (the one bound to `y`). The challenge is
   that, even though the intermediate variant in `y` is indeed unboxed, the
   final call to `bind` (after inlining) sometimes return that value, and thus
   without specialization/duplication of the code, we cannot easily erase the
   boxed version. *)
module Bind = struct
  type 'a foo = Null | This of 'a

  let[@inline never] f x =
    if x = 0 then This x else Null

  let e1 = Error "aye"
  let e2 = Error "ouille"

  let[@inline] bind x ~f =
      match x with
      | Error _ as e -> e
      | Ok x -> (f [@inlined hint]) x

  let g x =
    let y =
      match f x with
      | Null -> Ok x
      | This _ -> e1
    in
    bind y ~f:(fun y -> e2) [@nontail]

  let test i = g i

end

(* Test harness *)

let check_noalloc ~n name f =
  let a0 = Gc.allocated_bytes () in
  let a1 = Gc.allocated_bytes () in
  for i = 1 to n do
    ignore ((f[@inlined never]) i)
  done;
  let a2 = Gc.allocated_bytes () in
  let alloc = (a2 -. 2. *. a1 +. a0) in
  if alloc > 100. then
    failwith (Printf.sprintf "%s; alloc = %.0f" name alloc)

let () =
  check_noalloc ~n:1_000 "bind" Bind.test

