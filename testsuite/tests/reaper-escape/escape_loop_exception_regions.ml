(* TEST
   flambda2;
   stack-allocation;
   flags += "-flambda2-reaper -X reaper-escape=1 -reaper-debug-flags=nostamps";
   { native with dump-reaper; check-fexpr-dump; }
 *)

(* Unlike [escape_loop_try], there is no pre-existing region for [acc]. In
   [inside], the reaper can introduce an iteration region: the handler's
   try-region reset must preserve [p], which was allocated before the try.
   In [outside], the handler is outside the iteration scope, which must remain
   unsafe for local allocation. [boom] also opens a function region: on raising,
   the caller's try-region reset must unwind that region too. *)
external local_stack_offset : unit -> int = "caml_local_stack_offset"

let[@inline never] sum_pair (p : int * int) = fst p + snd p

let[@inline never] boom (p : int * int) =
  let q = fst p + 1, snd p + 2 in
  let value = sum_pair q in
  if fst p mod 3 = 0 then raise Exit else value

let[@inline never] inside n =
  let acc @ global = ref 0 in
  for i = 1 to n do
    let p = i, i in
    try acc := !acc + boom p with Exit ->
      let q = i + 10, i + 20 in
      acc := !acc + sum_pair p + sum_pair q
  done;
  !acc

let[@inline never] outside n =
  let acc @ global = ref 0 in
  (try
     for i = 1 to n do
       let p = i, i in
       acc := !acc + boom p
     done
   with Exit -> ());
  !acc

let () =
  let before = local_stack_offset () in
  let a = inside 10 in
  let b = outside 10 in
  let after = local_stack_offset () in
  (Printf.printf [@inlined never]) "%d %d; stack growth %d\n"
    a b (after - before)
