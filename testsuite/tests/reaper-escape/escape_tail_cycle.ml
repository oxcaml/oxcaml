(* TEST
   flambda2;
   stack-allocation;
   flags += "-flambda2-reaper -X reaper-escape=1 -reaper-debug-flags=nostamps";
   { native with dump-reaper; check-fexpr-dump; }
 *)

(* [ping] and [pong] call each other in tail position and return an allocation
   to a known call site. They must not be made to return at mode local: having
   then no region of their own, they would leave [tmp] behind in the region of
   [run] at every iteration, in a loop that runs in constant space. The pairs
   they return stay on the heap, [tmp] is local to each activation, and the
   local stack does not grow. *)
external local_stack_offset : unit -> int = "caml_local_stack_offset"

let[@inline never] sum_pair (p : int * int) = fst p + snd p

let peak = ref 0

module M : sig
  val run : int -> int
end = struct
  let rec ping n acc =
    if n = 0
    then (
      peak := local_stack_offset ();
      acc, acc)
    else
      let tmp = n, acc in
      pong (n - 1) (sum_pair tmp land 0xffff)

  and pong n acc =
    if n = 0
    then (
      peak := local_stack_offset ();
      acc, n)
    else
      let tmp = acc, n in
      ping (n - 1) (sum_pair tmp land 0xffff)

  let[@inline never] run n =
    let p = ping n 0 in
    (sum_pair p [@nontail])
end

let () =
  let before = local_stack_offset () in
  let r = M.run 1_000_000 in
  (Printf.printf [@inlined never]) "%d; stack growth %d\n" r (!peak - before)
