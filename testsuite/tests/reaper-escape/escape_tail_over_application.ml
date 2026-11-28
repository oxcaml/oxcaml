(* TEST
   flambda2;
   stack-allocation;
   flags += "-flambda2-reaper -experimental-reaper-escape -reaper-debug-flags=nostamps";
   { native with dump-reaper; check-fexpr-dump; }
 *)

(* An unknown-arity application of more arguments than the callee has
   parameters goes through [caml_applyN], which applies the callee to its first
   arguments in a region of its own. If the callee were a local closure with
   several parameters, the partial application made on the way would be
   allocated in that region, and [caml_applyN] could no longer tail call the
   last function applied. Here [over] applies [h], which has two parameters, to
   three arguments in tail position, in a loop made of tail calls only: [h] must
   stay on the heap, and the local stack must not grow with the number of
   iterations. *)
external local_stack_offset : unit -> int = "caml_local_stack_offset"

type fn = F of (int -> int -> fn -> int) [@@unboxed]

let depth = ref 0

let peak = ref 0

module M : sig
  val run : int -> int -> int
end = struct
  let[@inline never] over (F f) a b c = f a b c

  let rec loop f =
    if !depth = 0
    then (
      peak := local_stack_offset ();
      0)
    else (
      decr depth;
      over f 1 2 f)

  and step f = loop f

  let[@inline never] run n d =
    let h x y = if x + y + n > 0 then step else step in
    depth := d;
    (loop (F h) [@nontail])
end

let () =
  let growth d =
    let before = local_stack_offset () in
    let _ = M.run 1 d in
    !peak - before
  in
  let g5 = growth 5 in
  let g100 = growth 100 in
  (Printf.printf [@inlined never]) "stack growth %d %d\n" g5 g100
