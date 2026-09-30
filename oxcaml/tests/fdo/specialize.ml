(* End-to-end FDO test program (see dune): compiled with -fdo-counters, run with
   the hot part traced by the runtime's single-stepping perf emulator, decoded,
   and recompiled with the resulting profile; the block frequencies and layouts
   of the recompilation are checked. The trace is complete (perf-like samples
   of 16 branches, but nothing in between them is lost), so the counts are
   exact and reproducible.

   [Make] is applied twice through [Outer], so its functions are specialized
   twice over (nested specialization: [Outer]'s inlined body defines [Inner] by
   inlining [Make]). Every path through [count] executes in both
   specializations, but with different frequencies: [Low.run] sees most values
   far above its limit, [High.run] sees most below. Their profiles must be kept
   apart for their layouts to come out different.

   The profile also holds the call graph: each function's entry (the trie root)
   refined by call site, whether the call was real ([run] calling [count],
   [count] calling [tally]) or inlined out ([classify] and [bump] in [count];
   their counters are on the branches into the code they were inlined into). *)

external trace : append:bool -> string -> (unit -> 'a) -> 'a
  = "caml_singlestep_trace"

module type Config = sig
  val limit : int
end

(* Toplevel, so that it is inlined by Flambda 2 (into [count]): its branches'
   counters get the call site as an inlining context, on top of the
   specialization of the copy of [count] they end up in. *)
let[@inline] classify (limit : int) x =
  if x < limit then if x land 1 = 0 then 0 else 1 else if x < 2 * limit then 2 else 3

(* Called side by side from one arm of the match in [count], so that they must
   come out with the same profile despite the different mechanisms: [bump] is
   inlined and straight-line, so nothing of it is left to carry its counter,
   which goes on the arm's edge instead; [tally] is a real call, which the call graph
   gets from the branch of the call instruction landing on its entry. *)
let[@inline] bump r = r := !r + 1

let[@inline never] tally r = r := !r + 1

module Make (C : Config) = struct
  (* A [while] loop is compiled with its test at the top and a jump back to it
     from the end of the body, the shape the layout rotates so that the loop
     is closed by the conditional branch instead. *)
  let[@inline never] count a =
    let even = ref 0 and odd = ref 0 and near = ref 0 and far = ref 0 in
    let i = ref 0 in
    while !i < Array.length a do
      (match classify C.limit (Array.unsafe_get a !i) with
      | 0 -> incr even
      | 1 -> incr odd
      | 2 ->
        bump near;
        tally near
      | _ -> incr far);
      incr i
    done;
    !even + (10 * !odd) + (100 * !near) + (1000 * !far)
end
[@@inline always]

module Outer (C : Config) = struct
  module Inner = Make (C)

  let[@inline never] run a = Inner.count a
end
[@@inline always]

module Low = Outer (struct
  let limit = 10
end)

module High = Outer (struct
  let limit = 30
end)

let () =
  (* Low: 5 even, 5 odd, 10 near, 80 far. High: 15 even, 15 odd, 30 near, 20
     far. *)
  let a = Array.init 100 (fun i -> i) in
  let b = Array.init 80 (fun i -> i) in
  let low, high =
    trace ~append:false Sys.argv.(1) (fun () -> Low.run a, High.run b)
  in
  Printf.printf "%d %d\n" low high

(* Exercise call/return markers and handler resets in emitted code, not just
   synthetic decoder traces: ordinary and tail calls, nested handlers, raising
   through an uninstrumented runtime helper, and returning after the catch. *)
let[@inline never] fail_or_return x = if x = 0 then raise Not_found else x
let[@inline never] tail_fail x = fail_or_return x

let[@inline never] protected x =
  try
    try tail_fail x with
    | Not_found -> raise Exit
  with
  | Exit -> 7

let[@inline never] after_protected x = protected x + 1

let () =
  let result =
    trace ~append:true Sys.argv.(1) (fun () ->
        after_protected 0 + after_protected 3)
  in
  assert (result = 12)
