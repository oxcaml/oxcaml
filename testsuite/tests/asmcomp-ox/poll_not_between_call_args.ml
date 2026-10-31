(* TEST
 modules = "polling.c";
 flambda2;
 poll-insertion;
 flags = "-O3 -flambda2-reaper";
 native;
*)

(* Back-edge polls must not be inserted between the moves of a call's
   arguments into their calling-convention locations and the call itself:
   those locations are not GC roots. Both tests below have a loop whose body
   ends with a call whose continuation is the loop header, so that the block
   ending with the call is the source of the back edge. *)

(* External call returning [void], so that it has no result move and its
   successor is the loop header. [request_minor_gc] arranges for the next
   poll to run a minor GC. If the poll sits between the argument moves and
   the call, the minor GC moves [b] but the C stub still receives the old
   address and writes to the old copy. The loop counter is kept in a record
   so that it is not turned into a loop parameter updated after the call. *)

external request_minor_gc : unit -> unit = "request_minor_gc"

type void : void

external touch_bytes : bytes -> int -> void = "unused_bytecode" "touch_bytes"
  [@@noalloc]

type counter = { mutable i : int }

let extcall () =
  let b = Bytes.make 8 'x' in
  let n = Sys.opaque_identity 4 in
  let c = Sys.opaque_identity { i = 0 } in
  while c.i < n do
    let v = c.i in
    c.i <- v + 1;
    if v > 0 && Bytes.unsafe_get b 0 <> Char.unsafe_chr (v - 1)
    then (
      print_endline "FAIL: stale pointer passed to external call";
      exit 1);
    request_minor_gc ();
    let _ = touch_bytes b v in
    ()
  done

(* OCaml call with more arguments than argument registers. The reaper
   removes the unused result, so the call's continuation block is empty and
   is eliminated, making the call's successor the loop header. With the poll
   between the argument moves and the call, the emitter rejects the frame
   descriptor ("Not a stack slot") for the arguments passed in the domain
   state's extra_params area. *)

type 'a queue = { mutable items : 'a list }

let is_empty q = match q.items with [] -> true | _ :: _ -> false

let dequeue q =
  match q.items with
  | [] -> assert false
  | x :: rest ->
    q.items <- rest;
    x

let queue_dequeue_all q ~f =
  while not (is_empty q) do
    f (dequeue q)
  done

type sink = { mutable total : int }

let many_args (q : string queue) (sink : sink) a0 a1 a2 a3 a4 a5 a6 a7 a8 a9
    a10 a11 a12 a13 a14 =
  let[@inline never] consume (a0 : string) (a1 : string) (a2 : string)
      (a3 : string) (a4 : string) (a5 : string) (a6 : string) (a7 : string)
      (a8 : string) (a9 : string) (a10 : string) (a11 : string)
      (a12 : string) (a13 : string) (a14 : string) (a15 : string) =
    sink.total
      <- sink.total + String.length a0 + String.length a1 + String.length a2
         + String.length a3 + String.length a4 + String.length a5
         + String.length a6 + String.length a7 + String.length a8
         + String.length a9 + String.length a10 + String.length a11
         + String.length a12 + String.length a13 + String.length a14
         + String.length a15
  in
  queue_dequeue_all q ~f:(fun a15 ->
      consume a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15)

let () =
  extcall ();
  let s i = Sys.opaque_identity (string_of_int i) in
  let q = { items = List.init 1000 s } in
  let sink = { total = 0 } in
  many_args q sink (s 0) (s 1) (s 2) (s 3) (s 4) (s 5) (s 6) (s 7) (s 8) (s 9)
    (s 10) (s 11) (s 12) (s 13) (s 14);
  assert (sink.total > 0)
