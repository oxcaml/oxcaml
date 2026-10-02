(* End-to-end test of how the decoder interprets LBR samples (see dune): traced
   with the runtime's single-stepping emulator and decoded with -dump-trace,
   which prints every decision the decoder takes. Each function exercises one
   mechanism. *)

external trace : append:bool -> string -> (unit -> 'a) -> 'a
  = "caml_singlestep_trace"

let[@inline never] leaf x = if x > 2 then x - 1 else x + 1

(* A tail call extends the caller's context without a return address. *)
let[@inline never] tail x = leaf (x * 2)

(* Recursion nests frames of the same function, unwound one by one. *)
let[@inline never] rec depth n = if n = 0 then 0 else 1 + depth (n - 1)

(* The handler discards the dynamic context. *)
let[@inline never] fail x = if x > 0 then raise Exit else x

let[@inline never] protected x = try fail x with Exit -> -1

(* A callback through the stdlib, which has no counters. *)
let[@inline never] callback l =
  (List.fold_left [@inlined never]) (fun acc x -> acc + leaf x) 0 l

(* Inlined: no call happens, so its counters carry the call site as static
   context. *)
let[@inline always] inlined x = if x > 5 then leaf x else x * 3

(* A call into libc, outside the executable. *)
let[@inline never] outside () = Sys.time () > 0.

let[@inline never] run x =
  leaf x + tail x + depth 2 + protected x + inlined x
  + callback [1; 2]
  + if outside () then 1 else 0

let () =
  let result =
    trace ~append:false Sys.argv.(1) (fun () -> run (Sys.opaque_identity 3))
  in
  Printf.printf "%d\n" result
