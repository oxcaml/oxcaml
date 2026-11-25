(* TEST
 flags = "-gated-trace-pc-guard -gated-trace-pc-guard-define-gate";
 modules = "define_gate_stubs.c";
 linux;
 native;
*)

(* With [-gated-trace-pc-guard-define-gate] the startup file defines the gate.
   The stubs have a weak definition whose value is 1: the gate is nevertheless
   closed on startup, so the definition of the startup file is the one in use. *)

external set_gate : bool -> unit = "caml_tpcg_define_gate_set" [@@noalloc]

external num_calls : unit -> int = "caml_tpcg_define_gate_num_calls"
[@@noalloc]

let[@inline never] rec fib n = if n < 2 then n else fib (n - 1) + fib (n - 2)

let () =
  let expected = fib 15 in
  let calls_with_gate_closed = num_calls () in
  set_gate true;
  let actual = fib 15 in
  set_gate false;
  Printf.printf "calls with the gate closed: %d\n" calls_with_gate_closed;
  Printf.printf "callback called: %b\n" (num_calls () > 0);
  Printf.printf "result preserved: %b\n" (Int.equal expected actual)
