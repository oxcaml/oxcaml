(* TEST
   { bytecode; }
   { native; }
*)

(* An interrupt (here a memprof callback) may run while the state is grown for
   a fresh key, after the old state is read but before it is copied. If the
   interrupt accesses another key and grows the state meanwhile, the access is
   restored in the interrupt's state, leaving a [borrowed] sentinel in the old
   one. The interrupted growth must not install a copy of the old state. *)

module TLS = Domain.Safe.TLS

let k = TLS.new_key (fun () -> ref 0)
let far_keys = Array.init 200 (fun _ -> TLS.new_key (fun () -> ref 0))
let k_main = far_keys.(20)
let k_interrupt = far_keys.(150)

let read (k : int ref TLS.key) = TLS.access k (fun r -> !r) [@nontail]

let fired = ref false

let tracker =
  { Gc.Memprof.null_tracker with
    alloc_minor = (fun info ->
      (* The first large allocation is the grown state for [k_main]. *)
      if not !fired && info.size >= 15 then begin
        fired := true;
        TLS.access k (fun _ ->
          TLS.access k_interrupt (fun r -> r := 2) [@nontail]) [@nontail]
      end;
      None) }

let () =
  TLS.access k (fun r -> r := 1);
  let _ : Gc.Memprof.t = Gc.Memprof.start ~sampling_rate:1. tracker in
  TLS.access k_main (fun r -> r := 3);
  Gc.Memprof.stop ();
  assert !fired;
  assert (read k = 1);
  assert (read k_interrupt = 2);
  assert (read k_main = 3);
  print_endline "OK"
