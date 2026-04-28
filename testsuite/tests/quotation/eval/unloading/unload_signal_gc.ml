(* TEST
  include eval;
  flags = "-extension runtime_metaprogramming";
  runtime5;
  no-address-sanitizer;
  { native; }
*)

#syntax quotations on

(* H.1: GC while the eval'd function's frame is on the stack must not
   unload the unit. The eval'd worker calls a host callback midway
   through; the callback drops the only [worker] reference and runs
   [Gc.compact]. At that point the eval'd unit's only reachability is
   the worker's still-active stack frame whose return address lies in
   the unit's text section. The F.2 path in [caml_scan_stack] must
   look up the code fragment for that RA and darken the Code_block.

   After the callback returns, the worker continues executing inside
   the (already-collected) unit. If the unit had been incorrectly
   unloaded, the RX text behind the worker would be munmapped and we
   would crash on the next instruction. *)

let report label =
  let r = Eval.unloadable_units_registered_total () in
  let u = Eval.unloadable_units_unloaded_total () in
  Printf.printf "%s: registered=%d unloaded=%d live=%d\n%!"
    label r u (r - u)

(* The eval'd worker takes a unit-callback as its first argument. *)
let worker = ref (fun _cb _n -> 0)

let callback_fired = ref false

let[@inline never] host_callback () =
  callback_fired := true;
  (* Drop the only explicit reference to the eval'd worker. *)
  worker := (fun _ _ -> 0);
  (* Force full GCs. The eval'd worker's frame is still on the stack
     above us (we were called from inside it), so its RA lies in the
     unit's text. F.2 must keep the unit alive across these GCs. *)
  Gc.compact ();
  Gc.compact ()

let[@inline never] populate () =
  worker := Eval.eval <[
    fun (cb : unit -> unit) (n : int) ->
      (* Compute, then trigger GC mid-flight, then keep computing in
         the eval'd unit's text. If the unit had been freed during
         [cb ()] the next instruction would crash. *)
      let acc = ref 0 in
      for i = 1 to n do
        acc := !acc + i
      done;
      cb ();
      for i = 1 to n do
        acc := !acc + i
      done;
      !acc
  ]>

let () =
  report "start";
  populate ();
  let r = !worker host_callback 10 in
  (* worker(host_callback, 10) does sum 1..10 = 55, then cb (drop+GC),
     then sum 1..10 again = 110 total. *)
  Printf.printf "worker 10 returned %d (expected 110)\n" r;
  Printf.printf "callback_fired = %b\n" !callback_fired;
  report "after run (worker should still be live mid-call but dropped now)";

  (* The unit was alive during the call; now it is unreachable.
     A final compaction should unload it. *)
  Gc.compact ();
  report "after final Gc.compact"
