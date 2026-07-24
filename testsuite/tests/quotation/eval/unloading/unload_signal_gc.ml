(* TEST
  include eval;
  flags = "-extension runtime_metaprogramming";
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
   would crash on the next instruction.

   --------------------------------------------------------------
   Liveness proof that F.2 is the only path keeping the unit alive
   --------------------------------------------------------------

   The above is a "by elimination" argument: we believe the closure
   has no remaining heap reference at the in-call GC point, so F.2 is
   the only mechanism left that could be keeping the unit alive. This
   was verified by inspecting the compiler's Linear IR
   (-dlinear) on an AOT stand-in [my_worker] equivalent to the eval'd
   body. The JIT uses the same Flambda2 + Cmm + emit pipeline so
   liveness analysis is identical; only the closinfo and
   frame-descriptor flag bits differ between AOT and the JIT path.

   The two relevant call sites:

   (1) Toplevel [()] calling [!worker host_callback 10] via
       [caml_apply2]:

       anon:V/83[x2] := val mut[worker:V/64[x19]]   ;; load !worker
       anon:I/84[x1] := 21                          ;; arg n=10
       anon:I/85[x0] := host_callback               ;; arg cb
       {}                                           ;; <-- LIVE SET
       call "caml_apply2" x0 x1 x2

       The {} is the GC live set across the call (printlinear.ml:87
       prints [i.live] in that form). It is empty: the toplevel
       frame's [live_ofs] tracks no slots, so nothing in this frame
       reaches the eval'd worker's closure during the in-call GC.

   (2) Worker calling [cb ()] (i.e. host_callback) from inside its
       body:

       stack-acc:I/100[s[i:1]] := acc:I/62[x4]      (spill)
       stack-n:I/99[s[i:0]]    := n:I/61[x1]       (spill)
       anon:I/81[x3]           := int mut[cb:V/60[x2]]
       pin:anon:I/1[x1]        := cb:V/60[x2]
       {stack-n:I/99[s[i:0]] stack-acc:I/100[s[i:1]]}  ;; <-- LIVE SET
       call anon:I/81[x3] x0 x1

       Live set across the call is [{n, acc_unboxed}]: two integers.
       The worker has no captured env so its prologue does not spill
       a self-pointer; consequently nothing in this frame's
       [live_ofs] references the worker's closure or any Code_block
       / data block of the unit.

   Combined with:
     - [worker := (fun _ _ -> 0)] (overwrites the only heap ref)
     - [callback_fired] is a bool ref, doesn't reach the closure
     - No other heap object captures the worker
     - The closure register has been clobbered by intervening calls

   the eval'd worker's closure address appears in NO live root at the
   in-call GC point. So the unit can only be kept alive via F.2 (the
   worker's own stack frame's RA, looked up via
   caml_find_code_fragment_by_pc, darkening the Code_block via the
   [entry - 1] back-pointer).

   The test passing therefore directly demonstrates F.2 functioning
   correctly.

   --------------------------------------------------------------
   TODO: strengthen this test further
   --------------------------------------------------------------
   - Add a runtime probe (gated by a debug env var) that captures the
     eval'd unit's Code_block address and, just before
     [caml_unloadable_check_and_unload_dead], walks every domain's
     stack and asserts the address appears in no [live_ofs] slot of
     any non-eval'd frame. This would turn the "by elimination"
     argument into "by observation".
   - Add a [OCAML_JIT_DEBUG=linear] mode to dump the JIT's Linear IR
     for the eval'd worker, so the live-set claim above can be
     verified directly against the JIT output rather than against an
     AOT stand-in.
   - Add a sibling test where the eval'd function explicitly takes a
     [Sys.opaque_identity]-wrapped self-pointer parameter, to rule
     out any compiler-introduced spills the simpler version might
     leave in.
*)

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
