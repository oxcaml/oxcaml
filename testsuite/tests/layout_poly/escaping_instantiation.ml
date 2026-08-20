(* TEST
 flags = "-extension layout_poly_alpha";
 native;
*)

(* Regression test: an instantiated closure that escapes the function it was
   created in must be heap-allocated (its allocation mode is the template
   environment's mode, not the instantiation's local-returning mode). This
   used to be miscompiled: the closure was allocated in the enclosing
   function's region, so the captured [r] was lost once the region ended. *)

external to_float : float# -> float = "%box_float"

let[@inline never] f r (v : float#) =
  let poly_ k x =
    incr r;
    ignore (to_float v);
    x
  in
  Sys.opaque_identity (k : int -> int)

let () =
  let r = ref 0 in
  let g = f r #1.0 in
  Gc.compact ();
  ignore (Sys.opaque_identity (g 5));
  ignore (Sys.opaque_identity (g 6));
  assert (!r = 2)
