(* TEST
 modules = "pure_alloc_extcall_stubs.c";
 native;
*)

(* An external declared with [@@no_effects] and [@@no_coeffects] but without
   [@@noalloc] goes through [caml_c_call] and may trigger a GC. Instruction
   selection used to treat such a call as a "simple" expression and defer it
   until after the allocation of the block its result is stored into, so a GC
   during the call saw the uninitialised fields of that block, and the
   initialising stores that followed missed the write barrier if the block had
   been promoted. *)

external gc_then_box : int -> float = "pure_alloc_extcall_gc_then_box"
  [@@no_effects] [@@no_coeffects]

let[@inline never] make x = (gc_then_box x, x)

let () =
  (* Leave a recognisable non-pointer bit pattern where the pair will be
     allocated, so that a GC scanning its uninitialised fields fails
     immediately rather than silently accepting zero words. *)
  Gc.minor ();
  let s = Sys.opaque_identity (Bytes.make 1000 '\x10') in
  ignore (Sys.opaque_identity s);
  Gc.minor ();
  let p = Sys.opaque_identity (make 42) in
  (* If [p] was promoted during the call, its float field was stored without a
     write barrier: make sure the float survives a minor collection and the
     reuse of the minor heap. *)
  Gc.minor ();
  let junk = Sys.opaque_identity (List.init 100_000 float_of_int) in
  ignore (Sys.opaque_identity junk);
  let f, i = p in
  Printf.printf "%g %d\n" f i
