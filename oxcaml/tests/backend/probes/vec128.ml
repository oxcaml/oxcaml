(* The wrapper of an optimized probe saves in its frame the registers that are
   live across the probe and the arguments of the handler. The handlers of the
   two probes differ by one argument, so that a 128-bit vector that is aligned
   in the frame of one wrapper is not in the other. With [-flambda2-reaper], the
   vectors are also arguments of the handlers. *)

external get : bytes -> int -> int8x16 = "%caml_bytes_getu128"

external set : bytes -> int -> int8x16 -> unit = "%caml_bytes_setu128"

let src = Bytes.init 32 Char.chr

let in_handler = Bytes.make 32 '?'

let after = Bytes.make 32 '?'

let total = ref 0

let[@inline never] one a =
  let v0 = get src 0 in
  let v1 = get src 16 in
  let n = Sys.opaque_identity (a + 1) in
  [%probe
    "one" ~enabled_at_init:true
      (set in_handler 0 v0;
       set in_handler 16 v1;
       total := !total + n)];
  set after 0 v0;
  set after 16 v1

let[@inline never] two a =
  let v0 = get src 0 in
  let v1 = get src 16 in
  let n = Sys.opaque_identity (a + 1) in
  let m = Sys.opaque_identity (a + 2) in
  [%probe
    "two" ~enabled_at_init:true
      (set in_handler 0 v0;
       set in_handler 16 v1;
       total := !total + n + m)];
  set after 0 v0;
  set after 16 v1

let check name =
  Printf.printf "%s: %b %b %d\n" name
    (Bytes.equal in_handler src)
    (Bytes.equal after src) !total;
  Bytes.fill in_handler 0 32 '?';
  Bytes.fill after 0 32 '?'

let () =
  one 1;
  check "one";
  two 2;
  check "two"
