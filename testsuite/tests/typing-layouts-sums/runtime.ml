(* TEST
 modules = "boundary.mli boundary.ml";
 {
   bytecode;
 } {
   native;
 }
*)

type outer = Empty | Inside of Boundary.t [@@unboxed]
type envelope = { payload : outer; guard : string }

let[@inline never] pass (x : Boundary.t) = x

let[@inline never] wrap payload =
  { payload = Inside payload; guard = String.make 1 'g' }

let[@inline never] read envelope =
  assert (envelope.guard = "g");
  match envelope.payload with
  | Empty -> -1
  | Inside x -> Boundary.read (pass x)

let () =
  let raw = wrap (pass (Boundary.raw #37l)) in
  let count = wrap (pass (Boundary.count 83)) in
  let empty = { payload = Empty; guard = "g" } in
  Gc.compact ();
  assert (read (wrap (pass Boundary.sample)) = 19);
  assert (read empty = -1);
  assert (read raw = 37);
  assert (read count = 83)

type scalar = Float of float# | Int of int32_u [@@unboxed]

external unbox_float : float -> float# = "%unbox_float"
external box_float : float# -> float = "%box_float"
external box_int32 : int32_u -> int32 = "%box_int32"

let[@inline never] read_bits = function
  | Float x -> Int64.bits_of_float (box_float x)
  | Int x -> Int64.of_int32 (box_int32 x)

let () =
  let bits = 0x7ff8_0000_0000_002aL in
  assert (read_bits (Float (unbox_float (Int64.float_of_bits bits))) = bits);
  assert (read_bits (Int (-#17l)) = -17L)
