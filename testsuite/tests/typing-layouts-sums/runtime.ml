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

let[@inline never] choose (first : Boundary.t) (second : Boundary.t) =
  if Boundary.read second < 0 then first else second

let () =
  let choose = Sys.opaque_identity choose in
  let choose_raw = Sys.opaque_identity (choose (Boundary.raw #37l)) in
  assert (Boundary.read (choose_raw (Boundary.count (-1))) = 37);
  assert (Boundary.read (choose_raw (Boundary.count 83)) = 83)

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

type packet = Empty_packet | Sample of #(int * bool) | Offset of int
  [@@unboxed]

let[@inline never] classify packet accept =
  match packet with
  | (Sample #(number, true) | Offset number) when accept number -> 1
  | (Sample #(number, true) | Offset number) when accept number -> 2
  | _ -> 3

let () =
  let classify_on_call accept_on packet =
    let arguments = ref [] in
    let accept number =
      arguments := number :: !arguments;
      List.length !arguments = accept_on
    in
    let result = classify packet accept in
    result, List.rev !arguments
  in
  assert (classify_on_call 1 (Sample #(17, true)) = (1, [17]));
  assert (classify_on_call 2 (Offset 29) = (2, [29; 29]));
  assert (classify_on_call 0 (Sample #(31, true)) = (3, [31; 31]));
  assert (classify_on_call 1 (Sample #(37, false)) = (3, []));
  assert (classify_on_call 1 Empty_packet = (3, []))

let[@inline never] adjacent_field = function
  | #(true, Sample #(0, _), _) -> 1
  | #(false, _, true) -> 2
  | #(_, (Sample _ | Offset _), false) -> 3
  | _ -> 4

let () =
  assert (adjacent_field #(true, Sample #(0, false), false) = 1);
  assert (adjacent_field #(false, Offset 1, true) = 2);
  assert (adjacent_field #(true, Sample #(1, true), false) = 3);
  assert (adjacent_field #(true, Offset 1, false) = 3);
  assert (adjacent_field #(true, Sample #(1, true), true) = 4);
  assert (adjacent_field #(true, Empty_packet, false) = 4)
