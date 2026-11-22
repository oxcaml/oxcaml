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

type void : void
external void : unit -> void = "%unbox_unit"

type with_void = Absent | Present of void * int * void [@@unboxed]

let[@inline never] read_present = function
  | Absent -> None
  | Present (_, number, _) -> Some number

let () =
  let calls = ref [] in
  let mark index = calls := index :: !calls in
  let make_void index = mark index; void () in
  let value = Present (make_void 1, (mark 2; 19), make_void 3) in
  assert (List.rev !calls = [3; 2; 1]);
  assert (read_present value = Some 19)

type signal = Stop | Wait | Go [@@unboxed]

let[@inline never] signal_number = function Stop -> 0 | Wait -> 1 | Go -> 2

let () =
  assert (signal_number Stop = 0);
  assert (signal_number Wait = 1);
  assert (signal_number Go = 2)

type nested = Before | Nested of Boundary.t | After of float# [@@unboxed]
type flat = First | Raw of int32_u | Count of int | Last of float# [@@unboxed]
type nested_cell = { nested : nested }
type flat_cell = { flat : flat }

let[@inline never] unwrap = function
  | Nested inner -> Boundary.read (pass inner)
  | Before -> -1
  | After value -> int_of_float (box_float value)

let[@inline never] read_flat_cell cell =
  match cell.flat with
  | First -> -1
  | Raw value -> Int32.to_int (box_int32 value)
  | Count number -> number
  | Last value -> int_of_float (box_float value)

let () =
  assert (unwrap Before = -1);
  assert (unwrap (Nested (Boundary.raw (-#123l))) = -123);
  assert (unwrap (Nested (Boundary.count 42)) = 42);
  assert (unwrap (After #7.0) = 7);
  assert (Obj.size (Obj.repr {nested = Nested Boundary.sample})
          = Obj.size (Obj.repr {flat = Count 19}));
  let check_layout nested expected =
    assert (read_flat_cell (Obj.magic {nested}) = expected)
  in
  check_layout Before (-1);
  check_layout (Nested (Boundary.raw (-#7l))) (-7);
  check_layout (Nested Boundary.sample) 19;
  check_layout (After #23.0) 23

type pair = Neither | Both of nested * nested [@@unboxed]

let[@inline never] read_pair = function
  | Neither -> None
  | Both (left, right) -> Some (unwrap left, unwrap right)

let () =
  assert (read_pair Neither = None);
  assert (read_pair (Both (Nested Boundary.sample, After #4.0))
          = Some (19, 4));
  assert (read_pair (Both (Before, Nested (Boundary.raw #31l)))
          = Some (-1, 31))

type signed = Number of int | Signal of signal [@@unboxed]
type command = Begin of signed | Cancel | End of signed [@@unboxed]

let[@inline never] classify_signed = function
  | Number number -> number
  | Signal signal -> signal_number signal

let[@inline never] classify_command = function
  | End (Signal Wait) -> 100
  | Begin payload -> classify_signed payload
  | End payload -> 10 + classify_signed payload
  | Cancel -> -1

let () =
  assert (classify_command (Begin (Number 70)) = 70);
  assert (classify_command (Begin (Signal Go)) = 2);
  assert (classify_command Cancel = -1);
  assert (classify_command (End (Signal Wait)) = 100);
  assert (classify_command (End (Signal Stop)) = 10);
  assert (classify_command (End (Number 4)) = 14)

type message = Ready of Boundary.wrapped | Busy [@@unboxed]

let[@inline never] receive = function
  | Busy -> -2
  | Ready inner -> Boundary.read_wrapped inner

let () =
  assert (receive Busy = -2);
  assert (receive (Ready (Boundary.wrap (Boundary.raw (-#21l)))) = -21);
  assert (receive (Ready (Boundary.wrap Boundary.sample)) = 19)
