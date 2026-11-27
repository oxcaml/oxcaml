(* Expected values are ordinary boxed OCaml data. The test can check any
   generated constructor by its number and ordered list of scalar fields. *)
type kind = Text | Tagged | Narrow | Wide | Fraction
type atom = S of string | I of int | I32 of int32 | I64 of int64 | F of float

let number seed field = seed * 13 + field
let text seed field = Printf.sprintf "text:%d:%d" seed field
let narrow seed field = Int32.add 0x8123_0000l (Int32.of_int (number seed field))
let wide seed field = Int64.add 0x4567_89ab_cdef_0000L (Int64.of_int (number seed field))
let fraction seed field = -. float_of_int (number seed field) -. 0.25

external unbox32 : int32 -> int32_u = "%unbox_int32"
external box32 : int32_u -> int32 = "%box_int32"
external unbox64 : int64 -> int64_u = "%unbox_int64"
external box64 : int64_u -> int64 = "%box_int64"
external unbox_float : float -> float# = "%unbox_float"
external box_float : float# -> float = "%box_float"

let expected seed field = function
  | Text -> S (text seed field) | Tagged -> I (number seed field)
  | Narrow -> I32 (narrow seed field) | Wide -> I64 (wide seed field)
  | Fraction -> F (fraction seed field)

let guard side seed = Printf.sprintf "%s guard:%d" side seed

let exercise name arms ~create ~replace ~observe =
  Array.iteri (fun old_arm _ ->
    Array.iteri (fun new_arm _ ->
      let seed = old_arm * Array.length arms + new_arm in
      let check cell arm payload_seed phase =
        let left, left_bits, actual, right_bits, right = observe cell in
        let require field ok =
          if not ok then failwith (Printf.sprintf "%s: K%d -> K%d (%s %s)"
                                     name old_arm new_arm phase field)
        in
        require "payload" (actual = (arm, List.mapi (expected payload_seed) arms.(arm)));
        require "left" (left = guard "left" seed);
        require "right" (right = guard "right" seed);
        require "left_bits" (left_bits = 0xdead_beefl);
        require "right_bits" (right_bits = 0x1234_5678_9abc_def0L)
      in
      let cell = create seed old_arm in
      Gc.minor ();
      Gc.compact ();
      check cell old_arm seed "initial";
      replace cell (seed + 100) new_arm;
      Gc.minor ();
      Gc.compact ();
      check cell new_arm (seed + 100) "replaced"
    ) arms
  ) arms
