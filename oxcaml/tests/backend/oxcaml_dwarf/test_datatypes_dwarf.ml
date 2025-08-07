let[@inline never] [@local never] f_start () = ()
let _ = f_start ()

(* Simple variants *)
type simple_variant = A | B | C of int | D of float

let[@inline never] [@local never] f_simple_variant (x: simple_variant) = x
let _ = f_simple_variant A
let _ = f_simple_variant B
let _ = f_simple_variant (C 42)
let _ = f_simple_variant (D 3.14)

(* Complex variants with records *)
type complex_variant =
  | Empty
  | Single of int
  | Pair of int * float
  | Record of { x: int; y: float }
  | Mixed of { a: int; b: float#; c: bool }

let[@inline never] [@local never] f_complex_variant (x: complex_variant) = x
let _ = f_complex_variant Empty
let _ = f_complex_variant (Single 123)
let _ = f_complex_variant (Pair (42, 1.5))
let _ = f_complex_variant (Record { x = 10; y = 2.5 })
let _ = f_complex_variant (Mixed { a = 100; b = #3.14; c = true })

(* Regular records *)
type basic_record = { x: int; y: float }
type mixed_record = { a: int; b: float#; c: bool; d: int32 }

let[@inline never] [@local never] f_basic_record (x: basic_record) = x
let _ = f_basic_record { x = 42; y = 3.14 }
let _ = f_basic_record { x = 0; y = 0.0 }
let _ = f_basic_record { x = -123; y = -2.5 }

let[@inline never] [@local never] f_mixed_record (x: mixed_record) = x
let _ = f_mixed_record { a = 42; b = #3.14; c = true; d = 1000l }
let _ = f_mixed_record { a = 0; b = #0.0; c = false; d = 0l }

(* Unboxed variants *)
type unboxed_variant_float = Simple of float# [@@unboxed]
type unboxed_variant_int = Complex of int32# [@@unboxed]

let[@inline never] [@local never] f_unboxed_variant_float (x: unboxed_variant_float) = x
let _ = f_unboxed_variant_float (Simple #4.1)
let _ = f_unboxed_variant_float (Simple #0.0)
let _ = f_unboxed_variant_float (Simple (-#2.5))

let[@inline never] [@local never] f_unboxed_variant_int (x: unboxed_variant_int) = x
let _ = f_unboxed_variant_int (Complex #42l)
let _ = f_unboxed_variant_int (Complex #0l)

(* Unboxed records *)
type unboxed_record_simple = { value: int } [@@unboxed]
type unboxed_record_complex = { data: basic_record } [@@unboxed]

let[@inline never] [@local never] f_unboxed_record_simple (x: unboxed_record_simple) = x
let _ = f_unboxed_record_simple { value = 42 }
let _ = f_unboxed_record_simple { value = 0 }
let _ = f_unboxed_record_simple { value = -999 }

let[@inline never] [@local never] f_unboxed_record_complex (x: unboxed_record_complex) = x
let _ = f_unboxed_record_complex { data = { x = 10; y = 2.0 } }
let _ = f_unboxed_record_complex { data = { x = 0; y = 0.0 } }

(* Polymorphic variants *)
type poly_variant = [`Red | `Blue of int | `Green of float | `Yellow of string]

let[@inline never] [@local never] f_poly_variant (x: poly_variant) = x
let _ = f_poly_variant `Red
let _ = f_poly_variant (`Blue 42)
let _ = f_poly_variant (`Green 3.14)
let _ = f_poly_variant (`Yellow "test")

(* Open polymorphic variants *)
let[@inline never] [@local never] f_open_poly_variant (x: [> `Alpha | `Beta of int]) = x
let _ = f_open_poly_variant `Alpha
let _ = f_open_poly_variant (`Beta 123)

(* Nested combinations *)
type nested_variant =
  | Leaf of int
  | Node of { left: nested_variant; right: nested_variant; value: float }

let[@inline never] [@local never] f_nested_variant (x: nested_variant) = x
let _ = f_nested_variant (Leaf 42)
let _ = f_nested_variant (Node {
  left = Leaf 1;
  right = Leaf 2;
  value = 3.14
})

(* Mixed boxed/unboxed combinations *)
type mixed_combo = {
  boxed_field: int;
  unboxed_field: float#;
  variant_field: simple_variant;
  record_field: basic_record
}

let[@inline never] [@local never] f_mixed_combo (x: mixed_combo) = x
let _ = f_mixed_combo {
  boxed_field = 42;
  unboxed_field = #3.14;
  variant_field = C 100;
  record_field = { x = 10; y = 2.5 }
}

(* Parametric types *)
type 'a option_variant = None | Some of 'a
type ('a, 'b) either = Left of 'a | Right of 'b

let[@inline never] [@local never] f_option_variant (x: int option_variant) = x
let _ = f_option_variant None
let _ = f_option_variant (Some 42)

let[@inline never] [@local never] f_either (x: (int, string) either) = x
let _ = f_either (Left 42)
let _ = f_either (Right "hello")

(* Layout-constrained polymorphic variants *)
let[@inline never] [@local never] f_poly_float64 (type a : float64) (x: a) = x
let _ = f_poly_float64 (Simple #4.1)
let _ = f_poly_float64 (Simple #2.5)

let[@inline never] [@local never] f_poly_bits32 (type a : bits32) (x: a) = x
let _ = f_poly_bits32 #42l
let _ = f_poly_bits32 (-#123l)
