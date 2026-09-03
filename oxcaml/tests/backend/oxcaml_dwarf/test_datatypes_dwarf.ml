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

(* Test that the same type works across multiple functions when cached *)
let[@inline never] [@local never] f_complex_variant_second (x: complex_variant) = x
let _ = f_complex_variant_second (Single 456)
let _ = f_complex_variant_second (Pair (999, 9.99))
let _ = f_complex_variant_second (Record { x = 20; y = 5.0 })

(* Test that the same type works across multiple functions when cached *)
let[@inline never] [@local never] f_complex_variant_third (x: complex_variant) = x
let _ = f_complex_variant_third Empty
let _ = f_complex_variant_third (Mixed { a = 200; b = #1.41; c = false })
let _ = f_complex_variant_third (Pair (777, 7.77))

(* Regular records *)
type basic_record = { x: int; y: float }
type mixed_record = { a: int; b: float#; c: bool; d: int32 }

let[@inline never] [@local never] f_basic_record (x: basic_record) =
  let { x; y } = x in { x; y }
let _ = f_basic_record { x = 42; y = 3.14 }
let _ = f_basic_record { x = 0; y = 0.0 }
let _ = f_basic_record { x = -123; y = -2.5 }

let[@inline never] [@local never] f_mixed_record (x: mixed_record) =
  let { a; b; c; d } = x in { a; b; c; d }
let _ = f_mixed_record { a = 42; b = #3.14; c = true; d = 1000l }
let _ = f_mixed_record { a = 0; b = #0.0; c = false; d = 0l }

(* Unboxed variants *)
type unboxed_variant_float = Simple of float# [@@unboxed]
type unboxed_variant_int = Complex of int32# [@@unboxed]

let[@inline never] [@local never] f_unboxed_variant_float
    (x: unboxed_variant_float) = x
let _ = f_unboxed_variant_float (Simple #4.1)
let _ = f_unboxed_variant_float (Simple #0.0)
let _ = f_unboxed_variant_float (Simple (-#2.5))

let[@inline never] [@local never] f_unboxed_variant_int
    (x: unboxed_variant_int) = x
let _ = f_unboxed_variant_int (Complex #42l)
let _ = f_unboxed_variant_int (Complex #0l)

type unboxed_variant_value_int = ValueInt of int [@@unboxed]
type unboxed_variant_value_string = ValueString of string [@@unboxed]
type unboxed_variant_value_bool = ValueBool of bool [@@unboxed]

let[@inline never] [@local never] f_unboxed_variant_value_int
    (x: unboxed_variant_value_int) = x
let _ = f_unboxed_variant_value_int (ValueInt 42)
let _ = f_unboxed_variant_value_int (ValueInt 0)

let[@inline never] [@local never] f_unboxed_variant_value_string
    (x: unboxed_variant_value_string) = x
let _ = f_unboxed_variant_value_string (ValueString "hello")
let _ = f_unboxed_variant_value_string (ValueString "")

let[@inline never] [@local never] f_unboxed_variant_value_bool
    (x: unboxed_variant_value_bool) = x
let _ = f_unboxed_variant_value_bool (ValueBool true)
let _ = f_unboxed_variant_value_bool (ValueBool false)

(* Unboxed records *)
type unboxed_record_simple = { value: int } [@@unboxed]
type unboxed_record_complex = { data: basic_record } [@@unboxed]

let[@inline never] [@local never] f_unboxed_record_simple
    (x: unboxed_record_simple) =
  let { value } = x in { value }
let _ = f_unboxed_record_simple { value = 42 }
let _ = f_unboxed_record_simple { value = 0 }
let _ = f_unboxed_record_simple { value = -999 }

let[@inline never] [@local never] f_unboxed_record_complex
    (x: unboxed_record_complex) =
  let { data } = x in { data }
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
let[@inline never] [@local never] f_open_poly_variant
    (x: [> `Alpha | `Beta of int]) = x
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

let[@inline never] [@local never] f_mixed_combo (x: mixed_combo) =
  let { boxed_field; unboxed_field; variant_field; record_field } = x in
  { boxed_field; unboxed_field; variant_field; record_field }
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

(* Unboxed tuple field *)
type t = { f : #(int64# * int); s : string; b : bool }

let[@inline never] [@local never] f_unboxed_tuple_field (x: t) =
  let { f; s; b } = x in { f; s; b }
let _ = f_unboxed_tuple_field { f = #(#42L, 24); s = "hello"; b = true }
let _ = f_unboxed_tuple_field { f = #(#0L, 1); s = ""; b = false }
let _ = f_unboxed_tuple_field { f = #(#10L, -100); s = "world"; b = true }

(* Custom exceptions *)
exception Simple_exception
exception Exception_with_int of int
exception Exception_with_string of string
exception Exception_with_multiple of int * string * float
type exception_record = { code: int; message: string }
exception Exception_with_record of exception_record
type unboxed_exception_data = { value: float#; flag: bool }
exception Exception_with_unboxed_record of unboxed_exception_data

let[@inline never] [@local never] f_exception_specific (x: exn) = x
let _ = f_exception_specific Simple_exception
let _ = f_exception_specific (Exception_with_int 42)
let _ = f_exception_specific (Exception_with_string "error occurred")
let _ = f_exception_specific (Exception_with_multiple (404, "not found", 1.5))
let _ = f_exception_specific
    (Exception_with_record { code = 500; message = "internal error" })
let _ = f_exception_specific
    (Exception_with_unboxed_record { value = #2.71; flag = true })
let _ = f_exception_specific (Failure "standard failure")
let _ = f_exception_specific (Invalid_argument "bad input")

let[@inline never] [@local never] f_exception_poly (x: 'a) = x
let _ = f_exception_poly Simple_exception
let _ = f_exception_poly (Exception_with_int 123)
let _ = f_exception_poly (Exception_with_string "polymorphic error")
let _ = f_exception_poly (Exception_with_multiple (200, "ok", 0.0))
let _ = f_exception_poly
    (Exception_with_record { code = 201; message = "created" })
let _ = f_exception_poly
    (Exception_with_unboxed_record { value = #3.14; flag = false })
let _ = f_exception_poly (Failure "polymorphic failure")

let[@inline never] [@local never] f_exception_with_record (x: exn) =
  match x with
  | Exception_with_record data ->
    let { code; message } = data in
    let _ = { code; message } in x
  | _ -> x
let _ = f_exception_with_record
    (Exception_with_record { code = 300; message = "redirect" })

let[@inline never] [@local never] f_exception_with_unboxed (x: exn) =
  match x with
  | Exception_with_unboxed_record data ->
    let { value; flag } = data in
    let _ = { value; flag } in x
  | _ -> x
let _ = f_exception_with_unboxed
    (Exception_with_unboxed_record { value = #1.41; flag = true })

(* Mutually recursive types - tree and forest *)
type tree =
  | Empty
  | Node of { value: int [@warning "-69"]; children: forest [@warning "-69"] }
and forest =
  | EmptyForest
  | WithTree of { tree: tree [@warning "-69"]; rest: forest [@warning "-69"] }

let[@inline never] [@local never] f_tree (x: tree) = x
let[@inline never] [@local never] f_forest (x: forest) = x

(* Test empty tree *)
let _ = f_tree Empty

(* Test simple leaf tree with empty forest *)
let _ = f_tree (Node { value = 42; children = EmptyForest })

(* Test nested tree structure *)
let _ = f_tree (Node {
  value = 1;
  children = WithTree {
    tree = Node { value = 2; children = EmptyForest };
    rest = WithTree {
      tree = Node { value = 3; children = WithTree {
        tree = Node { value = 4; children = EmptyForest };
        rest = WithTree {
          tree = Node { value = 5; children = EmptyForest };
          rest = EmptyForest
        }
      }};
      rest = EmptyForest
    }
  }
})

(* Test empty forest *)
let _ = f_forest EmptyForest

(* Test forest with single tree *)
let _ = f_forest (WithTree { tree = Node { value = 10; children = EmptyForest }; rest = EmptyForest })

(* Test complex forest *)
let _ = f_forest (WithTree {
  tree = Node { value = 100; children = EmptyForest };
  rest = WithTree {
    tree = Node { value = 200; children = WithTree {
      tree = Node { value = 201; children = EmptyForest };
      rest = WithTree {
        tree = Node { value = 202; children = WithTree {
          tree = Node { value = 2021; children = EmptyForest };
          rest = EmptyForest
        }};
        rest = EmptyForest
      }
    }};
    rest = WithTree {
      tree = Node { value = 300; children = EmptyForest };
      rest = EmptyForest
    }
  }
})
(* Regular tuples *)
let[@inline never] [@local never] f_tuple_int_int (x: int * int) = x
let _ = f_tuple_int_int (42, 123)
let _ = f_tuple_int_int (0, -1)
let _ = f_tuple_int_int (-999, 1000)

let[@inline never] [@local never] f_tuple_mixed_two (x: float * string) = x
let _ = f_tuple_mixed_two (3.14, "hello")
let _ = f_tuple_mixed_two (0.0, "")
let _ = f_tuple_mixed_two (-2.5, "world")

let[@inline never] [@local never] f_tuple_three (x: int * bool * float) = x
let _ = f_tuple_three (42, true, 3.14)
let _ = f_tuple_three (0, false, 0.0)
let _ = f_tuple_three (-123, true, -1.5)

let[@inline never] [@local never] f_tuple_nested (x: (int * int) * (float * bool)) = x
let _ = f_tuple_nested ((1, 2), (3.14, true))
let _ = f_tuple_nested ((0, 0), (0.0, false))
let _ = f_tuple_nested ((-5, 10), (-2.5, true))

let[@inline never] [@local never] f_tuple_with_complex
    (x: int list * char array * string option) = x
let _ = f_tuple_with_complex ([1; 2; 3], [|'a'; 'b'|], Some "test")
let _ = f_tuple_with_complex ([], [||], None)
let _ = f_tuple_with_complex ([42], [|'x'|], Some "")

let[@inline never] [@local never] f_tuple_large
    (x: int * float * string * bool * char * int32) = x
let _ = f_tuple_large (42, 3.14, "hello", true, 'A', 123l)
let _ = f_tuple_large (0, 0.0, "", false, '\000', 0l)

let[@inline never] [@local never] f_poly_tuple (x: 'a * 'b) = x
let _ = f_poly_tuple (42, "test")
let _ = f_poly_tuple (3.14, true)
let _ = f_poly_tuple ("hello", [1; 2; 3])

(* Bigarrays *)
let[@inline never] [@local never] f_bigarray1_int (x: (int, Bigarray.int_elt, Bigarray.c_layout) Bigarray.Array1.t) = x
let bigarray1_int = Bigarray.Array1.create Bigarray.int Bigarray.c_layout 5
let _ = for i = 0 to 4 do Bigarray.Array1.set bigarray1_int i (i * 10) done
let _ = f_bigarray1_int bigarray1_int

let[@inline never] [@local never] f_bigarray1_float (x: (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t) = x
let bigarray1_float = Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout 4
let _ = for i = 0 to 3 do Bigarray.Array1.set bigarray1_float i (Float.of_int i +. 0.5) done
let _ = f_bigarray1_float bigarray1_float

let[@inline never] [@local never] f_bigarray2_int (x: (int, Bigarray.int_elt, Bigarray.c_layout) Bigarray.Array2.t) = x
let bigarray2_int = Bigarray.Array2.create Bigarray.int Bigarray.c_layout 3 3
let _ = for i = 0 to 2 do for j = 0 to 2 do Bigarray.Array2.set bigarray2_int i j (i + j) done done
let _ = f_bigarray2_int bigarray2_int

let[@inline never] [@local never] f_bigarray2_float (x: (float, Bigarray.float64_elt, Bigarray.fortran_layout) Bigarray.Array2.t) = x
let bigarray2_float = Bigarray.Array2.create Bigarray.float64 Bigarray.fortran_layout 2 4
let _ = for i = 1 to 2 do for j = 1 to 4 do Bigarray.Array2.set bigarray2_float i j (Float.of_int (i * j) *. 0.1) done done
let _ = f_bigarray2_float bigarray2_float

let[@inline never] [@local never] f_bigarray3_int32 (x: (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array3.t) = x
let bigarray3_int32 = Bigarray.Array3.create Bigarray.int32 Bigarray.c_layout 2 2 2
let _ = for i = 0 to 1 do for j = 0 to 1 do for k = 0 to 1 do
  Bigarray.Array3.set bigarray3_int32 i j k (Int32.of_int (i + j + k))
done done done
let _ = f_bigarray3_int32 bigarray3_int32


(* CR sspies: Add testing for Maps and Hashtables once oxcaml dwarf is enabled on the compiler. *)
