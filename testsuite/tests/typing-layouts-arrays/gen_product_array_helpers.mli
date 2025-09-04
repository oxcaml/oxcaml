(* This module defines some helpers for writing tests on arays of unboxed
   products.  See [README.md] in this directory. *)

module type Element_intf = Test_gen_u_array.Element_intf

type ('a : value_or_null) elem =
  | Number : { ops : (module Element_intf with type t = 'a) } -> 'a elem
  | Option : ('a : value_or_null) . 'a elem -> ('a option) elem
  | Or_null : 'a elem -> ('a or_null) elem
  | Tup2 : ('a1 : value_or_null) ('a2 : value_or_null) . 'a1 elem * 'a2 elem -> ('a1 * 'a2) elem
  | Tup3 : 'a1 elem * 'a2 elem * 'a3 elem -> ('a1 * 'a2 * 'a3) elem
  | Tup4 : 'a1 elem * 'a2 elem * 'a3 elem * 'a4 elem
      -> ('a1 * 'a2 * 'a3 * 'a4) elem
  | Tup5 : 'a1 elem * 'a2 elem * 'a3 elem * 'a4 elem * 'a5 elem
      -> ('a1 * 'a2 * 'a3 * 'a4 * 'a5) elem
  | Tup6 : 'a1 elem * 'a2 elem * 'a3 elem * 'a4 elem * 'a5 elem * 'a6 elem
      -> ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6) elem

val int_elem : int elem
val int32_elem : int32 elem
val int64_elem : int64 elem
val nativeint_elem : nativeint elem

val float_elem : float elem
val float32_elem : float32 elem

val make_element_ops : 'a elem -> (module Element_intf with type t = 'a)
