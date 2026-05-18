module Int8_u = Stdlib_stable.Int8_u
module Int16_u = Stdlib_stable.Int16_u

let[@inline never] [@local never] f_start () = ()
let _ = f_start ()

let[@inline never] [@local never] f_unboxed_float (x: float#) = x
let _ = f_unboxed_float #4.1
let _ = f_unboxed_float #0.0
(* CR sspies: floats that end in .0 are printed with just .
   It would be more uniform to always print the trailing 0. *)
let _ = f_unboxed_float (-#3.14)
let _ = f_unboxed_float #1e10

let[@inline never] [@local never] f_unboxed_float32 (x: float32#) = x
let _ = f_unboxed_float32 #4.1s
let _ = f_unboxed_float32 #0.0s
let _ = f_unboxed_float32 (-#2.5s)

let[@inline never] [@local never] f_unboxed_nativeint (x: nativeint#) = x
let _ = f_unboxed_nativeint #0n
let _ = f_unboxed_nativeint #0x123456789abcdefn
let _ = f_unboxed_nativeint (-#999n)

let[@inline never] [@local never] f_unboxed_int32 (x: int32#) = x
let _ = f_unboxed_int32 #0l
(* CR sspies: unboxed integers are currently not printed correctly
   (missing the hash and the suffix) *)
let _ = f_unboxed_int32 #0x12345678l
let _ = f_unboxed_int32 (-#456l)

let[@inline never] [@local never] f_unboxed_int64 (x: int64#) = x
let _ = f_unboxed_int64 #0L
let _ = f_unboxed_int64 #0x123456789abcdefL
let _ = f_unboxed_int64 (-#789L)

let[@inline never] [@local never] f_unboxed_int8 (x: int8#) = x
let _ = f_unboxed_int8 (Int8_u.of_int 42)
let _ = f_unboxed_int8 (Int8_u.of_int 0)
let _ = f_unboxed_int8 (Int8_u.of_int (-25))
let _ = f_unboxed_int8 (Int8_u.of_int 127)
let _ = f_unboxed_int8 (Int8_u.of_int (-128))

let[@inline never] [@local never] f_unboxed_int16 (x: int16#) = x
let _ = f_unboxed_int16 (Int16_u.of_int 1000)
let _ = f_unboxed_int16 (Int16_u.of_int 0)
let _ = f_unboxed_int16 (Int16_u.of_int (-2000))
let _ = f_unboxed_int16 (Int16_u.of_int 32767)
let _ = f_unboxed_int16 (Int16_u.of_int (-32768))

let[@inline never] [@local never] f_poly_float64 (type a : float64) (x: a) = x
let _ = f_poly_float64 #4.1
let _ = f_poly_float64 (-#3.14)
let _ = f_poly_float64 #1e10

let[@inline never] [@local never] f_poly_float32 (type a : float32) (x: a) = x
let _ = f_poly_float32 #4.1s
let _ = f_poly_float32 (-#2.5s)

let[@inline never] [@local never] f_poly_bits64 (type a : bits64) (x: a) = x
let _ = f_poly_bits64 #0x123456789abcdefL
let _ = f_poly_bits64 (-#789L)

let[@inline never] [@local never] f_poly_bits32 (type a : bits32) (x: a) = x
let _ = f_poly_bits32 #0x12345678l
let _ = f_poly_bits32 (-#456l)

let[@inline never] [@local never] f_poly_word (type a : word) (x: a) = x
let _ = f_poly_word #0x123456789abcdefn
let _ = f_poly_word (-#999n)

let[@inline never] [@local never] f_poly_bits8 (type a : bits8) (x: a) = x
let _ = f_poly_bits8 (Int8_u.of_int 42)
let _ = f_poly_bits8 (Int8_u.of_int (-25))

let[@inline never] [@local never] f_poly_bits16 (type a : bits16) (x: a) = x
let _ = f_poly_bits16 (Int16_u.of_int 1000)
let _ = f_poly_bits16 (Int16_u.of_int (-2000))

type simple_product = #(float# * int32#)
type mixed_product = #(int64# * bool * float#)
type nested_product = #(simple_product * int64#)
type small_int_product = #(int8# * int16#)
type mixed_small_product = #(int8# * bool * int16#)

let[@inline never] [@local never] f_simple_product (x: simple_product) = x
let _ = f_simple_product #(#4.1, #42l)
let _ = f_simple_product #(#0.0, #0l)
let _ = f_simple_product #(-#3.14, -#123l)

let[@inline never] [@local never] f_mixed_product (x: mixed_product) = x
let _ = f_mixed_product #(-#100L, true, -#2.5)
let _ = f_mixed_product #(#0L, false, #0.0)

let[@inline never] [@local never] f_nested_product (x: nested_product) = x
let _ = f_nested_product #(#(#1.5, #10l), #200L)
let _ = f_nested_product #(#(#0.0, #0l), #0L)

let[@inline never] [@local never] f_small_int_product (x: small_int_product) = x
let _ = f_small_int_product #((Int8_u.of_int 42), (Int16_u.of_int 1000))
let _ = f_small_int_product #((Int8_u.of_int 0), (Int16_u.of_int 0))
let _ = f_small_int_product #((Int8_u.of_int (-25)), (Int16_u.of_int (-2000)))

let[@inline never] [@local never] f_mixed_small_product
    (x: mixed_small_product) = x
let _ = f_mixed_small_product #((Int8_u.of_int 42), true, (Int16_u.of_int 1000))
let _ = f_mixed_small_product #((Int8_u.of_int 0), false, (Int16_u.of_int 0))
let _ = f_mixed_small_product
    #((Int8_u.of_int (-25)), true, (Int16_u.of_int (-2000)))

type simple_record = #{ x: float#; y: int32# }
type mixed_record = #{ a: int64#; b: bool; c: float# }
type nested_record = #{ inner: simple_record; outer: int64# }
type small_int_record = #{ a: int8#; b: int16# }
type mixed_small_record = #{ i8: int8#; flag: bool; i16: int16# }

let[@inline never] [@local never] f_simple_record (r: simple_record) =
  let #{ x; y } = r in #{ x; y }
let _ = f_simple_record #{ x = #4.1; y = #42l }
let _ = f_simple_record #{ x = #0.0; y = #0l }
let _ = f_simple_record #{ x = -#3.14; y = -#123l }

let[@inline never] [@local never] f_mixed_record (r: mixed_record) =
  let #{ a; b; c } = r in #{ a; b; c }
let _ = f_mixed_record #{ a = #100L; b = true; c = #2.5 }
let _ = f_mixed_record #{ a = #0L; b = false; c = #0.0 }

let[@inline never] [@local never] f_nested_record (r: nested_record) =
  let #{ inner; outer } = r in #{ inner; outer }
let _ = f_nested_record #{ inner = #{ x = #1.5; y = #10l }; outer = #200L }
let _ = f_nested_record #{ inner = #{ x = #0.0; y = #0l }; outer = #0L }

let[@inline never] [@local never] f_poly_product
    (type a : bits64 & value) (x: a) = x

let[@inline never] [@local never] f_small_int_record (r: small_int_record) =
  let #{ a; b } = r in #{ a; b }
let _ = f_small_int_record
    #{ a = (Int8_u.of_int 42); b = (Int16_u.of_int 1000) }
let _ = f_small_int_record #{ a = (Int8_u.of_int 0); b = (Int16_u.of_int 0) }
let _ = f_small_int_record
    #{ a = (Int8_u.of_int (-25)); b = (Int16_u.of_int (-2000)) }

let[@inline never] [@local never] f_mixed_small_record (r: mixed_small_record) =
  let #{ i8; flag; i16 } = r in #{ i8; flag; i16 }
let _ = f_mixed_small_record
    #{ i8 = (Int8_u.of_int 42); flag = true; i16 = (Int16_u.of_int 1000) }
let _ = f_mixed_small_record
    #{ i8 = (Int8_u.of_int 0); flag = false; i16 = (Int16_u.of_int 0) }
let _ = f_mixed_small_record
    #{ i8 = (Int8_u.of_int (-25)); flag = true; i16 = (Int16_u.of_int (-2000)) }
let _ = f_poly_product #(#4L, 4L)
let _ = f_poly_product #(#100L, true)

(* Arrays of int64# *)
let[@inline never] [@local never] f_int64_array (arr: int64# array) = arr
let _ = f_int64_array [|#0L; #100L; #200L; #300L; #400L|]
let _ = f_int64_array [|#0L; #1L; #42L; #9999L|]
let _ = f_int64_array [|#0x123456789abcdefL; #1L; #0L|]

(* Arrays of int32# *)
let[@inline never] [@local never] f_int32_array (arr: int32# array) = arr
let _ = f_int32_array [|#0l; #10l; #20l; #30l|]
let _ = f_int32_array [|#0l; #42l; #123l; #1000l|]
let _ = f_int32_array [|#0x12345678l; #456l; #0l|]

(* Arrays of unboxed records *)
type array_record = #{ a: int64#; b: int32#; c: float# } [@@warning "-69"]

let[@inline never] [@local never] f_array_record_array
    (arr: array_record array) = arr

let _ = f_array_record_array [|#{ a = #1L; b = #2l; c = #3.0 }|]
let _ = f_array_record_array
  [|#{ a = #10L;
       b = #200l;
       c = #3.14 };
    #{ a = #5L;
       b = #1000l;
       c = #2.71 };
    #{ a = #0L;
       b = #0l;
       c = #0.0 }|]

(* Arrays of float# *)
let[@inline never] [@local never] f_float_array (arr: float# array) = arr
let _ = f_float_array [|#1.0; #2.5; #3.14; #0.0; #1e10|]
let _ = f_float_array [|#1.0; #2.0; #3.0|]

(* Arrays of unboxed tuples with mixed types *)
type mixed_tuple_for_array = #(int32# * float# * bool)

let[@inline never] [@local never] f_mixed_tuple_array
    (arr: mixed_tuple_for_array array) = arr

let _ = f_mixed_tuple_array
  [|#(#42l, #3.14, true);
    #(#0l, #0.0, false);
    #(#100l, #2.5, true)|]

(* Arrays of nativeint# *)
let[@inline never] [@local never] f_nativeint_array
    (arr: nativeint# array) = arr
let _ = f_nativeint_array [|#123n; #0n; #456n; #0x7fffffffn|]
let _ = f_nativeint_array [|#0n; #1n; #4n|]

(* Nested - array of unboxed products containing int64# pairs *)
type int64_pair_for_array = #(int64# * int64#)

let[@inline never] [@local never] f_int64_pair_array
    (arr: int64_pair_for_array array) = arr

let _ = f_int64_pair_array
  [|#(#100L, #200L);
    #(#0L, #0L);
    #(#42L, #9999L)|]

(* Arrays of float32# *)
let[@inline never] [@local never] f_float32_array (arr: float32# array) = arr
let _ = f_float32_array [|#1.5s; #0.0s; #2.5s; #100.0s|]
let _ = f_float32_array [|#0.0s; #1.0s; #2.0s; #3.0s|]

(* Edge cases: empty and large arrays *)
let[@inline never] [@local never] f_empty_int64_array
    (arr: int64# array) = arr
let _ = f_empty_int64_array [||]

let[@inline never] [@local never] f_large_int64_array
    (arr: int64# array) = arr
let _ = f_large_int64_array
  [|#0L; #3L; #6L; #9L; #12L; #15L; #18L; #21L; #24L; #27L;
    #30L; #33L; #36L; #39L; #42L; #45L; #48L; #51L; #54L; #57L|]

(* Corner cases for sub-word elements. These exercise the bytes/halfwords
   that are likely to be mishandled if the backend ever changes how small
   unboxed integers are packed into arrays or unboxed records. Element
   counts that are not a multiple of (word_size / element_size) exercise
   the tag-aware count expression emitted in
   [backend/debug/dwarf/dwarf_ocaml/dwarf_type.ml]. *)
let[@inline never] [@local never] f_int8_array (arr: int8# array) = arr
let _ = f_int8_array
  [|(Int8_u.of_int 1); (Int8_u.of_int 2);
    (Int8_u.of_int 3); (Int8_u.of_int 127)|]
let _ = f_int8_array
  [|(Int8_u.of_int 0); (Int8_u.of_int (-128)); (Int8_u.of_int 50)|]

let[@inline never] [@local never] f_int16_array (arr: int16# array) = arr
let _ = f_int16_array
  [|(Int16_u.of_int 100); (Int16_u.of_int 200); (Int16_u.of_int 32767)|]
let _ = f_int16_array
  [|(Int16_u.of_int 0); (Int16_u.of_int (-32768));
    (Int16_u.of_int 1000); (Int16_u.of_int (-2000))|]

let[@inline never] [@local never] f_int32_array_packed_corner
    (arr: int32# array) = arr
let _ = f_int32_array_packed_corner [|#10l; #20l; #30l|]
let _ = f_int32_array_packed_corner [|#7l; #8l; #9l; #10l; #11l|]

let[@inline never] [@local never] f_float32_array_packed_corner
    (arr: float32# array) = arr
let _ = f_float32_array_packed_corner [|#1.0s; #2.0s; #3.0s|]
let _ = f_float32_array_packed_corner [|#0.5s; #1.5s; #2.5s; #3.5s; #4.5s|]

(* Unboxed record of two int8# fields - tests whether sub-word fields inside
   a record are word-extended in arrays (16 bytes/record) or kept dense
   (2 bytes/record). The current implementation word-extends each field. *)
type record_2xint8 = #{ a: int8#; b: int8# } [@@warning "-69"]

let[@inline never] [@local never] f_record_2xint8_array
    (arr: record_2xint8 array) = arr
let _ = f_record_2xint8_array
  [|#{ a = (Int8_u.of_int 1); b = (Int8_u.of_int 2) };
    #{ a = (Int8_u.of_int 3); b = (Int8_u.of_int 4) };
    #{ a = (Int8_u.of_int 5); b = (Int8_u.of_int 6) }|]

(* Unboxed record with a single int64# field (8 bytes when densely packed). *)
type record_one_int64 = #{ x: int64# } [@@warning "-69"]

let[@inline never] [@local never] f_record_one_int64_array
    (arr: record_one_int64 array) = arr
let _ = f_record_one_int64_array
  [|#{ x = #1L }; #{ x = #100L }; #{ x = #0x123456789abcdefL }|]

(* Unboxed record of four 16-bit integers (8 bytes when densely packed,
   32 bytes when each field is word-extended). *)
type record_4xint16 = #{ a: int16#; b: int16#; c: int16#; d: int16# }
  [@@warning "-69"]

let[@inline never] [@local never] f_record_4xint16_array
    (arr: record_4xint16 array) = arr
let _ = f_record_4xint16_array
  [|#{ a = (Int16_u.of_int 1); b = (Int16_u.of_int 2);
       c = (Int16_u.of_int 3); d = (Int16_u.of_int 4) };
    #{ a = (Int16_u.of_int 100); b = (Int16_u.of_int 200);
       c = (Int16_u.of_int 300); d = (Int16_u.of_int 400) }|]

(* Unboxed record summing to 72 bits (9 bytes when densely packed) mixing
   32-bit, 16-bit, and 8-bit integers: 32 + 16 + 8 + 8 + 8 = 72. *)
type record_72bit =
  #{ a: int32#; b: int16#; c: int8#; d: int8#; e: int8# } [@@warning "-69"]

let[@inline never] [@local never] f_record_72bit_array
    (arr: record_72bit array) = arr
let _ = f_record_72bit_array
  [|#{ a = #1l; b = (Int16_u.of_int 2); c = (Int8_u.of_int 3);
       d = (Int8_u.of_int 4); e = (Int8_u.of_int 5) };
    #{ a = #100l; b = (Int16_u.of_int 200); c = (Int8_u.of_int 30);
       d = (Int8_u.of_int 40); e = (Int8_u.of_int 50) }|]

(* Element stride beyond a byte: 16 int64# fields word-extend to 128 bytes
   per element, which exceeds the range of a [Data1] byte_stride attribute. *)
type record_16xint64 =
  #{ f01: int64#; f02: int64#; f03: int64#; f04: int64#;
     f05: int64#; f06: int64#; f07: int64#; f08: int64#;
     f09: int64#; f10: int64#; f11: int64#; f12: int64#;
     f13: int64#; f14: int64#; f15: int64#; f16: int64# }
  [@@warning "-69"]

let[@inline never] [@local never] f_record_16xint64_array
    (arr: record_16xint64 array) = arr
let _ = f_record_16xint64_array
  [|#{ f01 = #1L; f02 = #2L; f03 = #3L; f04 = #4L;
       f05 = #5L; f06 = #6L; f07 = #7L; f08 = #8L;
       f09 = #9L; f10 = #10L; f11 = #11L; f12 = #12L;
       f13 = #13L; f14 = #14L; f15 = #15L; f16 = #16L }|]
