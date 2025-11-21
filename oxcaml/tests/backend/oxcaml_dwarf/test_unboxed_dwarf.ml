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
