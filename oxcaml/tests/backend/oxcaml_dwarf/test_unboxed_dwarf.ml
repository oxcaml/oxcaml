let[@inline never] [@local never] f_start () = ()
let _ = f_start ()

let[@inline never] [@local never] f_unboxed_float (x: float#) = x
let _ = f_unboxed_float #4.1
let _ = f_unboxed_float #0.0
(* CR sspies: floats that end in .0 are printed with just .
   It would be more uniform to always print the trailing 0. *)
let _ = f_unboxed_float (-#3.14)
(* CR sspies: debugger shows as #-3.14 rather than -#3.14 *)
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

type simple_product = #(float# * int32#)
type mixed_product = #(int64# * bool * float#)
type nested_product = #(simple_product * int64#)

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

type simple_record = #{ x: float#; y: int32# }
type mixed_record = #{ a: int64#; b: bool; c: float# }
type nested_record = #{ inner: simple_record; outer: int64# }

let[@inline never] [@local never] f_simple_record (x: simple_record) =
  let #{ x; y } = x in #{ x; y }
let _ = f_simple_record #{ x = #4.1; y = #42l }
let _ = f_simple_record #{ x = #0.0; y = #0l }
let _ = f_simple_record #{ x = -#3.14; y = -#123l }

let[@inline never] [@local never] f_mixed_record (x: mixed_record) =
  let #{ a; b; c } = x in #{ a; b; c }
let _ = f_mixed_record #{ a = #100L; b = true; c = #2.5 }
let _ = f_mixed_record #{ a = #0L; b = false; c = #0.0 }

let[@inline never] [@local never] f_nested_record (x: nested_record) =
  let #{ inner; outer } = x in #{ inner; outer }
let _ = f_nested_record #{ inner = #{ x = #1.5; y = #10l }; outer = #200L }
let _ = f_nested_record #{ inner = #{ x = #0.0; y = #0l }; outer = #0L }

let[@inline never] [@local never] f_poly_product
    (type a : bits64 & value) (x: a) = x
let _ = f_poly_product #(#4L, 4L)
let _ = f_poly_product #(#100L, true)
