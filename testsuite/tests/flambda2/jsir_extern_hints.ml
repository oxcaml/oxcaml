(* TEST
 flags = "-djsir -nopervasives -extension small_numbers";
 jsir;
*)

(* Checks the optimisation hints that the JSIR produced by Flambda 2 attaches
   to external primitives, and the primitives it selects for them:

   - string, bytes and bigstring accesses are bounds-checked by Flambda, so
     they use the unchecked runtime primitives ([caml_string_get16u], ...)
     and carry [hint:unsafe]; signed accesses are sign-extended afterwards;
   - bigarray accesses carry the kind and layout of the bigarray;
   - comparisons of boxed int64s carry [hint:int64];
   - array lengths carry the kind of the array;
   - externals taking or returning unboxed numbers carry no hint. *)

external ( = ) : 'a -> 'a -> bool = "%equal"
external ( <> ) : 'a -> 'a -> bool = "%notequal"
external ( < ) : 'a -> 'a -> bool = "%lessthan"
external ( <= ) : 'a -> 'a -> bool = "%lessequal"
external ( > ) : 'a -> 'a -> bool = "%greaterthan"
external ( >= ) : 'a -> 'a -> bool = "%greaterequal"
external not : bool -> bool = "%boolnot"
external opaque : 'a -> 'a = "%opaque"

external bytes_create : int -> bytes = "caml_create_bytes"
external array_length : 'a array -> int = "%array_length"
external float_of_float32 : float32 -> float = "%floatoffloat32"

external string_get16 : string -> int -> int = "%caml_string_get16"
external string_geti8 : string -> int -> int = "%caml_string_geti8"
external string_geti16 : string -> int -> int = "%caml_string_geti16"
external string_get32 : string -> int -> int32 = "%caml_string_get32"
external string_getf32 : string -> int -> float32 = "%caml_string_getf32"
external string_get64 : string -> int -> int64 = "%caml_string_get64"

external bytes_get16 : bytes -> int -> int = "%caml_bytes_get16"
external bytes_geti8 : bytes -> int -> int = "%caml_bytes_geti8"
external bytes_geti16 : bytes -> int -> int = "%caml_bytes_geti16"
external bytes_get32 : bytes -> int -> int32 = "%caml_bytes_get32"
external bytes_getf32 : bytes -> int -> float32 = "%caml_bytes_getf32"
external bytes_get64 : bytes -> int -> int64 = "%caml_bytes_get64"
external bytes_set8 : bytes -> int -> int -> unit = "%caml_bytes_set8"
external bytes_set16 : bytes -> int -> int -> unit = "%caml_bytes_set16"
external bytes_set32 : bytes -> int -> int32 -> unit = "%caml_bytes_set32"
external bytes_setf32 : bytes -> int -> float32 -> unit = "%caml_bytes_setf32"
external bytes_set64 : bytes -> int -> int64 -> unit = "%caml_bytes_set64"

type bigstring =
  (char, Stdlib.Bigarray.int8_unsigned_elt, Stdlib.Bigarray.c_layout)
    Stdlib.Bigarray.Array1.t

type float_array =
  (float, Stdlib.Bigarray.float64_elt, Stdlib.Bigarray.c_layout)
    Stdlib.Bigarray.Array1.t

type int32_fortran_array =
  (int32, Stdlib.Bigarray.int32_elt, Stdlib.Bigarray.fortran_layout)
    Stdlib.Bigarray.Array1.t

(* The kind and layout are passed as their runtime encodings. *)
external ba_create : int -> int -> int array -> ('a, 'b, 'c) Stdlib.Bigarray.Array1.t
  = "caml_ba_create"

external bigstring_get16 : bigstring -> int -> int = "%caml_bigstring_get16"
external bigstring_geti8 : bigstring -> int -> int = "%caml_bigstring_geti8"
external bigstring_geti16 : bigstring -> int -> int = "%caml_bigstring_geti16"
external bigstring_get32 : bigstring -> int -> int32 = "%caml_bigstring_get32"
external bigstring_getf32 : bigstring -> int -> float32 = "%caml_bigstring_getf32"
external bigstring_get64 : bigstring -> int -> int64 = "%caml_bigstring_get64"
external bigstring_set8 : bigstring -> int -> int -> unit = "%caml_bigstring_set8"
external bigstring_set16 : bigstring -> int -> int -> unit = "%caml_bigstring_set16"
external bigstring_set32 : bigstring -> int -> int32 -> unit = "%caml_bigstring_set32"
external bigstring_setf32 : bigstring -> int -> float32 -> unit = "%caml_bigstring_setf32"
external bigstring_set64 : bigstring -> int -> int64 -> unit = "%caml_bigstring_set64"

external float_array_get : float_array -> int -> float = "%caml_ba_ref_1"
external float_array_set : float_array -> int -> float -> unit = "%caml_ba_set_1"
external int32_fortran_get : int32_fortran_array -> int -> int32 = "%caml_ba_ref_1"
external int32_fortran_set : int32_fortran_array -> int -> int32 -> unit
  = "%caml_ba_set_1"

external sqrt : float -> float = "caml_sqrt_float" "sqrt" [@@unboxed] [@@noalloc]
external int32_of_float : (float [@unboxed]) -> (int32 [@unboxed])
  = "caml_int32_of_float" "caml_int32_of_float_unboxed" [@@noalloc]

let[@inline never] string_loads () =
  let s = opaque "\x01\x02\x03\x04\x05\x06\x07\x08\x80\xff" in
  assert (string_get16 s 0 = 0x0201);
  assert (string_geti8 s 8 = -128);
  assert (string_geti16 s 8 = -128);
  assert (string_get32 s 0 = 0x04030201l);
  assert (string_get64 s 0 = 0x0807060504030201L);
  (* 0x3f800000 is 1.0 *)
  let f = opaque "\x00\x00\x80\x3f" in
  assert (float_of_float32 (string_getf32 f 0) = 1.)

let[@inline never] bytes_accesses () =
  let b = opaque (bytes_create 8) in
  bytes_set8 b 0 0xf0;
  assert (bytes_geti8 b 0 = -16);
  bytes_set16 b 0 (-2);
  assert (bytes_get16 b 0 = 0xfffe);
  assert (bytes_geti16 b 0 = -2);
  bytes_set32 b 0 0x12345678l;
  assert (bytes_get32 b 0 = 0x12345678l);
  bytes_setf32 b 0 1.5s;
  assert (float_of_float32 (bytes_getf32 b 0) = 1.5);
  bytes_set64 b 0 0x0102030405060708L;
  assert (bytes_get64 b 0 = 0x0102030405060708L)

let[@inline never] bigstring_accesses () =
  (* kind 12 is [char], layout 0 is [c_layout] *)
  let b : bigstring = opaque (ba_create 12 0 [| 8 |]) in
  bigstring_set8 b 0 0xf0;
  assert (bigstring_geti8 b 0 = -16);
  bigstring_set16 b 0 (-2);
  assert (bigstring_get16 b 0 = 0xfffe);
  assert (bigstring_geti16 b 0 = -2);
  bigstring_set32 b 0 0x12345678l;
  assert (bigstring_get32 b 0 = 0x12345678l);
  bigstring_setf32 b 0 1.5s;
  assert (float_of_float32 (bigstring_getf32 b 0) = 1.5);
  bigstring_set64 b 0 0x0102030405060708L;
  assert (bigstring_get64 b 0 = 0x0102030405060708L)

let[@inline never] bigarray_accesses () =
  (* kind 1 is [float64], layout 0 is [c_layout] *)
  let a : float_array = opaque (ba_create 1 0 [| 4 |]) in
  float_array_set a 2 3.5;
  assert (float_array_get a 2 = 3.5);
  (* kind 6 is [int32], layout 1 is [fortran_layout] *)
  let i : int32_fortran_array = opaque (ba_create 6 1 [| 4 |]) in
  int32_fortran_set i 1 42l;
  assert (int32_fortran_get i 1 = 42l)

let[@inline never] int64_comparisons () =
  let a = opaque 5L in
  let b = opaque 7L in
  assert (a < b);
  assert (a <= b);
  assert (b > a);
  assert (b >= a);
  assert (not (a = b));
  assert (a <> b)

let[@inline never] generic_length (a : 'a array) = array_length a

let[@inline never] array_lengths () =
  let floats : float array = opaque [| 1.; 2. |] in
  let ints : int array = opaque [| 1; 2; 3 |] in
  assert (array_length floats = 2);
  assert (array_length ints = 3);
  assert (generic_length ints = 3)

let[@inline never] unboxed_externals () =
  assert (sqrt (opaque 16.) = 4.);
  assert (int32_of_float (opaque 3.7) = 3l)

let () =
  string_loads ();
  bytes_accesses ();
  bigstring_accesses ();
  bigarray_accesses ();
  int64_comparisons ();
  array_lengths ();
  unboxed_externals ()
