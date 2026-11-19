open Stdlib

type void : void

type addr = nativeint_u

external int64x8_of_int64s :
  int64 -> int64 -> int64 -> int64 -> int64 -> int64 -> int64 -> int64
  -> int64x8 = "" "vec512_of_int64s"
[@@noalloc] [@@unboxed]

external int64x4_of_int64s : int64 -> int64 -> int64 -> int64 -> int64x4
  = "" "vec256_of_int64s"
[@@noalloc] [@@unboxed]

external int64x2_of_int64s : int64 -> int64 -> int64x2 = "" "vec128_of_int64s"
[@@noalloc] [@@unboxed]

external reint : 'a -> 'b = "%identity"

external int64x8_get :
  (int64x8[@unboxed]) -> (int[@untagged]) -> (int64[@unboxed]) = "" "vec512_wi"
[@@noalloc]

external int64x4_get :
  (int64x4[@unboxed]) -> (int[@untagged]) -> (int64[@unboxed]) = "" "vec256_wi"
[@@noalloc]

external int64x2_get :
  (int64x2[@unboxed]) -> (int[@untagged]) -> (int64[@unboxed]) = "" "vec128_wi"
[@@noalloc]

external mask_of_int64 : int64 -> mask
  = "caml_vec512_unreachable" "caml_mask_of_int64"
[@@noalloc] [@@unboxed] [@@builtin]

external int64_of_mask : mask -> int64
  = "caml_vec512_unreachable" "caml_int64_of_mask"
[@@noalloc] [@@unboxed] [@@builtin]

external buf : (int[@untagged]) -> addr = "" "test_buf" [@@noalloc]

external buf_reset : (int[@untagged]) -> (int[@untagged])
  = "" "test_buf_reset"
[@@noalloc]

external buf_eq : (int[@untagged]) -> (int[@untagged]) = "" "test_buf_eq"
[@@noalloc]

(* Each test is its own function: a module initializer containing all of them
   is too large for the register allocators. *)
let[@inline never] test name f = f name

(* Include both outcomes of mask flag tests and both scalar mask-bit states. *)
let[@inline never] test_masks name f =
  List.iteri
    (fun i masks -> f (Printf.sprintf "%s/mask%d" name i) masks)
    [ 0L, 0L; -1L, 0L; 0L, -1L; -1L, -1L;
      0xa5a5a5a5a5a5a5a5L, 0x3c3c3c3c3c3c3c3cL;
      0xa5a5a5a5a5a5a5a5L, 0x5a5a5a5a5a5a5a5aL;
      0xa5a5a5a5a5a5a5a5L, 0xa5a5a5a5a5a5a5a5L;
      1L, 1L; Int64.min_int, Int64.min_int ]

let mask_arg masks idx width =
  let bits = if idx mod 2 = 0 then fst masks else snd masks in
  mask_of_int64 (Int64.logand bits width)

let failures = ref 0

let fail name =
  incr failures;
  Printf.printf "MISMATCH %s\n" name

let check_lanes ~get n name a b =
  let ok = ref true in
  for i = 0 to n - 1 do
    if not (Int64.equal (get a i) (get b i)) then ok := false
  done;
  if not !ok then fail name

let check512 name a b = check_lanes ~get:int64x8_get 8 name (reint a) (reint b)

let check256 name a b = check_lanes ~get:int64x4_get 4 name (reint a) (reint b)

let check128 name a b = check_lanes ~get:int64x2_get 2 name (reint a) (reint b)

let check_mask name (a : mask) (b : mask) =
  if not (Int64.equal (int64_of_mask a) (int64_of_mask b)) then fail name

let check_i32 name (a : int32) (b : int32) =
  if not (Int32.equal a b) then fail name

let check_i64 name (a : int64) (b : int64) =
  if not (Int64.equal a b) then fail name

let check_int name (a : int) (b : int) = if a <> b then fail name

let check_f32 name (a : float32) (b : float32) =
  if
    not
      (Int32.equal (Stdlib_stable.Float32.to_bits a)
         (Stdlib_stable.Float32.to_bits b))
  then fail name

let check_f64 name (a : float) (b : float) =
  if not (Int64.equal (Int64.bits_of_float a) (Int64.bits_of_float b))
  then fail name

let check_buf name = if buf_eq 0 <> 1 then fail name

let va =
  int64x8_of_int64s 0x3f8000004048f5c3L 0xbff0000040a00000L 0x0000000100000002L
    0xfffffffe7fffffffL 0x8000000012345678L 0x40490fdbc0000000L
    0x0102030405060708L 0x1122334455667788L

let vb =
  int64x8_of_int64s 0x4000000040400000L 0x3fc00000c1200000L 0x00000003fffffffdL
    0x000000057fffffffL 0x9abcdef0deadbeefL 0x3ff0000040000000L
    0x0807060504030201L 0x8877665544332211L

let vc =
  int64x8_of_int64s 0x41000000c1a00000L 0x4048f5c33f800000L 0x0000000600000007L
    0xaaaaaaaa55555555L 0x0f0f0f0ff0f0f0f0L 0x400921fb54442d18L
    0xdeadbeefcafebabeL 0x0123456789abcdefL

let va256 = int64x4_of_int64s 0x3f8000004048f5c3L 0xbff0000040a00000L
    0x0000000100000002L 0xfffffffe7fffffffL

let vb256 = int64x4_of_int64s 0x4000000040400000L 0x3fc00000c1200000L
    0x00000003fffffffdL 0x000000057fffffffL

let vc256 = int64x4_of_int64s 0x41000000c1a00000L 0x4048f5c33f800000L
    0x0000000600000007L 0xaaaaaaaa55555555L

let f32a = Stdlib_stable.Float32.of_float 1.5

let f32b = Stdlib_stable.Float32.of_float (-2.25)

let f32c = Stdlib_stable.Float32.of_float 3.0e10

let va128 = int64x2_of_int64s 0x3f8000004048f5c3L 0xbff0000040a00000L

let vb128 = int64x2_of_int64s 0x4000000040400000L 0x3fc00000c1200000L

let vc128 = int64x2_of_int64s 0x41000000c1a00000L 0x4048f5c33f800000L

(* Gather/scatter indices: distinct lanes within [-8, 31], lane [j] being
   [(j * 7 mod 40) - 8] for 32-bit indices and [(j * 11 mod 40) - 8] for 64-bit
   ones. *)
let idx32_512 = int64x8_of_int64s 0xfffffffffffffff8L 0xd00000006L 0x1b00000014L
    0x1fffffffaL 0xf00000008L 0x1d00000016L 0x3fffffffcL 0x110000000aL

let idx32_256 = int64x4_of_int64s 0xfffffffffffffff8L 0xd00000006L 0x1b00000014L
    0x1fffffffaL

let idx32_128 = int64x2_of_int64s 0xfffffffffffffff8L 0xd00000006L

let idx64_512 = int64x8_of_int64s 0xfffffffffffffff8L 0x3L 0xeL 0x19L
    0xfffffffffffffffcL 0x7L 0x12L 0x1dL

let idx64_256 = int64x4_of_int64s 0xfffffffffffffff8L 0x3L 0xeL 0x19L

let idx64_128 = int64x2_of_int64s 0xfffffffffffffff8L 0x3L
