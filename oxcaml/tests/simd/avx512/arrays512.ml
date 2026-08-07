open Stdlib
open! Stdlib_stable

let () = Printexc.record_backtrace true

[@@@ocaml.warning "-unused-value-declaration"]
[@@@ocaml.warning "-unused-module"]

external int8x64_of_int64s : int64 -> int64 -> int64 -> int64 -> int64 -> int64 -> int64 -> int64 -> int8x64 = "" "vec512_of_int64s" [@@noalloc] [@@unboxed]
external int8x64_first_int64 : int8x64 -> int64 = "" "vec512_w0" [@@noalloc] [@@unboxed]
external int8x64_second_int64 : int8x64 -> int64 = "" "vec512_w1" [@@noalloc] [@@unboxed]
external int8x64_third_int64 : int8x64 -> int64 = "" "vec512_w2" [@@noalloc] [@@unboxed]
external int8x64_fourth_int64 : int8x64 -> int64 = "" "vec512_w3" [@@noalloc] [@@unboxed]
external int8x64_fifth_int64 : int8x64 -> int64 = "" "vec512_w4" [@@noalloc] [@@unboxed]
external int8x64_sixth_int64 : int8x64 -> int64 = "" "vec512_w5" [@@noalloc] [@@unboxed]
external int8x64_seventh_int64 : int8x64 -> int64 = "" "vec512_w6" [@@noalloc] [@@unboxed]
external int8x64_eighth_int64 : int8x64 -> int64 = "" "vec512_w7" [@@noalloc] [@@unboxed]

external int16x32_of_int64s : int64 -> int64 -> int64 -> int64 -> int64 -> int64 -> int64 -> int64 -> int16x32 = "" "vec512_of_int64s" [@@noalloc] [@@unboxed]
external int16x32_first_int64 : int16x32 -> int64 = "" "vec512_w0" [@@noalloc] [@@unboxed]
external int16x32_second_int64 : int16x32 -> int64 = "" "vec512_w1" [@@noalloc] [@@unboxed]
external int16x32_third_int64 : int16x32 -> int64 = "" "vec512_w2" [@@noalloc] [@@unboxed]
external int16x32_fourth_int64 : int16x32 -> int64 = "" "vec512_w3" [@@noalloc] [@@unboxed]
external int16x32_fifth_int64 : int16x32 -> int64 = "" "vec512_w4" [@@noalloc] [@@unboxed]
external int16x32_sixth_int64 : int16x32 -> int64 = "" "vec512_w5" [@@noalloc] [@@unboxed]
external int16x32_seventh_int64 : int16x32 -> int64 = "" "vec512_w6" [@@noalloc] [@@unboxed]
external int16x32_eighth_int64 : int16x32 -> int64 = "" "vec512_w7" [@@noalloc] [@@unboxed]

external int32x16_of_int64s : int64 -> int64 -> int64 -> int64 -> int64 -> int64 -> int64 -> int64 -> int32x16 = "" "vec512_of_int64s" [@@noalloc] [@@unboxed]
external int32x16_first_int64 : int32x16 -> int64 = "" "vec512_w0" [@@noalloc] [@@unboxed]
external int32x16_second_int64 : int32x16 -> int64 = "" "vec512_w1" [@@noalloc] [@@unboxed]
external int32x16_third_int64 : int32x16 -> int64 = "" "vec512_w2" [@@noalloc] [@@unboxed]
external int32x16_fourth_int64 : int32x16 -> int64 = "" "vec512_w3" [@@noalloc] [@@unboxed]
external int32x16_fifth_int64 : int32x16 -> int64 = "" "vec512_w4" [@@noalloc] [@@unboxed]
external int32x16_sixth_int64 : int32x16 -> int64 = "" "vec512_w5" [@@noalloc] [@@unboxed]
external int32x16_seventh_int64 : int32x16 -> int64 = "" "vec512_w6" [@@noalloc] [@@unboxed]
external int32x16_eighth_int64 : int32x16 -> int64 = "" "vec512_w7" [@@noalloc] [@@unboxed]

external int64x8_of_int64s : int64 -> int64 -> int64 -> int64 -> int64 -> int64 -> int64 -> int64 -> int64x8 = "" "vec512_of_int64s" [@@noalloc] [@@unboxed]
external int64x8_first_int64 : int64x8 -> int64 = "" "vec512_w0" [@@noalloc] [@@unboxed]
external int64x8_second_int64 : int64x8 -> int64 = "" "vec512_w1" [@@noalloc] [@@unboxed]
external int64x8_third_int64 : int64x8 -> int64 = "" "vec512_w2" [@@noalloc] [@@unboxed]
external int64x8_fourth_int64 : int64x8 -> int64 = "" "vec512_w3" [@@noalloc] [@@unboxed]
external int64x8_fifth_int64 : int64x8 -> int64 = "" "vec512_w4" [@@noalloc] [@@unboxed]
external int64x8_sixth_int64 : int64x8 -> int64 = "" "vec512_w5" [@@noalloc] [@@unboxed]
external int64x8_seventh_int64 : int64x8 -> int64 = "" "vec512_w6" [@@noalloc] [@@unboxed]
external int64x8_eighth_int64 : int64x8 -> int64 = "" "vec512_w7" [@@noalloc] [@@unboxed]

external float32x16_of_int64s : int64 -> int64 -> int64 -> int64 -> int64 -> int64 -> int64 -> int64 -> float32x16 = "" "vec512_of_int64s" [@@noalloc] [@@unboxed]
external float32x16_first_int64 : float32x16 -> int64 = "" "vec512_w0" [@@noalloc] [@@unboxed]
external float32x16_second_int64 : float32x16 -> int64 = "" "vec512_w1" [@@noalloc] [@@unboxed]
external float32x16_third_int64 : float32x16 -> int64 = "" "vec512_w2" [@@noalloc] [@@unboxed]
external float32x16_fourth_int64 : float32x16 -> int64 = "" "vec512_w3" [@@noalloc] [@@unboxed]
external float32x16_fifth_int64 : float32x16 -> int64 = "" "vec512_w4" [@@noalloc] [@@unboxed]
external float32x16_sixth_int64 : float32x16 -> int64 = "" "vec512_w5" [@@noalloc] [@@unboxed]
external float32x16_seventh_int64 : float32x16 -> int64 = "" "vec512_w6" [@@noalloc] [@@unboxed]
external float32x16_eighth_int64 : float32x16 -> int64 = "" "vec512_w7" [@@noalloc] [@@unboxed]

external float64x8_of_int64s : int64 -> int64 -> int64 -> int64 -> int64 -> int64 -> int64 -> int64 -> float64x8 = "" "vec512_of_int64s" [@@noalloc] [@@unboxed]
external float64x8_first_int64 : float64x8 -> int64 = "" "vec512_w0" [@@noalloc] [@@unboxed]
external float64x8_second_int64 : float64x8 -> int64 = "" "vec512_w1" [@@noalloc] [@@unboxed]
external float64x8_third_int64 : float64x8 -> int64 = "" "vec512_w2" [@@noalloc] [@@unboxed]
external float64x8_fourth_int64 : float64x8 -> int64 = "" "vec512_w3" [@@noalloc] [@@unboxed]
external float64x8_fifth_int64 : float64x8 -> int64 = "" "vec512_w4" [@@noalloc] [@@unboxed]
external float64x8_sixth_int64 : float64x8 -> int64 = "" "vec512_w5" [@@noalloc] [@@unboxed]
external float64x8_seventh_int64 : float64x8 -> int64 = "" "vec512_w6" [@@noalloc] [@@unboxed]
external float64x8_eighth_int64 : float64x8 -> int64 = "" "vec512_w7" [@@noalloc] [@@unboxed]

let eq a0 a1 a2 a3 a4 a5 a6 a7 b0 b1 b2 b3 b4 b5 b6 b7 =
  if b0 <> a0 then Printf.printf "%016Lx <> %016Lx\n" a0 b0;
  if b1 <> a1 then Printf.printf "%016Lx <> %016Lx\n" a1 b1;
  if b2 <> a2 then Printf.printf "%016Lx <> %016Lx\n" a2 b2;
  if b3 <> a3 then Printf.printf "%016Lx <> %016Lx\n" a3 b3;
  if b4 <> a4 then Printf.printf "%016Lx <> %016Lx\n" a4 b4;
  if b5 <> a5 then Printf.printf "%016Lx <> %016Lx\n" a5 b5;
  if b6 <> a6 then Printf.printf "%016Lx <> %016Lx\n" a6 b6;
  if b7 <> a7 then Printf.printf "%016Lx <> %016Lx\n" a7 b7
;;

let assert_raises_out_of_bounds thunk =
  try
    thunk ();
    assert false
  with
  | Invalid_argument s when s = "index out of bounds" -> ()
  | Invalid_argument s -> failwith s
  | _ -> assert false
;;

let test_data = "\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0a\x0b\x0c\x0d\x0e\x0f\x10\x11\x12\x13\x14\x15\x16\x17\x18\x19\x1a\x1b\x1c\x1d\x1e\x1f\x20\x21\x22\x23\x24\x25\x26\x27\x28\x29\x2a\x2b\x2c\x2d\x2e\x2f\x30\x31\x32\x33\x34\x35\x36\x37\x38\x39\x3a\x3b\x3c\x3d\x3e\x3f\x00\x01\x02\x03\x04\x05\x06\x07"

module Bytes (Primitives : sig
  val get_int8x64_unaligned : bytes -> int -> int8x64
  val get_int8x64_unaligned_unsafe : bytes -> int -> int8x64
  val set_int8x64_unaligned : bytes -> int -> int8x64 -> unit
  val set_int8x64_unaligned_unsafe : bytes -> int -> int8x64 -> unit
  val extra_checks : bytes -> unit
end) =
struct
  open Primitives

  let data = Bytes.of_string test_data

  let first = 0x0706050403020100L
  let second = 0x0f0e0d0c0b0a0908L
  let third = 0x1716151413121110L
  let fourth = 0x1f1e1d1c1b1a1918L
  let fifth = 0x2726252423222120L
  let sixth = 0x2f2e2d2c2b2a2928L
  let seventh = 0x3736353433323130L
  let eighth = 0x3f3e3d3c3b3a3938L

  (* Getters *)

  let () =
    let v = get_int8x64_unaligned data 0 in
    eq first second third fourth fifth sixth seventh eighth (int8x64_first_int64 v) (int8x64_second_int64 v) (int8x64_third_int64 v) (int8x64_fourth_int64 v) (int8x64_fifth_int64 v) (int8x64_sixth_int64 v) (int8x64_seventh_int64 v) (int8x64_eighth_int64 v);
    let v = get_int8x64_unaligned_unsafe data 0 in
    eq first second third fourth fifth sixth seventh eighth (int8x64_first_int64 v) (int8x64_second_int64 v) (int8x64_third_int64 v) (int8x64_fourth_int64 v) (int8x64_fifth_int64 v) (int8x64_sixth_int64 v) (int8x64_seventh_int64 v) (int8x64_eighth_int64 v);
    let v = get_int8x64_unaligned data 8 in
    eq second third fourth fifth sixth seventh eighth first (int8x64_first_int64 v) (int8x64_second_int64 v) (int8x64_third_int64 v) (int8x64_fourth_int64 v) (int8x64_fifth_int64 v) (int8x64_sixth_int64 v) (int8x64_seventh_int64 v) (int8x64_eighth_int64 v);
    let v = get_int8x64_unaligned_unsafe data 8 in
    eq second third fourth fifth sixth seventh eighth first (int8x64_first_int64 v) (int8x64_second_int64 v) (int8x64_third_int64 v) (int8x64_fourth_int64 v) (int8x64_fifth_int64 v) (int8x64_sixth_int64 v) (int8x64_seventh_int64 v) (int8x64_eighth_int64 v);
  ;;

  let () =
    for bad = 9 to 72 do
      try
        let _ = get_int8x64_unaligned data bad in
        assert false
      with | Invalid_argument s when s = "index out of bounds" -> ()
    done;
  ;;

  (* Setters *)

  let set_unaligned first second third fourth fifth sixth seventh eighth offset =
    let set = int8x64_of_int64s first second third fourth fifth sixth seventh eighth in
    set_int8x64_unaligned data offset set;
    let v = get_int8x64_unaligned data offset in
    eq first second third fourth fifth sixth seventh eighth (int8x64_first_int64 v) (int8x64_second_int64 v) (int8x64_third_int64 v) (int8x64_fourth_int64 v) (int8x64_fifth_int64 v) (int8x64_sixth_int64 v) (int8x64_seventh_int64 v) (int8x64_eighth_int64 v);
  ;;

  let set_unaligned_unsafe first second third fourth fifth sixth seventh eighth offset =
    let set = int8x64_of_int64s first second third fourth fifth sixth seventh eighth in
    set_int8x64_unaligned_unsafe data offset set;
    let v = get_int8x64_unaligned data offset in
    eq first second third fourth fifth sixth seventh eighth (int8x64_first_int64 v) (int8x64_second_int64 v) (int8x64_third_int64 v) (int8x64_fourth_int64 v) (int8x64_fifth_int64 v) (int8x64_sixth_int64 v) (int8x64_seventh_int64 v) (int8x64_eighth_int64 v);
  ;;

  let () =
    set_unaligned 0x1010101010101010L 0x1010101010101010L 0x1010101010101010L 0x1010101010101010L 0x1010101010101010L 0x1010101010101010L 0x1010101010101010L 0x1010101010101010L 0;
    set_unaligned 0x2020202020202020L 0x2020202020202020L 0x2020202020202020L 0x2020202020202020L 0x2020202020202020L 0x2020202020202020L 0x2020202020202020L 0x2020202020202020L 8;
    set_unaligned_unsafe 0x3030303030303030L 0x3030303030303030L 0x3030303030303030L 0x3030303030303030L 0x3030303030303030L 0x3030303030303030L 0x3030303030303030L 0x3030303030303030L 0;
    set_unaligned_unsafe 0x4040404040404040L 0x4040404040404040L 0x4040404040404040L 0x4040404040404040L 0x4040404040404040L 0x4040404040404040L 0x4040404040404040L 0x4040404040404040L 8;
    Random.init 1234;
    for _ = 1 to 1000 do
      set_unaligned (Random.int64 Int64.max_int) (Random.int64 Int64.max_int) (Random.int64 Int64.max_int) (Random.int64 Int64.max_int) (Random.int64 Int64.max_int) (Random.int64 Int64.max_int) (Random.int64 Int64.max_int) (Random.int64 Int64.max_int) (Random.int 9);
      set_unaligned_unsafe (Random.int64 Int64.max_int) (Random.int64 Int64.max_int) (Random.int64 Int64.max_int) (Random.int64 Int64.max_int) (Random.int64 Int64.max_int) (Random.int64 Int64.max_int) (Random.int64 Int64.max_int) (Random.int64 Int64.max_int) (Random.int 9)
    done;
  ;;

  let () =
    let set = int8x64_of_int64s 0xFFFFFFFFFFFFFFFFL 0xFFFFFFFFFFFFFFFFL 0xFFFFFFFFFFFFFFFFL 0xFFFFFFFFFFFFFFFFL 0xFFFFFFFFFFFFFFFFL 0xFFFFFFFFFFFFFFFFL 0xFFFFFFFFFFFFFFFFL 0xFFFFFFFFFFFFFFFFL in
    for bad = 9 to 72 do
      try
        let _ = set_int8x64_unaligned data bad set in
        assert false
      with | Invalid_argument s when s = "index out of bounds" -> ()
    done;
  ;;

  (* Extra checks *)

  let () = extra_checks data
end

module _ = Bytes(struct
  external get_int8x64_unaligned : bytes -> int -> int8x64 = "%caml_bytes_getu512"
  external get_int8x64_unaligned_unsafe : bytes -> int -> int8x64 = "%caml_bytes_getu512u"

  external set_int8x64_unaligned : bytes -> int -> int8x64 -> unit = "%caml_bytes_setu512"
  external set_int8x64_unaligned_unsafe : bytes -> int -> int8x64 -> unit = "%caml_bytes_setu512u"

  let extra_checks bytes =
    List.iter
      (fun index ->
        assert_raises_out_of_bounds (fun () ->
          let _ = get_int8x64_unaligned bytes index in
          ());
        assert_raises_out_of_bounds (fun () ->
          set_int8x64_unaligned bytes index (int8x64_of_int64s 1L 2L 3L 4L 5L 6L 7L 8L)))
      Int.[ min_int; add min_int one; sub zero one; max_int ]
  ;;
end)

module _ = Bytes(struct
  external get_int8x64_unaligned_prim : bytes -> int8# -> int8x64 = "%caml_bytes_getu512_indexed_by_int8#"
  let get_int8x64_unaligned b i = get_int8x64_unaligned_prim b (Stdlib_stable.Int8_u.of_int i)
  external get_int8x64_unaligned_unsafe_prim : bytes -> int8# -> int8x64 = "%caml_bytes_getu512u_indexed_by_int8#"
  let get_int8x64_unaligned_unsafe b i = get_int8x64_unaligned_unsafe_prim b (Stdlib_stable.Int8_u.of_int i)

  external set_int8x64_unaligned_prim : bytes -> int8# -> int8x64 -> unit = "%caml_bytes_setu512_indexed_by_int8#"
  let set_int8x64_unaligned b i v = set_int8x64_unaligned_prim b (Stdlib_stable.Int8_u.of_int i) v
  external set_int8x64_unaligned_unsafe_prim : bytes -> int8# -> int8x64 -> unit = "%caml_bytes_setu512u_indexed_by_int8#"
  let set_int8x64_unaligned_unsafe b i v = set_int8x64_unaligned_unsafe_prim b (Stdlib_stable.Int8_u.of_int i) v

  let extra_checks bytes =
    List.iter
      (fun index ->
        let index = Stdlib_stable.Int8_u.of_int8 index in
        assert_raises_out_of_bounds (fun () ->
          let _ = get_int8x64_unaligned_prim bytes index in
          ());
        assert_raises_out_of_bounds (fun () ->
          set_int8x64_unaligned_prim bytes index (int8x64_of_int64s 1L 2L 3L 4L 5L 6L 7L 8L)))
      Stdlib_stable.Int8.[ min_int; add min_int one; sub zero one; max_int ]
  ;;
end)

module _ = Bytes(struct
  external get_int8x64_unaligned_prim : bytes -> int16# -> int8x64 = "%caml_bytes_getu512_indexed_by_int16#"
  let get_int8x64_unaligned b i = get_int8x64_unaligned_prim b (Stdlib_stable.Int16_u.of_int i)
  external get_int8x64_unaligned_unsafe_prim : bytes -> int16# -> int8x64 = "%caml_bytes_getu512u_indexed_by_int16#"
  let get_int8x64_unaligned_unsafe b i = get_int8x64_unaligned_unsafe_prim b (Stdlib_stable.Int16_u.of_int i)

  external set_int8x64_unaligned_prim : bytes -> int16# -> int8x64 -> unit = "%caml_bytes_setu512_indexed_by_int16#"
  let set_int8x64_unaligned b i v = set_int8x64_unaligned_prim b (Stdlib_stable.Int16_u.of_int i) v
  external set_int8x64_unaligned_unsafe_prim : bytes -> int16# -> int8x64 -> unit = "%caml_bytes_setu512u_indexed_by_int16#"
  let set_int8x64_unaligned_unsafe b i v = set_int8x64_unaligned_unsafe_prim b (Stdlib_stable.Int16_u.of_int i) v

  let extra_checks bytes =
    List.iter
      (fun index ->
        let index = Stdlib_stable.Int16_u.of_int16 index in
        assert_raises_out_of_bounds (fun () ->
          let _ = get_int8x64_unaligned_prim bytes index in
          ());
        assert_raises_out_of_bounds (fun () ->
          set_int8x64_unaligned_prim bytes index (int8x64_of_int64s 1L 2L 3L 4L 5L 6L 7L 8L)))
      Stdlib_stable.Int16.[ min_int; add min_int one; sub zero one; max_int ]
  ;;
end)

module _ = Bytes(struct
  external get_int8x64_unaligned_prim : bytes -> int32# -> int8x64 = "%caml_bytes_getu512_indexed_by_int32#"
  let get_int8x64_unaligned b i = get_int8x64_unaligned_prim b (Stdlib_upstream_compatible.Int32_u.of_int i)
  external get_int8x64_unaligned_unsafe_prim : bytes -> int32# -> int8x64 = "%caml_bytes_getu512u_indexed_by_int32#"
  let get_int8x64_unaligned_unsafe b i = get_int8x64_unaligned_unsafe_prim b (Stdlib_upstream_compatible.Int32_u.of_int i)

  external set_int8x64_unaligned_prim : bytes -> int32# -> int8x64 -> unit = "%caml_bytes_setu512_indexed_by_int32#"
  let set_int8x64_unaligned b i v = set_int8x64_unaligned_prim b (Stdlib_upstream_compatible.Int32_u.of_int i) v
  external set_int8x64_unaligned_unsafe_prim : bytes -> int32# -> int8x64 -> unit = "%caml_bytes_setu512u_indexed_by_int32#"
  let set_int8x64_unaligned_unsafe b i v = set_int8x64_unaligned_unsafe_prim b (Stdlib_upstream_compatible.Int32_u.of_int i) v

  let extra_checks bytes =
    List.iter
      (fun index ->
        let index = Stdlib_upstream_compatible.Int32_u.of_int32 index in
        assert_raises_out_of_bounds (fun () ->
          let _ = get_int8x64_unaligned_prim bytes index in
          ());
        assert_raises_out_of_bounds (fun () ->
          set_int8x64_unaligned_prim bytes index (int8x64_of_int64s 1L 2L 3L 4L 5L 6L 7L 8L)))
      Int32.[ min_int; add min_int one; sub zero one; max_int ]
  ;;
end)

module _ = Bytes(struct
  external get_int8x64_unaligned_prim : bytes -> int64# -> int8x64 = "%caml_bytes_getu512_indexed_by_int64#"
  let get_int8x64_unaligned b i = get_int8x64_unaligned_prim b (Stdlib_upstream_compatible.Int64_u.of_int i)
  external get_int8x64_unaligned_unsafe_prim : bytes -> int64# -> int8x64 = "%caml_bytes_getu512u_indexed_by_int64#"
  let get_int8x64_unaligned_unsafe b i = get_int8x64_unaligned_unsafe_prim b (Stdlib_upstream_compatible.Int64_u.of_int i)

  external set_int8x64_unaligned_prim : bytes -> int64# -> int8x64 -> unit = "%caml_bytes_setu512_indexed_by_int64#"
  let set_int8x64_unaligned b i v = set_int8x64_unaligned_prim b (Stdlib_upstream_compatible.Int64_u.of_int i) v
  external set_int8x64_unaligned_unsafe_prim : bytes -> int64# -> int8x64 -> unit = "%caml_bytes_setu512u_indexed_by_int64#"
  let set_int8x64_unaligned_unsafe b i v = set_int8x64_unaligned_unsafe_prim b (Stdlib_upstream_compatible.Int64_u.of_int i) v

  let extra_checks bytes =
    List.iter
      (fun index ->
        let index = Stdlib_upstream_compatible.Int64_u.of_int64 index in
        assert_raises_out_of_bounds (fun () ->
          let _ = get_int8x64_unaligned_prim bytes index in
          ());
        assert_raises_out_of_bounds (fun () ->
          set_int8x64_unaligned_prim bytes index (int8x64_of_int64s 1L 2L 3L 4L 5L 6L 7L 8L)))
      Int64.[ min_int; add min_int one; sub zero one; max_int ]
  ;;
end)

module _ = Bytes(struct
  external get_int8x64_unaligned_prim : bytes -> nativeint# -> int8x64 = "%caml_bytes_getu512_indexed_by_nativeint#"
  let get_int8x64_unaligned b i = get_int8x64_unaligned_prim b (Stdlib_upstream_compatible.Nativeint_u.of_int i)
  external get_int8x64_unaligned_unsafe_prim : bytes -> nativeint# -> int8x64 = "%caml_bytes_getu512u_indexed_by_nativeint#"
  let get_int8x64_unaligned_unsafe b i = get_int8x64_unaligned_unsafe_prim b (Stdlib_upstream_compatible.Nativeint_u.of_int i)

  external set_int8x64_unaligned_prim : bytes -> nativeint# -> int8x64 -> unit = "%caml_bytes_setu512_indexed_by_nativeint#"
  let set_int8x64_unaligned b i v = set_int8x64_unaligned_prim b (Stdlib_upstream_compatible.Nativeint_u.of_int i) v
  external set_int8x64_unaligned_unsafe_prim : bytes -> nativeint# -> int8x64 -> unit = "%caml_bytes_setu512u_indexed_by_nativeint#"
  let set_int8x64_unaligned_unsafe b i v = set_int8x64_unaligned_unsafe_prim b (Stdlib_upstream_compatible.Nativeint_u.of_int i) v

  let extra_checks bytes =
    List.iter
      (fun index ->
        let index = Stdlib_upstream_compatible.Nativeint_u.of_nativeint index in
        assert_raises_out_of_bounds (fun () ->
          let _ = get_int8x64_unaligned_prim bytes index in
          ());
        assert_raises_out_of_bounds (fun () ->
          set_int8x64_unaligned_prim bytes index (int8x64_of_int64s 1L 2L 3L 4L 5L 6L 7L 8L)))
      Nativeint.[ min_int; add min_int one; sub zero one; max_int ]
  ;;
end)

module String_ (Primitives : sig
  val get_int8x64_unaligned : string -> int -> int8x64
  val get_int8x64_unaligned_unsafe : string -> int -> int8x64
  val extra_checks : string -> unit
end) =
struct
  open Primitives

  let data = test_data

  let first = 0x0706050403020100L
  let second = 0x0f0e0d0c0b0a0908L
  let third = 0x1716151413121110L
  let fourth = 0x1f1e1d1c1b1a1918L
  let fifth = 0x2726252423222120L
  let sixth = 0x2f2e2d2c2b2a2928L
  let seventh = 0x3736353433323130L
  let eighth = 0x3f3e3d3c3b3a3938L

  (* Getters *)

  let () =
    let v = get_int8x64_unaligned data 0 in
    eq first second third fourth fifth sixth seventh eighth (int8x64_first_int64 v) (int8x64_second_int64 v) (int8x64_third_int64 v) (int8x64_fourth_int64 v) (int8x64_fifth_int64 v) (int8x64_sixth_int64 v) (int8x64_seventh_int64 v) (int8x64_eighth_int64 v);
    let v = get_int8x64_unaligned_unsafe data 0 in
    eq first second third fourth fifth sixth seventh eighth (int8x64_first_int64 v) (int8x64_second_int64 v) (int8x64_third_int64 v) (int8x64_fourth_int64 v) (int8x64_fifth_int64 v) (int8x64_sixth_int64 v) (int8x64_seventh_int64 v) (int8x64_eighth_int64 v);
    let v = get_int8x64_unaligned data 8 in
    eq second third fourth fifth sixth seventh eighth first (int8x64_first_int64 v) (int8x64_second_int64 v) (int8x64_third_int64 v) (int8x64_fourth_int64 v) (int8x64_fifth_int64 v) (int8x64_sixth_int64 v) (int8x64_seventh_int64 v) (int8x64_eighth_int64 v);
    let v = get_int8x64_unaligned_unsafe data 8 in
    eq second third fourth fifth sixth seventh eighth first (int8x64_first_int64 v) (int8x64_second_int64 v) (int8x64_third_int64 v) (int8x64_fourth_int64 v) (int8x64_fifth_int64 v) (int8x64_sixth_int64 v) (int8x64_seventh_int64 v) (int8x64_eighth_int64 v);
  ;;

  let () =
    for bad = 9 to 72 do
      try
        let _ = get_int8x64_unaligned data bad in
        assert false
      with | Invalid_argument s when s = "index out of bounds" -> ()
    done;
  ;;

  (* Extra checks *)

  let () = extra_checks data
end

module _ = String_(struct
  external get_int8x64_unaligned : string -> int -> int8x64 = "%caml_string_getu512"
  external get_int8x64_unaligned_unsafe : string -> int -> int8x64 = "%caml_string_getu512u"

  let extra_checks string =
    List.iter
      (fun index ->
        assert_raises_out_of_bounds (fun () ->
          let _ = get_int8x64_unaligned string index in
          ()))
      Int.[ min_int; add min_int one; sub zero one; max_int ]
  ;;
end)

module _ = String_(struct
  external get_int8x64_unaligned_prim : string -> int8# -> int8x64 = "%caml_string_getu512_indexed_by_int8#"
  let get_int8x64_unaligned b i = get_int8x64_unaligned_prim b (Stdlib_stable.Int8_u.of_int i)
  external get_int8x64_unaligned_unsafe_prim : string -> int8# -> int8x64 = "%caml_string_getu512u_indexed_by_int8#"
  let get_int8x64_unaligned_unsafe b i = get_int8x64_unaligned_unsafe_prim b (Stdlib_stable.Int8_u.of_int i)

  let extra_checks string =
    List.iter
      (fun index ->
        let index = Stdlib_stable.Int8_u.of_int8 index in
        assert_raises_out_of_bounds (fun () ->
          let _ = get_int8x64_unaligned_prim string index in
          ()))
      Stdlib_stable.Int8.[ min_int; add min_int one; sub zero one; max_int ]
  ;;
end)

module _ = String_(struct
  external get_int8x64_unaligned_prim : string -> int16# -> int8x64 = "%caml_string_getu512_indexed_by_int16#"
  let get_int8x64_unaligned b i = get_int8x64_unaligned_prim b (Stdlib_stable.Int16_u.of_int i)
  external get_int8x64_unaligned_unsafe_prim : string -> int16# -> int8x64 = "%caml_string_getu512u_indexed_by_int16#"
  let get_int8x64_unaligned_unsafe b i = get_int8x64_unaligned_unsafe_prim b (Stdlib_stable.Int16_u.of_int i)

  let extra_checks string =
    List.iter
      (fun index ->
        let index = Stdlib_stable.Int16_u.of_int16 index in
        assert_raises_out_of_bounds (fun () ->
          let _ = get_int8x64_unaligned_prim string index in
          ()))
      Stdlib_stable.Int16.[ min_int; add min_int one; sub zero one; max_int ]
  ;;
end)

module _ = String_(struct
  external get_int8x64_unaligned_prim : string -> int32# -> int8x64 = "%caml_string_getu512_indexed_by_int32#"
  let get_int8x64_unaligned b i = get_int8x64_unaligned_prim b (Stdlib_upstream_compatible.Int32_u.of_int i)
  external get_int8x64_unaligned_unsafe_prim : string -> int32# -> int8x64 = "%caml_string_getu512u_indexed_by_int32#"
  let get_int8x64_unaligned_unsafe b i = get_int8x64_unaligned_unsafe_prim b (Stdlib_upstream_compatible.Int32_u.of_int i)

  let extra_checks string =
    List.iter
      (fun index ->
        let index = Stdlib_upstream_compatible.Int32_u.of_int32 index in
        assert_raises_out_of_bounds (fun () ->
          let _ = get_int8x64_unaligned_prim string index in
          ()))
      Int32.[ min_int; add min_int one; sub zero one; max_int ]
  ;;
end)

module _ = String_(struct
  external get_int8x64_unaligned_prim : string -> int64# -> int8x64 = "%caml_string_getu512_indexed_by_int64#"
  let get_int8x64_unaligned b i = get_int8x64_unaligned_prim b (Stdlib_upstream_compatible.Int64_u.of_int i)
  external get_int8x64_unaligned_unsafe_prim : string -> int64# -> int8x64 = "%caml_string_getu512u_indexed_by_int64#"
  let get_int8x64_unaligned_unsafe b i = get_int8x64_unaligned_unsafe_prim b (Stdlib_upstream_compatible.Int64_u.of_int i)

  let extra_checks string =
    List.iter
      (fun index ->
        let index = Stdlib_upstream_compatible.Int64_u.of_int64 index in
        assert_raises_out_of_bounds (fun () ->
          let _ = get_int8x64_unaligned_prim string index in
          ()))
      Int64.[ min_int; add min_int one; sub zero one; max_int ]
  ;;
end)

module _ = String_(struct
  external get_int8x64_unaligned_prim : string -> nativeint# -> int8x64 = "%caml_string_getu512_indexed_by_nativeint#"
  let get_int8x64_unaligned b i = get_int8x64_unaligned_prim b (Stdlib_upstream_compatible.Nativeint_u.of_int i)
  external get_int8x64_unaligned_unsafe_prim : string -> nativeint# -> int8x64 = "%caml_string_getu512u_indexed_by_nativeint#"
  let get_int8x64_unaligned_unsafe b i = get_int8x64_unaligned_unsafe_prim b (Stdlib_upstream_compatible.Nativeint_u.of_int i)

  let extra_checks string =
    List.iter
      (fun index ->
        let index = Stdlib_upstream_compatible.Nativeint_u.of_nativeint index in
        assert_raises_out_of_bounds (fun () ->
          let _ = get_int8x64_unaligned_prim string index in
          ()))
      Nativeint.[ min_int; add min_int one; sub zero one; max_int ]
  ;;
end)

open struct
  open Bigarray
  type bigstring = (char, int8_unsigned_elt, c_layout) Array1.t

  module Bigstring (Primitives : sig
    val get_int8x64_unaligned : bigstring -> int -> int8x64
    val get_int8x64_unaligned_unsafe : bigstring -> int -> int8x64
    val get_int8x64_aligned : bigstring -> int -> int8x64
    val get_int8x64_aligned_unsafe : bigstring -> int -> int8x64

    val set_int8x64_unaligned : bigstring -> int -> int8x64 -> unit
    val set_int8x64_unaligned_unsafe : bigstring -> int -> int8x64 -> unit
    val set_int8x64_aligned : bigstring -> int -> int8x64 -> unit
    val set_int8x64_aligned_unsafe : bigstring -> int -> int8x64 -> unit

    val extra_checks : bigstring -> unit
  end) =
  struct
    open Primitives

    let bigstring_of_string s =
      let a = Array1.create char c_layout (String.length s + 64) in
      let rec find zero =
        if zero > 64 then failwith "no 64-byte-aligned offset found"
        else
          match get_int8x64_aligned a zero with
          | _ -> zero
          | exception _ -> find (zero + 16)
      in
      let zero = find 0 in
      for i = 0 to String.length s - 1 do
        a.{i + zero} <- s.[i]
      done;
      Array1.sub a zero (String.length s)

    (* Data is allocated off-heap, and will always be 16-byte aligned. *)
    let data = bigstring_of_string test_data

    let first = 0x0706050403020100L
    let second = 0x0f0e0d0c0b0a0908L
    let third = 0x1716151413121110L
    let fourth = 0x1f1e1d1c1b1a1918L
    let fifth = 0x2726252423222120L
    let sixth = 0x2f2e2d2c2b2a2928L
    let seventh = 0x3736353433323130L
    let eighth = 0x3f3e3d3c3b3a3938L

    (* Getters *)

    let () =
      let v = get_int8x64_unaligned data 0 in
      eq first second third fourth fifth sixth seventh eighth (int8x64_first_int64 v) (int8x64_second_int64 v) (int8x64_third_int64 v) (int8x64_fourth_int64 v) (int8x64_fifth_int64 v) (int8x64_sixth_int64 v) (int8x64_seventh_int64 v) (int8x64_eighth_int64 v);
      let v = get_int8x64_unaligned_unsafe data 0 in
      eq first second third fourth fifth sixth seventh eighth (int8x64_first_int64 v) (int8x64_second_int64 v) (int8x64_third_int64 v) (int8x64_fourth_int64 v) (int8x64_fifth_int64 v) (int8x64_sixth_int64 v) (int8x64_seventh_int64 v) (int8x64_eighth_int64 v);
      let v = get_int8x64_unaligned data 8 in
      eq second third fourth fifth sixth seventh eighth first (int8x64_first_int64 v) (int8x64_second_int64 v) (int8x64_third_int64 v) (int8x64_fourth_int64 v) (int8x64_fifth_int64 v) (int8x64_sixth_int64 v) (int8x64_seventh_int64 v) (int8x64_eighth_int64 v);
      let v = get_int8x64_unaligned_unsafe data 8 in
      eq second third fourth fifth sixth seventh eighth first (int8x64_first_int64 v) (int8x64_second_int64 v) (int8x64_third_int64 v) (int8x64_fourth_int64 v) (int8x64_fifth_int64 v) (int8x64_sixth_int64 v) (int8x64_seventh_int64 v) (int8x64_eighth_int64 v);
    ;;

    let () =
      for bad = 9 to 72 do
        try
          let _ = get_int8x64_unaligned data bad in
          assert false
        with | Invalid_argument s when s = "index out of bounds" -> ()
      done;
    ;;

    let () =
      let v = get_int8x64_aligned data 0 in
      eq first second third fourth fifth sixth seventh eighth (int8x64_first_int64 v) (int8x64_second_int64 v) (int8x64_third_int64 v) (int8x64_fourth_int64 v) (int8x64_fifth_int64 v) (int8x64_sixth_int64 v) (int8x64_seventh_int64 v) (int8x64_eighth_int64 v);
      let v = get_int8x64_aligned_unsafe data 0 in
      eq first second third fourth fifth sixth seventh eighth (int8x64_first_int64 v) (int8x64_second_int64 v) (int8x64_third_int64 v) (int8x64_fourth_int64 v) (int8x64_fifth_int64 v) (int8x64_sixth_int64 v) (int8x64_seventh_int64 v) (int8x64_eighth_int64 v);
      for bad = 1 to 8 do
        try
          let _ = get_int8x64_aligned data bad in
          assert false
        with | Invalid_argument s when s = "address was misaligned" -> ()
      done;
      for bad = 9 to 72 do
        try
          let _ = get_int8x64_aligned data bad in
          assert false
        with | Invalid_argument s when s = "index out of bounds" -> ()
      done;
    ;;

    (* Setters *)

    let set_unaligned first second third fourth fifth sixth seventh eighth offset =
      let set = int8x64_of_int64s first second third fourth fifth sixth seventh eighth in
      set_int8x64_unaligned data offset set;
      let v = get_int8x64_unaligned data offset in
      eq first second third fourth fifth sixth seventh eighth (int8x64_first_int64 v) (int8x64_second_int64 v) (int8x64_third_int64 v) (int8x64_fourth_int64 v) (int8x64_fifth_int64 v) (int8x64_sixth_int64 v) (int8x64_seventh_int64 v) (int8x64_eighth_int64 v);
    ;;

    let set_unaligned_unsafe first second third fourth fifth sixth seventh eighth offset =
      let set = int8x64_of_int64s first second third fourth fifth sixth seventh eighth in
      set_int8x64_unaligned_unsafe data offset set;
      let v = get_int8x64_unaligned data offset in
      eq first second third fourth fifth sixth seventh eighth (int8x64_first_int64 v) (int8x64_second_int64 v) (int8x64_third_int64 v) (int8x64_fourth_int64 v) (int8x64_fifth_int64 v) (int8x64_sixth_int64 v) (int8x64_seventh_int64 v) (int8x64_eighth_int64 v);
    ;;

    let set_aligned first second third fourth fifth sixth seventh eighth offset =
      let set = int8x64_of_int64s first second third fourth fifth sixth seventh eighth in
      set_int8x64_aligned data offset set;
      let v = get_int8x64_aligned data offset in
      eq first second third fourth fifth sixth seventh eighth (int8x64_first_int64 v) (int8x64_second_int64 v) (int8x64_third_int64 v) (int8x64_fourth_int64 v) (int8x64_fifth_int64 v) (int8x64_sixth_int64 v) (int8x64_seventh_int64 v) (int8x64_eighth_int64 v);
    ;;

    let set_aligned_unsafe first second third fourth fifth sixth seventh eighth offset =
      let set = int8x64_of_int64s first second third fourth fifth sixth seventh eighth in
      set_int8x64_aligned_unsafe data offset set;
      let v = get_int8x64_aligned_unsafe data offset in
      eq first second third fourth fifth sixth seventh eighth (int8x64_first_int64 v) (int8x64_second_int64 v) (int8x64_third_int64 v) (int8x64_fourth_int64 v) (int8x64_fifth_int64 v) (int8x64_sixth_int64 v) (int8x64_seventh_int64 v) (int8x64_eighth_int64 v);
    ;;

    let () =
      set_unaligned 0x1010101010101010L 0x1010101010101010L 0x1010101010101010L 0x1010101010101010L 0x1010101010101010L 0x1010101010101010L 0x1010101010101010L 0x1010101010101010L 0;
      set_unaligned 0x2020202020202020L 0x2020202020202020L 0x2020202020202020L 0x2020202020202020L 0x2020202020202020L 0x2020202020202020L 0x2020202020202020L 0x2020202020202020L 8;
      set_unaligned_unsafe 0x3030303030303030L 0x3030303030303030L 0x3030303030303030L 0x3030303030303030L 0x3030303030303030L 0x3030303030303030L 0x3030303030303030L 0x3030303030303030L 0;
      set_unaligned_unsafe 0x4040404040404040L 0x4040404040404040L 0x4040404040404040L 0x4040404040404040L 0x4040404040404040L 0x4040404040404040L 0x4040404040404040L 0x4040404040404040L 8;
      set_aligned 0x5050505050505050L 0x5050505050505050L 0x5050505050505050L 0x5050505050505050L 0x5050505050505050L 0x5050505050505050L 0x5050505050505050L 0x5050505050505050L 0;
      set_aligned_unsafe 0x6060606060606060L 0x6060606060606060L 0x6060606060606060L 0x6060606060606060L 0x6060606060606060L 0x6060606060606060L 0x6060606060606060L 0x6060606060606060L 0;
      Random.init 1234;
      for _ = 1 to 1000 do
        set_unaligned (Random.int64 Int64.max_int) (Random.int64 Int64.max_int) (Random.int64 Int64.max_int) (Random.int64 Int64.max_int) (Random.int64 Int64.max_int) (Random.int64 Int64.max_int) (Random.int64 Int64.max_int) (Random.int64 Int64.max_int) (Random.int 9);
        set_unaligned_unsafe (Random.int64 Int64.max_int) (Random.int64 Int64.max_int) (Random.int64 Int64.max_int) (Random.int64 Int64.max_int) (Random.int64 Int64.max_int) (Random.int64 Int64.max_int) (Random.int64 Int64.max_int) (Random.int64 Int64.max_int) (Random.int 9);
        set_aligned (Random.int64 Int64.max_int) (Random.int64 Int64.max_int) (Random.int64 Int64.max_int) (Random.int64 Int64.max_int) (Random.int64 Int64.max_int) (Random.int64 Int64.max_int) (Random.int64 Int64.max_int) (Random.int64 Int64.max_int) 0;
        set_aligned_unsafe (Random.int64 Int64.max_int) (Random.int64 Int64.max_int) (Random.int64 Int64.max_int) (Random.int64 Int64.max_int) (Random.int64 Int64.max_int) (Random.int64 Int64.max_int) (Random.int64 Int64.max_int) (Random.int64 Int64.max_int) 0;
      done;
    ;;

    let () =
      let set = int8x64_of_int64s 0xFFFFFFFFFFFFFFFFL 0xFFFFFFFFFFFFFFFFL 0xFFFFFFFFFFFFFFFFL 0xFFFFFFFFFFFFFFFFL 0xFFFFFFFFFFFFFFFFL 0xFFFFFFFFFFFFFFFFL 0xFFFFFFFFFFFFFFFFL 0xFFFFFFFFFFFFFFFFL in
      for bad = 1 to 8 do
        try
          let _ = set_int8x64_aligned data bad set in
          assert false
        with | Invalid_argument s when s = "address was misaligned" -> ()
      done;
      for bad = 9 to 72 do
        try
          let _ = get_int8x64_aligned data bad in
          assert false
        with | Invalid_argument s when s = "index out of bounds" -> ()
      done;
    ;;

    (* Extra checks *)

    let () = extra_checks data
  end

  module _ = Bigstring(struct
    external get_int8x64_unaligned : bigstring -> int -> int8x64 = "%caml_bigstring_getu512"
    external get_int8x64_unaligned_unsafe : bigstring -> int -> int8x64 = "%caml_bigstring_getu512u"
    external get_int8x64_aligned : bigstring -> int -> int8x64 = "%caml_bigstring_geta512"
    external get_int8x64_aligned_unsafe : bigstring -> int -> int8x64 = "%caml_bigstring_geta512u"

    external set_int8x64_unaligned : bigstring -> int -> int8x64 -> unit = "%caml_bigstring_setu512"
    external set_int8x64_unaligned_unsafe : bigstring -> int -> int8x64 -> unit = "%caml_bigstring_setu512u"
    external set_int8x64_aligned : bigstring -> int -> int8x64 -> unit = "%caml_bigstring_seta512"
    external set_int8x64_aligned_unsafe : bigstring -> int -> int8x64 -> unit = "%caml_bigstring_seta512u"

    let extra_checks bigstring =
      List.iter
        (fun index ->
          assert_raises_out_of_bounds (fun () ->
            let _ = get_int8x64_unaligned bigstring index in
            ());
          assert_raises_out_of_bounds (fun () ->
            set_int8x64_unaligned bigstring index (int8x64_of_int64s 1L 2L 3L 4L 5L 6L 7L 8L));
          assert_raises_out_of_bounds (fun () ->
            let _ = get_int8x64_aligned bigstring index in
            ());
          assert_raises_out_of_bounds (fun () ->
            set_int8x64_aligned bigstring index (int8x64_of_int64s 1L 2L 3L 4L 5L 6L 7L 8L)))
        Int.[ min_int; add min_int one; sub zero one; max_int ]
    ;;
  end)

  module _ = Bigstring(struct
    external get_int8x64_unaligned_prim : bigstring -> int8# -> int8x64 = "%caml_bigstring_getu512_indexed_by_int8#"
    let get_int8x64_unaligned b i = get_int8x64_unaligned_prim b (Stdlib_stable.Int8_u.of_int i)
    external get_int8x64_unaligned_unsafe_prim : bigstring -> int8# -> int8x64 = "%caml_bigstring_getu512u_indexed_by_int8#"
    let get_int8x64_unaligned_unsafe b i = get_int8x64_unaligned_unsafe_prim b (Stdlib_stable.Int8_u.of_int i)
    external get_int8x64_aligned_prim : bigstring -> int8# -> int8x64 = "%caml_bigstring_geta512_indexed_by_int8#"
    let get_int8x64_aligned b i = get_int8x64_aligned_prim b (Stdlib_stable.Int8_u.of_int i)
    external get_int8x64_aligned_unsafe_prim : bigstring -> int8# -> int8x64 = "%caml_bigstring_geta512u_indexed_by_int8#"
    let get_int8x64_aligned_unsafe b i = get_int8x64_aligned_unsafe_prim b (Stdlib_stable.Int8_u.of_int i)

    external set_int8x64_unaligned_prim : bigstring -> int8# -> int8x64 -> unit = "%caml_bigstring_setu512_indexed_by_int8#"
    let set_int8x64_unaligned b i v = set_int8x64_unaligned_prim b (Stdlib_stable.Int8_u.of_int i) v
    external set_int8x64_unaligned_unsafe_prim : bigstring -> int8# -> int8x64 -> unit = "%caml_bigstring_setu512u_indexed_by_int8#"
    let set_int8x64_unaligned_unsafe b i v = set_int8x64_unaligned_unsafe_prim b (Stdlib_stable.Int8_u.of_int i) v
    external set_int8x64_aligned_prim : bigstring -> int8# -> int8x64 -> unit = "%caml_bigstring_seta512_indexed_by_int8#"
    let set_int8x64_aligned b i v = set_int8x64_aligned_prim b (Stdlib_stable.Int8_u.of_int i) v
    external set_int8x64_aligned_unsafe_prim : bigstring -> int8# -> int8x64 -> unit = "%caml_bigstring_seta512u_indexed_by_int8#"
    let set_int8x64_aligned_unsafe b i v = set_int8x64_aligned_unsafe_prim b (Stdlib_stable.Int8_u.of_int i) v

    let extra_checks bigstring =
      List.iter
        (fun index ->
          let index = Stdlib_stable.Int8_u.of_int8 index in
          assert_raises_out_of_bounds (fun () ->
            let _ = get_int8x64_unaligned_prim bigstring index in
            ());
          assert_raises_out_of_bounds (fun () ->
            set_int8x64_unaligned_prim bigstring index (int8x64_of_int64s 1L 2L 3L 4L 5L 6L 7L 8L));
          assert_raises_out_of_bounds (fun () ->
            let _ = get_int8x64_aligned_prim bigstring index in
            ());
          assert_raises_out_of_bounds (fun () ->
            set_int8x64_aligned_prim bigstring index (int8x64_of_int64s 1L 2L 3L 4L 5L 6L 7L 8L)))
        Int8.[ min_int; add min_int one; sub zero one; max_int ]
    ;;
  end)

  module _ = Bigstring(struct
    external get_int8x64_unaligned_prim : bigstring -> int16# -> int8x64 = "%caml_bigstring_getu512_indexed_by_int16#"
    let get_int8x64_unaligned b i = get_int8x64_unaligned_prim b (Stdlib_stable.Int16_u.of_int i)
    external get_int8x64_unaligned_unsafe_prim : bigstring -> int16# -> int8x64 = "%caml_bigstring_getu512u_indexed_by_int16#"
    let get_int8x64_unaligned_unsafe b i = get_int8x64_unaligned_unsafe_prim b (Stdlib_stable.Int16_u.of_int i)
    external get_int8x64_aligned_prim : bigstring -> int16# -> int8x64 = "%caml_bigstring_geta512_indexed_by_int16#"
    let get_int8x64_aligned b i = get_int8x64_aligned_prim b (Stdlib_stable.Int16_u.of_int i)
    external get_int8x64_aligned_unsafe_prim : bigstring -> int16# -> int8x64 = "%caml_bigstring_geta512u_indexed_by_int16#"
    let get_int8x64_aligned_unsafe b i = get_int8x64_aligned_unsafe_prim b (Stdlib_stable.Int16_u.of_int i)

    external set_int8x64_unaligned_prim : bigstring -> int16# -> int8x64 -> unit = "%caml_bigstring_setu512_indexed_by_int16#"
    let set_int8x64_unaligned b i v = set_int8x64_unaligned_prim b (Stdlib_stable.Int16_u.of_int i) v
    external set_int8x64_unaligned_unsafe_prim : bigstring -> int16# -> int8x64 -> unit = "%caml_bigstring_setu512u_indexed_by_int16#"
    let set_int8x64_unaligned_unsafe b i v = set_int8x64_unaligned_unsafe_prim b (Stdlib_stable.Int16_u.of_int i) v
    external set_int8x64_aligned_prim : bigstring -> int16# -> int8x64 -> unit = "%caml_bigstring_seta512_indexed_by_int16#"
    let set_int8x64_aligned b i v = set_int8x64_aligned_prim b (Stdlib_stable.Int16_u.of_int i) v
    external set_int8x64_aligned_unsafe_prim : bigstring -> int16# -> int8x64 -> unit = "%caml_bigstring_seta512u_indexed_by_int16#"
    let set_int8x64_aligned_unsafe b i v = set_int8x64_aligned_unsafe_prim b (Stdlib_stable.Int16_u.of_int i) v

    let extra_checks bigstring =
      List.iter
        (fun index ->
          let index = Stdlib_stable.Int16_u.of_int16 index in
          assert_raises_out_of_bounds (fun () ->
            let _ = get_int8x64_unaligned_prim bigstring index in
            ());
          assert_raises_out_of_bounds (fun () ->
            set_int8x64_unaligned_prim bigstring index (int8x64_of_int64s 1L 2L 3L 4L 5L 6L 7L 8L));
          assert_raises_out_of_bounds (fun () ->
            let _ = get_int8x64_aligned_prim bigstring index in
            ());
          assert_raises_out_of_bounds (fun () ->
            set_int8x64_aligned_prim bigstring index (int8x64_of_int64s 1L 2L 3L 4L 5L 6L 7L 8L)))
        Int16.[ min_int; add min_int one; sub zero one; max_int ]
    ;;
  end)

  module _ = Bigstring(struct
    external get_int8x64_unaligned_prim : bigstring -> int32# -> int8x64 = "%caml_bigstring_getu512_indexed_by_int32#"
    let get_int8x64_unaligned b i = get_int8x64_unaligned_prim b (Stdlib_upstream_compatible.Int32_u.of_int i)
    external get_int8x64_unaligned_unsafe_prim : bigstring -> int32# -> int8x64 = "%caml_bigstring_getu512u_indexed_by_int32#"
    let get_int8x64_unaligned_unsafe b i = get_int8x64_unaligned_unsafe_prim b (Stdlib_upstream_compatible.Int32_u.of_int i)
    external get_int8x64_aligned_prim : bigstring -> int32# -> int8x64 = "%caml_bigstring_geta512_indexed_by_int32#"
    let get_int8x64_aligned b i = get_int8x64_aligned_prim b (Stdlib_upstream_compatible.Int32_u.of_int i)
    external get_int8x64_aligned_unsafe_prim : bigstring -> int32# -> int8x64 = "%caml_bigstring_geta512u_indexed_by_int32#"
    let get_int8x64_aligned_unsafe b i = get_int8x64_aligned_unsafe_prim b (Stdlib_upstream_compatible.Int32_u.of_int i)

    external set_int8x64_unaligned_prim : bigstring -> int32# -> int8x64 -> unit = "%caml_bigstring_setu512_indexed_by_int32#"
    let set_int8x64_unaligned b i v = set_int8x64_unaligned_prim b (Stdlib_upstream_compatible.Int32_u.of_int i) v
    external set_int8x64_unaligned_unsafe_prim : bigstring -> int32# -> int8x64 -> unit = "%caml_bigstring_setu512u_indexed_by_int32#"
    let set_int8x64_unaligned_unsafe b i v = set_int8x64_unaligned_unsafe_prim b (Stdlib_upstream_compatible.Int32_u.of_int i) v
    external set_int8x64_aligned_prim : bigstring -> int32# -> int8x64 -> unit = "%caml_bigstring_seta512_indexed_by_int32#"
    let set_int8x64_aligned b i v = set_int8x64_aligned_prim b (Stdlib_upstream_compatible.Int32_u.of_int i) v
    external set_int8x64_aligned_unsafe_prim : bigstring -> int32# -> int8x64 -> unit = "%caml_bigstring_seta512u_indexed_by_int32#"
    let set_int8x64_aligned_unsafe b i v = set_int8x64_aligned_unsafe_prim b (Stdlib_upstream_compatible.Int32_u.of_int i) v

    let extra_checks bigstring =
      List.iter
        (fun index ->
          let index = Stdlib_upstream_compatible.Int32_u.of_int32 index in
          assert_raises_out_of_bounds (fun () ->
            let _ = get_int8x64_unaligned_prim bigstring index in
            ());
          assert_raises_out_of_bounds (fun () ->
            set_int8x64_unaligned_prim bigstring index (int8x64_of_int64s 1L 2L 3L 4L 5L 6L 7L 8L));
          assert_raises_out_of_bounds (fun () ->
            let _ = get_int8x64_aligned_prim bigstring index in
            ());
          assert_raises_out_of_bounds (fun () ->
            set_int8x64_aligned_prim bigstring index (int8x64_of_int64s 1L 2L 3L 4L 5L 6L 7L 8L)))
        Int32.[ min_int; add min_int one; sub zero one; max_int ]
    ;;
  end)

  module _ = Bigstring(struct
    external get_int8x64_unaligned_prim : bigstring -> int64# -> int8x64 = "%caml_bigstring_getu512_indexed_by_int64#"
    let get_int8x64_unaligned b i = get_int8x64_unaligned_prim b (Stdlib_upstream_compatible.Int64_u.of_int i)
    external get_int8x64_unaligned_unsafe_prim : bigstring -> int64# -> int8x64 = "%caml_bigstring_getu512u_indexed_by_int64#"
    let get_int8x64_unaligned_unsafe b i = get_int8x64_unaligned_unsafe_prim b (Stdlib_upstream_compatible.Int64_u.of_int i)
    external get_int8x64_aligned_prim : bigstring -> int64# -> int8x64 = "%caml_bigstring_geta512_indexed_by_int64#"
    let get_int8x64_aligned b i = get_int8x64_aligned_prim b (Stdlib_upstream_compatible.Int64_u.of_int i)
    external get_int8x64_aligned_unsafe_prim : bigstring -> int64# -> int8x64 = "%caml_bigstring_geta512u_indexed_by_int64#"
    let get_int8x64_aligned_unsafe b i = get_int8x64_aligned_unsafe_prim b (Stdlib_upstream_compatible.Int64_u.of_int i)

    external set_int8x64_unaligned_prim : bigstring -> int64# -> int8x64 -> unit = "%caml_bigstring_setu512_indexed_by_int64#"
    let set_int8x64_unaligned b i v = set_int8x64_unaligned_prim b (Stdlib_upstream_compatible.Int64_u.of_int i) v
    external set_int8x64_unaligned_unsafe_prim : bigstring -> int64# -> int8x64 -> unit = "%caml_bigstring_setu512u_indexed_by_int64#"
    let set_int8x64_unaligned_unsafe b i v = set_int8x64_unaligned_unsafe_prim b (Stdlib_upstream_compatible.Int64_u.of_int i) v
    external set_int8x64_aligned_prim : bigstring -> int64# -> int8x64 -> unit = "%caml_bigstring_seta512_indexed_by_int64#"
    let set_int8x64_aligned b i v = set_int8x64_aligned_prim b (Stdlib_upstream_compatible.Int64_u.of_int i) v
    external set_int8x64_aligned_unsafe_prim : bigstring -> int64# -> int8x64 -> unit = "%caml_bigstring_seta512u_indexed_by_int64#"
    let set_int8x64_aligned_unsafe b i v = set_int8x64_aligned_unsafe_prim b (Stdlib_upstream_compatible.Int64_u.of_int i) v

    let extra_checks bigstring =
      List.iter
        (fun index ->
          let index = Stdlib_upstream_compatible.Int64_u.of_int64 index in
          assert_raises_out_of_bounds (fun () ->
            let _ = get_int8x64_unaligned_prim bigstring index in
            ());
          assert_raises_out_of_bounds (fun () ->
            set_int8x64_unaligned_prim bigstring index (int8x64_of_int64s 1L 2L 3L 4L 5L 6L 7L 8L));
          assert_raises_out_of_bounds (fun () ->
            let _ = get_int8x64_aligned_prim bigstring index in
            ());
          assert_raises_out_of_bounds (fun () ->
            set_int8x64_aligned_prim bigstring index (int8x64_of_int64s 1L 2L 3L 4L 5L 6L 7L 8L)))
        Int64.[ min_int; add min_int one; sub zero one; max_int ]
    ;;
  end)

  module _ = Bigstring(struct
    external get_int8x64_unaligned_prim : bigstring -> nativeint# -> int8x64 = "%caml_bigstring_getu512_indexed_by_nativeint#"
    let get_int8x64_unaligned b i = get_int8x64_unaligned_prim b (Stdlib_upstream_compatible.Nativeint_u.of_int i)
    external get_int8x64_unaligned_unsafe_prim : bigstring -> nativeint# -> int8x64 = "%caml_bigstring_getu512u_indexed_by_nativeint#"
    let get_int8x64_unaligned_unsafe b i = get_int8x64_unaligned_unsafe_prim b (Stdlib_upstream_compatible.Nativeint_u.of_int i)
    external get_int8x64_aligned_prim : bigstring -> nativeint# -> int8x64 = "%caml_bigstring_geta512_indexed_by_nativeint#"
    let get_int8x64_aligned b i = get_int8x64_aligned_prim b (Stdlib_upstream_compatible.Nativeint_u.of_int i)
    external get_int8x64_aligned_unsafe_prim : bigstring -> nativeint# -> int8x64 = "%caml_bigstring_geta512u_indexed_by_nativeint#"
    let get_int8x64_aligned_unsafe b i = get_int8x64_aligned_unsafe_prim b (Stdlib_upstream_compatible.Nativeint_u.of_int i)

    external set_int8x64_unaligned_prim : bigstring -> nativeint# -> int8x64 -> unit = "%caml_bigstring_setu512_indexed_by_nativeint#"
    let set_int8x64_unaligned b i v = set_int8x64_unaligned_prim b (Stdlib_upstream_compatible.Nativeint_u.of_int i) v
    external set_int8x64_unaligned_unsafe_prim : bigstring -> nativeint# -> int8x64 -> unit = "%caml_bigstring_setu512u_indexed_by_nativeint#"
    let set_int8x64_unaligned_unsafe b i v = set_int8x64_unaligned_unsafe_prim b (Stdlib_upstream_compatible.Nativeint_u.of_int i) v
    external set_int8x64_aligned_prim : bigstring -> nativeint# -> int8x64 -> unit = "%caml_bigstring_seta512_indexed_by_nativeint#"
    let set_int8x64_aligned b i v = set_int8x64_aligned_prim b (Stdlib_upstream_compatible.Nativeint_u.of_int i) v
    external set_int8x64_aligned_unsafe_prim : bigstring -> nativeint# -> int8x64 -> unit = "%caml_bigstring_seta512u_indexed_by_nativeint#"
    let set_int8x64_aligned_unsafe b i v = set_int8x64_aligned_unsafe_prim b (Stdlib_upstream_compatible.Nativeint_u.of_int i) v

    let extra_checks bigstring =
      List.iter
        (fun index ->
          let index = Stdlib_upstream_compatible.Nativeint_u.of_nativeint index in
          assert_raises_out_of_bounds (fun () ->
            let _ = get_int8x64_unaligned_prim bigstring index in
            ());
          assert_raises_out_of_bounds (fun () ->
            set_int8x64_unaligned_prim bigstring index (int8x64_of_int64s 1L 2L 3L 4L 5L 6L 7L 8L));
          assert_raises_out_of_bounds (fun () ->
            let _ = get_int8x64_aligned_prim bigstring index in
            ());
          assert_raises_out_of_bounds (fun () ->
            set_int8x64_aligned_prim bigstring index (int8x64_of_int64s 1L 2L 3L 4L 5L 6L 7L 8L)))
        Nativeint.[ min_int; add min_int one; sub zero one; max_int ]
    ;;
  end)
end

module Float_arrays (Primitives : sig

  val floatarray_get_float64x8 : floatarray -> int -> float64x8
  val floatarray_get_float64x8_unsafe : floatarray -> int -> float64x8

  val floatarray_set_float64x8 : floatarray -> int -> float64x8 -> unit
  val floatarray_set_float64x8_unsafe : floatarray -> int -> float64x8 -> unit

  val unboxed_float_array_get_float64x8 : float# array -> int -> float64x8
  val unboxed_float_array_get_float64x8_unsafe : float# array -> int -> float64x8

  val unboxed_float_array_set_float64x8 : float# array -> int -> float64x8 -> unit
  val unboxed_float_array_set_float64x8_unsafe : float# array -> int -> float64x8 -> unit

  val unboxed_float32_array_get_float32x16 : float32# array -> int -> float32x16
  val unboxed_float32_array_get_float32x16_unsafe : float32# array -> int -> float32x16

  val unboxed_float32_array_set_float32x16 : float32# array -> int -> float32x16 -> unit
  val unboxed_float32_array_set_float32x16_unsafe : float32# array -> int -> float32x16 -> unit

end) = struct
  open Primitives

  let check64 e a =
    eq (float64x8_first_int64 e) (float64x8_second_int64 e)
       (float64x8_third_int64 e) (float64x8_fourth_int64 e)
       (float64x8_fifth_int64 e) (float64x8_sixth_int64 e)
       (float64x8_seventh_int64 e) (float64x8_eighth_int64 e)
       (float64x8_first_int64 a) (float64x8_second_int64 a)
       (float64x8_third_int64 a) (float64x8_fourth_int64 a)
       (float64x8_fifth_int64 a) (float64x8_sixth_int64 a)
       (float64x8_seventh_int64 a) (float64x8_eighth_int64 a)

  let check32 e a =
    eq (float32x16_first_int64 e) (float32x16_second_int64 e)
       (float32x16_third_int64 e) (float32x16_fourth_int64 e)
       (float32x16_fifth_int64 e) (float32x16_sixth_int64 e)
       (float32x16_seventh_int64 e) (float32x16_eighth_int64 e)
       (float32x16_first_int64 a) (float32x16_second_int64 a)
       (float32x16_third_int64 a) (float32x16_fourth_int64 a)
       (float32x16_fifth_int64 a) (float32x16_sixth_int64 a)
       (float32x16_seventh_int64 a) (float32x16_eighth_int64 a)

  let f64x8 a b c d e f g h =
    float64x8_of_int64s (Int64.bits_of_float a) (Int64.bits_of_float b)
                        (Int64.bits_of_float c) (Int64.bits_of_float d)
                        (Int64.bits_of_float e) (Int64.bits_of_float f)
                        (Int64.bits_of_float g) (Int64.bits_of_float h)

  let f32x16 a b c d e f g h i j k l m n o p =
    let pack x y =
      let x_bits = Int64.of_int32 (Float32.to_bits x) in
      let y_bits = Int64.of_int32 (Float32.to_bits y) in
      Int64.logor (Int64.logand x_bits 0xFFFFFFFFL) (Int64.shift_left y_bits 32)
    in
    float32x16_of_int64s (pack a b) (pack c d) (pack e f) (pack g h)
                         (pack i j) (pack k l) (pack m n) (pack o p)

  let floatarray () =
    let a = Array.Floatarray.create 9 in
    for i = 0 to 8 do Array.Floatarray.set a i (Float.of_int i) done;
    a
  ;;
  let unboxed_float_array () =
    [| #0.0; #1.0; #2.0; #3.0; #4.0; #5.0; #6.0; #7.0; #8.0 |]
  let unboxed_float32_array () =
    [| #0.0s; #1.0s; #2.0s; #3.0s; #4.0s; #5.0s; #6.0s; #7.0s; #8.0s;
       #9.0s; #10.0s; #11.0s; #12.0s; #13.0s; #14.0s; #15.0s; #16.0s |]

  (* floatarray *)
  let () =
    let fa = floatarray () in
    check64 (f64x8 0.0 1.0 2.0 3.0 4.0 5.0 6.0 7.0)
      (floatarray_get_float64x8_unsafe fa 0);
    check64 (f64x8 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0)
      (floatarray_get_float64x8_unsafe fa 1);
    let s_0 = f64x8 4.0 5.0 6.0 7.0 8.0 9.0 10.0 11.0 in
    let s_1 = f64x8 6.0 7.0 8.0 9.0 10.0 11.0 12.0 13.0 in
    floatarray_set_float64x8_unsafe fa 0 s_0;
    check64 s_0 (floatarray_get_float64x8_unsafe fa 0);
    floatarray_set_float64x8_unsafe fa 1 s_1;
    check64 s_1 (floatarray_get_float64x8_unsafe fa 1)
  ;;

  let () =
    let a = floatarray () in
    let f_0 = f64x8 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 in
    let fail a i =
      (try
        let _ = floatarray_get_float64x8 a i in
        Printf.printf "Did not fail on index %d\n" i
      with | Invalid_argument s when s = "index out of bounds" -> ());
      (try
        let _ = floatarray_set_float64x8 a i f_0 in
        Printf.printf "Did not fail on index %d\n" i
      with | Invalid_argument s when s = "index out of bounds" -> ())
    in
    fail a (-1);
    fail a 2;
    fail a 3;
    fail (Array.Floatarray.create 0) 0;
    let a = Array.Floatarray.create 1 in
    Array.Floatarray.set a 0 0.0;
    fail a 0;
    fail a 1;
    fail a (-1)
  ;;

  (* float# array *)
  let () =
    let a = unboxed_float_array () in
    check64 (f64x8 0.0 1.0 2.0 3.0 4.0 5.0 6.0 7.0)
      (unboxed_float_array_get_float64x8 a 0);
    check64 (f64x8 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0)
      (unboxed_float_array_get_float64x8 a 1);
    let s_0 = f64x8 4.0 5.0 6.0 7.0 8.0 9.0 10.0 11.0 in
    let s_1 = f64x8 6.0 7.0 8.0 9.0 10.0 11.0 12.0 13.0 in
    unboxed_float_array_set_float64x8 a 0 s_0;
    check64 s_0 (unboxed_float_array_get_float64x8 a 0);
    unboxed_float_array_set_float64x8 a 1 s_1;
    check64 s_1 (unboxed_float_array_get_float64x8 a 1)
  ;;

  let () =
    let a = unboxed_float_array () in
    check64 (f64x8 0.0 1.0 2.0 3.0 4.0 5.0 6.0 7.0)
      (unboxed_float_array_get_float64x8_unsafe a 0);
    let s_0 = f64x8 4.0 5.0 6.0 7.0 8.0 9.0 10.0 11.0 in
    unboxed_float_array_set_float64x8_unsafe a 0 s_0;
    check64 s_0 (unboxed_float_array_get_float64x8_unsafe a 0)
  ;;

  let () =
    let f_0 = f64x8 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 in
    let fail a i =
      try
        let _ = unboxed_float_array_get_float64x8 a i in
        let _ = unboxed_float_array_set_float64x8 a i f_0 in
        Printf.printf "Did not fail on index %d\n" i
      with | Invalid_argument s when s = "index out of bounds" -> ()
    in
    fail (unboxed_float_array ()) (-1);
    fail (unboxed_float_array ()) 2;
    fail [||] 0;
    fail [|#0.0|] 0;
    fail [|#0.0|] (-1)
  ;;

  (* float32# array *)
  let () =
    let a = unboxed_float32_array () in
    check32
      (f32x16 0.0s 1.0s 2.0s 3.0s 4.0s 5.0s 6.0s 7.0s
              8.0s 9.0s 10.0s 11.0s 12.0s 13.0s 14.0s 15.0s)
      (unboxed_float32_array_get_float32x16 a 0);
    check32
      (f32x16 1.0s 2.0s 3.0s 4.0s 5.0s 6.0s 7.0s 8.0s
              9.0s 10.0s 11.0s 12.0s 13.0s 14.0s 15.0s 16.0s)
      (unboxed_float32_array_get_float32x16 a 1);
    let s_0 =
      f32x16 16.0s 17.0s 18.0s 19.0s 20.0s 21.0s 22.0s 23.0s
             24.0s 25.0s 26.0s 27.0s 28.0s 29.0s 30.0s 31.0s
    in
    unboxed_float32_array_set_float32x16 a 0 s_0;
    check32 s_0 (unboxed_float32_array_get_float32x16 a 0)
  ;;

  let () =
    let a = unboxed_float32_array () in
    check32
      (f32x16 0.0s 1.0s 2.0s 3.0s 4.0s 5.0s 6.0s 7.0s
              8.0s 9.0s 10.0s 11.0s 12.0s 13.0s 14.0s 15.0s)
      (unboxed_float32_array_get_float32x16_unsafe a 0);
    let s_0 =
      f32x16 16.0s 17.0s 18.0s 19.0s 20.0s 21.0s 22.0s 23.0s
             24.0s 25.0s 26.0s 27.0s 28.0s 29.0s 30.0s 31.0s
    in
    unboxed_float32_array_set_float32x16_unsafe a 0 s_0;
    check32 s_0 (unboxed_float32_array_get_float32x16_unsafe a 0)
  ;;

  let () =
    let f_0 =
      f32x16 0.0s 0.0s 0.0s 0.0s 0.0s 0.0s 0.0s 0.0s
             0.0s 0.0s 0.0s 0.0s 0.0s 0.0s 0.0s 0.0s
    in
    let fail a i =
      try
        let _ = unboxed_float32_array_get_float32x16 a i in
        let _ = unboxed_float32_array_set_float32x16 a i f_0 in
        Printf.printf "Did not fail on index %d\n" i
      with | Invalid_argument s when s = "index out of bounds" -> ()
    in
    fail (unboxed_float32_array ()) (-1);
    fail (unboxed_float32_array ()) 2;
    fail [||] 0;
    fail [|#0.0s|] 0;
    fail [|#0.0s|] (-1)
  ;;
end

module _ = Float_arrays(struct

  external floatarray_get_float64x8 : floatarray -> int -> float64x8 = "%caml_floatarray_get512"
  external floatarray_get_float64x8_unsafe : floatarray -> int -> float64x8 = "%caml_floatarray_get512u"

  external floatarray_set_float64x8 : floatarray -> int -> float64x8 -> unit = "%caml_floatarray_set512"
  external floatarray_set_float64x8_unsafe : floatarray -> int -> float64x8 -> unit = "%caml_floatarray_set512u"

  external unboxed_float_array_get_float64x8 : float# array -> int -> float64x8 = "%caml_unboxed_float_array_get512"
  external unboxed_float_array_get_float64x8_unsafe : float# array -> int -> float64x8 = "%caml_unboxed_float_array_get512u"

  external unboxed_float_array_set_float64x8 : float# array -> int -> float64x8 -> unit = "%caml_unboxed_float_array_set512"
  external unboxed_float_array_set_float64x8_unsafe : float# array -> int -> float64x8 -> unit = "%caml_unboxed_float_array_set512u"

  external unboxed_float32_array_get_float32x16 : float32# array -> int -> float32x16 = "%caml_unboxed_float32_array_get512"
  external unboxed_float32_array_get_float32x16_unsafe : float32# array -> int -> float32x16 = "%caml_unboxed_float32_array_get512u"

  external unboxed_float32_array_set_float32x16 : float32# array -> int -> float32x16 -> unit = "%caml_unboxed_float32_array_set512"
  external unboxed_float32_array_set_float32x16_unsafe : float32# array -> int -> float32x16 -> unit = "%caml_unboxed_float32_array_set512u"

end)

module _ = Float_arrays(struct

  external floatarray_get_float64x8 : floatarray -> int8# -> float64x8 = "%caml_floatarray_get512_indexed_by_int8#"
  let floatarray_get_float64x8 arr i = floatarray_get_float64x8 arr (Stdlib_stable.Int8_u.of_int i)
  external floatarray_get_float64x8_unsafe : floatarray -> int8# -> float64x8 = "%caml_floatarray_get512u_indexed_by_int8#"
  let floatarray_get_float64x8_unsafe arr i = floatarray_get_float64x8_unsafe arr (Stdlib_stable.Int8_u.of_int i)

  external floatarray_set_float64x8 : floatarray -> int8# -> float64x8 -> unit = "%caml_floatarray_set512_indexed_by_int8#"
  let floatarray_set_float64x8 arr i v = floatarray_set_float64x8 arr (Stdlib_stable.Int8_u.of_int i) v
  external floatarray_set_float64x8_unsafe : floatarray -> int8# -> float64x8 -> unit = "%caml_floatarray_set512u_indexed_by_int8#"
  let floatarray_set_float64x8_unsafe arr i v = floatarray_set_float64x8_unsafe arr (Stdlib_stable.Int8_u.of_int i) v

  external unboxed_float_array_get_float64x8 : float# array -> int8# -> float64x8 = "%caml_unboxed_float_array_get512_indexed_by_int8#"
  let unboxed_float_array_get_float64x8 arr i = unboxed_float_array_get_float64x8 arr (Stdlib_stable.Int8_u.of_int i)
  external unboxed_float_array_get_float64x8_unsafe : float# array -> int8# -> float64x8 = "%caml_unboxed_float_array_get512u_indexed_by_int8#"
  let unboxed_float_array_get_float64x8_unsafe arr i = unboxed_float_array_get_float64x8_unsafe arr (Stdlib_stable.Int8_u.of_int i)

  external unboxed_float_array_set_float64x8 : float# array -> int8# -> float64x8 -> unit = "%caml_unboxed_float_array_set512_indexed_by_int8#"
  let unboxed_float_array_set_float64x8 arr i v = unboxed_float_array_set_float64x8 arr (Stdlib_stable.Int8_u.of_int i) v
  external unboxed_float_array_set_float64x8_unsafe : float# array -> int8# -> float64x8 -> unit = "%caml_unboxed_float_array_set512u_indexed_by_int8#"
  let unboxed_float_array_set_float64x8_unsafe arr i v = unboxed_float_array_set_float64x8_unsafe arr (Stdlib_stable.Int8_u.of_int i) v

  external unboxed_float32_array_get_float32x16 : float32# array -> int8# -> float32x16 = "%caml_unboxed_float32_array_get512_indexed_by_int8#"
  let unboxed_float32_array_get_float32x16 arr i = unboxed_float32_array_get_float32x16 arr (Stdlib_stable.Int8_u.of_int i)
  external unboxed_float32_array_get_float32x16_unsafe : float32# array -> int8# -> float32x16 = "%caml_unboxed_float32_array_get512u_indexed_by_int8#"
  let unboxed_float32_array_get_float32x16_unsafe arr i = unboxed_float32_array_get_float32x16_unsafe arr (Stdlib_stable.Int8_u.of_int i)

  external unboxed_float32_array_set_float32x16 : float32# array -> int8# -> float32x16 -> unit = "%caml_unboxed_float32_array_set512_indexed_by_int8#"
  let unboxed_float32_array_set_float32x16 arr i v = unboxed_float32_array_set_float32x16 arr (Stdlib_stable.Int8_u.of_int i) v
  external unboxed_float32_array_set_float32x16_unsafe : float32# array -> int8# -> float32x16 -> unit = "%caml_unboxed_float32_array_set512u_indexed_by_int8#"
  let unboxed_float32_array_set_float32x16_unsafe arr i v = unboxed_float32_array_set_float32x16_unsafe arr (Stdlib_stable.Int8_u.of_int i) v

end)

module _ = Float_arrays(struct

  external floatarray_get_float64x8 : floatarray -> int16# -> float64x8 = "%caml_floatarray_get512_indexed_by_int16#"
  let floatarray_get_float64x8 arr i = floatarray_get_float64x8 arr (Stdlib_stable.Int16_u.of_int i)
  external floatarray_get_float64x8_unsafe : floatarray -> int16# -> float64x8 = "%caml_floatarray_get512u_indexed_by_int16#"
  let floatarray_get_float64x8_unsafe arr i = floatarray_get_float64x8_unsafe arr (Stdlib_stable.Int16_u.of_int i)

  external floatarray_set_float64x8 : floatarray -> int16# -> float64x8 -> unit = "%caml_floatarray_set512_indexed_by_int16#"
  let floatarray_set_float64x8 arr i v = floatarray_set_float64x8 arr (Stdlib_stable.Int16_u.of_int i) v
  external floatarray_set_float64x8_unsafe : floatarray -> int16# -> float64x8 -> unit = "%caml_floatarray_set512u_indexed_by_int16#"
  let floatarray_set_float64x8_unsafe arr i v = floatarray_set_float64x8_unsafe arr (Stdlib_stable.Int16_u.of_int i) v

  external unboxed_float_array_get_float64x8 : float# array -> int16# -> float64x8 = "%caml_unboxed_float_array_get512_indexed_by_int16#"
  let unboxed_float_array_get_float64x8 arr i = unboxed_float_array_get_float64x8 arr (Stdlib_stable.Int16_u.of_int i)
  external unboxed_float_array_get_float64x8_unsafe : float# array -> int16# -> float64x8 = "%caml_unboxed_float_array_get512u_indexed_by_int16#"
  let unboxed_float_array_get_float64x8_unsafe arr i = unboxed_float_array_get_float64x8_unsafe arr (Stdlib_stable.Int16_u.of_int i)

  external unboxed_float_array_set_float64x8 : float# array -> int16# -> float64x8 -> unit = "%caml_unboxed_float_array_set512_indexed_by_int16#"
  let unboxed_float_array_set_float64x8 arr i v = unboxed_float_array_set_float64x8 arr (Stdlib_stable.Int16_u.of_int i) v
  external unboxed_float_array_set_float64x8_unsafe : float# array -> int16# -> float64x8 -> unit = "%caml_unboxed_float_array_set512u_indexed_by_int16#"
  let unboxed_float_array_set_float64x8_unsafe arr i v = unboxed_float_array_set_float64x8_unsafe arr (Stdlib_stable.Int16_u.of_int i) v

  external unboxed_float32_array_get_float32x16 : float32# array -> int16# -> float32x16 = "%caml_unboxed_float32_array_get512_indexed_by_int16#"
  let unboxed_float32_array_get_float32x16 arr i = unboxed_float32_array_get_float32x16 arr (Stdlib_stable.Int16_u.of_int i)
  external unboxed_float32_array_get_float32x16_unsafe : float32# array -> int16# -> float32x16 = "%caml_unboxed_float32_array_get512u_indexed_by_int16#"
  let unboxed_float32_array_get_float32x16_unsafe arr i = unboxed_float32_array_get_float32x16_unsafe arr (Stdlib_stable.Int16_u.of_int i)

  external unboxed_float32_array_set_float32x16 : float32# array -> int16# -> float32x16 -> unit = "%caml_unboxed_float32_array_set512_indexed_by_int16#"
  let unboxed_float32_array_set_float32x16 arr i v = unboxed_float32_array_set_float32x16 arr (Stdlib_stable.Int16_u.of_int i) v
  external unboxed_float32_array_set_float32x16_unsafe : float32# array -> int16# -> float32x16 -> unit = "%caml_unboxed_float32_array_set512u_indexed_by_int16#"
  let unboxed_float32_array_set_float32x16_unsafe arr i v = unboxed_float32_array_set_float32x16_unsafe arr (Stdlib_stable.Int16_u.of_int i) v

end)

module _ = Float_arrays(struct

  external floatarray_get_float64x8 : floatarray -> int32# -> float64x8 = "%caml_floatarray_get512_indexed_by_int32#"
  let floatarray_get_float64x8 arr i = floatarray_get_float64x8 arr (Stdlib_upstream_compatible.Int32_u.of_int i)
  external floatarray_get_float64x8_unsafe : floatarray -> int32# -> float64x8 = "%caml_floatarray_get512u_indexed_by_int32#"
  let floatarray_get_float64x8_unsafe arr i = floatarray_get_float64x8_unsafe arr (Stdlib_upstream_compatible.Int32_u.of_int i)

  external floatarray_set_float64x8 : floatarray -> int32# -> float64x8 -> unit = "%caml_floatarray_set512_indexed_by_int32#"
  let floatarray_set_float64x8 arr i v = floatarray_set_float64x8 arr (Stdlib_upstream_compatible.Int32_u.of_int i) v
  external floatarray_set_float64x8_unsafe : floatarray -> int32# -> float64x8 -> unit = "%caml_floatarray_set512u_indexed_by_int32#"
  let floatarray_set_float64x8_unsafe arr i v = floatarray_set_float64x8_unsafe arr (Stdlib_upstream_compatible.Int32_u.of_int i) v

  external unboxed_float_array_get_float64x8 : float# array -> int32# -> float64x8 = "%caml_unboxed_float_array_get512_indexed_by_int32#"
  let unboxed_float_array_get_float64x8 arr i = unboxed_float_array_get_float64x8 arr (Stdlib_upstream_compatible.Int32_u.of_int i)
  external unboxed_float_array_get_float64x8_unsafe : float# array -> int32# -> float64x8 = "%caml_unboxed_float_array_get512u_indexed_by_int32#"
  let unboxed_float_array_get_float64x8_unsafe arr i = unboxed_float_array_get_float64x8_unsafe arr (Stdlib_upstream_compatible.Int32_u.of_int i)

  external unboxed_float_array_set_float64x8 : float# array -> int32# -> float64x8 -> unit = "%caml_unboxed_float_array_set512_indexed_by_int32#"
  let unboxed_float_array_set_float64x8 arr i v = unboxed_float_array_set_float64x8 arr (Stdlib_upstream_compatible.Int32_u.of_int i) v
  external unboxed_float_array_set_float64x8_unsafe : float# array -> int32# -> float64x8 -> unit = "%caml_unboxed_float_array_set512u_indexed_by_int32#"
  let unboxed_float_array_set_float64x8_unsafe arr i v = unboxed_float_array_set_float64x8_unsafe arr (Stdlib_upstream_compatible.Int32_u.of_int i) v

  external unboxed_float32_array_get_float32x16 : float32# array -> int32# -> float32x16 = "%caml_unboxed_float32_array_get512_indexed_by_int32#"
  let unboxed_float32_array_get_float32x16 arr i = unboxed_float32_array_get_float32x16 arr (Stdlib_upstream_compatible.Int32_u.of_int i)
  external unboxed_float32_array_get_float32x16_unsafe : float32# array -> int32# -> float32x16 = "%caml_unboxed_float32_array_get512u_indexed_by_int32#"
  let unboxed_float32_array_get_float32x16_unsafe arr i = unboxed_float32_array_get_float32x16_unsafe arr (Stdlib_upstream_compatible.Int32_u.of_int i)

  external unboxed_float32_array_set_float32x16 : float32# array -> int32# -> float32x16 -> unit = "%caml_unboxed_float32_array_set512_indexed_by_int32#"
  let unboxed_float32_array_set_float32x16 arr i v = unboxed_float32_array_set_float32x16 arr (Stdlib_upstream_compatible.Int32_u.of_int i) v
  external unboxed_float32_array_set_float32x16_unsafe : float32# array -> int32# -> float32x16 -> unit = "%caml_unboxed_float32_array_set512u_indexed_by_int32#"
  let unboxed_float32_array_set_float32x16_unsafe arr i v = unboxed_float32_array_set_float32x16_unsafe arr (Stdlib_upstream_compatible.Int32_u.of_int i) v

end)

module _ = Float_arrays(struct

  external floatarray_get_float64x8 : floatarray -> int64# -> float64x8 = "%caml_floatarray_get512_indexed_by_int64#"
  let floatarray_get_float64x8 arr i = floatarray_get_float64x8 arr (Stdlib_upstream_compatible.Int64_u.of_int i)
  external floatarray_get_float64x8_unsafe : floatarray -> int64# -> float64x8 = "%caml_floatarray_get512u_indexed_by_int64#"
  let floatarray_get_float64x8_unsafe arr i = floatarray_get_float64x8_unsafe arr (Stdlib_upstream_compatible.Int64_u.of_int i)

  external floatarray_set_float64x8 : floatarray -> int64# -> float64x8 -> unit = "%caml_floatarray_set512_indexed_by_int64#"
  let floatarray_set_float64x8 arr i v = floatarray_set_float64x8 arr (Stdlib_upstream_compatible.Int64_u.of_int i) v
  external floatarray_set_float64x8_unsafe : floatarray -> int64# -> float64x8 -> unit = "%caml_floatarray_set512u_indexed_by_int64#"
  let floatarray_set_float64x8_unsafe arr i v = floatarray_set_float64x8_unsafe arr (Stdlib_upstream_compatible.Int64_u.of_int i) v

  external unboxed_float_array_get_float64x8 : float# array -> int64# -> float64x8 = "%caml_unboxed_float_array_get512_indexed_by_int64#"
  let unboxed_float_array_get_float64x8 arr i = unboxed_float_array_get_float64x8 arr (Stdlib_upstream_compatible.Int64_u.of_int i)
  external unboxed_float_array_get_float64x8_unsafe : float# array -> int64# -> float64x8 = "%caml_unboxed_float_array_get512u_indexed_by_int64#"
  let unboxed_float_array_get_float64x8_unsafe arr i = unboxed_float_array_get_float64x8_unsafe arr (Stdlib_upstream_compatible.Int64_u.of_int i)

  external unboxed_float_array_set_float64x8 : float# array -> int64# -> float64x8 -> unit = "%caml_unboxed_float_array_set512_indexed_by_int64#"
  let unboxed_float_array_set_float64x8 arr i v = unboxed_float_array_set_float64x8 arr (Stdlib_upstream_compatible.Int64_u.of_int i) v
  external unboxed_float_array_set_float64x8_unsafe : float# array -> int64# -> float64x8 -> unit = "%caml_unboxed_float_array_set512u_indexed_by_int64#"
  let unboxed_float_array_set_float64x8_unsafe arr i v = unboxed_float_array_set_float64x8_unsafe arr (Stdlib_upstream_compatible.Int64_u.of_int i) v

  external unboxed_float32_array_get_float32x16 : float32# array -> int64# -> float32x16 = "%caml_unboxed_float32_array_get512_indexed_by_int64#"
  let unboxed_float32_array_get_float32x16 arr i = unboxed_float32_array_get_float32x16 arr (Stdlib_upstream_compatible.Int64_u.of_int i)
  external unboxed_float32_array_get_float32x16_unsafe : float32# array -> int64# -> float32x16 = "%caml_unboxed_float32_array_get512u_indexed_by_int64#"
  let unboxed_float32_array_get_float32x16_unsafe arr i = unboxed_float32_array_get_float32x16_unsafe arr (Stdlib_upstream_compatible.Int64_u.of_int i)

  external unboxed_float32_array_set_float32x16 : float32# array -> int64# -> float32x16 -> unit = "%caml_unboxed_float32_array_set512_indexed_by_int64#"
  let unboxed_float32_array_set_float32x16 arr i v = unboxed_float32_array_set_float32x16 arr (Stdlib_upstream_compatible.Int64_u.of_int i) v
  external unboxed_float32_array_set_float32x16_unsafe : float32# array -> int64# -> float32x16 -> unit = "%caml_unboxed_float32_array_set512u_indexed_by_int64#"
  let unboxed_float32_array_set_float32x16_unsafe arr i v = unboxed_float32_array_set_float32x16_unsafe arr (Stdlib_upstream_compatible.Int64_u.of_int i) v

end)

module _ = Float_arrays(struct

  external floatarray_get_float64x8 : floatarray -> nativeint# -> float64x8 = "%caml_floatarray_get512_indexed_by_nativeint#"
  let floatarray_get_float64x8 arr i = floatarray_get_float64x8 arr (Stdlib_upstream_compatible.Nativeint_u.of_int i)
  external floatarray_get_float64x8_unsafe : floatarray -> nativeint# -> float64x8 = "%caml_floatarray_get512u_indexed_by_nativeint#"
  let floatarray_get_float64x8_unsafe arr i = floatarray_get_float64x8_unsafe arr (Stdlib_upstream_compatible.Nativeint_u.of_int i)

  external floatarray_set_float64x8 : floatarray -> nativeint# -> float64x8 -> unit = "%caml_floatarray_set512_indexed_by_nativeint#"
  let floatarray_set_float64x8 arr i v = floatarray_set_float64x8 arr (Stdlib_upstream_compatible.Nativeint_u.of_int i) v
  external floatarray_set_float64x8_unsafe : floatarray -> nativeint# -> float64x8 -> unit = "%caml_floatarray_set512u_indexed_by_nativeint#"
  let floatarray_set_float64x8_unsafe arr i v = floatarray_set_float64x8_unsafe arr (Stdlib_upstream_compatible.Nativeint_u.of_int i) v

  external unboxed_float_array_get_float64x8 : float# array -> nativeint# -> float64x8 = "%caml_unboxed_float_array_get512_indexed_by_nativeint#"
  let unboxed_float_array_get_float64x8 arr i = unboxed_float_array_get_float64x8 arr (Stdlib_upstream_compatible.Nativeint_u.of_int i)
  external unboxed_float_array_get_float64x8_unsafe : float# array -> nativeint# -> float64x8 = "%caml_unboxed_float_array_get512u_indexed_by_nativeint#"
  let unboxed_float_array_get_float64x8_unsafe arr i = unboxed_float_array_get_float64x8_unsafe arr (Stdlib_upstream_compatible.Nativeint_u.of_int i)

  external unboxed_float_array_set_float64x8 : float# array -> nativeint# -> float64x8 -> unit = "%caml_unboxed_float_array_set512_indexed_by_nativeint#"
  let unboxed_float_array_set_float64x8 arr i v = unboxed_float_array_set_float64x8 arr (Stdlib_upstream_compatible.Nativeint_u.of_int i) v
  external unboxed_float_array_set_float64x8_unsafe : float# array -> nativeint# -> float64x8 -> unit = "%caml_unboxed_float_array_set512u_indexed_by_nativeint#"
  let unboxed_float_array_set_float64x8_unsafe arr i v = unboxed_float_array_set_float64x8_unsafe arr (Stdlib_upstream_compatible.Nativeint_u.of_int i) v

  external unboxed_float32_array_get_float32x16 : float32# array -> nativeint# -> float32x16 = "%caml_unboxed_float32_array_get512_indexed_by_nativeint#"
  let unboxed_float32_array_get_float32x16 arr i = unboxed_float32_array_get_float32x16 arr (Stdlib_upstream_compatible.Nativeint_u.of_int i)
  external unboxed_float32_array_get_float32x16_unsafe : float32# array -> nativeint# -> float32x16 = "%caml_unboxed_float32_array_get512u_indexed_by_nativeint#"
  let unboxed_float32_array_get_float32x16_unsafe arr i = unboxed_float32_array_get_float32x16_unsafe arr (Stdlib_upstream_compatible.Nativeint_u.of_int i)

  external unboxed_float32_array_set_float32x16 : float32# array -> nativeint# -> float32x16 -> unit = "%caml_unboxed_float32_array_set512_indexed_by_nativeint#"
  let unboxed_float32_array_set_float32x16 arr i v = unboxed_float32_array_set_float32x16 arr (Stdlib_upstream_compatible.Nativeint_u.of_int i) v
  external unboxed_float32_array_set_float32x16_unsafe : float32# array -> nativeint# -> float32x16 -> unit = "%caml_unboxed_float32_array_set512u_indexed_by_nativeint#"
  let unboxed_float32_array_set_float32x16_unsafe arr i v = unboxed_float32_array_set_float32x16_unsafe arr (Stdlib_upstream_compatible.Nativeint_u.of_int i) v

end)

module Int_arrays (Primitives : sig

  val int_array_get_int64x8 : int array -> int -> int64x8
  val int_array_get_int64x8_unsafe : int array -> int -> int64x8

  val int_iarray_get_int64x8 : int iarray -> int -> int64x8
  val int_iarray_get_int64x8_unsafe : int iarray -> int -> int64x8

  val int_array_set_int64x8 : int array -> int -> int64x8 -> unit
  val int_array_set_int64x8_unsafe : int array -> int -> int64x8 -> unit

  val unboxed_int64_array_get_int64x8 : int64# array -> int -> int64x8
  val unboxed_int64_array_get_int64x8_unsafe : int64# array -> int -> int64x8

  val unboxed_int64_array_set_int64x8 : int64# array -> int -> int64x8 -> unit
  val unboxed_int64_array_set_int64x8_unsafe : int64# array -> int -> int64x8 -> unit

  val unboxed_nativeint_array_get_int64x8 : nativeint# array -> int -> int64x8
  val unboxed_nativeint_array_get_int64x8_unsafe : nativeint# array -> int -> int64x8

  val unboxed_nativeint_array_set_int64x8 : nativeint# array -> int -> int64x8 -> unit
  val unboxed_nativeint_array_set_int64x8_unsafe : nativeint# array -> int -> int64x8 -> unit

  val unboxed_int32_array_get_int32x16 : int32# array -> int -> int32x16
  val unboxed_int32_array_get_int32x16_unsafe : int32# array -> int -> int32x16

  val unboxed_int32_array_set_int32x16 : int32# array -> int -> int32x16 -> unit
  val unboxed_int32_array_set_int32x16_unsafe : int32# array -> int -> int32x16 -> unit

  val untagged_int16_array_get_int16x32 : int16# array -> int -> int16x32
  val untagged_int16_array_get_int16x32_unsafe : int16# array -> int -> int16x32

  val untagged_int16_array_set_int16x32 : int16# array -> int -> int16x32 -> unit
  val untagged_int16_array_set_int16x32_unsafe : int16# array -> int -> int16x32 -> unit

  val untagged_int8_array_get_int8x64 : int8# array -> int -> int8x64
  val untagged_int8_array_get_int8x64_unsafe : int8# array -> int -> int8x64

  val untagged_int8_array_set_int8x64 : int8# array -> int -> int8x64 -> unit
  val untagged_int8_array_set_int8x64_unsafe : int8# array -> int -> int8x64 -> unit

end) = struct
  open Primitives

  let checki64 e a =
    eq (int64x8_first_int64 e) (int64x8_second_int64 e)
       (int64x8_third_int64 e) (int64x8_fourth_int64 e)
       (int64x8_fifth_int64 e) (int64x8_sixth_int64 e)
       (int64x8_seventh_int64 e) (int64x8_eighth_int64 e)
       (int64x8_first_int64 a) (int64x8_second_int64 a)
       (int64x8_third_int64 a) (int64x8_fourth_int64 a)
       (int64x8_fifth_int64 a) (int64x8_sixth_int64 a)
       (int64x8_seventh_int64 a) (int64x8_eighth_int64 a)

  let checki32 e a =
    eq (int32x16_first_int64 e) (int32x16_second_int64 e)
       (int32x16_third_int64 e) (int32x16_fourth_int64 e)
       (int32x16_fifth_int64 e) (int32x16_sixth_int64 e)
       (int32x16_seventh_int64 e) (int32x16_eighth_int64 e)
       (int32x16_first_int64 a) (int32x16_second_int64 a)
       (int32x16_third_int64 a) (int32x16_fourth_int64 a)
       (int32x16_fifth_int64 a) (int32x16_sixth_int64 a)
       (int32x16_seventh_int64 a) (int32x16_eighth_int64 a)

  let checki16 e a =
    eq (int16x32_first_int64 e) (int16x32_second_int64 e)
       (int16x32_third_int64 e) (int16x32_fourth_int64 e)
       (int16x32_fifth_int64 e) (int16x32_sixth_int64 e)
       (int16x32_seventh_int64 e) (int16x32_eighth_int64 e)
       (int16x32_first_int64 a) (int16x32_second_int64 a)
       (int16x32_third_int64 a) (int16x32_fourth_int64 a)
       (int16x32_fifth_int64 a) (int16x32_sixth_int64 a)
       (int16x32_seventh_int64 a) (int16x32_eighth_int64 a)

  let checki8 e a =
    eq (int8x64_first_int64 e) (int8x64_second_int64 e)
       (int8x64_third_int64 e) (int8x64_fourth_int64 e)
       (int8x64_fifth_int64 e) (int8x64_sixth_int64 e)
       (int8x64_seventh_int64 e) (int8x64_eighth_int64 e)
       (int8x64_first_int64 a) (int8x64_second_int64 a)
       (int8x64_third_int64 a) (int8x64_fourth_int64 a)
       (int8x64_fifth_int64 a) (int8x64_sixth_int64 a)
       (int8x64_seventh_int64 a) (int8x64_eighth_int64 a)

  let i64x8 x0 x1 x2 x3 x4 x5 x6 x7 =
    int64x8_of_int64s x0 x1 x2 x3 x4 x5 x6 x7

  let i32x16 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 =
    let pack a b =
      Int64.(logor (shift_left (logand (of_int32 b) 0xFFFFFFFFL) 32)
               (logand (of_int32 a) 0xFFFFFFFFL))
    in
    int32x16_of_int64s
      (pack x0 x1) (pack x2 x3) (pack x4 x5) (pack x6 x7)
      (pack x8 x9) (pack x10 x11) (pack x12 x13) (pack x14 x15)

  let i16x32
        x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15
        x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 =
    let cons x y =
      Int64.(logor (shift_left y 16) (of_int (Stdlib_stable.Int16.to_int x)))
    in
    int16x32_of_int64s
      (cons x0 @@ cons x1 @@ cons x2 @@ cons x3 0L)
      (cons x4 @@ cons x5 @@ cons x6 @@ cons x7 0L)
      (cons x8 @@ cons x9 @@ cons x10 @@ cons x11 0L)
      (cons x12 @@ cons x13 @@ cons x14 @@ cons x15 0L)
      (cons x16 @@ cons x17 @@ cons x18 @@ cons x19 0L)
      (cons x20 @@ cons x21 @@ cons x22 @@ cons x23 0L)
      (cons x24 @@ cons x25 @@ cons x26 @@ cons x27 0L)
      (cons x28 @@ cons x29 @@ cons x30 @@ cons x31 0L)

  let i8x64
        x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15
        x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31
        x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47
        x48 x49 x50 x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 x61 x62 x63 =
    let cons x y =
      Int64.(logor (shift_left y 8) (of_int (Stdlib_stable.Int8.to_int x)))
    in
    int8x64_of_int64s
      (cons x0 @@ cons x1 @@ cons x2 @@ cons x3 @@ cons x4 @@ cons x5 @@ cons x6 @@ cons x7 0L)
      (cons x8 @@ cons x9 @@ cons x10 @@ cons x11 @@ cons x12 @@ cons x13 @@ cons x14 @@ cons x15 0L)
      (cons x16 @@ cons x17 @@ cons x18 @@ cons x19 @@ cons x20 @@ cons x21 @@ cons x22 @@ cons x23 0L)
      (cons x24 @@ cons x25 @@ cons x26 @@ cons x27 @@ cons x28 @@ cons x29 @@ cons x30 @@ cons x31 0L)
      (cons x32 @@ cons x33 @@ cons x34 @@ cons x35 @@ cons x36 @@ cons x37 @@ cons x38 @@ cons x39 0L)
      (cons x40 @@ cons x41 @@ cons x42 @@ cons x43 @@ cons x44 @@ cons x45 @@ cons x46 @@ cons x47 0L)
      (cons x48 @@ cons x49 @@ cons x50 @@ cons x51 @@ cons x52 @@ cons x53 @@ cons x54 @@ cons x55 0L)
      (cons x56 @@ cons x57 @@ cons x58 @@ cons x59 @@ cons x60 @@ cons x61 @@ cons x62 @@ cons x63 0L)

  let tag i = Int64.(add (shift_left i 1) 1L)
  let int_array () = [| 0; 1; 2; 3; 4; 5; 6; 7; 8 |]
  let int_iarray () = [: 0; 1; 2; 3; 4; 5; 6; 7; 8 :]
  let unboxed_int64_array () = [| #0L; #1L; #2L; #3L; #4L; #5L; #6L; #7L; #8L |]
  let unboxed_nativeint_array () =
    [| #0n; #1n; #2n; #3n; #4n; #5n; #6n; #7n; #8n |]
  let unboxed_int32_array () =
    [| #0l; #1l; #2l; #3l; #4l; #5l; #6l; #7l; #8l;
       #9l; #10l; #11l; #12l; #13l; #14l; #15l; #16l |]
  let untagged_int16_array () =
    [| #0S; #1S; #2S; #3S; #4S; #5S; #6S; #7S;
       #8S; #9S; #10S; #11S; #12S; #13S; #14S; #15S;
       #16S; #17S; #18S; #19S; #20S; #21S; #22S; #23S;
       #24S; #25S; #26S; #27S; #28S; #29S; #30S; #31S; #32S |]
  let untagged_int8_array () =
    [| #0s; #1s; #2s; #3s; #4s; #5s; #6s; #7s;
       #8s; #9s; #10s; #11s; #12s; #13s; #14s; #15s;
       #16s; #17s; #18s; #19s; #20s; #21s; #22s; #23s;
       #24s; #25s; #26s; #27s; #28s; #29s; #30s; #31s;
       #32s; #33s; #34s; #35s; #36s; #37s; #38s; #39s;
       #40s; #41s; #42s; #43s; #44s; #45s; #46s; #47s;
       #48s; #49s; #50s; #51s; #52s; #53s; #54s; #55s;
       #56s; #57s; #58s; #59s; #60s; #61s; #62s; #63s; #64s |]

  (* int array (tagged) *)
  let () =
    let a = int_array () in
    checki64
      (i64x8 (tag 0L) (tag 1L) (tag 2L) (tag 3L)
             (tag 4L) (tag 5L) (tag 6L) (tag 7L))
      (int_array_get_int64x8 a 0);
    checki64
      (i64x8 (tag 1L) (tag 2L) (tag 3L) (tag 4L)
             (tag 5L) (tag 6L) (tag 7L) (tag 8L))
      (int_array_get_int64x8 a 1);
    let s_0 =
      i64x8 (tag 8L) (tag 7L) (tag 6L) (tag 5L)
            (tag 4L) (tag 3L) (tag 2L) (tag 1L)
    in
    int_array_set_int64x8 a 0 s_0;
    checki64 s_0 (int_array_get_int64x8 a 0)
  ;;

  let () =
    let a = int_array () in
    checki64
      (i64x8 (tag 0L) (tag 1L) (tag 2L) (tag 3L)
             (tag 4L) (tag 5L) (tag 6L) (tag 7L))
      (int_array_get_int64x8_unsafe a 0);
    let s_0 =
      i64x8 (tag 8L) (tag 7L) (tag 6L) (tag 5L)
            (tag 4L) (tag 3L) (tag 2L) (tag 1L)
    in
    int_array_set_int64x8_unsafe a 0 s_0;
    checki64 s_0 (int_array_get_int64x8_unsafe a 0)
  ;;

  let () =
    let i_0 = i64x8 (tag 0L) (tag 0L) (tag 0L) (tag 0L)
                    (tag 0L) (tag 0L) (tag 0L) (tag 0L) in
    let fail a i =
      (try
        let _ = int_array_get_int64x8 a i in
        Printf.printf "Did not fail on index %d\n" i
      with | Invalid_argument s when s = "index out of bounds" -> ());
      (try
        let _ = int_array_set_int64x8 a i i_0 in
        Printf.printf "Did not fail on index %d\n" i
      with | Invalid_argument s when s = "index out of bounds" -> ())
    in
    fail (int_array ()) (-1);
    fail (int_array ()) 2;
    fail [||] 0;
    fail [|0|] 0;
    fail [|0|] (-1)
  ;;

  (* int iarray *)
  let () =
    let a = int_iarray () in
    checki64
      (i64x8 (tag 0L) (tag 1L) (tag 2L) (tag 3L)
             (tag 4L) (tag 5L) (tag 6L) (tag 7L))
      (int_iarray_get_int64x8_unsafe a 0);
    checki64
      (i64x8 (tag 1L) (tag 2L) (tag 3L) (tag 4L)
             (tag 5L) (tag 6L) (tag 7L) (tag 8L))
      (int_iarray_get_int64x8_unsafe a 1)
  ;;

  let () =
    let fail a i =
      try
        let _ = int_iarray_get_int64x8 a i in
        Printf.printf "Did not fail on index %d\n" i
      with | Invalid_argument s when s = "index out of bounds" -> ()
    in
    fail (int_iarray ()) (-1);
    fail (int_iarray ()) 2;
    fail [::] 0;
    fail [: 0 :] 0;
    fail [: 0 :] (-1)
  ;;

  (* int64# array *)
  let () =
    let a = unboxed_int64_array () in
    checki64 (i64x8 0L 1L 2L 3L 4L 5L 6L 7L)
      (unboxed_int64_array_get_int64x8 a 0);
    checki64 (i64x8 1L 2L 3L 4L 5L 6L 7L 8L)
      (unboxed_int64_array_get_int64x8 a 1);
    let s_0 = i64x8 8L 7L 6L 5L 4L 3L 2L 1L in
    unboxed_int64_array_set_int64x8 a 0 s_0;
    checki64 s_0 (unboxed_int64_array_get_int64x8 a 0)
  ;;

  let () =
    let a = unboxed_int64_array () in
    checki64 (i64x8 0L 1L 2L 3L 4L 5L 6L 7L)
      (unboxed_int64_array_get_int64x8_unsafe a 0);
    let s_0 = i64x8 8L 7L 6L 5L 4L 3L 2L 1L in
    unboxed_int64_array_set_int64x8_unsafe a 0 s_0;
    checki64 s_0 (unboxed_int64_array_get_int64x8_unsafe a 0)
  ;;

  let () =
    let i_0 = i64x8 0L 0L 0L 0L 0L 0L 0L 0L in
    let fail a i =
      try
        let _ = unboxed_int64_array_get_int64x8 a i in
        let _ = unboxed_int64_array_set_int64x8 a i i_0 in
        Printf.printf "Did not fail on index %d\n" i
      with | Invalid_argument s when s = "index out of bounds" -> ()
    in
    fail (unboxed_int64_array ()) (-1);
    fail (unboxed_int64_array ()) 2;
    fail [||] 0;
    fail [|#0L|] 0;
    fail [|#0L|] (-1)
  ;;

  (* nativeint# array *)
  let () =
    let a = unboxed_nativeint_array () in
    checki64 (i64x8 0L 1L 2L 3L 4L 5L 6L 7L)
      (unboxed_nativeint_array_get_int64x8 a 0);
    checki64 (i64x8 1L 2L 3L 4L 5L 6L 7L 8L)
      (unboxed_nativeint_array_get_int64x8 a 1);
    let s_0 = i64x8 8L 7L 6L 5L 4L 3L 2L 1L in
    unboxed_nativeint_array_set_int64x8 a 0 s_0;
    checki64 s_0 (unboxed_nativeint_array_get_int64x8 a 0)
  ;;

  let () =
    let i_0 = i64x8 0L 0L 0L 0L 0L 0L 0L 0L in
    let fail a i =
      try
        let _ = unboxed_nativeint_array_get_int64x8 a i in
        let _ = unboxed_nativeint_array_set_int64x8 a i i_0 in
        Printf.printf "Did not fail on index %d\n" i
      with | Invalid_argument s when s = "index out of bounds" -> ()
    in
    fail (unboxed_nativeint_array ()) (-1);
    fail (unboxed_nativeint_array ()) 2;
    fail [||] 0;
    fail [|#0n|] 0;
    fail [|#0n|] (-1)
  ;;

  (* int32# array *)
  let () =
    let a = unboxed_int32_array () in
    checki32
      (i32x16 0l 1l 2l 3l 4l 5l 6l 7l 8l 9l 10l 11l 12l 13l 14l 15l)
      (unboxed_int32_array_get_int32x16 a 0);
    checki32
      (i32x16 1l 2l 3l 4l 5l 6l 7l 8l 9l 10l 11l 12l 13l 14l 15l 16l)
      (unboxed_int32_array_get_int32x16 a 1);
    let s_0 =
      i32x16 16l 15l 14l 13l 12l 11l 10l 9l 8l 7l 6l 5l 4l 3l 2l 1l
    in
    unboxed_int32_array_set_int32x16 a 0 s_0;
    checki32 s_0 (unboxed_int32_array_get_int32x16 a 0)
  ;;

  let () =
    let a = unboxed_int32_array () in
    checki32
      (i32x16 0l 1l 2l 3l 4l 5l 6l 7l 8l 9l 10l 11l 12l 13l 14l 15l)
      (unboxed_int32_array_get_int32x16_unsafe a 0);
    let s_0 =
      i32x16 16l 15l 14l 13l 12l 11l 10l 9l 8l 7l 6l 5l 4l 3l 2l 1l
    in
    unboxed_int32_array_set_int32x16_unsafe a 0 s_0;
    checki32 s_0 (unboxed_int32_array_get_int32x16_unsafe a 0)
  ;;

  let () =
    let i_0 = i32x16 0l 0l 0l 0l 0l 0l 0l 0l 0l 0l 0l 0l 0l 0l 0l 0l in
    let fail a i =
      try
        let _ = unboxed_int32_array_get_int32x16 a i in
        let _ = unboxed_int32_array_set_int32x16 a i i_0 in
        Printf.printf "Did not fail on index %d\n" i
      with | Invalid_argument s when s = "index out of bounds" -> ()
    in
    fail (unboxed_int32_array ()) (-1);
    fail (unboxed_int32_array ()) 2;
    fail [||] 0;
    fail [|#0l|] 0;
    fail [|#0l|] (-1)
  ;;

  (* int16# array *)
  let () =
    let a = untagged_int16_array () in
    checki16
      (i16x32 0S 1S 2S 3S 4S 5S 6S 7S 8S 9S 10S 11S 12S 13S 14S 15S
              16S 17S 18S 19S 20S 21S 22S 23S 24S 25S 26S 27S 28S 29S 30S 31S)
      (untagged_int16_array_get_int16x32 a 0);
    checki16
      (i16x32 1S 2S 3S 4S 5S 6S 7S 8S 9S 10S 11S 12S 13S 14S 15S 16S
              17S 18S 19S 20S 21S 22S 23S 24S 25S 26S 27S 28S 29S 30S 31S 32S)
      (untagged_int16_array_get_int16x32 a 1);
    let s_0 =
      i16x32 31S 30S 29S 28S 27S 26S 25S 24S 23S 22S 21S 20S 19S 18S 17S 16S
             15S 14S 13S 12S 11S 10S 9S 8S 7S 6S 5S 4S 3S 2S 1S 0S
    in
    untagged_int16_array_set_int16x32 a 0 s_0;
    checki16 s_0 (untagged_int16_array_get_int16x32 a 0)
  ;;

  let () =
    let a = untagged_int16_array () in
    checki16
      (i16x32 0S 1S 2S 3S 4S 5S 6S 7S 8S 9S 10S 11S 12S 13S 14S 15S
              16S 17S 18S 19S 20S 21S 22S 23S 24S 25S 26S 27S 28S 29S 30S 31S)
      (untagged_int16_array_get_int16x32_unsafe a 0);
    let s_0 =
      i16x32 31S 30S 29S 28S 27S 26S 25S 24S 23S 22S 21S 20S 19S 18S 17S 16S
             15S 14S 13S 12S 11S 10S 9S 8S 7S 6S 5S 4S 3S 2S 1S 0S
    in
    untagged_int16_array_set_int16x32_unsafe a 0 s_0;
    checki16 s_0 (untagged_int16_array_get_int16x32_unsafe a 0)
  ;;

  let () =
    let i_0 =
      i16x32 0S 0S 0S 0S 0S 0S 0S 0S 0S 0S 0S 0S 0S 0S 0S 0S
             0S 0S 0S 0S 0S 0S 0S 0S 0S 0S 0S 0S 0S 0S 0S 0S
    in
    let fail a i =
      try
        let _ = untagged_int16_array_get_int16x32 a i in
        let _ = untagged_int16_array_set_int16x32 a i i_0 in
        Printf.printf "Did not fail on index %d\n" i
      with | Invalid_argument s when s = "index out of bounds" -> ()
    in
    fail (untagged_int16_array ()) (-1);
    fail (untagged_int16_array ()) 2;
    fail [||] 0;
    fail [|#0S|] 0;
    fail [|#0S|] (-1)
  ;;

  (* int8# array *)
  let () =
    let a = untagged_int8_array () in
    checki8
      (i8x64 0s 1s 2s 3s 4s 5s 6s 7s 8s 9s 10s 11s 12s 13s 14s 15s
             16s 17s 18s 19s 20s 21s 22s 23s 24s 25s 26s 27s 28s 29s 30s 31s
             32s 33s 34s 35s 36s 37s 38s 39s 40s 41s 42s 43s 44s 45s 46s 47s
             48s 49s 50s 51s 52s 53s 54s 55s 56s 57s 58s 59s 60s 61s 62s 63s)
      (untagged_int8_array_get_int8x64 a 0);
    checki8
      (i8x64 1s 2s 3s 4s 5s 6s 7s 8s 9s 10s 11s 12s 13s 14s 15s 16s
             17s 18s 19s 20s 21s 22s 23s 24s 25s 26s 27s 28s 29s 30s 31s 32s
             33s 34s 35s 36s 37s 38s 39s 40s 41s 42s 43s 44s 45s 46s 47s 48s
             49s 50s 51s 52s 53s 54s 55s 56s 57s 58s 59s 60s 61s 62s 63s 64s)
      (untagged_int8_array_get_int8x64 a 1);
    let s_0 =
      i8x64 63s 62s 61s 60s 59s 58s 57s 56s 55s 54s 53s 52s 51s 50s 49s 48s
            47s 46s 45s 44s 43s 42s 41s 40s 39s 38s 37s 36s 35s 34s 33s 32s
            31s 30s 29s 28s 27s 26s 25s 24s 23s 22s 21s 20s 19s 18s 17s 16s
            15s 14s 13s 12s 11s 10s 9s 8s 7s 6s 5s 4s 3s 2s 1s 0s
    in
    untagged_int8_array_set_int8x64 a 0 s_0;
    checki8 s_0 (untagged_int8_array_get_int8x64 a 0)
  ;;

  let () =
    let a = untagged_int8_array () in
    checki8
      (i8x64 0s 1s 2s 3s 4s 5s 6s 7s 8s 9s 10s 11s 12s 13s 14s 15s
             16s 17s 18s 19s 20s 21s 22s 23s 24s 25s 26s 27s 28s 29s 30s 31s
             32s 33s 34s 35s 36s 37s 38s 39s 40s 41s 42s 43s 44s 45s 46s 47s
             48s 49s 50s 51s 52s 53s 54s 55s 56s 57s 58s 59s 60s 61s 62s 63s)
      (untagged_int8_array_get_int8x64_unsafe a 0);
    let s_0 =
      i8x64 63s 62s 61s 60s 59s 58s 57s 56s 55s 54s 53s 52s 51s 50s 49s 48s
            47s 46s 45s 44s 43s 42s 41s 40s 39s 38s 37s 36s 35s 34s 33s 32s
            31s 30s 29s 28s 27s 26s 25s 24s 23s 22s 21s 20s 19s 18s 17s 16s
            15s 14s 13s 12s 11s 10s 9s 8s 7s 6s 5s 4s 3s 2s 1s 0s
    in
    untagged_int8_array_set_int8x64_unsafe a 0 s_0;
    checki8 s_0 (untagged_int8_array_get_int8x64_unsafe a 0)
  ;;

  let () =
    let i_0 =
      i8x64 0s 0s 0s 0s 0s 0s 0s 0s 0s 0s 0s 0s 0s 0s 0s 0s
            0s 0s 0s 0s 0s 0s 0s 0s 0s 0s 0s 0s 0s 0s 0s 0s
            0s 0s 0s 0s 0s 0s 0s 0s 0s 0s 0s 0s 0s 0s 0s 0s
            0s 0s 0s 0s 0s 0s 0s 0s 0s 0s 0s 0s 0s 0s 0s 0s
    in
    let fail a i =
      try
        let _ = untagged_int8_array_get_int8x64 a i in
        let _ = untagged_int8_array_set_int8x64 a i i_0 in
        Printf.printf "Did not fail on index %d\n" i
      with | Invalid_argument s when s = "index out of bounds" -> ()
    in
    fail (untagged_int8_array ()) (-1);
    fail (untagged_int8_array ()) 2;
    fail [||] 0;
    fail [|#0s|] 0;
    fail [|#0s|] (-1)
  ;;
end

module _ = Int_arrays(struct

  external int_array_get_int64x8 : int array -> int -> int64x8 = "%caml_int_array_get512"
  external int_array_get_int64x8_unsafe : int array -> int -> int64x8 = "%caml_int_array_get512u"

  external int_iarray_get_int64x8 : int iarray -> int -> int64x8 = "%caml_int_array_get512"
  external int_iarray_get_int64x8_unsafe : int iarray -> int -> int64x8 = "%caml_int_array_get512u"

  external int_array_set_int64x8 : int array -> int -> int64x8 -> unit = "%caml_int_array_set512"
  external int_array_set_int64x8_unsafe : int array -> int -> int64x8 -> unit = "%caml_int_array_set512u"

  external unboxed_int64_array_get_int64x8 : int64# array -> int -> int64x8 = "%caml_unboxed_int64_array_get512"
  external unboxed_int64_array_get_int64x8_unsafe : int64# array -> int -> int64x8 = "%caml_unboxed_int64_array_get512u"

  external unboxed_int64_array_set_int64x8 : int64# array -> int -> int64x8 -> unit = "%caml_unboxed_int64_array_set512"
  external unboxed_int64_array_set_int64x8_unsafe : int64# array -> int -> int64x8 -> unit = "%caml_unboxed_int64_array_set512u"

  external unboxed_nativeint_array_get_int64x8 : nativeint# array -> int -> int64x8 = "%caml_unboxed_nativeint_array_get512"
  external unboxed_nativeint_array_get_int64x8_unsafe : nativeint# array -> int -> int64x8 = "%caml_unboxed_nativeint_array_get512u"

  external unboxed_nativeint_array_set_int64x8 : nativeint# array -> int -> int64x8 -> unit = "%caml_unboxed_nativeint_array_set512"
  external unboxed_nativeint_array_set_int64x8_unsafe : nativeint# array -> int -> int64x8 -> unit = "%caml_unboxed_nativeint_array_set512u"

  external unboxed_int32_array_get_int32x16 : int32# array -> int -> int32x16 = "%caml_unboxed_int32_array_get512"
  external unboxed_int32_array_get_int32x16_unsafe : int32# array -> int -> int32x16 = "%caml_unboxed_int32_array_get512u"

  external unboxed_int32_array_set_int32x16 : int32# array -> int -> int32x16 -> unit = "%caml_unboxed_int32_array_set512"
  external unboxed_int32_array_set_int32x16_unsafe : int32# array -> int -> int32x16 -> unit = "%caml_unboxed_int32_array_set512u"

  external untagged_int16_array_get_int16x32 : int16# array -> int -> int16x32 = "%caml_untagged_int16_array_get512"
  external untagged_int16_array_get_int16x32_unsafe : int16# array -> int -> int16x32 = "%caml_untagged_int16_array_get512u"

  external untagged_int16_array_set_int16x32 : int16# array -> int -> int16x32 -> unit = "%caml_untagged_int16_array_set512"
  external untagged_int16_array_set_int16x32_unsafe : int16# array -> int -> int16x32 -> unit = "%caml_untagged_int16_array_set512u"

  external untagged_int8_array_get_int8x64 : int8# array -> int -> int8x64 = "%caml_untagged_int8_array_get512"
  external untagged_int8_array_get_int8x64_unsafe : int8# array -> int -> int8x64 = "%caml_untagged_int8_array_get512u"

  external untagged_int8_array_set_int8x64 : int8# array -> int -> int8x64 -> unit = "%caml_untagged_int8_array_set512"
  external untagged_int8_array_set_int8x64_unsafe : int8# array -> int -> int8x64 -> unit = "%caml_untagged_int8_array_set512u"
end)

module _ = Int_arrays(struct

  external int_array_get_int64x8 : int array -> int8# -> int64x8 = "%caml_int_array_get512_indexed_by_int8#"
  let int_array_get_int64x8 arr i = int_array_get_int64x8 arr (Stdlib_stable.Int8_u.of_int i)
  external int_array_get_int64x8_unsafe : int array -> int8# -> int64x8 = "%caml_int_array_get512u_indexed_by_int8#"
  let int_array_get_int64x8_unsafe arr i = int_array_get_int64x8_unsafe arr (Stdlib_stable.Int8_u.of_int i)

  external int_iarray_get_int64x8 : int iarray -> int8# -> int64x8 = "%caml_int_array_get512_indexed_by_int8#"
  let int_iarray_get_int64x8 arr i = int_iarray_get_int64x8 arr (Stdlib_stable.Int8_u.of_int i)
  external int_iarray_get_int64x8_unsafe : int iarray -> int8# -> int64x8 = "%caml_int_array_get512u_indexed_by_int8#"
  let int_iarray_get_int64x8_unsafe arr i = int_iarray_get_int64x8_unsafe arr (Stdlib_stable.Int8_u.of_int i)

  external int_array_set_int64x8 : int array -> int8# -> int64x8 -> unit = "%caml_int_array_set512_indexed_by_int8#"
  let int_array_set_int64x8 arr i v = int_array_set_int64x8 arr (Stdlib_stable.Int8_u.of_int i) v
  external int_array_set_int64x8_unsafe : int array -> int8# -> int64x8 -> unit = "%caml_int_array_set512u_indexed_by_int8#"
  let int_array_set_int64x8_unsafe arr i v = int_array_set_int64x8_unsafe arr (Stdlib_stable.Int8_u.of_int i) v

  external unboxed_int64_array_get_int64x8 : int64# array -> int8# -> int64x8 = "%caml_unboxed_int64_array_get512_indexed_by_int8#"
  let unboxed_int64_array_get_int64x8 arr i = unboxed_int64_array_get_int64x8 arr (Stdlib_stable.Int8_u.of_int i)
  external unboxed_int64_array_get_int64x8_unsafe : int64# array -> int8# -> int64x8 = "%caml_unboxed_int64_array_get512u_indexed_by_int8#"
  let unboxed_int64_array_get_int64x8_unsafe arr i = unboxed_int64_array_get_int64x8_unsafe arr (Stdlib_stable.Int8_u.of_int i)

  external unboxed_int64_array_set_int64x8 : int64# array -> int8# -> int64x8 -> unit = "%caml_unboxed_int64_array_set512_indexed_by_int8#"
  let unboxed_int64_array_set_int64x8 arr i v = unboxed_int64_array_set_int64x8 arr (Stdlib_stable.Int8_u.of_int i) v
  external unboxed_int64_array_set_int64x8_unsafe : int64# array -> int8# -> int64x8 -> unit = "%caml_unboxed_int64_array_set512u_indexed_by_int8#"
  let unboxed_int64_array_set_int64x8_unsafe arr i v = unboxed_int64_array_set_int64x8_unsafe arr (Stdlib_stable.Int8_u.of_int i) v

  external unboxed_nativeint_array_get_int64x8 : nativeint# array -> int8# -> int64x8 = "%caml_unboxed_nativeint_array_get512_indexed_by_int8#"
  let unboxed_nativeint_array_get_int64x8 arr i = unboxed_nativeint_array_get_int64x8 arr (Stdlib_stable.Int8_u.of_int i)
  external unboxed_nativeint_array_get_int64x8_unsafe : nativeint# array -> int8# -> int64x8 = "%caml_unboxed_nativeint_array_get512u_indexed_by_int8#"
  let unboxed_nativeint_array_get_int64x8_unsafe arr i = unboxed_nativeint_array_get_int64x8_unsafe arr (Stdlib_stable.Int8_u.of_int i)

  external unboxed_nativeint_array_set_int64x8 : nativeint# array -> int8# -> int64x8 -> unit = "%caml_unboxed_nativeint_array_set512_indexed_by_int8#"
  let unboxed_nativeint_array_set_int64x8 arr i v = unboxed_nativeint_array_set_int64x8 arr (Stdlib_stable.Int8_u.of_int i) v
  external unboxed_nativeint_array_set_int64x8_unsafe : nativeint# array -> int8# -> int64x8 -> unit = "%caml_unboxed_nativeint_array_set512u_indexed_by_int8#"
  let unboxed_nativeint_array_set_int64x8_unsafe arr i v = unboxed_nativeint_array_set_int64x8_unsafe arr (Stdlib_stable.Int8_u.of_int i) v

  external unboxed_int32_array_get_int32x16 : int32# array -> int8# -> int32x16 = "%caml_unboxed_int32_array_get512_indexed_by_int8#"
  let unboxed_int32_array_get_int32x16 arr i = unboxed_int32_array_get_int32x16 arr (Stdlib_stable.Int8_u.of_int i)
  external unboxed_int32_array_get_int32x16_unsafe : int32# array -> int8# -> int32x16 = "%caml_unboxed_int32_array_get512u_indexed_by_int8#"
  let unboxed_int32_array_get_int32x16_unsafe arr i = unboxed_int32_array_get_int32x16_unsafe arr (Stdlib_stable.Int8_u.of_int i)

  external unboxed_int32_array_set_int32x16 : int32# array -> int8# -> int32x16 -> unit = "%caml_unboxed_int32_array_set512_indexed_by_int8#"
  let unboxed_int32_array_set_int32x16 arr i v = unboxed_int32_array_set_int32x16 arr (Stdlib_stable.Int8_u.of_int i) v
  external unboxed_int32_array_set_int32x16_unsafe : int32# array -> int8# -> int32x16 -> unit = "%caml_unboxed_int32_array_set512u_indexed_by_int8#"
  let unboxed_int32_array_set_int32x16_unsafe arr i v = unboxed_int32_array_set_int32x16_unsafe arr (Stdlib_stable.Int8_u.of_int i) v

  external untagged_int16_array_get_int16x32 : int16# array -> int8# -> int16x32 = "%caml_untagged_int16_array_get512_indexed_by_int8#"
  let untagged_int16_array_get_int16x32 arr i = untagged_int16_array_get_int16x32 arr (Stdlib_stable.Int8_u.of_int i)
  external untagged_int16_array_get_int16x32_unsafe : int16# array -> int8# -> int16x32 = "%caml_untagged_int16_array_get512u_indexed_by_int8#"
  let untagged_int16_array_get_int16x32_unsafe arr i = untagged_int16_array_get_int16x32_unsafe arr (Stdlib_stable.Int8_u.of_int i)

  external untagged_int16_array_set_int16x32 : int16# array -> int8# -> int16x32 -> unit = "%caml_untagged_int16_array_set512_indexed_by_int8#"
  let untagged_int16_array_set_int16x32 arr i v = untagged_int16_array_set_int16x32 arr (Stdlib_stable.Int8_u.of_int i) v
  external untagged_int16_array_set_int16x32_unsafe : int16# array -> int8# -> int16x32 -> unit = "%caml_untagged_int16_array_set512u_indexed_by_int8#"
  let untagged_int16_array_set_int16x32_unsafe arr i v = untagged_int16_array_set_int16x32_unsafe arr (Stdlib_stable.Int8_u.of_int i) v

  external untagged_int8_array_get_int8x64 : int8# array -> int8# -> int8x64 = "%caml_untagged_int8_array_get512_indexed_by_int8#"
  let untagged_int8_array_get_int8x64 arr i = untagged_int8_array_get_int8x64 arr (Stdlib_stable.Int8_u.of_int i)
  external untagged_int8_array_get_int8x64_unsafe : int8# array -> int8# -> int8x64 = "%caml_untagged_int8_array_get512u_indexed_by_int8#"
  let untagged_int8_array_get_int8x64_unsafe arr i = untagged_int8_array_get_int8x64_unsafe arr (Stdlib_stable.Int8_u.of_int i)

  external untagged_int8_array_set_int8x64 : int8# array -> int8# -> int8x64 -> unit = "%caml_untagged_int8_array_set512_indexed_by_int8#"
  let untagged_int8_array_set_int8x64 arr i v = untagged_int8_array_set_int8x64 arr (Stdlib_stable.Int8_u.of_int i) v
  external untagged_int8_array_set_int8x64_unsafe : int8# array -> int8# -> int8x64 -> unit = "%caml_untagged_int8_array_set512u_indexed_by_int8#"
  let untagged_int8_array_set_int8x64_unsafe arr i v = untagged_int8_array_set_int8x64_unsafe arr (Stdlib_stable.Int8_u.of_int i) v
end)

module _ = Int_arrays(struct

  external int_array_get_int64x8 : int array -> int16# -> int64x8 = "%caml_int_array_get512_indexed_by_int16#"
  let int_array_get_int64x8 arr i = int_array_get_int64x8 arr (Stdlib_stable.Int16_u.of_int i)
  external int_array_get_int64x8_unsafe : int array -> int16# -> int64x8 = "%caml_int_array_get512u_indexed_by_int16#"
  let int_array_get_int64x8_unsafe arr i = int_array_get_int64x8_unsafe arr (Stdlib_stable.Int16_u.of_int i)

  external int_iarray_get_int64x8 : int iarray -> int16# -> int64x8 = "%caml_int_array_get512_indexed_by_int16#"
  let int_iarray_get_int64x8 arr i = int_iarray_get_int64x8 arr (Stdlib_stable.Int16_u.of_int i)
  external int_iarray_get_int64x8_unsafe : int iarray -> int16# -> int64x8 = "%caml_int_array_get512u_indexed_by_int16#"
  let int_iarray_get_int64x8_unsafe arr i = int_iarray_get_int64x8_unsafe arr (Stdlib_stable.Int16_u.of_int i)

  external int_array_set_int64x8 : int array -> int16# -> int64x8 -> unit = "%caml_int_array_set512_indexed_by_int16#"
  let int_array_set_int64x8 arr i v = int_array_set_int64x8 arr (Stdlib_stable.Int16_u.of_int i) v
  external int_array_set_int64x8_unsafe : int array -> int16# -> int64x8 -> unit = "%caml_int_array_set512u_indexed_by_int16#"
  let int_array_set_int64x8_unsafe arr i v = int_array_set_int64x8_unsafe arr (Stdlib_stable.Int16_u.of_int i) v

  external unboxed_int64_array_get_int64x8 : int64# array -> int16# -> int64x8 = "%caml_unboxed_int64_array_get512_indexed_by_int16#"
  let unboxed_int64_array_get_int64x8 arr i = unboxed_int64_array_get_int64x8 arr (Stdlib_stable.Int16_u.of_int i)
  external unboxed_int64_array_get_int64x8_unsafe : int64# array -> int16# -> int64x8 = "%caml_unboxed_int64_array_get512u_indexed_by_int16#"
  let unboxed_int64_array_get_int64x8_unsafe arr i = unboxed_int64_array_get_int64x8_unsafe arr (Stdlib_stable.Int16_u.of_int i)

  external unboxed_int64_array_set_int64x8 : int64# array -> int16# -> int64x8 -> unit = "%caml_unboxed_int64_array_set512_indexed_by_int16#"
  let unboxed_int64_array_set_int64x8 arr i v = unboxed_int64_array_set_int64x8 arr (Stdlib_stable.Int16_u.of_int i) v
  external unboxed_int64_array_set_int64x8_unsafe : int64# array -> int16# -> int64x8 -> unit = "%caml_unboxed_int64_array_set512u_indexed_by_int16#"
  let unboxed_int64_array_set_int64x8_unsafe arr i v = unboxed_int64_array_set_int64x8_unsafe arr (Stdlib_stable.Int16_u.of_int i) v

  external unboxed_nativeint_array_get_int64x8 : nativeint# array -> int16# -> int64x8 = "%caml_unboxed_nativeint_array_get512_indexed_by_int16#"
  let unboxed_nativeint_array_get_int64x8 arr i = unboxed_nativeint_array_get_int64x8 arr (Stdlib_stable.Int16_u.of_int i)
  external unboxed_nativeint_array_get_int64x8_unsafe : nativeint# array -> int16# -> int64x8 = "%caml_unboxed_nativeint_array_get512u_indexed_by_int16#"
  let unboxed_nativeint_array_get_int64x8_unsafe arr i = unboxed_nativeint_array_get_int64x8_unsafe arr (Stdlib_stable.Int16_u.of_int i)

  external unboxed_nativeint_array_set_int64x8 : nativeint# array -> int16# -> int64x8 -> unit = "%caml_unboxed_nativeint_array_set512_indexed_by_int16#"
  let unboxed_nativeint_array_set_int64x8 arr i v = unboxed_nativeint_array_set_int64x8 arr (Stdlib_stable.Int16_u.of_int i) v
  external unboxed_nativeint_array_set_int64x8_unsafe : nativeint# array -> int16# -> int64x8 -> unit = "%caml_unboxed_nativeint_array_set512u_indexed_by_int16#"
  let unboxed_nativeint_array_set_int64x8_unsafe arr i v = unboxed_nativeint_array_set_int64x8_unsafe arr (Stdlib_stable.Int16_u.of_int i) v

  external unboxed_int32_array_get_int32x16 : int32# array -> int16# -> int32x16 = "%caml_unboxed_int32_array_get512_indexed_by_int16#"
  let unboxed_int32_array_get_int32x16 arr i = unboxed_int32_array_get_int32x16 arr (Stdlib_stable.Int16_u.of_int i)
  external unboxed_int32_array_get_int32x16_unsafe : int32# array -> int16# -> int32x16 = "%caml_unboxed_int32_array_get512u_indexed_by_int16#"
  let unboxed_int32_array_get_int32x16_unsafe arr i = unboxed_int32_array_get_int32x16_unsafe arr (Stdlib_stable.Int16_u.of_int i)

  external unboxed_int32_array_set_int32x16 : int32# array -> int16# -> int32x16 -> unit = "%caml_unboxed_int32_array_set512_indexed_by_int16#"
  let unboxed_int32_array_set_int32x16 arr i v = unboxed_int32_array_set_int32x16 arr (Stdlib_stable.Int16_u.of_int i) v
  external unboxed_int32_array_set_int32x16_unsafe : int32# array -> int16# -> int32x16 -> unit = "%caml_unboxed_int32_array_set512u_indexed_by_int16#"
  let unboxed_int32_array_set_int32x16_unsafe arr i v = unboxed_int32_array_set_int32x16_unsafe arr (Stdlib_stable.Int16_u.of_int i) v

  external untagged_int16_array_get_int16x32 : int16# array -> int16# -> int16x32 = "%caml_untagged_int16_array_get512_indexed_by_int16#"
  let untagged_int16_array_get_int16x32 arr i = untagged_int16_array_get_int16x32 arr (Stdlib_stable.Int16_u.of_int i)
  external untagged_int16_array_get_int16x32_unsafe : int16# array -> int16# -> int16x32 = "%caml_untagged_int16_array_get512u_indexed_by_int16#"
  let untagged_int16_array_get_int16x32_unsafe arr i = untagged_int16_array_get_int16x32_unsafe arr (Stdlib_stable.Int16_u.of_int i)

  external untagged_int16_array_set_int16x32 : int16# array -> int16# -> int16x32 -> unit = "%caml_untagged_int16_array_set512_indexed_by_int16#"
  let untagged_int16_array_set_int16x32 arr i v = untagged_int16_array_set_int16x32 arr (Stdlib_stable.Int16_u.of_int i) v
  external untagged_int16_array_set_int16x32_unsafe : int16# array -> int16# -> int16x32 -> unit = "%caml_untagged_int16_array_set512u_indexed_by_int16#"
  let untagged_int16_array_set_int16x32_unsafe arr i v = untagged_int16_array_set_int16x32_unsafe arr (Stdlib_stable.Int16_u.of_int i) v

  external untagged_int8_array_get_int8x64 : int8# array -> int16# -> int8x64 = "%caml_untagged_int8_array_get512_indexed_by_int16#"
  let untagged_int8_array_get_int8x64 arr i = untagged_int8_array_get_int8x64 arr (Stdlib_stable.Int16_u.of_int i)
  external untagged_int8_array_get_int8x64_unsafe : int8# array -> int16# -> int8x64 = "%caml_untagged_int8_array_get512u_indexed_by_int16#"
  let untagged_int8_array_get_int8x64_unsafe arr i = untagged_int8_array_get_int8x64_unsafe arr (Stdlib_stable.Int16_u.of_int i)

  external untagged_int8_array_set_int8x64 : int8# array -> int16# -> int8x64 -> unit = "%caml_untagged_int8_array_set512_indexed_by_int16#"
  let untagged_int8_array_set_int8x64 arr i v = untagged_int8_array_set_int8x64 arr (Stdlib_stable.Int16_u.of_int i) v
  external untagged_int8_array_set_int8x64_unsafe : int8# array -> int16# -> int8x64 -> unit = "%caml_untagged_int8_array_set512u_indexed_by_int16#"
  let untagged_int8_array_set_int8x64_unsafe arr i v = untagged_int8_array_set_int8x64_unsafe arr (Stdlib_stable.Int16_u.of_int i) v

end)

module _ = Int_arrays(struct

  external int_array_get_int64x8 : int array -> int32# -> int64x8 = "%caml_int_array_get512_indexed_by_int32#"
  let int_array_get_int64x8 arr i = int_array_get_int64x8 arr (Stdlib_upstream_compatible.Int32_u.of_int i)
  external int_array_get_int64x8_unsafe : int array -> int32# -> int64x8 = "%caml_int_array_get512u_indexed_by_int32#"
  let int_array_get_int64x8_unsafe arr i = int_array_get_int64x8_unsafe arr (Stdlib_upstream_compatible.Int32_u.of_int i)

  external int_iarray_get_int64x8 : int iarray -> int32# -> int64x8 = "%caml_int_array_get512_indexed_by_int32#"
  let int_iarray_get_int64x8 arr i = int_iarray_get_int64x8 arr (Stdlib_upstream_compatible.Int32_u.of_int i)
  external int_iarray_get_int64x8_unsafe : int iarray -> int32# -> int64x8 = "%caml_int_array_get512u_indexed_by_int32#"
  let int_iarray_get_int64x8_unsafe arr i = int_iarray_get_int64x8_unsafe arr (Stdlib_upstream_compatible.Int32_u.of_int i)

  external int_array_set_int64x8 : int array -> int32# -> int64x8 -> unit = "%caml_int_array_set512_indexed_by_int32#"
  let int_array_set_int64x8 arr i v = int_array_set_int64x8 arr (Stdlib_upstream_compatible.Int32_u.of_int i) v
  external int_array_set_int64x8_unsafe : int array -> int32# -> int64x8 -> unit = "%caml_int_array_set512u_indexed_by_int32#"
  let int_array_set_int64x8_unsafe arr i v = int_array_set_int64x8_unsafe arr (Stdlib_upstream_compatible.Int32_u.of_int i) v

  external unboxed_int64_array_get_int64x8 : int64# array -> int32# -> int64x8 = "%caml_unboxed_int64_array_get512_indexed_by_int32#"
  let unboxed_int64_array_get_int64x8 arr i = unboxed_int64_array_get_int64x8 arr (Stdlib_upstream_compatible.Int32_u.of_int i)
  external unboxed_int64_array_get_int64x8_unsafe : int64# array -> int32# -> int64x8 = "%caml_unboxed_int64_array_get512u_indexed_by_int32#"
  let unboxed_int64_array_get_int64x8_unsafe arr i = unboxed_int64_array_get_int64x8_unsafe arr (Stdlib_upstream_compatible.Int32_u.of_int i)

  external unboxed_int64_array_set_int64x8 : int64# array -> int32# -> int64x8 -> unit = "%caml_unboxed_int64_array_set512_indexed_by_int32#"
  let unboxed_int64_array_set_int64x8 arr i v = unboxed_int64_array_set_int64x8 arr (Stdlib_upstream_compatible.Int32_u.of_int i) v
  external unboxed_int64_array_set_int64x8_unsafe : int64# array -> int32# -> int64x8 -> unit = "%caml_unboxed_int64_array_set512u_indexed_by_int32#"
  let unboxed_int64_array_set_int64x8_unsafe arr i v = unboxed_int64_array_set_int64x8_unsafe arr (Stdlib_upstream_compatible.Int32_u.of_int i) v

  external unboxed_nativeint_array_get_int64x8 : nativeint# array -> int32# -> int64x8 = "%caml_unboxed_nativeint_array_get512_indexed_by_int32#"
  let unboxed_nativeint_array_get_int64x8 arr i = unboxed_nativeint_array_get_int64x8 arr (Stdlib_upstream_compatible.Int32_u.of_int i)
  external unboxed_nativeint_array_get_int64x8_unsafe : nativeint# array -> int32# -> int64x8 = "%caml_unboxed_nativeint_array_get512u_indexed_by_int32#"
  let unboxed_nativeint_array_get_int64x8_unsafe arr i = unboxed_nativeint_array_get_int64x8_unsafe arr (Stdlib_upstream_compatible.Int32_u.of_int i)

  external unboxed_nativeint_array_set_int64x8 : nativeint# array -> int32# -> int64x8 -> unit = "%caml_unboxed_nativeint_array_set512_indexed_by_int32#"
  let unboxed_nativeint_array_set_int64x8 arr i v = unboxed_nativeint_array_set_int64x8 arr (Stdlib_upstream_compatible.Int32_u.of_int i) v
  external unboxed_nativeint_array_set_int64x8_unsafe : nativeint# array -> int32# -> int64x8 -> unit = "%caml_unboxed_nativeint_array_set512u_indexed_by_int32#"
  let unboxed_nativeint_array_set_int64x8_unsafe arr i v = unboxed_nativeint_array_set_int64x8_unsafe arr (Stdlib_upstream_compatible.Int32_u.of_int i) v

  external unboxed_int32_array_get_int32x16 : int32# array -> int32# -> int32x16 = "%caml_unboxed_int32_array_get512_indexed_by_int32#"
  let unboxed_int32_array_get_int32x16 arr i = unboxed_int32_array_get_int32x16 arr (Stdlib_upstream_compatible.Int32_u.of_int i)
  external unboxed_int32_array_get_int32x16_unsafe : int32# array -> int32# -> int32x16 = "%caml_unboxed_int32_array_get512u_indexed_by_int32#"
  let unboxed_int32_array_get_int32x16_unsafe arr i = unboxed_int32_array_get_int32x16_unsafe arr (Stdlib_upstream_compatible.Int32_u.of_int i)

  external unboxed_int32_array_set_int32x16 : int32# array -> int32# -> int32x16 -> unit = "%caml_unboxed_int32_array_set512_indexed_by_int32#"
  let unboxed_int32_array_set_int32x16 arr i v = unboxed_int32_array_set_int32x16 arr (Stdlib_upstream_compatible.Int32_u.of_int i) v
  external unboxed_int32_array_set_int32x16_unsafe : int32# array -> int32# -> int32x16 -> unit = "%caml_unboxed_int32_array_set512u_indexed_by_int32#"
  let unboxed_int32_array_set_int32x16_unsafe arr i v = unboxed_int32_array_set_int32x16_unsafe arr (Stdlib_upstream_compatible.Int32_u.of_int i) v

  external untagged_int16_array_get_int16x32 : int16# array -> int32# -> int16x32 = "%caml_untagged_int16_array_get512_indexed_by_int32#"
  let untagged_int16_array_get_int16x32 arr i = untagged_int16_array_get_int16x32 arr (Stdlib_upstream_compatible.Int32_u.of_int i)
  external untagged_int16_array_get_int16x32_unsafe : int16# array -> int32# -> int16x32 = "%caml_untagged_int16_array_get512u_indexed_by_int32#"
  let untagged_int16_array_get_int16x32_unsafe arr i = untagged_int16_array_get_int16x32_unsafe arr (Stdlib_upstream_compatible.Int32_u.of_int i)

  external untagged_int16_array_set_int16x32 : int16# array -> int32# -> int16x32 -> unit = "%caml_untagged_int16_array_set512_indexed_by_int32#"
  let untagged_int16_array_set_int16x32 arr i v = untagged_int16_array_set_int16x32 arr (Stdlib_upstream_compatible.Int32_u.of_int i) v
  external untagged_int16_array_set_int16x32_unsafe : int16# array -> int32# -> int16x32 -> unit = "%caml_untagged_int16_array_set512u_indexed_by_int32#"
  let untagged_int16_array_set_int16x32_unsafe arr i v = untagged_int16_array_set_int16x32_unsafe arr (Stdlib_upstream_compatible.Int32_u.of_int i) v

  external untagged_int8_array_get_int8x64 : int8# array -> int32# -> int8x64 = "%caml_untagged_int8_array_get512_indexed_by_int32#"
  let untagged_int8_array_get_int8x64 arr i = untagged_int8_array_get_int8x64 arr (Stdlib_upstream_compatible.Int32_u.of_int i)
  external untagged_int8_array_get_int8x64_unsafe : int8# array -> int32# -> int8x64 = "%caml_untagged_int8_array_get512u_indexed_by_int32#"
  let untagged_int8_array_get_int8x64_unsafe arr i = untagged_int8_array_get_int8x64_unsafe arr (Stdlib_upstream_compatible.Int32_u.of_int i)

  external untagged_int8_array_set_int8x64 : int8# array -> int32# -> int8x64 -> unit = "%caml_untagged_int8_array_set512_indexed_by_int32#"
  let untagged_int8_array_set_int8x64 arr i v = untagged_int8_array_set_int8x64 arr (Stdlib_upstream_compatible.Int32_u.of_int i) v
  external untagged_int8_array_set_int8x64_unsafe : int8# array -> int32# -> int8x64 -> unit = "%caml_untagged_int8_array_set512u_indexed_by_int32#"
  let untagged_int8_array_set_int8x64_unsafe arr i v = untagged_int8_array_set_int8x64_unsafe arr (Stdlib_upstream_compatible.Int32_u.of_int i) v

end)

module _ = Int_arrays(struct

  external int_array_get_int64x8 : int array -> int64# -> int64x8 = "%caml_int_array_get512_indexed_by_int64#"
  let int_array_get_int64x8 arr i = int_array_get_int64x8 arr (Stdlib_upstream_compatible.Int64_u.of_int i)
  external int_array_get_int64x8_unsafe : int array -> int64# -> int64x8 = "%caml_int_array_get512u_indexed_by_int64#"
  let int_array_get_int64x8_unsafe arr i = int_array_get_int64x8_unsafe arr (Stdlib_upstream_compatible.Int64_u.of_int i)

  external int_iarray_get_int64x8 : int iarray -> int64# -> int64x8 = "%caml_int_array_get512_indexed_by_int64#"
  let int_iarray_get_int64x8 arr i = int_iarray_get_int64x8 arr (Stdlib_upstream_compatible.Int64_u.of_int i)
  external int_iarray_get_int64x8_unsafe : int iarray -> int64# -> int64x8 = "%caml_int_array_get512u_indexed_by_int64#"
  let int_iarray_get_int64x8_unsafe arr i = int_iarray_get_int64x8_unsafe arr (Stdlib_upstream_compatible.Int64_u.of_int i)

  external int_array_set_int64x8 : int array -> int64# -> int64x8 -> unit = "%caml_int_array_set512_indexed_by_int64#"
  let int_array_set_int64x8 arr i v = int_array_set_int64x8 arr (Stdlib_upstream_compatible.Int64_u.of_int i) v
  external int_array_set_int64x8_unsafe : int array -> int64# -> int64x8 -> unit = "%caml_int_array_set512u_indexed_by_int64#"
  let int_array_set_int64x8_unsafe arr i v = int_array_set_int64x8_unsafe arr (Stdlib_upstream_compatible.Int64_u.of_int i) v

  external unboxed_int64_array_get_int64x8 : int64# array -> int64# -> int64x8 = "%caml_unboxed_int64_array_get512_indexed_by_int64#"
  let unboxed_int64_array_get_int64x8 arr i = unboxed_int64_array_get_int64x8 arr (Stdlib_upstream_compatible.Int64_u.of_int i)
  external unboxed_int64_array_get_int64x8_unsafe : int64# array -> int64# -> int64x8 = "%caml_unboxed_int64_array_get512u_indexed_by_int64#"
  let unboxed_int64_array_get_int64x8_unsafe arr i = unboxed_int64_array_get_int64x8_unsafe arr (Stdlib_upstream_compatible.Int64_u.of_int i)

  external unboxed_int64_array_set_int64x8 : int64# array -> int64# -> int64x8 -> unit = "%caml_unboxed_int64_array_set512_indexed_by_int64#"
  let unboxed_int64_array_set_int64x8 arr i v = unboxed_int64_array_set_int64x8 arr (Stdlib_upstream_compatible.Int64_u.of_int i) v
  external unboxed_int64_array_set_int64x8_unsafe : int64# array -> int64# -> int64x8 -> unit = "%caml_unboxed_int64_array_set512u_indexed_by_int64#"
  let unboxed_int64_array_set_int64x8_unsafe arr i v = unboxed_int64_array_set_int64x8_unsafe arr (Stdlib_upstream_compatible.Int64_u.of_int i) v

  external unboxed_nativeint_array_get_int64x8 : nativeint# array -> int64# -> int64x8 = "%caml_unboxed_nativeint_array_get512_indexed_by_int64#"
  let unboxed_nativeint_array_get_int64x8 arr i = unboxed_nativeint_array_get_int64x8 arr (Stdlib_upstream_compatible.Int64_u.of_int i)
  external unboxed_nativeint_array_get_int64x8_unsafe : nativeint# array -> int64# -> int64x8 = "%caml_unboxed_nativeint_array_get512u_indexed_by_int64#"
  let unboxed_nativeint_array_get_int64x8_unsafe arr i = unboxed_nativeint_array_get_int64x8_unsafe arr (Stdlib_upstream_compatible.Int64_u.of_int i)

  external unboxed_nativeint_array_set_int64x8 : nativeint# array -> int64# -> int64x8 -> unit = "%caml_unboxed_nativeint_array_set512_indexed_by_int64#"
  let unboxed_nativeint_array_set_int64x8 arr i v = unboxed_nativeint_array_set_int64x8 arr (Stdlib_upstream_compatible.Int64_u.of_int i) v
  external unboxed_nativeint_array_set_int64x8_unsafe : nativeint# array -> int64# -> int64x8 -> unit = "%caml_unboxed_nativeint_array_set512u_indexed_by_int64#"
  let unboxed_nativeint_array_set_int64x8_unsafe arr i v = unboxed_nativeint_array_set_int64x8_unsafe arr (Stdlib_upstream_compatible.Int64_u.of_int i) v

  external unboxed_int32_array_get_int32x16 : int32# array -> int64# -> int32x16 = "%caml_unboxed_int32_array_get512_indexed_by_int64#"
  let unboxed_int32_array_get_int32x16 arr i = unboxed_int32_array_get_int32x16 arr (Stdlib_upstream_compatible.Int64_u.of_int i)
  external unboxed_int32_array_get_int32x16_unsafe : int32# array -> int64# -> int32x16 = "%caml_unboxed_int32_array_get512u_indexed_by_int64#"
  let unboxed_int32_array_get_int32x16_unsafe arr i = unboxed_int32_array_get_int32x16_unsafe arr (Stdlib_upstream_compatible.Int64_u.of_int i)

  external unboxed_int32_array_set_int32x16 : int32# array -> int64# -> int32x16 -> unit = "%caml_unboxed_int32_array_set512_indexed_by_int64#"
  let unboxed_int32_array_set_int32x16 arr i v = unboxed_int32_array_set_int32x16 arr (Stdlib_upstream_compatible.Int64_u.of_int i) v
  external unboxed_int32_array_set_int32x16_unsafe : int32# array -> int64# -> int32x16 -> unit = "%caml_unboxed_int32_array_set512u_indexed_by_int64#"
  let unboxed_int32_array_set_int32x16_unsafe arr i v = unboxed_int32_array_set_int32x16_unsafe arr (Stdlib_upstream_compatible.Int64_u.of_int i) v

  external untagged_int16_array_get_int16x32 : int16# array -> int64# -> int16x32 = "%caml_untagged_int16_array_get512_indexed_by_int64#"
  let untagged_int16_array_get_int16x32 arr i = untagged_int16_array_get_int16x32 arr (Stdlib_upstream_compatible.Int64_u.of_int i)
  external untagged_int16_array_get_int16x32_unsafe : int16# array -> int64# -> int16x32 = "%caml_untagged_int16_array_get512u_indexed_by_int64#"
  let untagged_int16_array_get_int16x32_unsafe arr i = untagged_int16_array_get_int16x32_unsafe arr (Stdlib_upstream_compatible.Int64_u.of_int i)

  external untagged_int16_array_set_int16x32 : int16# array -> int64# -> int16x32 -> unit = "%caml_untagged_int16_array_set512_indexed_by_int64#"
  let untagged_int16_array_set_int16x32 arr i v = untagged_int16_array_set_int16x32 arr (Stdlib_upstream_compatible.Int64_u.of_int i) v
  external untagged_int16_array_set_int16x32_unsafe : int16# array -> int64# -> int16x32 -> unit = "%caml_untagged_int16_array_set512u_indexed_by_int64#"
  let untagged_int16_array_set_int16x32_unsafe arr i v = untagged_int16_array_set_int16x32_unsafe arr (Stdlib_upstream_compatible.Int64_u.of_int i) v

  external untagged_int8_array_get_int8x64 : int8# array -> int64# -> int8x64 = "%caml_untagged_int8_array_get512_indexed_by_int64#"
  let untagged_int8_array_get_int8x64 arr i = untagged_int8_array_get_int8x64 arr (Stdlib_upstream_compatible.Int64_u.of_int i)
  external untagged_int8_array_get_int8x64_unsafe : int8# array -> int64# -> int8x64 = "%caml_untagged_int8_array_get512u_indexed_by_int64#"
  let untagged_int8_array_get_int8x64_unsafe arr i = untagged_int8_array_get_int8x64_unsafe arr (Stdlib_upstream_compatible.Int64_u.of_int i)

  external untagged_int8_array_set_int8x64 : int8# array -> int64# -> int8x64 -> unit = "%caml_untagged_int8_array_set512_indexed_by_int64#"
  let untagged_int8_array_set_int8x64 arr i v = untagged_int8_array_set_int8x64 arr (Stdlib_upstream_compatible.Int64_u.of_int i) v
  external untagged_int8_array_set_int8x64_unsafe : int8# array -> int64# -> int8x64 -> unit = "%caml_untagged_int8_array_set512u_indexed_by_int64#"
  let untagged_int8_array_set_int8x64_unsafe arr i v = untagged_int8_array_set_int8x64_unsafe arr (Stdlib_upstream_compatible.Int64_u.of_int i) v

end)

module _ = Int_arrays(struct

  external int_array_get_int64x8 : int array -> nativeint# -> int64x8 = "%caml_int_array_get512_indexed_by_nativeint#"
  let int_array_get_int64x8 arr i = int_array_get_int64x8 arr (Stdlib_upstream_compatible.Nativeint_u.of_int i)
  external int_array_get_int64x8_unsafe : int array -> nativeint# -> int64x8 = "%caml_int_array_get512u_indexed_by_nativeint#"
  let int_array_get_int64x8_unsafe arr i = int_array_get_int64x8_unsafe arr (Stdlib_upstream_compatible.Nativeint_u.of_int i)

  external int_iarray_get_int64x8 : int iarray -> nativeint# -> int64x8 = "%caml_int_array_get512_indexed_by_nativeint#"
  let int_iarray_get_int64x8 arr i = int_iarray_get_int64x8 arr (Stdlib_upstream_compatible.Nativeint_u.of_int i)
  external int_iarray_get_int64x8_unsafe : int iarray -> nativeint# -> int64x8 = "%caml_int_array_get512u_indexed_by_nativeint#"
  let int_iarray_get_int64x8_unsafe arr i = int_iarray_get_int64x8_unsafe arr (Stdlib_upstream_compatible.Nativeint_u.of_int i)

  external int_array_set_int64x8 : int array -> nativeint# -> int64x8 -> unit = "%caml_int_array_set512_indexed_by_nativeint#"
  let int_array_set_int64x8 arr i v = int_array_set_int64x8 arr (Stdlib_upstream_compatible.Nativeint_u.of_int i) v
  external int_array_set_int64x8_unsafe : int array -> nativeint# -> int64x8 -> unit = "%caml_int_array_set512u_indexed_by_nativeint#"
  let int_array_set_int64x8_unsafe arr i v = int_array_set_int64x8_unsafe arr (Stdlib_upstream_compatible.Nativeint_u.of_int i) v

  external unboxed_int64_array_get_int64x8 : int64# array -> nativeint# -> int64x8 = "%caml_unboxed_int64_array_get512_indexed_by_nativeint#"
  let unboxed_int64_array_get_int64x8 arr i = unboxed_int64_array_get_int64x8 arr (Stdlib_upstream_compatible.Nativeint_u.of_int i)
  external unboxed_int64_array_get_int64x8_unsafe : int64# array -> nativeint# -> int64x8 = "%caml_unboxed_int64_array_get512u_indexed_by_nativeint#"
  let unboxed_int64_array_get_int64x8_unsafe arr i = unboxed_int64_array_get_int64x8_unsafe arr (Stdlib_upstream_compatible.Nativeint_u.of_int i)

  external unboxed_int64_array_set_int64x8 : int64# array -> nativeint# -> int64x8 -> unit = "%caml_unboxed_int64_array_set512_indexed_by_nativeint#"
  let unboxed_int64_array_set_int64x8 arr i v = unboxed_int64_array_set_int64x8 arr (Stdlib_upstream_compatible.Nativeint_u.of_int i) v
  external unboxed_int64_array_set_int64x8_unsafe : int64# array -> nativeint# -> int64x8 -> unit = "%caml_unboxed_int64_array_set512u_indexed_by_nativeint#"
  let unboxed_int64_array_set_int64x8_unsafe arr i v = unboxed_int64_array_set_int64x8_unsafe arr (Stdlib_upstream_compatible.Nativeint_u.of_int i) v

  external unboxed_nativeint_array_get_int64x8 : nativeint# array -> nativeint# -> int64x8 = "%caml_unboxed_nativeint_array_get512_indexed_by_nativeint#"
  let unboxed_nativeint_array_get_int64x8 arr i = unboxed_nativeint_array_get_int64x8 arr (Stdlib_upstream_compatible.Nativeint_u.of_int i)
  external unboxed_nativeint_array_get_int64x8_unsafe : nativeint# array -> nativeint# -> int64x8 = "%caml_unboxed_nativeint_array_get512u_indexed_by_nativeint#"
  let unboxed_nativeint_array_get_int64x8_unsafe arr i = unboxed_nativeint_array_get_int64x8_unsafe arr (Stdlib_upstream_compatible.Nativeint_u.of_int i)

  external unboxed_nativeint_array_set_int64x8 : nativeint# array -> nativeint# -> int64x8 -> unit = "%caml_unboxed_nativeint_array_set512_indexed_by_nativeint#"
  let unboxed_nativeint_array_set_int64x8 arr i v = unboxed_nativeint_array_set_int64x8 arr (Stdlib_upstream_compatible.Nativeint_u.of_int i) v
  external unboxed_nativeint_array_set_int64x8_unsafe : nativeint# array -> nativeint# -> int64x8 -> unit = "%caml_unboxed_nativeint_array_set512u_indexed_by_nativeint#"
  let unboxed_nativeint_array_set_int64x8_unsafe arr i v = unboxed_nativeint_array_set_int64x8_unsafe arr (Stdlib_upstream_compatible.Nativeint_u.of_int i) v

  external unboxed_int32_array_get_int32x16 : int32# array -> nativeint# -> int32x16 = "%caml_unboxed_int32_array_get512_indexed_by_nativeint#"
  let unboxed_int32_array_get_int32x16 arr i = unboxed_int32_array_get_int32x16 arr (Stdlib_upstream_compatible.Nativeint_u.of_int i)
  external unboxed_int32_array_get_int32x16_unsafe : int32# array -> nativeint# -> int32x16 = "%caml_unboxed_int32_array_get512u_indexed_by_nativeint#"
  let unboxed_int32_array_get_int32x16_unsafe arr i = unboxed_int32_array_get_int32x16_unsafe arr (Stdlib_upstream_compatible.Nativeint_u.of_int i)

  external unboxed_int32_array_set_int32x16 : int32# array -> nativeint# -> int32x16 -> unit = "%caml_unboxed_int32_array_set512_indexed_by_nativeint#"
  let unboxed_int32_array_set_int32x16 arr i v = unboxed_int32_array_set_int32x16 arr (Stdlib_upstream_compatible.Nativeint_u.of_int i) v
  external unboxed_int32_array_set_int32x16_unsafe : int32# array -> nativeint# -> int32x16 -> unit = "%caml_unboxed_int32_array_set512u_indexed_by_nativeint#"
  let unboxed_int32_array_set_int32x16_unsafe arr i v = unboxed_int32_array_set_int32x16_unsafe arr (Stdlib_upstream_compatible.Nativeint_u.of_int i) v

  external untagged_int16_array_get_int16x32 : int16# array -> nativeint# -> int16x32 = "%caml_untagged_int16_array_get512_indexed_by_nativeint#"
  let untagged_int16_array_get_int16x32 arr i = untagged_int16_array_get_int16x32 arr (Stdlib_upstream_compatible.Nativeint_u.of_int i)
  external untagged_int16_array_get_int16x32_unsafe : int16# array -> nativeint# -> int16x32 = "%caml_untagged_int16_array_get512u_indexed_by_nativeint#"
  let untagged_int16_array_get_int16x32_unsafe arr i = untagged_int16_array_get_int16x32_unsafe arr (Stdlib_upstream_compatible.Nativeint_u.of_int i)

  external untagged_int16_array_set_int16x32 : int16# array -> nativeint# -> int16x32 -> unit = "%caml_untagged_int16_array_set512_indexed_by_nativeint#"
  let untagged_int16_array_set_int16x32 arr i v = untagged_int16_array_set_int16x32 arr (Stdlib_upstream_compatible.Nativeint_u.of_int i) v
  external untagged_int16_array_set_int16x32_unsafe : int16# array -> nativeint# -> int16x32 -> unit = "%caml_untagged_int16_array_set512u_indexed_by_nativeint#"
  let untagged_int16_array_set_int16x32_unsafe arr i v = untagged_int16_array_set_int16x32_unsafe arr (Stdlib_upstream_compatible.Nativeint_u.of_int i) v

  external untagged_int8_array_get_int8x64 : int8# array -> nativeint# -> int8x64 = "%caml_untagged_int8_array_get512_indexed_by_nativeint#"
  let untagged_int8_array_get_int8x64 arr i = untagged_int8_array_get_int8x64 arr (Stdlib_upstream_compatible.Nativeint_u.of_int i)
  external untagged_int8_array_get_int8x64_unsafe : int8# array -> nativeint# -> int8x64 = "%caml_untagged_int8_array_get512u_indexed_by_nativeint#"
  let untagged_int8_array_get_int8x64_unsafe arr i = untagged_int8_array_get_int8x64_unsafe arr (Stdlib_upstream_compatible.Nativeint_u.of_int i)

  external untagged_int8_array_set_int8x64 : int8# array -> nativeint# -> int8x64 -> unit = "%caml_untagged_int8_array_set512_indexed_by_nativeint#"
  let untagged_int8_array_set_int8x64 arr i v = untagged_int8_array_set_int8x64 arr (Stdlib_upstream_compatible.Nativeint_u.of_int i) v
  external untagged_int8_array_set_int8x64_unsafe : int8# array -> nativeint# -> int8x64 -> unit = "%caml_untagged_int8_array_set512u_indexed_by_nativeint#"
  let untagged_int8_array_set_int8x64_unsafe arr i v = untagged_int8_array_set_int8x64_unsafe arr (Stdlib_upstream_compatible.Nativeint_u.of_int i) v

end)
