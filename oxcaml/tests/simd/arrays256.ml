open Stdlib
open! Stdlib_stable

[@@@ocaml.warning "-unused-value-declaration"]
[@@@ocaml.warning "-unused-module"]

external int8x32_of_int64s : int64 -> int64 -> int64 -> int64 -> int8x32 = "" "vec256_of_int64s" [@@noalloc] [@@unboxed]
external int8x32_first_int64 : int8x32 -> int64 = "" "vec256_first_int64" [@@noalloc] [@@unboxed]
external int8x32_second_int64 : int8x32 -> int64 = "" "vec256_second_int64" [@@noalloc] [@@unboxed]
external int8x32_third_int64 : int8x32 -> int64 = "" "vec256_third_int64" [@@noalloc] [@@unboxed]
external int8x32_fourth_int64 : int8x32 -> int64 = "" "vec256_fourth_int64" [@@noalloc] [@@unboxed]

external int64x4_of_int64s : int64 -> int64 -> int64 -> int64 -> int64x4 = "" "vec256_of_int64s" [@@noalloc] [@@unboxed]
external int64x4_first_int64 : int64x4 -> int64 = "" "vec256_first_int64" [@@noalloc] [@@unboxed]
external int64x4_second_int64 : int64x4 -> int64 = "" "vec256_second_int64" [@@noalloc] [@@unboxed]
external int64x4_third_int64 : int64x4 -> int64 = "" "vec256_third_int64" [@@noalloc] [@@unboxed]
external int64x4_fourth_int64 : int64x4 -> int64 = "" "vec256_fourth_int64" [@@noalloc] [@@unboxed]

external int32x8_of_int64s : int64 -> int64 -> int64 -> int64 -> int32x8 = "" "vec256_of_int64s" [@@noalloc] [@@unboxed]
external int32x8_first_int64 : int32x8 -> int64 = "" "vec256_first_int64" [@@noalloc] [@@unboxed]
external int32x8_second_int64 : int32x8 -> int64 = "" "vec256_second_int64" [@@noalloc] [@@unboxed]
external int32x8_third_int64 : int32x8 -> int64 = "" "vec256_third_int64" [@@noalloc] [@@unboxed]
external int32x8_fourth_int64 : int32x8 -> int64 = "" "vec256_fourth_int64" [@@noalloc] [@@unboxed]

external float64x4_first_int64 : float64x4 -> int64 = "" "vec256_first_int64" [@@noalloc] [@@unboxed]
external float64x4_second_int64 : float64x4 -> int64 = "" "vec256_second_int64" [@@noalloc] [@@unboxed]
external float64x4_third_int64 : float64x4 -> int64 = "" "vec256_third_int64" [@@noalloc] [@@unboxed]
external float64x4_fourth_int64 : float64x4 -> int64 = "" "vec256_fourth_int64" [@@noalloc] [@@unboxed]

external float32x8_first_int64 : float32x8 -> int64 = "" "vec256_first_int64" [@@noalloc] [@@unboxed]
external float32x8_second_int64 : float32x8 -> int64 = "" "vec256_second_int64" [@@noalloc] [@@unboxed]
external float32x8_third_int64 : float32x8 -> int64 = "" "vec256_third_int64" [@@noalloc] [@@unboxed]
external float32x8_fourth_int64 : float32x8 -> int64 = "" "vec256_fourth_int64" [@@noalloc] [@@unboxed]

external float64x4_of_int64s : int64 -> int64 -> int64 -> int64 -> float64x4 = "" "vec256_of_int64s" [@@noalloc] [@@unboxed]
external float32x8_of_int64s : int64 -> int64 -> int64 -> int64 -> float32x8 = "" "vec256_of_int64s" [@@noalloc] [@@unboxed]

let eq av bv cv dv a b c d =
  if a <> av then Printf.printf "%016Lx <> %016Lx\n" av a;
  if b <> bv then Printf.printf "%016Lx <> %016Lx\n" bv b;
  if c <> cv then Printf.printf "%016Lx <> %016Lx\n" cv c;
  if d <> dv then Printf.printf "%016Lx <> %016Lx\n" dv d
;;

let assert_raises_out_of_bounds thunk =
  try
    thunk ();
    assert false
  with
  | Invalid_argument s when s = "index out of bounds" -> ()
  | _ -> assert false
;;

let test_data = "\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0a\x0b\x0c\x0d\x0e\x0f\x10\x11\x12\x13\x14\x15\x16\x17\x18\x19\x1a\x1b\x1c\x1d\x1e\x1f\x00\x01\x02\x03\x04\x05\x06\x07"

module Bytes (Primitives : sig
  val get_int8x32_unaligned : bytes -> int -> int8x32
  val get_int8x32_unaligned_unsafe : bytes -> int -> int8x32
  val set_int8x32_unaligned : bytes -> int -> int8x32 -> unit
  val set_int8x32_unaligned_unsafe : bytes -> int -> int8x32 -> unit
  val extra_checks : bytes -> unit
end) =
struct
  open Primitives

  let data = Bytes.of_string test_data

  let first = 0x0706050403020100L
  let second = 0x0f0e0d0c0b0a0908L
  let third = 0x1716151413121110L
  let fourth = 0x1f1e1d1c1b1a1918L

  (* Getters *)

  let () =
    let v = get_int8x32_unaligned data 0 in
    eq first second third fourth (int8x32_first_int64 v) (int8x32_second_int64 v) (int8x32_third_int64 v) (int8x32_fourth_int64 v);
    let v = get_int8x32_unaligned_unsafe data 0 in
    eq first second third fourth (int8x32_first_int64 v) (int8x32_second_int64 v) (int8x32_third_int64 v) (int8x32_fourth_int64 v);
    let v = get_int8x32_unaligned data 8 in
    eq second third fourth first (int8x32_first_int64 v) (int8x32_second_int64 v) (int8x32_third_int64 v) (int8x32_fourth_int64 v);
    let v = get_int8x32_unaligned_unsafe data 8 in
    eq second third fourth first (int8x32_first_int64 v) (int8x32_second_int64 v) (int8x32_third_int64 v) (int8x32_fourth_int64 v);
  ;;

  let () =
    for bad = 9 to 40 do
      try
        let _ = get_int8x32_unaligned data bad in
        assert false
      with | Invalid_argument s when s = "index out of bounds" -> ()
    done;
  ;;

  (* Setters *)

  let set_unaligned first second third fourth offset =
    let set = int8x32_of_int64s first second third fourth in
    set_int8x32_unaligned data offset set;
    let v = get_int8x32_unaligned data offset in
    eq first second third fourth (int8x32_first_int64 v) (int8x32_second_int64 v) (int8x32_third_int64 v) (int8x32_fourth_int64 v);
  ;;

  let set_unaligned_unsafe first second third fourth offset =
    let set = int8x32_of_int64s first second third fourth in
    set_int8x32_unaligned_unsafe data offset set;
    let v = get_int8x32_unaligned data offset in
    eq first second third fourth (int8x32_first_int64 v) (int8x32_second_int64 v) (int8x32_third_int64 v) (int8x32_fourth_int64 v);
  ;;

  let () =
    set_unaligned 0x1010101010101010L 0x1010101010101010L 0x1010101010101010L 0x1010101010101010L 0;
    set_unaligned 0x2020202020202020L 0x2020202020202020L 0x2020202020202020L 0x2020202020202020L 8;
    set_unaligned_unsafe 0x3030303030303030L 0x3030303030303030L 0x3030303030303030L 0x3030303030303030L 0;
    set_unaligned_unsafe 0x4040404040404040L 0x4040404040404040L 0x4040404040404040L 0x4040404040404040L 8;
    Random.init 1234;
    for _ = 1 to 1000 do
      set_unaligned (Random.int64 Int64.max_int) (Random.int64 Int64.max_int) (Random.int64 Int64.max_int) (Random.int64 Int64.max_int) (Random.int 9);
      set_unaligned_unsafe (Random.int64 Int64.max_int) (Random.int64 Int64.max_int) (Random.int64 Int64.max_int) (Random.int64 Int64.max_int) (Random.int 9)
    done;
  ;;

  let () =
    let set = int8x32_of_int64s 0xFFFFFFFFFFFFFFFFL 0xFFFFFFFFFFFFFFFFL 0xFFFFFFFFFFFFFFFFL 0xFFFFFFFFFFFFFFFFL in
    for bad = 9 to 40 do
      try
        let _ = set_int8x32_unaligned data bad set in
        assert false
      with | Invalid_argument s when s = "index out of bounds" -> ()
    done;
  ;;

  (* Extra checks *)

  let () = extra_checks data
end

module _ = Bytes(struct
  external get_int8x32_unaligned : bytes -> int -> int8x32 = "%caml_bytes_getu256"
  external get_int8x32_unaligned_unsafe : bytes -> int -> int8x32 = "%caml_bytes_getu256u"

  external set_int8x32_unaligned : bytes -> int -> int8x32 -> unit = "%caml_bytes_setu256"
  external set_int8x32_unaligned_unsafe : bytes -> int -> int8x32 -> unit = "%caml_bytes_setu256u"

  let extra_checks bytes =
    List.iter
      (fun index ->
        assert_raises_out_of_bounds (fun () ->
          let _ = get_int8x32_unaligned bytes index in
          ());
        assert_raises_out_of_bounds (fun () ->
          set_int8x32_unaligned bytes index (int8x32_of_int64s 1L 2L 3L 4L)))
      Int.[ min_int; add min_int one; sub zero one; max_int ]
  ;;
end)

module _ = Bytes(struct
  external get_int8x32_unaligned_prim : bytes -> int32# -> int8x32 = "%caml_bytes_getu256_indexed_by_int32#"
  let get_int8x32_unaligned b i = get_int8x32_unaligned_prim b (Stdlib_upstream_compatible.Int32_u.of_int i)
  external get_int8x32_unaligned_unsafe_prim : bytes -> int32# -> int8x32 = "%caml_bytes_getu256u_indexed_by_int32#"
  let get_int8x32_unaligned_unsafe b i = get_int8x32_unaligned_unsafe_prim b (Stdlib_upstream_compatible.Int32_u.of_int i)

  external set_int8x32_unaligned_prim : bytes -> int32# -> int8x32 -> unit = "%caml_bytes_setu256_indexed_by_int32#"
  let set_int8x32_unaligned b i v = set_int8x32_unaligned_prim b (Stdlib_upstream_compatible.Int32_u.of_int i) v
  external set_int8x32_unaligned_unsafe_prim : bytes -> int32# -> int8x32 -> unit = "%caml_bytes_setu256u_indexed_by_int32#"
  let set_int8x32_unaligned_unsafe b i v = set_int8x32_unaligned_unsafe_prim b (Stdlib_upstream_compatible.Int32_u.of_int i) v

  let extra_checks bytes =
    List.iter
      (fun index ->
        let index = Stdlib_upstream_compatible.Int32_u.of_int32 index in
        assert_raises_out_of_bounds (fun () ->
          let _ = get_int8x32_unaligned_prim bytes index in
          ());
        assert_raises_out_of_bounds (fun () ->
          set_int8x32_unaligned_prim bytes index (int8x32_of_int64s 1L 2L 3L 4L)))
      Int32.[ min_int; add min_int one; sub zero one; max_int ]
  ;;
end)

module _ = Bytes(struct
  external get_int8x32_unaligned_prim : bytes -> int64# -> int8x32 = "%caml_bytes_getu256_indexed_by_int64#"
  let get_int8x32_unaligned b i = get_int8x32_unaligned_prim b (Stdlib_upstream_compatible.Int64_u.of_int i)
  external get_int8x32_unaligned_unsafe_prim : bytes -> int64# -> int8x32 = "%caml_bytes_getu256u_indexed_by_int64#"
  let get_int8x32_unaligned_unsafe b i = get_int8x32_unaligned_unsafe_prim b (Stdlib_upstream_compatible.Int64_u.of_int i)

  external set_int8x32_unaligned_prim : bytes -> int64# -> int8x32 -> unit = "%caml_bytes_setu256_indexed_by_int64#"
  let set_int8x32_unaligned b i v = set_int8x32_unaligned_prim b (Stdlib_upstream_compatible.Int64_u.of_int i) v
  external set_int8x32_unaligned_unsafe_prim : bytes -> int64# -> int8x32 -> unit = "%caml_bytes_setu256u_indexed_by_int64#"
  let set_int8x32_unaligned_unsafe b i v = set_int8x32_unaligned_unsafe_prim b (Stdlib_upstream_compatible.Int64_u.of_int i) v

  let extra_checks bytes =
    List.iter
      (fun index ->
        let index = Stdlib_upstream_compatible.Int64_u.of_int64 index in
        assert_raises_out_of_bounds (fun () ->
          let _ = get_int8x32_unaligned_prim bytes index in
          ());
        assert_raises_out_of_bounds (fun () ->
          set_int8x32_unaligned_prim bytes index (int8x32_of_int64s 1L 2L 3L 4L)))
      Int64.[ min_int; add min_int one; sub zero one; max_int ]
  ;;
end)

module _ = Bytes(struct
  external get_int8x32_unaligned_prim : bytes -> nativeint# -> int8x32 = "%caml_bytes_getu256_indexed_by_nativeint#"
  let get_int8x32_unaligned b i = get_int8x32_unaligned_prim b (Stdlib_upstream_compatible.Nativeint_u.of_int i)
  external get_int8x32_unaligned_unsafe_prim : bytes -> nativeint# -> int8x32 = "%caml_bytes_getu256u_indexed_by_nativeint#"
  let get_int8x32_unaligned_unsafe b i = get_int8x32_unaligned_unsafe_prim b (Stdlib_upstream_compatible.Nativeint_u.of_int i)

  external set_int8x32_unaligned_prim : bytes -> nativeint# -> int8x32 -> unit = "%caml_bytes_setu256_indexed_by_nativeint#"
  let set_int8x32_unaligned b i v = set_int8x32_unaligned_prim b (Stdlib_upstream_compatible.Nativeint_u.of_int i) v
  external set_int8x32_unaligned_unsafe_prim : bytes -> nativeint# -> int8x32 -> unit = "%caml_bytes_setu256u_indexed_by_nativeint#"
  let set_int8x32_unaligned_unsafe b i v = set_int8x32_unaligned_unsafe_prim b (Stdlib_upstream_compatible.Nativeint_u.of_int i) v

  let extra_checks bytes =
    List.iter
      (fun index ->
        let index = Stdlib_upstream_compatible.Nativeint_u.of_nativeint index in
        assert_raises_out_of_bounds (fun () ->
          let _ = get_int8x32_unaligned_prim bytes index in
          ());
        assert_raises_out_of_bounds (fun () ->
          set_int8x32_unaligned_prim bytes index (int8x32_of_int64s 1L 2L 3L 4L)))
      Nativeint.[ min_int; add min_int one; sub zero one; max_int ]
  ;;
end)

module String_ (Primitives : sig
  val get_int8x32_unaligned : string -> int -> int8x32
  val get_int8x32_unaligned_unsafe : string -> int -> int8x32
  val extra_checks : string -> unit
end) =
struct
  open Primitives

  let data = test_data

  let first = 0x0706050403020100L
  let second = 0x0f0e0d0c0b0a0908L
  let third = 0x1716151413121110L
  let fourth = 0x1f1e1d1c1b1a1918L

  (* Getters *)

  let () =
    let v = get_int8x32_unaligned data 0 in
    eq first second third fourth (int8x32_first_int64 v) (int8x32_second_int64 v) (int8x32_third_int64 v) (int8x32_fourth_int64 v);
    let v = get_int8x32_unaligned_unsafe data 0 in
    eq first second third fourth (int8x32_first_int64 v) (int8x32_second_int64 v) (int8x32_third_int64 v) (int8x32_fourth_int64 v);
    let v = get_int8x32_unaligned data 8 in
    eq second third fourth first (int8x32_first_int64 v) (int8x32_second_int64 v) (int8x32_third_int64 v) (int8x32_fourth_int64 v);
    let v = get_int8x32_unaligned_unsafe data 8 in
    eq second third fourth first (int8x32_first_int64 v) (int8x32_second_int64 v) (int8x32_third_int64 v) (int8x32_fourth_int64 v);
  ;;

  let () =
    for bad = 9 to 40 do
      try
        let _ = get_int8x32_unaligned data bad in
        assert false
      with | Invalid_argument s when s = "index out of bounds" -> ()
    done;
  ;;

  (* Extra checks *)

  let () = extra_checks data
end

module _ = String_(struct
  external get_int8x32_unaligned : string -> int -> int8x32 = "%caml_string_getu256"
  external get_int8x32_unaligned_unsafe : string -> int -> int8x32 = "%caml_string_getu256u"

  let extra_checks string =
    List.iter
      (fun index ->
        assert_raises_out_of_bounds (fun () ->
          let _ = get_int8x32_unaligned string index in
          ()))
      Int.[ min_int; add min_int one; sub zero one; max_int ]
  ;;
end)

module _ = String_(struct
  external get_int8x32_unaligned_prim : string -> int32# -> int8x32 = "%caml_string_getu256_indexed_by_int32#"
  let get_int8x32_unaligned b i = get_int8x32_unaligned_prim b (Stdlib_upstream_compatible.Int32_u.of_int i)
  external get_int8x32_unaligned_unsafe_prim : string -> int32# -> int8x32 = "%caml_string_getu256u_indexed_by_int32#"
  let get_int8x32_unaligned_unsafe b i = get_int8x32_unaligned_unsafe_prim b (Stdlib_upstream_compatible.Int32_u.of_int i)

  let extra_checks string =
    List.iter
      (fun index ->
        let index = Stdlib_upstream_compatible.Int32_u.of_int32 index in
        assert_raises_out_of_bounds (fun () ->
          let _ = get_int8x32_unaligned_prim string index in
          ()))
      Int32.[ min_int; add min_int one; sub zero one; max_int ]
  ;;
end)

module _ = String_(struct
  external get_int8x32_unaligned_prim : string -> int64# -> int8x32 = "%caml_string_getu256_indexed_by_int64#"
  let get_int8x32_unaligned b i = get_int8x32_unaligned_prim b (Stdlib_upstream_compatible.Int64_u.of_int i)
  external get_int8x32_unaligned_unsafe_prim : string -> int64# -> int8x32 = "%caml_string_getu256u_indexed_by_int64#"
  let get_int8x32_unaligned_unsafe b i = get_int8x32_unaligned_unsafe_prim b (Stdlib_upstream_compatible.Int64_u.of_int i)

  let extra_checks string =
    List.iter
      (fun index ->
        let index = Stdlib_upstream_compatible.Int64_u.of_int64 index in
        assert_raises_out_of_bounds (fun () ->
          let _ = get_int8x32_unaligned_prim string index in
          ()))
      Int64.[ min_int; add min_int one; sub zero one; max_int ]
  ;;
end)

module _ = String_(struct
  external get_int8x32_unaligned_prim : string -> nativeint# -> int8x32 = "%caml_string_getu256_indexed_by_nativeint#"
  let get_int8x32_unaligned b i = get_int8x32_unaligned_prim b (Stdlib_upstream_compatible.Nativeint_u.of_int i)
  external get_int8x32_unaligned_unsafe_prim : string -> nativeint# -> int8x32 = "%caml_string_getu256u_indexed_by_nativeint#"
  let get_int8x32_unaligned_unsafe b i = get_int8x32_unaligned_unsafe_prim b (Stdlib_upstream_compatible.Nativeint_u.of_int i)

  let extra_checks string =
    List.iter
      (fun index ->
        let index = Stdlib_upstream_compatible.Nativeint_u.of_nativeint index in
        assert_raises_out_of_bounds (fun () ->
          let _ = get_int8x32_unaligned_prim string index in
          ()))
      Nativeint.[ min_int; add min_int one; sub zero one; max_int ]
  ;;
end)

open struct
  open Bigarray
  type bigstring = (char, int8_unsigned_elt, c_layout) Array1.t

  module Bigstring (Primitives : sig
    val get_int8x32_unaligned : bigstring -> int -> int8x32
    val get_int8x32_unaligned_unsafe : bigstring -> int -> int8x32
    val get_int8x32_aligned : bigstring -> int -> int8x32
    val get_int8x32_aligned_unsafe : bigstring -> int -> int8x32

    val set_int8x32_unaligned : bigstring -> int -> int8x32 -> unit
    val set_int8x32_unaligned_unsafe : bigstring -> int -> int8x32 -> unit
    val set_int8x32_aligned : bigstring -> int -> int8x32 -> unit
    val set_int8x32_aligned_unsafe : bigstring -> int -> int8x32 -> unit

    val extra_checks : bigstring -> unit
  end) =
  struct
    open Primitives

    let bigstring_of_string s =
      let a = Array1.create char c_layout (String.length s + 16) in
      let zero =
        try let _ = get_int8x32_aligned a 0 in 0 with
        | _ -> 16
      in
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

    (* Getters *)

    let () =
      let v = get_int8x32_unaligned data 0 in
      eq first second third fourth (int8x32_first_int64 v) (int8x32_second_int64 v) (int8x32_third_int64 v) (int8x32_fourth_int64 v);
      let v = get_int8x32_unaligned_unsafe data 0 in
      eq first second third fourth (int8x32_first_int64 v) (int8x32_second_int64 v) (int8x32_third_int64 v) (int8x32_fourth_int64 v);
      let v = get_int8x32_unaligned data 8 in
      eq second third fourth first (int8x32_first_int64 v) (int8x32_second_int64 v) (int8x32_third_int64 v) (int8x32_fourth_int64 v);
      let v = get_int8x32_unaligned_unsafe data 8 in
      eq second third fourth first (int8x32_first_int64 v) (int8x32_second_int64 v) (int8x32_third_int64 v) (int8x32_fourth_int64 v);
    ;;

    let () =
      for bad = 9 to 40 do
        try
          let _ = get_int8x32_unaligned data bad in
          assert false
        with | Invalid_argument s when s = "index out of bounds" -> ()
      done;
    ;;

    let () =
      let v = get_int8x32_aligned data 0 in
      eq first second third fourth (int8x32_first_int64 v) (int8x32_second_int64 v) (int8x32_third_int64 v) (int8x32_fourth_int64 v);
      let v = get_int8x32_aligned_unsafe data 0 in
      eq first second third fourth (int8x32_first_int64 v) (int8x32_second_int64 v) (int8x32_third_int64 v) (int8x32_fourth_int64 v);
      for bad = 1 to 8 do
        try
          let _ = get_int8x32_aligned data bad in
          assert false
        with | Invalid_argument s when s = "address was misaligned" -> ()
      done;
      for bad = 9 to 40 do
        try
          let _ = get_int8x32_aligned data bad in
          assert false
        with | Invalid_argument s when s = "index out of bounds" -> ()
      done;
    ;;

    (* Setters *)

    let set_unaligned first second third fourth offset =
      let set = int8x32_of_int64s first second third fourth in
      set_int8x32_unaligned data offset set;
      let v = get_int8x32_unaligned data offset in
      eq first second third fourth (int8x32_first_int64 v) (int8x32_second_int64 v) (int8x32_third_int64 v) (int8x32_fourth_int64 v);
    ;;

    let set_unaligned_unsafe first second third fourth offset =
      let set = int8x32_of_int64s first second third fourth in
      set_int8x32_unaligned_unsafe data offset set;
      let v = get_int8x32_unaligned data offset in
      eq first second third fourth (int8x32_first_int64 v) (int8x32_second_int64 v) (int8x32_third_int64 v) (int8x32_fourth_int64 v);
    ;;

    let set_aligned first second third fourth offset =
      let set = int8x32_of_int64s first second third fourth in
      set_int8x32_aligned data offset set;
      let v = get_int8x32_aligned data offset in
      eq first second third fourth (int8x32_first_int64 v) (int8x32_second_int64 v) (int8x32_third_int64 v) (int8x32_fourth_int64 v);
    ;;

    let set_aligned_unsafe first second third fourth offset =
      let set = int8x32_of_int64s first second third fourth in
      set_int8x32_aligned_unsafe data offset set;
      let v = get_int8x32_aligned_unsafe data offset in
      eq first second third fourth (int8x32_first_int64 v) (int8x32_second_int64 v) (int8x32_third_int64 v) (int8x32_fourth_int64 v);
    ;;

    let () =
      set_unaligned 0x1010101010101010L 0x1010101010101010L 0x1010101010101010L 0x1010101010101010L 0;
      set_unaligned 0x2020202020202020L 0x2020202020202020L 0x2020202020202020L 0x2020202020202020L 8;
      set_unaligned_unsafe 0x3030303030303030L 0x3030303030303030L 0x3030303030303030L 0x3030303030303030L 0;
      set_unaligned_unsafe 0x4040404040404040L 0x4040404040404040L 0x4040404040404040L 0x4040404040404040L 8;
      set_aligned 0x5050505050505050L 0x5050505050505050L 0x5050505050505050L 0x5050505050505050L 0;
      set_aligned_unsafe 0x6060606060606060L 0x6060606060606060L 0x6060606060606060L 0x6060606060606060L 0;
      Random.init 1234;
      for _ = 1 to 1000 do
        set_unaligned (Random.int64 Int64.max_int) (Random.int64 Int64.max_int) (Random.int64 Int64.max_int) (Random.int64 Int64.max_int) (Random.int 9);
        set_unaligned_unsafe (Random.int64 Int64.max_int) (Random.int64 Int64.max_int) (Random.int64 Int64.max_int) (Random.int64 Int64.max_int) (Random.int 9);
        set_aligned (Random.int64 Int64.max_int) (Random.int64 Int64.max_int) (Random.int64 Int64.max_int) (Random.int64 Int64.max_int) 0;
        set_aligned_unsafe (Random.int64 Int64.max_int) (Random.int64 Int64.max_int) (Random.int64 Int64.max_int) (Random.int64 Int64.max_int) 0;
      done;
    ;;

    let () =
      let set = int8x32_of_int64s 0xFFFFFFFFFFFFFFFFL 0xFFFFFFFFFFFFFFFFL 0xFFFFFFFFFFFFFFFFL 0xFFFFFFFFFFFFFFFFL in
      for bad = 1 to 8 do
        try
          let _ = set_int8x32_aligned data bad set in
          assert false
        with | Invalid_argument s when s = "address was misaligned" -> ()
      done;
      for bad = 9 to 40 do
        try
          let _ = get_int8x32_aligned data bad in
          assert false
        with | Invalid_argument s when s = "index out of bounds" -> ()
      done;
    ;;

    (* Extra checks *)

    let () = extra_checks data
  end

  module _ = Bigstring(struct
    external get_int8x32_unaligned : bigstring -> int -> int8x32 = "%caml_bigstring_getu256"
    external get_int8x32_unaligned_unsafe : bigstring -> int -> int8x32 = "%caml_bigstring_getu256u"
    external get_int8x32_aligned : bigstring -> int -> int8x32 = "%caml_bigstring_geta256"
    external get_int8x32_aligned_unsafe : bigstring -> int -> int8x32 = "%caml_bigstring_geta256u"

    external set_int8x32_unaligned : bigstring -> int -> int8x32 -> unit = "%caml_bigstring_setu256"
    external set_int8x32_unaligned_unsafe : bigstring -> int -> int8x32 -> unit = "%caml_bigstring_setu256u"
    external set_int8x32_aligned : bigstring -> int -> int8x32 -> unit = "%caml_bigstring_seta256"
    external set_int8x32_aligned_unsafe : bigstring -> int -> int8x32 -> unit = "%caml_bigstring_seta256u"

    let extra_checks bigstring =
      List.iter
        (fun index ->
          assert_raises_out_of_bounds (fun () ->
            let _ = get_int8x32_unaligned bigstring index in
            ());
          assert_raises_out_of_bounds (fun () ->
            set_int8x32_unaligned bigstring index (int8x32_of_int64s 1L 2L 3L 4L));
          assert_raises_out_of_bounds (fun () ->
            let _ = get_int8x32_aligned bigstring index in
            ());
          assert_raises_out_of_bounds (fun () ->
            set_int8x32_aligned bigstring index (int8x32_of_int64s 1L 2L 3L 4L)))
        Int.[ min_int; add min_int one; sub zero one; max_int ]
    ;;
  end)

  module _ = Bigstring(struct
    external get_int8x32_unaligned_prim : bigstring -> int32# -> int8x32 = "%caml_bigstring_getu256_indexed_by_int32#"
    let get_int8x32_unaligned b i = get_int8x32_unaligned_prim b (Stdlib_upstream_compatible.Int32_u.of_int i)
    external get_int8x32_unaligned_unsafe_prim : bigstring -> int32# -> int8x32 = "%caml_bigstring_getu256u_indexed_by_int32#"
    let get_int8x32_unaligned_unsafe b i = get_int8x32_unaligned_unsafe_prim b (Stdlib_upstream_compatible.Int32_u.of_int i)
    external get_int8x32_aligned_prim : bigstring -> int32# -> int8x32 = "%caml_bigstring_geta256_indexed_by_int32#"
    let get_int8x32_aligned b i = get_int8x32_aligned_prim b (Stdlib_upstream_compatible.Int32_u.of_int i)
    external get_int8x32_aligned_unsafe_prim : bigstring -> int32# -> int8x32 = "%caml_bigstring_geta256u_indexed_by_int32#"
    let get_int8x32_aligned_unsafe b i = get_int8x32_aligned_unsafe_prim b (Stdlib_upstream_compatible.Int32_u.of_int i)

    external set_int8x32_unaligned_prim : bigstring -> int32# -> int8x32 -> unit = "%caml_bigstring_setu256_indexed_by_int32#"
    let set_int8x32_unaligned b i v = set_int8x32_unaligned_prim b (Stdlib_upstream_compatible.Int32_u.of_int i) v
    external set_int8x32_unaligned_unsafe_prim : bigstring -> int32# -> int8x32 -> unit = "%caml_bigstring_setu256u_indexed_by_int32#"
    let set_int8x32_unaligned_unsafe b i v = set_int8x32_unaligned_unsafe_prim b (Stdlib_upstream_compatible.Int32_u.of_int i) v
    external set_int8x32_aligned_prim : bigstring -> int32# -> int8x32 -> unit = "%caml_bigstring_seta256_indexed_by_int32#"
    let set_int8x32_aligned b i v = set_int8x32_aligned_prim b (Stdlib_upstream_compatible.Int32_u.of_int i) v
    external set_int8x32_aligned_unsafe_prim : bigstring -> int32# -> int8x32 -> unit = "%caml_bigstring_seta256u_indexed_by_int32#"
    let set_int8x32_aligned_unsafe b i v = set_int8x32_aligned_unsafe_prim b (Stdlib_upstream_compatible.Int32_u.of_int i) v

    let extra_checks bigstring =
      List.iter
        (fun index ->
          let index = Stdlib_upstream_compatible.Int32_u.of_int32 index in
          assert_raises_out_of_bounds (fun () ->
            let _ = get_int8x32_unaligned_prim bigstring index in
            ());
          assert_raises_out_of_bounds (fun () ->
            set_int8x32_unaligned_prim bigstring index (int8x32_of_int64s 1L 2L 3L 4L));
          assert_raises_out_of_bounds (fun () ->
            let _ = get_int8x32_aligned_prim bigstring index in
            ());
          assert_raises_out_of_bounds (fun () ->
            set_int8x32_aligned_prim bigstring index (int8x32_of_int64s 1L 2L 3L 4L)))
        Int32.[ min_int; add min_int one; sub zero one; max_int ]
    ;;
  end)

  module _ = Bigstring(struct
    external get_int8x32_unaligned_prim : bigstring -> int64# -> int8x32 = "%caml_bigstring_getu256_indexed_by_int64#"
    let get_int8x32_unaligned b i = get_int8x32_unaligned_prim b (Stdlib_upstream_compatible.Int64_u.of_int i)
    external get_int8x32_unaligned_unsafe_prim : bigstring -> int64# -> int8x32 = "%caml_bigstring_getu256u_indexed_by_int64#"
    let get_int8x32_unaligned_unsafe b i = get_int8x32_unaligned_unsafe_prim b (Stdlib_upstream_compatible.Int64_u.of_int i)
    external get_int8x32_aligned_prim : bigstring -> int64# -> int8x32 = "%caml_bigstring_geta256_indexed_by_int64#"
    let get_int8x32_aligned b i = get_int8x32_aligned_prim b (Stdlib_upstream_compatible.Int64_u.of_int i)
    external get_int8x32_aligned_unsafe_prim : bigstring -> int64# -> int8x32 = "%caml_bigstring_geta256u_indexed_by_int64#"
    let get_int8x32_aligned_unsafe b i = get_int8x32_aligned_unsafe_prim b (Stdlib_upstream_compatible.Int64_u.of_int i)

    external set_int8x32_unaligned_prim : bigstring -> int64# -> int8x32 -> unit = "%caml_bigstring_setu256_indexed_by_int64#"
    let set_int8x32_unaligned b i v = set_int8x32_unaligned_prim b (Stdlib_upstream_compatible.Int64_u.of_int i) v
    external set_int8x32_unaligned_unsafe_prim : bigstring -> int64# -> int8x32 -> unit = "%caml_bigstring_setu256u_indexed_by_int64#"
    let set_int8x32_unaligned_unsafe b i v = set_int8x32_unaligned_unsafe_prim b (Stdlib_upstream_compatible.Int64_u.of_int i) v
    external set_int8x32_aligned_prim : bigstring -> int64# -> int8x32 -> unit = "%caml_bigstring_seta256_indexed_by_int64#"
    let set_int8x32_aligned b i v = set_int8x32_aligned_prim b (Stdlib_upstream_compatible.Int64_u.of_int i) v
    external set_int8x32_aligned_unsafe_prim : bigstring -> int64# -> int8x32 -> unit = "%caml_bigstring_seta256u_indexed_by_int64#"
    let set_int8x32_aligned_unsafe b i v = set_int8x32_aligned_unsafe_prim b (Stdlib_upstream_compatible.Int64_u.of_int i) v

    let extra_checks bigstring =
      List.iter
        (fun index ->
          let index = Stdlib_upstream_compatible.Int64_u.of_int64 index in
          assert_raises_out_of_bounds (fun () ->
            let _ = get_int8x32_unaligned_prim bigstring index in
            ());
          assert_raises_out_of_bounds (fun () ->
            set_int8x32_unaligned_prim bigstring index (int8x32_of_int64s 1L 2L 3L 4L));
          assert_raises_out_of_bounds (fun () ->
            let _ = get_int8x32_aligned_prim bigstring index in
            ());
          assert_raises_out_of_bounds (fun () ->
            set_int8x32_aligned_prim bigstring index (int8x32_of_int64s 1L 2L 3L 4L)))
        Int64.[ min_int; add min_int one; sub zero one; max_int ]
    ;;
  end)

  module _ = Bigstring(struct
    external get_int8x32_unaligned_prim : bigstring -> nativeint# -> int8x32 = "%caml_bigstring_getu256_indexed_by_nativeint#"
    let get_int8x32_unaligned b i = get_int8x32_unaligned_prim b (Stdlib_upstream_compatible.Nativeint_u.of_int i)
    external get_int8x32_unaligned_unsafe_prim : bigstring -> nativeint# -> int8x32 = "%caml_bigstring_getu256u_indexed_by_nativeint#"
    let get_int8x32_unaligned_unsafe b i = get_int8x32_unaligned_unsafe_prim b (Stdlib_upstream_compatible.Nativeint_u.of_int i)
    external get_int8x32_aligned_prim : bigstring -> nativeint# -> int8x32 = "%caml_bigstring_geta256_indexed_by_nativeint#"
    let get_int8x32_aligned b i = get_int8x32_aligned_prim b (Stdlib_upstream_compatible.Nativeint_u.of_int i)
    external get_int8x32_aligned_unsafe_prim : bigstring -> nativeint# -> int8x32 = "%caml_bigstring_geta256u_indexed_by_nativeint#"
    let get_int8x32_aligned_unsafe b i = get_int8x32_aligned_unsafe_prim b (Stdlib_upstream_compatible.Nativeint_u.of_int i)

    external set_int8x32_unaligned_prim : bigstring -> nativeint# -> int8x32 -> unit = "%caml_bigstring_setu256_indexed_by_nativeint#"
    let set_int8x32_unaligned b i v = set_int8x32_unaligned_prim b (Stdlib_upstream_compatible.Nativeint_u.of_int i) v
    external set_int8x32_unaligned_unsafe_prim : bigstring -> nativeint# -> int8x32 -> unit = "%caml_bigstring_setu256u_indexed_by_nativeint#"
    let set_int8x32_unaligned_unsafe b i v = set_int8x32_unaligned_unsafe_prim b (Stdlib_upstream_compatible.Nativeint_u.of_int i) v
    external set_int8x32_aligned_prim : bigstring -> nativeint# -> int8x32 -> unit = "%caml_bigstring_seta256_indexed_by_nativeint#"
    let set_int8x32_aligned b i v = set_int8x32_aligned_prim b (Stdlib_upstream_compatible.Nativeint_u.of_int i) v
    external set_int8x32_aligned_unsafe_prim : bigstring -> nativeint# -> int8x32 -> unit = "%caml_bigstring_seta256u_indexed_by_nativeint#"
    let set_int8x32_aligned_unsafe b i v = set_int8x32_aligned_unsafe_prim b (Stdlib_upstream_compatible.Nativeint_u.of_int i) v

    let extra_checks bigstring =
      List.iter
        (fun index ->
          let index = Stdlib_upstream_compatible.Nativeint_u.of_nativeint index in
          assert_raises_out_of_bounds (fun () ->
            let _ = get_int8x32_unaligned_prim bigstring index in
            ());
          assert_raises_out_of_bounds (fun () ->
            set_int8x32_unaligned_prim bigstring index (int8x32_of_int64s 1L 2L 3L 4L));
          assert_raises_out_of_bounds (fun () ->
            let _ = get_int8x32_aligned_prim bigstring index in
            ());
          assert_raises_out_of_bounds (fun () ->
            set_int8x32_aligned_prim bigstring index (int8x32_of_int64s 1L 2L 3L 4L)))
        Nativeint.[ min_int; add min_int one; sub zero one; max_int ]
    ;;
  end)
end

module Float_arrays (Primitives : sig

  val float_array_get_float64x4 : float array -> int -> float64x4
  val float_array_get_float64x4_unsafe : float array -> int -> float64x4

  val float_iarray_get_float64x4 : float iarray -> int -> float64x4
  val float_iarray_get_float64x4_unsafe : float iarray -> int -> float64x4

  val float_array_set_float64x4 : float array -> int -> float64x4 -> unit
  val float_array_set_float64x4_unsafe : float array -> int -> float64x4 -> unit

  val floatarray_get_float64x4 : floatarray -> int -> float64x4
  val floatarray_get_float64x4_unsafe : floatarray -> int -> float64x4

  val floatarray_set_float64x4 : floatarray -> int -> float64x4 -> unit
  val floatarray_set_float64x4_unsafe : floatarray -> int -> float64x4 -> unit

  val unboxed_float_array_get_float64x4 : float# array -> int -> float64x4
  val unboxed_float_array_get_float64x4_unsafe : float# array -> int -> float64x4

  val unboxed_float_array_set_float64x4 : float# array -> int -> float64x4 -> unit
  val unboxed_float_array_set_float64x4_unsafe : float# array -> int -> float64x4 -> unit

  val unboxed_float32_array_get_float32x8 : float32# array -> int -> float32x8
  val unboxed_float32_array_get_float32x8_unsafe : float32# array -> int -> float32x8

  val unboxed_float32_array_set_float32x8 : float32# array -> int -> float32x8 -> unit
  val unboxed_float32_array_set_float32x8_unsafe : float32# array -> int -> float32x8 -> unit

end) = struct
  open Primitives

  external low_of64 : float -> float64x4 = "caml_vec256_unreachable" "caml_float64x4_low_of_float"
    [@@noalloc] [@@unboxed] [@@builtin]

  external low_of32 : float32 -> float32x8 = "caml_vec256_unreachable" "caml_float32x8_low_of_float32"
    [@@noalloc] [@@unboxed] [@@builtin]

  let f64x4 a b c d =
    float64x4_of_int64s (Int64.bits_of_float a) (Int64.bits_of_float b)
                        (Int64.bits_of_float c) (Int64.bits_of_float d)

  let f32x8 a b c d e f g h =
    let pack_pair x y =
      let x_bits = Int64.of_int32 (Float32.to_bits x) in
      let y_bits = Int64.of_int32 (Float32.to_bits y) in
      Int64.logor (Int64.logand x_bits 0xFFFFFFFFL) (Int64.shift_left y_bits 32)
    in
    float32x8_of_int64s (pack_pair a b) (pack_pair c d) (pack_pair e f) (pack_pair g h)

  let float_array () = [| 0.0; 1.0; 2.0; 3.0; 4.0 |]
  let float_iarray () = [: 0.0; 1.0; 2.0; 3.0; 4.0 :]
  let floatarray () =
    let a = Array.Floatarray.create 5 in
    Array.Floatarray.set a 0 0.0;
    Array.Floatarray.set a 1 1.0;
    Array.Floatarray.set a 2 2.0;
    Array.Floatarray.set a 3 3.0;
    Array.Floatarray.set a 4 4.0;
    a
  ;;
  let unboxed_float_array () = [| #0.0; #1.0; #2.0; #3.0; #4.0 |]
  let unboxed_float32_array () = [| #0.0s; #1.0s; #2.0s; #3.0s; #4.0s; #5.0s; #6.0s; #7.0s; #8.0s |]

  let () =
    let float_array = float_array () in
    let f_0123 = f64x4 0.0 1.0 2.0 3.0 in
    let f_1234 = f64x4 1.0 2.0 3.0 4.0 in
    let get = float_array_get_float64x4_unsafe float_array 0 in
    eq (float64x4_first_int64 f_0123) (float64x4_second_int64 f_0123) (float64x4_third_int64 f_0123) (float64x4_fourth_int64 f_0123)
       (float64x4_first_int64 get) (float64x4_second_int64 get) (float64x4_third_int64 get) (float64x4_fourth_int64 get);
    let get = float_array_get_float64x4_unsafe float_array 1 in
    eq (float64x4_first_int64 f_1234) (float64x4_second_int64 f_1234) (float64x4_third_int64 f_1234) (float64x4_fourth_int64 f_1234)
       (float64x4_first_int64 get) (float64x4_second_int64 get) (float64x4_third_int64 get) (float64x4_fourth_int64 get);

    let f_4567 = f64x4 4.0 5.0 6.0 7.0 in
    let f_6789 = f64x4 6.0 7.0 8.0 9.0 in
    float_array_set_float64x4_unsafe float_array 0 f_4567;
    let get = float_array_get_float64x4_unsafe float_array 0 in
    eq (float64x4_first_int64 f_4567) (float64x4_second_int64 f_4567) (float64x4_third_int64 f_4567) (float64x4_fourth_int64 f_4567)
       (float64x4_first_int64 get) (float64x4_second_int64 get) (float64x4_third_int64 get) (float64x4_fourth_int64 get);
    float_array_set_float64x4_unsafe float_array 1 f_6789;
    let get = float_array_get_float64x4_unsafe float_array 1 in
    eq (float64x4_first_int64 f_6789) (float64x4_second_int64 f_6789) (float64x4_third_int64 f_6789) (float64x4_fourth_int64 f_6789)
       (float64x4_first_int64 get) (float64x4_second_int64 get) (float64x4_third_int64 get) (float64x4_fourth_int64 get)
  ;;

  let () =
    let a = float_array () in
    let f_0 = f64x4 0.0 0.0 0.0 0.0 in
    let fail a i =
      try
        let _ = float_array_get_float64x4 a i in
        Printf.printf "Did not fail on index %d\n" i
      with | Invalid_argument s when s = "index out of bounds" -> ();
      try
        let _ = float_array_set_float64x4 a i f_0 in
        Printf.printf "Did not fail on index %d\n" i
      with | Invalid_argument s when s = "index out of bounds" -> ()
    in
    fail a (-1);
    fail a 3;
    fail a 4;
    fail [||] 0;
    fail [|0.0|] 0;
    fail [|0.0|] 1;
    fail [|0.0|] (-1)
  ;;

  let () =
    let floatarray = floatarray () in
    let f_0123 = f64x4 0.0 1.0 2.0 3.0 in
    let f_1234 = f64x4 1.0 2.0 3.0 4.0 in
    let get = floatarray_get_float64x4_unsafe floatarray 0 in
    eq (float64x4_first_int64 f_0123) (float64x4_second_int64 f_0123) (float64x4_third_int64 f_0123) (float64x4_fourth_int64 f_0123) (float64x4_first_int64 get) (float64x4_second_int64 get) (float64x4_third_int64 get) (float64x4_fourth_int64 get);
    let get = floatarray_get_float64x4_unsafe floatarray 1 in
    eq (float64x4_first_int64 f_1234) (float64x4_second_int64 f_1234) (float64x4_third_int64 f_1234) (float64x4_fourth_int64 f_1234) (float64x4_first_int64 get) (float64x4_second_int64 get) (float64x4_third_int64 get) (float64x4_fourth_int64 get);

    let f_4567 = f64x4 4.0 5.0 6.0 7.0 in
    let f_6789 = f64x4 6.0 7.0 8.0 9.0 in
    floatarray_set_float64x4_unsafe floatarray 0 f_4567;
    let get = floatarray_get_float64x4_unsafe floatarray 0 in
    eq (float64x4_first_int64 f_4567) (float64x4_second_int64 f_4567) (float64x4_third_int64 f_4567) (float64x4_fourth_int64 f_4567) (float64x4_first_int64 get) (float64x4_second_int64 get) (float64x4_third_int64 get) (float64x4_fourth_int64 get);
    floatarray_set_float64x4_unsafe floatarray 1 f_6789;
    let get = floatarray_get_float64x4_unsafe floatarray 1 in
    eq (float64x4_first_int64 f_6789) (float64x4_second_int64 f_6789) (float64x4_third_int64 f_6789) (float64x4_fourth_int64 f_6789) (float64x4_first_int64 get) (float64x4_second_int64 get) (float64x4_third_int64 get) (float64x4_fourth_int64 get)
  ;;

  let () =
    let a = floatarray () in
    let f_0 = f64x4 0.0 0.0 0.0 0.0 in
    let fail a i =
      try
        let _ = floatarray_get_float64x4 a i in
        Printf.printf "Did not fail on index %d\n" i
      with | Invalid_argument s when s = "index out of bounds" -> ();
      try
        let _ = floatarray_set_float64x4 a i f_0 in
        Printf.printf "Did not fail on index %d\n" i
      with | Invalid_argument s when s = "index out of bounds" -> ()
    in
    fail a (-1);
    fail a 3;
    fail a 4;
    fail (Array.Floatarray.create 0) 0;
    let a = Array.Floatarray.create 1 in
    Array.Floatarray.set a 0 0.0;
    fail a 0;
    fail a 1;
    fail a (-1)
  ;;

  let () =
    let float_iarray = float_iarray () in
    let f_0123 = f64x4 0.0 1.0 2.0 3.0 in
    let f_1234 = f64x4 1.0 2.0 3.0 4.0 in
    let get = float_iarray_get_float64x4_unsafe float_iarray 0 in
    eq (float64x4_first_int64 f_0123) (float64x4_second_int64 f_0123) (float64x4_third_int64 f_0123) (float64x4_fourth_int64 f_0123) (float64x4_first_int64 get) (float64x4_second_int64 get) (float64x4_third_int64 get) (float64x4_fourth_int64 get);
    let get = float_iarray_get_float64x4_unsafe float_iarray 1 in
    eq (float64x4_first_int64 f_1234) (float64x4_second_int64 f_1234) (float64x4_third_int64 f_1234) (float64x4_fourth_int64 f_1234) (float64x4_first_int64 get) (float64x4_second_int64 get) (float64x4_third_int64 get) (float64x4_fourth_int64 get);
  ;;

  let () =
    let a = float_iarray () in
    let fail a i =
      try
        let _ = float_iarray_get_float64x4 a i in
        Printf.printf "Did not fail on index %d\n" i
      with | Invalid_argument s when s = "index out of bounds" -> ();
    in
    fail a (-1);
    fail a 3;
    fail a 4;
    fail [::] 0;
    let a = [: 0.0 :] in
    fail a 0;
    fail a 1;
    fail a (-1)
  ;;

  let () =
    let unboxed_float_array = unboxed_float_array () in
    let f_0123 = f64x4 0.0 1.0 2.0 3.0 in
    let f_1234 = f64x4 1.0 2.0 3.0 4.0 in
    let get = unboxed_float_array_get_float64x4 unboxed_float_array 0 in
    eq (float64x4_first_int64 f_0123) (float64x4_second_int64 f_0123) (float64x4_third_int64 f_0123) (float64x4_fourth_int64 f_0123) (float64x4_first_int64 get) (float64x4_second_int64 get) (float64x4_third_int64 get) (float64x4_fourth_int64 get);
    let get = unboxed_float_array_get_float64x4 unboxed_float_array 1 in
    eq (float64x4_first_int64 f_1234) (float64x4_second_int64 f_1234) (float64x4_third_int64 f_1234) (float64x4_fourth_int64 f_1234) (float64x4_first_int64 get) (float64x4_second_int64 get) (float64x4_third_int64 get) (float64x4_fourth_int64 get);

    let f_4567 = f64x4 4.0 5.0 6.0 7.0 in
    let f_6789 = f64x4 6.0 7.0 8.0 9.0 in
    unboxed_float_array_set_float64x4 unboxed_float_array 0 f_4567;
    let get = unboxed_float_array_get_float64x4 unboxed_float_array 0 in
    eq (float64x4_first_int64 f_4567) (float64x4_second_int64 f_4567) (float64x4_third_int64 f_4567) (float64x4_fourth_int64 f_4567) (float64x4_first_int64 get) (float64x4_second_int64 get) (float64x4_third_int64 get) (float64x4_fourth_int64 get);
    unboxed_float_array_set_float64x4 unboxed_float_array 1 f_6789;
    let get = unboxed_float_array_get_float64x4 unboxed_float_array 1 in
    eq (float64x4_first_int64 f_6789) (float64x4_second_int64 f_6789) (float64x4_third_int64 f_6789) (float64x4_fourth_int64 f_6789) (float64x4_first_int64 get) (float64x4_second_int64 get) (float64x4_third_int64 get) (float64x4_fourth_int64 get)
  ;;

  let () =
    let unboxed_float_array = unboxed_float_array () in
    let f_0123 = f64x4 0.0 1.0 2.0 3.0 in
    let f_1234 = f64x4 1.0 2.0 3.0 4.0 in
    let get = unboxed_float_array_get_float64x4_unsafe unboxed_float_array 0 in
    eq (float64x4_first_int64 f_0123) (float64x4_second_int64 f_0123) (float64x4_third_int64 f_0123) (float64x4_fourth_int64 f_0123) (float64x4_first_int64 get) (float64x4_second_int64 get) (float64x4_third_int64 get) (float64x4_fourth_int64 get);
    let get = unboxed_float_array_get_float64x4_unsafe unboxed_float_array 1 in
    eq (float64x4_first_int64 f_1234) (float64x4_second_int64 f_1234) (float64x4_third_int64 f_1234) (float64x4_fourth_int64 f_1234) (float64x4_first_int64 get) (float64x4_second_int64 get) (float64x4_third_int64 get) (float64x4_fourth_int64 get);

    let f_4567 = f64x4 4.0 5.0 6.0 7.0 in
    let f_6789 = f64x4 6.0 7.0 8.0 9.0 in
    unboxed_float_array_set_float64x4_unsafe unboxed_float_array 0 f_4567;
    let get = unboxed_float_array_get_float64x4_unsafe unboxed_float_array 0 in
    eq (float64x4_first_int64 f_4567) (float64x4_second_int64 f_4567) (float64x4_third_int64 f_4567) (float64x4_fourth_int64 f_4567) (float64x4_first_int64 get) (float64x4_second_int64 get) (float64x4_third_int64 get) (float64x4_fourth_int64 get);
    unboxed_float_array_set_float64x4_unsafe unboxed_float_array 1 f_6789;
    let get = unboxed_float_array_get_float64x4_unsafe unboxed_float_array 1 in
    eq (float64x4_first_int64 f_6789) (float64x4_second_int64 f_6789) (float64x4_third_int64 f_6789) (float64x4_fourth_int64 f_6789) (float64x4_first_int64 get) (float64x4_second_int64 get) (float64x4_third_int64 get) (float64x4_fourth_int64 get)
  ;;

  let () =
    let a = unboxed_float_array () in
    let f_0 = f64x4 0.0 0.0 0.0 0.0 in
    let fail a i =
      try
        let _ = unboxed_float_array_get_float64x4 a i in
        let _ = unboxed_float_array_set_float64x4 a i f_0 in
        Printf.printf "Did not fail on index %d\n" i
      with | Invalid_argument s when s = "index out of bounds" -> ()
    in
    fail a (-1);
    fail a 3;
    fail a 4;
    fail [||] 0;
    fail [|#0.0|] 0;
    fail [|#0.0|] 1;
    fail [|#0.0|] (-1)
  ;;

  let () =
    let unboxed_float32_array = unboxed_float32_array () in
    let f_01234567 = f32x8 0.0s 1.0s 2.0s 3.0s 4.0s 5.0s 6.0s 7.0s in
    let f_12345678 = f32x8 1.0s 2.0s 3.0s 4.0s 5.0s 6.0s 7.0s 8.0s in
    let get = unboxed_float32_array_get_float32x8 unboxed_float32_array 0 in
    eq (float32x8_first_int64 f_01234567) (float32x8_second_int64 f_01234567) (float32x8_third_int64 f_01234567) (float32x8_fourth_int64 f_01234567) (float32x8_first_int64 get) (float32x8_second_int64 get) (float32x8_third_int64 get) (float32x8_fourth_int64 get);
    let get = unboxed_float32_array_get_float32x8 unboxed_float32_array 1 in
    eq (float32x8_first_int64 f_12345678) (float32x8_second_int64 f_12345678) (float32x8_third_int64 f_12345678) (float32x8_fourth_int64 f_12345678) (float32x8_first_int64 get) (float32x8_second_int64 get) (float32x8_third_int64 get) (float32x8_fourth_int64 get);

    let f_89abcdef = f32x8 8.0s 9.0s 10.0s 11.0s 12.0s 13.0s 14.0s 15.0s in
    let f_cdef0123 = f32x8 12.0s 13.0s 14.0s 15.0s 0.0s 1.0s 2.0s 3.0s in
    unboxed_float32_array_set_float32x8 unboxed_float32_array 0 f_89abcdef;
    let get = unboxed_float32_array_get_float32x8 unboxed_float32_array 0 in
    eq (float32x8_first_int64 f_89abcdef) (float32x8_second_int64 f_89abcdef) (float32x8_third_int64 f_89abcdef) (float32x8_fourth_int64 f_89abcdef) (float32x8_first_int64 get) (float32x8_second_int64 get) (float32x8_third_int64 get) (float32x8_fourth_int64 get);
    unboxed_float32_array_set_float32x8 unboxed_float32_array 1 f_cdef0123;
    let get = unboxed_float32_array_get_float32x8 unboxed_float32_array 1 in
    eq (float32x8_first_int64 f_cdef0123) (float32x8_second_int64 f_cdef0123) (float32x8_third_int64 f_cdef0123) (float32x8_fourth_int64 f_cdef0123) (float32x8_first_int64 get) (float32x8_second_int64 get) (float32x8_third_int64 get) (float32x8_fourth_int64 get)
  ;;

  let () =
    let unboxed_float32_array = unboxed_float32_array () in
    let f_01234567 = f32x8 0.0s 1.0s 2.0s 3.0s 4.0s 5.0s 6.0s 7.0s in
    let f_12345678 = f32x8 1.0s 2.0s 3.0s 4.0s 5.0s 6.0s 7.0s 8.0s in
    let get = unboxed_float32_array_get_float32x8_unsafe unboxed_float32_array 0 in
    eq (float32x8_first_int64 f_01234567) (float32x8_second_int64 f_01234567) (float32x8_third_int64 f_01234567) (float32x8_fourth_int64 f_01234567) (float32x8_first_int64 get) (float32x8_second_int64 get) (float32x8_third_int64 get) (float32x8_fourth_int64 get);
    let get = unboxed_float32_array_get_float32x8_unsafe unboxed_float32_array 1 in
    eq (float32x8_first_int64 f_12345678) (float32x8_second_int64 f_12345678) (float32x8_third_int64 f_12345678) (float32x8_fourth_int64 f_12345678) (float32x8_first_int64 get) (float32x8_second_int64 get) (float32x8_third_int64 get) (float32x8_fourth_int64 get);

    let f_89abcdef = f32x8 8.0s 9.0s 10.0s 11.0s 12.0s 13.0s 14.0s 15.0s in
    let f_9abcdefh = f32x8 9.0s 10.0s 11.0s 12.0s 13.0s 14.0s 15.0s 16.0s in
    unboxed_float32_array_set_float32x8_unsafe unboxed_float32_array 0 f_89abcdef;
    let get = unboxed_float32_array_get_float32x8_unsafe unboxed_float32_array 0 in
    eq (float32x8_first_int64 f_89abcdef) (float32x8_second_int64 f_89abcdef) (float32x8_third_int64 f_89abcdef) (float32x8_fourth_int64 f_89abcdef) (float32x8_first_int64 get) (float32x8_second_int64 get) (float32x8_third_int64 get) (float32x8_fourth_int64 get);
    unboxed_float32_array_set_float32x8_unsafe unboxed_float32_array 1 f_9abcdefh;
    let get = unboxed_float32_array_get_float32x8_unsafe unboxed_float32_array 1 in
    eq (float32x8_first_int64 f_9abcdefh) (float32x8_second_int64 f_9abcdefh) (float32x8_third_int64 f_9abcdefh) (float32x8_fourth_int64 f_9abcdefh) (float32x8_first_int64 get) (float32x8_second_int64 get) (float32x8_third_int64 get) (float32x8_fourth_int64 get)
  ;;

  let () =
    let a = unboxed_float32_array () in
    let f_0 = f32x8 0.0s 0.0s 0.0s 0.0s 0.0s 0.0s 0.0s 0.0s in
    let fail a i =
      try
        let _ = unboxed_float32_array_get_float32x8 a i in
        let _ = unboxed_float32_array_set_float32x8 a i f_0 in
        Printf.printf "Did not fail on index %d\n" i
      with | Invalid_argument s when s = "index out of bounds" -> ()
    in
    fail a (-1);
    fail a 5;
    fail a 6;
    fail [||] 0;
    fail [|#0.0s|] 0;
    fail [|#0.0s;#0.0s|] 0;
    fail [|#0.0s;#0.0s;#0.0s|] 0;
    fail [|#0.0s;#0.0s;#0.0s;#0.0s|] 1;
    fail [|#0.0s|] (-1)
  ;;
end

module _ = Float_arrays(struct

  external float_array_get_float64x4 : float array -> int -> float64x4 = "%caml_float_array_get256"
  external float_array_get_float64x4_unsafe : float array -> int -> float64x4 = "%caml_float_array_get256u"

  external float_iarray_get_float64x4 : float iarray -> int -> float64x4 = "%caml_float_array_get256"
  external float_iarray_get_float64x4_unsafe : float iarray -> int -> float64x4 = "%caml_float_array_get256u"

  external float_array_set_float64x4 : float array -> int -> float64x4 -> unit = "%caml_float_array_set256"
  external float_array_set_float64x4_unsafe : float array -> int -> float64x4 -> unit = "%caml_float_array_set256u"

  external floatarray_get_float64x4 : floatarray -> int -> float64x4 = "%caml_floatarray_get256"
  external floatarray_get_float64x4_unsafe : floatarray -> int -> float64x4 = "%caml_floatarray_get256u"

  external floatarray_set_float64x4 : floatarray -> int -> float64x4 -> unit = "%caml_floatarray_set256"
  external floatarray_set_float64x4_unsafe : floatarray -> int -> float64x4 -> unit = "%caml_floatarray_set256u"

  external unboxed_float_array_get_float64x4 : float# array -> int -> float64x4 = "%caml_unboxed_float_array_get256"
  external unboxed_float_array_get_float64x4_unsafe : float# array -> int -> float64x4 = "%caml_unboxed_float_array_get256u"

  external unboxed_float_array_set_float64x4 : float# array -> int -> float64x4 -> unit = "%caml_unboxed_float_array_set256"
  external unboxed_float_array_set_float64x4_unsafe : float# array -> int -> float64x4 -> unit = "%caml_unboxed_float_array_set256u"

  external unboxed_float32_array_get_float32x8 : float32# array -> int -> float32x8 = "%caml_unboxed_float32_array_get256"
  external unboxed_float32_array_get_float32x8_unsafe : float32# array -> int -> float32x8 = "%caml_unboxed_float32_array_get256u"

  external unboxed_float32_array_set_float32x8 : float32# array -> int -> float32x8 -> unit = "%caml_unboxed_float32_array_set256"
  external unboxed_float32_array_set_float32x8_unsafe : float32# array -> int -> float32x8 -> unit = "%caml_unboxed_float32_array_set256u"

end)

module _ = Float_arrays(struct

  external float_array_get_float64x4 : float array -> int32# -> float64x4 = "%caml_float_array_get256_indexed_by_int32#"
  let float_array_get_float64x4 arr i = float_array_get_float64x4 arr (Stdlib_upstream_compatible.Int32_u.of_int i)
  external float_array_get_float64x4_unsafe : float array -> int32# -> float64x4 = "%caml_float_array_get256u_indexed_by_int32#"
  let float_array_get_float64x4_unsafe arr i = float_array_get_float64x4_unsafe arr (Stdlib_upstream_compatible.Int32_u.of_int i)

  external float_iarray_get_float64x4 : float iarray -> int32# -> float64x4 = "%caml_float_array_get256_indexed_by_int32#"
  let float_iarray_get_float64x4 arr i = float_iarray_get_float64x4 arr (Stdlib_upstream_compatible.Int32_u.of_int i)
  external float_iarray_get_float64x4_unsafe : float iarray -> int32# -> float64x4 = "%caml_float_array_get256u_indexed_by_int32#"
  let float_iarray_get_float64x4_unsafe arr i = float_iarray_get_float64x4_unsafe arr (Stdlib_upstream_compatible.Int32_u.of_int i)

  external float_array_set_float64x4 : float array -> int32# -> float64x4 -> unit = "%caml_float_array_set256_indexed_by_int32#"
  let float_array_set_float64x4 arr i v = float_array_set_float64x4 arr (Stdlib_upstream_compatible.Int32_u.of_int i) v
  external float_array_set_float64x4_unsafe : float array -> int32# -> float64x4 -> unit = "%caml_float_array_set256u_indexed_by_int32#"
  let float_array_set_float64x4_unsafe arr i v = float_array_set_float64x4_unsafe arr (Stdlib_upstream_compatible.Int32_u.of_int i) v

  external floatarray_get_float64x4 : floatarray -> int32# -> float64x4 = "%caml_floatarray_get256_indexed_by_int32#"
  let floatarray_get_float64x4 arr i = floatarray_get_float64x4 arr (Stdlib_upstream_compatible.Int32_u.of_int i)
  external floatarray_get_float64x4_unsafe : floatarray -> int32# -> float64x4 = "%caml_floatarray_get256u_indexed_by_int32#"
  let floatarray_get_float64x4_unsafe arr i = floatarray_get_float64x4_unsafe arr (Stdlib_upstream_compatible.Int32_u.of_int i)

  external floatarray_set_float64x4 : floatarray -> int32# -> float64x4 -> unit = "%caml_floatarray_set256_indexed_by_int32#"
  let floatarray_set_float64x4 arr i v = floatarray_set_float64x4 arr (Stdlib_upstream_compatible.Int32_u.of_int i) v
  external floatarray_set_float64x4_unsafe : floatarray -> int32# -> float64x4 -> unit = "%caml_floatarray_set256u_indexed_by_int32#"
  let floatarray_set_float64x4_unsafe arr i v = floatarray_set_float64x4_unsafe arr (Stdlib_upstream_compatible.Int32_u.of_int i) v

  external unboxed_float_array_get_float64x4 : float# array -> int32# -> float64x4 = "%caml_unboxed_float_array_get256_indexed_by_int32#"
  let unboxed_float_array_get_float64x4 arr i = unboxed_float_array_get_float64x4 arr (Stdlib_upstream_compatible.Int32_u.of_int i)
  external unboxed_float_array_get_float64x4_unsafe : float# array -> int32# -> float64x4 = "%caml_unboxed_float_array_get256u_indexed_by_int32#"
  let unboxed_float_array_get_float64x4_unsafe arr i = unboxed_float_array_get_float64x4_unsafe arr (Stdlib_upstream_compatible.Int32_u.of_int i)

  external unboxed_float_array_set_float64x4 : float# array -> int32# -> float64x4 -> unit = "%caml_unboxed_float_array_set256_indexed_by_int32#"
  let unboxed_float_array_set_float64x4 arr i v = unboxed_float_array_set_float64x4 arr (Stdlib_upstream_compatible.Int32_u.of_int i) v
  external unboxed_float_array_set_float64x4_unsafe : float# array -> int32# -> float64x4 -> unit = "%caml_unboxed_float_array_set256u_indexed_by_int32#"
  let unboxed_float_array_set_float64x4_unsafe arr i v = unboxed_float_array_set_float64x4_unsafe arr (Stdlib_upstream_compatible.Int32_u.of_int i) v

  external unboxed_float32_array_get_float32x8 : float32# array -> int32# -> float32x8 = "%caml_unboxed_float32_array_get256_indexed_by_int32#"
  let unboxed_float32_array_get_float32x8 arr i = unboxed_float32_array_get_float32x8 arr (Stdlib_upstream_compatible.Int32_u.of_int i)
  external unboxed_float32_array_get_float32x8_unsafe : float32# array -> int32# -> float32x8 = "%caml_unboxed_float32_array_get256u_indexed_by_int32#"
  let unboxed_float32_array_get_float32x8_unsafe arr i = unboxed_float32_array_get_float32x8_unsafe arr (Stdlib_upstream_compatible.Int32_u.of_int i)

  external unboxed_float32_array_set_float32x8 : float32# array -> int32# -> float32x8 -> unit = "%caml_unboxed_float32_array_set256_indexed_by_int32#"
  let unboxed_float32_array_set_float32x8 arr i v = unboxed_float32_array_set_float32x8 arr (Stdlib_upstream_compatible.Int32_u.of_int i) v
  external unboxed_float32_array_set_float32x8_unsafe : float32# array -> int32# -> float32x8 -> unit = "%caml_unboxed_float32_array_set256u_indexed_by_int32#"
  let unboxed_float32_array_set_float32x8_unsafe arr i v = unboxed_float32_array_set_float32x8_unsafe arr (Stdlib_upstream_compatible.Int32_u.of_int i) v

end)

module _ = Float_arrays(struct

  external float_array_get_float64x4 : float array -> int64# -> float64x4 = "%caml_float_array_get256_indexed_by_int64#"
  let float_array_get_float64x4 arr i = float_array_get_float64x4 arr (Stdlib_upstream_compatible.Int64_u.of_int i)
  external float_array_get_float64x4_unsafe : float array -> int64# -> float64x4 = "%caml_float_array_get256u_indexed_by_int64#"
  let float_array_get_float64x4_unsafe arr i = float_array_get_float64x4_unsafe arr (Stdlib_upstream_compatible.Int64_u.of_int i)

  external float_iarray_get_float64x4 : float iarray -> int64# -> float64x4 = "%caml_float_array_get256_indexed_by_int64#"
  let float_iarray_get_float64x4 arr i = float_iarray_get_float64x4 arr (Stdlib_upstream_compatible.Int64_u.of_int i)
  external float_iarray_get_float64x4_unsafe : float iarray -> int64# -> float64x4 = "%caml_float_array_get256u_indexed_by_int64#"
  let float_iarray_get_float64x4_unsafe arr i = float_iarray_get_float64x4_unsafe arr (Stdlib_upstream_compatible.Int64_u.of_int i)

  external float_array_set_float64x4 : float array -> int64# -> float64x4 -> unit = "%caml_float_array_set256_indexed_by_int64#"
  let float_array_set_float64x4 arr i v = float_array_set_float64x4 arr (Stdlib_upstream_compatible.Int64_u.of_int i) v
  external float_array_set_float64x4_unsafe : float array -> int64# -> float64x4 -> unit = "%caml_float_array_set256u_indexed_by_int64#"
  let float_array_set_float64x4_unsafe arr i v = float_array_set_float64x4_unsafe arr (Stdlib_upstream_compatible.Int64_u.of_int i) v

  external floatarray_get_float64x4 : floatarray -> int64# -> float64x4 = "%caml_floatarray_get256_indexed_by_int64#"
  let floatarray_get_float64x4 arr i = floatarray_get_float64x4 arr (Stdlib_upstream_compatible.Int64_u.of_int i)
  external floatarray_get_float64x4_unsafe : floatarray -> int64# -> float64x4 = "%caml_floatarray_get256u_indexed_by_int64#"
  let floatarray_get_float64x4_unsafe arr i = floatarray_get_float64x4_unsafe arr (Stdlib_upstream_compatible.Int64_u.of_int i)

  external floatarray_set_float64x4 : floatarray -> int64# -> float64x4 -> unit = "%caml_floatarray_set256_indexed_by_int64#"
  let floatarray_set_float64x4 arr i v = floatarray_set_float64x4 arr (Stdlib_upstream_compatible.Int64_u.of_int i) v
  external floatarray_set_float64x4_unsafe : floatarray -> int64# -> float64x4 -> unit = "%caml_floatarray_set256u_indexed_by_int64#"
  let floatarray_set_float64x4_unsafe arr i v = floatarray_set_float64x4_unsafe arr (Stdlib_upstream_compatible.Int64_u.of_int i) v

  external unboxed_float_array_get_float64x4 : float# array -> int64# -> float64x4 = "%caml_unboxed_float_array_get256_indexed_by_int64#"
  let unboxed_float_array_get_float64x4 arr i = unboxed_float_array_get_float64x4 arr (Stdlib_upstream_compatible.Int64_u.of_int i)
  external unboxed_float_array_get_float64x4_unsafe : float# array -> int64# -> float64x4 = "%caml_unboxed_float_array_get256u_indexed_by_int64#"
  let unboxed_float_array_get_float64x4_unsafe arr i = unboxed_float_array_get_float64x4_unsafe arr (Stdlib_upstream_compatible.Int64_u.of_int i)

  external unboxed_float_array_set_float64x4 : float# array -> int64# -> float64x4 -> unit = "%caml_unboxed_float_array_set256_indexed_by_int64#"
  let unboxed_float_array_set_float64x4 arr i v = unboxed_float_array_set_float64x4 arr (Stdlib_upstream_compatible.Int64_u.of_int i) v
  external unboxed_float_array_set_float64x4_unsafe : float# array -> int64# -> float64x4 -> unit = "%caml_unboxed_float_array_set256u_indexed_by_int64#"
  let unboxed_float_array_set_float64x4_unsafe arr i v = unboxed_float_array_set_float64x4_unsafe arr (Stdlib_upstream_compatible.Int64_u.of_int i) v

  external unboxed_float32_array_get_float32x8 : float32# array -> int64# -> float32x8 = "%caml_unboxed_float32_array_get256_indexed_by_int64#"
  let unboxed_float32_array_get_float32x8 arr i = unboxed_float32_array_get_float32x8 arr (Stdlib_upstream_compatible.Int64_u.of_int i)
  external unboxed_float32_array_get_float32x8_unsafe : float32# array -> int64# -> float32x8 = "%caml_unboxed_float32_array_get256u_indexed_by_int64#"
  let unboxed_float32_array_get_float32x8_unsafe arr i = unboxed_float32_array_get_float32x8_unsafe arr (Stdlib_upstream_compatible.Int64_u.of_int i)

  external unboxed_float32_array_set_float32x8 : float32# array -> int64# -> float32x8 -> unit = "%caml_unboxed_float32_array_set256_indexed_by_int64#"
  let unboxed_float32_array_set_float32x8 arr i v = unboxed_float32_array_set_float32x8 arr (Stdlib_upstream_compatible.Int64_u.of_int i) v
  external unboxed_float32_array_set_float32x8_unsafe : float32# array -> int64# -> float32x8 -> unit = "%caml_unboxed_float32_array_set256u_indexed_by_int64#"
  let unboxed_float32_array_set_float32x8_unsafe arr i v = unboxed_float32_array_set_float32x8_unsafe arr (Stdlib_upstream_compatible.Int64_u.of_int i) v

end)

module _ = Float_arrays(struct

  external float_array_get_float64x4 : float array -> nativeint# -> float64x4 = "%caml_float_array_get256_indexed_by_nativeint#"
  let float_array_get_float64x4 arr i = float_array_get_float64x4 arr (Stdlib_upstream_compatible.Nativeint_u.of_int i)
  external float_array_get_float64x4_unsafe : float array -> nativeint# -> float64x4 = "%caml_float_array_get256u_indexed_by_nativeint#"
  let float_array_get_float64x4_unsafe arr i = float_array_get_float64x4_unsafe arr (Stdlib_upstream_compatible.Nativeint_u.of_int i)

  external float_iarray_get_float64x4 : float iarray -> nativeint# -> float64x4 = "%caml_float_array_get256_indexed_by_nativeint#"
  let float_iarray_get_float64x4 arr i = float_iarray_get_float64x4 arr (Stdlib_upstream_compatible.Nativeint_u.of_int i)
  external float_iarray_get_float64x4_unsafe : float iarray -> nativeint# -> float64x4 = "%caml_float_array_get256u_indexed_by_nativeint#"
  let float_iarray_get_float64x4_unsafe arr i = float_iarray_get_float64x4_unsafe arr (Stdlib_upstream_compatible.Nativeint_u.of_int i)

  external float_array_set_float64x4 : float array -> nativeint# -> float64x4 -> unit = "%caml_float_array_set256_indexed_by_nativeint#"
  let float_array_set_float64x4 arr i v = float_array_set_float64x4 arr (Stdlib_upstream_compatible.Nativeint_u.of_int i) v
  external float_array_set_float64x4_unsafe : float array -> nativeint# -> float64x4 -> unit = "%caml_float_array_set256u_indexed_by_nativeint#"
  let float_array_set_float64x4_unsafe arr i v = float_array_set_float64x4_unsafe arr (Stdlib_upstream_compatible.Nativeint_u.of_int i) v

  external floatarray_get_float64x4 : floatarray -> nativeint# -> float64x4 = "%caml_floatarray_get256_indexed_by_nativeint#"
  let floatarray_get_float64x4 arr i = floatarray_get_float64x4 arr (Stdlib_upstream_compatible.Nativeint_u.of_int i)
  external floatarray_get_float64x4_unsafe : floatarray -> nativeint# -> float64x4 = "%caml_floatarray_get256u_indexed_by_nativeint#"
  let floatarray_get_float64x4_unsafe arr i = floatarray_get_float64x4_unsafe arr (Stdlib_upstream_compatible.Nativeint_u.of_int i)

  external floatarray_set_float64x4 : floatarray -> nativeint# -> float64x4 -> unit = "%caml_floatarray_set256_indexed_by_nativeint#"
  let floatarray_set_float64x4 arr i v = floatarray_set_float64x4 arr (Stdlib_upstream_compatible.Nativeint_u.of_int i) v
  external floatarray_set_float64x4_unsafe : floatarray -> nativeint# -> float64x4 -> unit = "%caml_floatarray_set256u_indexed_by_nativeint#"
  let floatarray_set_float64x4_unsafe arr i v = floatarray_set_float64x4_unsafe arr (Stdlib_upstream_compatible.Nativeint_u.of_int i) v

  external unboxed_float_array_get_float64x4 : float# array -> nativeint# -> float64x4 = "%caml_unboxed_float_array_get256_indexed_by_nativeint#"
  let unboxed_float_array_get_float64x4 arr i = unboxed_float_array_get_float64x4 arr (Stdlib_upstream_compatible.Nativeint_u.of_int i)
  external unboxed_float_array_get_float64x4_unsafe : float# array -> nativeint# -> float64x4 = "%caml_unboxed_float_array_get256u_indexed_by_nativeint#"
  let unboxed_float_array_get_float64x4_unsafe arr i = unboxed_float_array_get_float64x4_unsafe arr (Stdlib_upstream_compatible.Nativeint_u.of_int i)

  external unboxed_float_array_set_float64x4 : float# array -> nativeint# -> float64x4 -> unit = "%caml_unboxed_float_array_set256_indexed_by_nativeint#"
  let unboxed_float_array_set_float64x4 arr i v = unboxed_float_array_set_float64x4 arr (Stdlib_upstream_compatible.Nativeint_u.of_int i) v
  external unboxed_float_array_set_float64x4_unsafe : float# array -> nativeint# -> float64x4 -> unit = "%caml_unboxed_float_array_set256u_indexed_by_nativeint#"
  let unboxed_float_array_set_float64x4_unsafe arr i v = unboxed_float_array_set_float64x4_unsafe arr (Stdlib_upstream_compatible.Nativeint_u.of_int i) v

  external unboxed_float32_array_get_float32x8 : float32# array -> nativeint# -> float32x8 = "%caml_unboxed_float32_array_get256_indexed_by_nativeint#"
  let unboxed_float32_array_get_float32x8 arr i = unboxed_float32_array_get_float32x8 arr (Stdlib_upstream_compatible.Nativeint_u.of_int i)
  external unboxed_float32_array_get_float32x8_unsafe : float32# array -> nativeint# -> float32x8 = "%caml_unboxed_float32_array_get256u_indexed_by_nativeint#"
  let unboxed_float32_array_get_float32x8_unsafe arr i = unboxed_float32_array_get_float32x8_unsafe arr (Stdlib_upstream_compatible.Nativeint_u.of_int i)

  external unboxed_float32_array_set_float32x8 : float32# array -> nativeint# -> float32x8 -> unit = "%caml_unboxed_float32_array_set256_indexed_by_nativeint#"
  let unboxed_float32_array_set_float32x8 arr i v = unboxed_float32_array_set_float32x8 arr (Stdlib_upstream_compatible.Nativeint_u.of_int i) v
  external unboxed_float32_array_set_float32x8_unsafe : float32# array -> nativeint# -> float32x8 -> unit = "%caml_unboxed_float32_array_set256u_indexed_by_nativeint#"
  let unboxed_float32_array_set_float32x8_unsafe arr i v = unboxed_float32_array_set_float32x8_unsafe arr (Stdlib_upstream_compatible.Nativeint_u.of_int i) v

end)

module Int_arrays (Primitives : sig

  val int_array_get_int64x4 : int array -> int -> int64x4
  val int_array_get_int64x4_unsafe : int array -> int -> int64x4

  val int_iarray_get_int64x4 : int iarray -> int -> int64x4
  val int_iarray_get_int64x4_unsafe : int iarray -> int -> int64x4

  val int_array_set_int64x4 : int array -> int -> int64x4 -> unit
  val int_array_set_int64x4_unsafe : int array -> int -> int64x4 -> unit

  val unboxed_int64_array_get_int64x4 : int64# array -> int -> int64x4
  val unboxed_int64_array_get_int64x4_unsafe : int64# array -> int -> int64x4

  val unboxed_int64_array_set_int64x4 : int64# array -> int -> int64x4 -> unit
  val unboxed_int64_array_set_int64x4_unsafe : int64# array -> int -> int64x4 -> unit

  val unboxed_nativeint_array_get_int64x4 : nativeint# array -> int -> int64x4
  val unboxed_nativeint_array_get_int64x4_unsafe : nativeint# array -> int -> int64x4

  val unboxed_nativeint_array_set_int64x4 : nativeint# array -> int -> int64x4 -> unit
  val unboxed_nativeint_array_set_int64x4_unsafe : nativeint# array -> int -> int64x4 -> unit

  val unboxed_int32_array_get_int32x8 : int32# array -> int -> int32x8
  val unboxed_int32_array_get_int32x8_unsafe : int32# array -> int -> int32x8

  val unboxed_int32_array_set_int32x8 : int32# array -> int -> int32x8 -> unit
  val unboxed_int32_array_set_int32x8_unsafe : int32# array -> int -> int32x8 -> unit

end) = struct
  open Primitives

  let i64x4 x y z w = int64x4_of_int64s x y z w

  let i32x8 x0 x1 x2 x3 x4 x5 x6 x7 =
    let pack a b c d = Int64.(logor (shift_left (of_int32 b) 32) (of_int32 a)),
                       Int64.(logor (shift_left (of_int32 d) 32) (of_int32 c)) in
    let l1, l2 = pack x0 x1 x2 x3 in
    let l3, l4 = pack x4 x5 x6 x7 in
    int32x8_of_int64s l1 l2 l3 l4

  let tag i = Int64.(add (shift_left i 1) 1L)
  let int_array () = [| 0; 1; 2; 3; 4 |]
  let int_iarray () = [: 0; 1; 2; 3; 4 :]
  let unboxed_int64_array () = [| #0L; #1L; #2L; #3L; #4L |]
  let unboxed_nativeint_array () = [| #0n; #1n; #2n; #3n; #4n |]
  let unboxed_int32_array () = [| #0l; #1l; #2l; #3l; #4l; #5l; #6l; #7l; #8l; #9l |]

  let () =
    let int_array = int_array () in
    let i_0123 = i64x4 (tag 0L) (tag 1L) (tag 2L) (tag 3L) in
    let i_1234 = i64x4 (tag 1L) (tag 2L) (tag 3L) (tag 4L) in
    let get = int_array_get_int64x4 int_array 0 in
    eq (int64x4_first_int64 i_0123) (int64x4_second_int64 i_0123) (int64x4_third_int64 i_0123) (int64x4_fourth_int64 i_0123) (int64x4_first_int64 get) (int64x4_second_int64 get) (int64x4_third_int64 get) (int64x4_fourth_int64 get);
    let get = int_array_get_int64x4 int_array 1 in
    eq (int64x4_first_int64 i_1234) (int64x4_second_int64 i_1234) (int64x4_third_int64 i_1234) (int64x4_fourth_int64 i_1234) (int64x4_first_int64 get) (int64x4_second_int64 get) (int64x4_third_int64 get) (int64x4_fourth_int64 get);

    let i_4567 = i64x4 (tag 4L) (tag 5L) (tag 6L) (tag 7L) in
    let i_6789 = i64x4 (tag 6L) (tag 7L) (tag 8L) (tag 9L) in
    int_array_set_int64x4 int_array 0 i_4567;
    let get = int_array_get_int64x4 int_array 0 in
    eq (int64x4_first_int64 i_4567) (int64x4_second_int64 i_4567) (int64x4_third_int64 i_4567) (int64x4_fourth_int64 i_4567) (int64x4_first_int64 get) (int64x4_second_int64 get) (int64x4_third_int64 get) (int64x4_fourth_int64 get);
    int_array_set_int64x4 int_array 1 i_6789;
    let get = int_array_get_int64x4 int_array 1 in
    eq (int64x4_first_int64 i_6789) (int64x4_second_int64 i_6789) (int64x4_third_int64 i_6789) (int64x4_fourth_int64 i_6789) (int64x4_first_int64 get) (int64x4_second_int64 get) (int64x4_third_int64 get) (int64x4_fourth_int64 get)
  ;;

  let () =
    let int_array = int_array () in
    let i_0123 = i64x4 (tag 0L) (tag 1L) (tag 2L) (tag 3L) in
    let i_1234 = i64x4 (tag 1L) (tag 2L) (tag 3L) (tag 4L) in
    let get = int_array_get_int64x4_unsafe int_array 0 in
    eq (int64x4_first_int64 i_0123) (int64x4_second_int64 i_0123) (int64x4_third_int64 i_0123) (int64x4_fourth_int64 i_0123) (int64x4_first_int64 get) (int64x4_second_int64 get) (int64x4_third_int64 get) (int64x4_fourth_int64 get);
    let get = int_array_get_int64x4_unsafe int_array 1 in
    eq (int64x4_first_int64 i_1234) (int64x4_second_int64 i_1234) (int64x4_third_int64 i_1234) (int64x4_fourth_int64 i_1234) (int64x4_first_int64 get) (int64x4_second_int64 get) (int64x4_third_int64 get) (int64x4_fourth_int64 get);

    let i_4567 = i64x4 (tag 4L) (tag 5L) (tag 6L) (tag 7L) in
    let i_6789 = i64x4 (tag 6L) (tag 7L) (tag 8L) (tag 9L) in
    int_array_set_int64x4 int_array 0 i_4567;
    let get = int_array_get_int64x4_unsafe int_array 0 in
    eq (int64x4_first_int64 i_4567) (int64x4_second_int64 i_4567) (int64x4_third_int64 i_4567) (int64x4_fourth_int64 i_4567) (int64x4_first_int64 get) (int64x4_second_int64 get) (int64x4_third_int64 get) (int64x4_fourth_int64 get);
    int_array_set_int64x4 int_array 1 i_6789;
    let get = int_array_get_int64x4_unsafe int_array 1 in
    eq (int64x4_first_int64 i_6789) (int64x4_second_int64 i_6789) (int64x4_third_int64 i_6789) (int64x4_fourth_int64 i_6789) (int64x4_first_int64 get) (int64x4_second_int64 get) (int64x4_third_int64 get) (int64x4_fourth_int64 get)
  ;;

  let () =
    let a = int_array () in
    let i_0 = i64x4 (tag 0L) (tag 0L) (tag 0L) (tag 0L) in
    let fail a i =
      try
        let _ = int_array_get_int64x4 a i in
        Printf.printf "Did not fail on index %d\n" i
      with | Invalid_argument s when s = "index out of bounds" -> ();
      try
        let _ = int_array_set_int64x4 a i i_0 in
        Printf.printf "Did not fail on index %d\n" i
      with | Invalid_argument s when s = "index out of bounds" -> ()
    in
    fail a (-1);
    fail a 3;
    fail a 4;
    fail [||] 0;
    fail [|0|] 0;
    fail [|0|] 1;
    fail [|0|] (-1)
  ;;

  let () =
    let int_iarray = int_iarray () in
    let i_0123 = i64x4 (tag 0L) (tag 1L) (tag 2L) (tag 3L) in
    let i_1234 = i64x4 (tag 1L) (tag 2L) (tag 3L) (tag 4L) in
    let get = int_iarray_get_int64x4_unsafe int_iarray 0 in
    eq (int64x4_first_int64 i_0123) (int64x4_second_int64 i_0123) (int64x4_third_int64 i_0123) (int64x4_fourth_int64 i_0123) (int64x4_first_int64 get) (int64x4_second_int64 get) (int64x4_third_int64 get) (int64x4_fourth_int64 get);
    let get = int_iarray_get_int64x4_unsafe int_iarray 1 in
    eq (int64x4_first_int64 i_1234) (int64x4_second_int64 i_1234) (int64x4_third_int64 i_1234) (int64x4_fourth_int64 i_1234) (int64x4_first_int64 get) (int64x4_second_int64 get) (int64x4_third_int64 get) (int64x4_fourth_int64 get);
  ;;

  let () =
    let a = int_iarray () in
    let fail a i =
      try
        let _ = int_iarray_get_int64x4 a i in
        Printf.printf "Did not fail on index %d\n" i
      with | Invalid_argument s when s = "index out of bounds" -> ();
    in
    fail a (-1);
    fail a 3;
    fail a 4;
    fail [::] 0;
    fail [: 0 :] 0;
    fail [: 0 :] 1;
    fail [: 0 :] (-1)
  ;;

  let () =
    let unboxed_int64_array = unboxed_int64_array () in
    let i_0123 = i64x4 0L 1L 2L 3L in
    let i_1234 = i64x4 1L 2L 3L 4L in
    let get = unboxed_int64_array_get_int64x4 unboxed_int64_array 0 in
    eq (int64x4_first_int64 i_0123) (int64x4_second_int64 i_0123) (int64x4_third_int64 i_0123) (int64x4_fourth_int64 i_0123) (int64x4_first_int64 get) (int64x4_second_int64 get) (int64x4_third_int64 get) (int64x4_fourth_int64 get);
    let get = unboxed_int64_array_get_int64x4 unboxed_int64_array 1 in
    eq (int64x4_first_int64 i_1234) (int64x4_second_int64 i_1234) (int64x4_third_int64 i_1234) (int64x4_fourth_int64 i_1234) (int64x4_first_int64 get) (int64x4_second_int64 get) (int64x4_third_int64 get) (int64x4_fourth_int64 get);

    let i_4567 = i64x4 4L 5L 6L 7L in
    let i_6789 = i64x4 6L 7L 8L 9L in
    unboxed_int64_array_set_int64x4 unboxed_int64_array 0 i_4567;
    let get = unboxed_int64_array_get_int64x4 unboxed_int64_array 0 in
    eq (int64x4_first_int64 i_4567) (int64x4_second_int64 i_4567) (int64x4_third_int64 i_4567) (int64x4_fourth_int64 i_4567) (int64x4_first_int64 get) (int64x4_second_int64 get) (int64x4_third_int64 get) (int64x4_fourth_int64 get);
    unboxed_int64_array_set_int64x4 unboxed_int64_array 1 i_6789;
    let get = unboxed_int64_array_get_int64x4 unboxed_int64_array 1 in
    eq (int64x4_first_int64 i_6789) (int64x4_second_int64 i_6789) (int64x4_third_int64 i_6789) (int64x4_fourth_int64 i_6789) (int64x4_first_int64 get) (int64x4_second_int64 get) (int64x4_third_int64 get) (int64x4_fourth_int64 get)
  ;;

  let () =
    let unboxed_int64_array = unboxed_int64_array () in
    let i_0123 = i64x4 0L 1L 2L 3L in
    let i_1234 = i64x4 1L 2L 3L 4L in
    let get = unboxed_int64_array_get_int64x4_unsafe unboxed_int64_array 0 in
    eq (int64x4_first_int64 i_0123) (int64x4_second_int64 i_0123) (int64x4_third_int64 i_0123) (int64x4_fourth_int64 i_0123) (int64x4_first_int64 get) (int64x4_second_int64 get) (int64x4_third_int64 get) (int64x4_fourth_int64 get);
    let get = unboxed_int64_array_get_int64x4_unsafe unboxed_int64_array 1 in
    eq (int64x4_first_int64 i_1234) (int64x4_second_int64 i_1234) (int64x4_third_int64 i_1234) (int64x4_fourth_int64 i_1234) (int64x4_first_int64 get) (int64x4_second_int64 get) (int64x4_third_int64 get) (int64x4_fourth_int64 get);

    let i_4567 = i64x4 4L 5L 6L 7L in
    let i_6789 = i64x4 6L 7L 8L 9L in
    unboxed_int64_array_set_int64x4_unsafe unboxed_int64_array 0 i_4567;
    let get = unboxed_int64_array_get_int64x4_unsafe unboxed_int64_array 0 in
    eq (int64x4_first_int64 i_4567) (int64x4_second_int64 i_4567) (int64x4_third_int64 i_4567) (int64x4_fourth_int64 i_4567) (int64x4_first_int64 get) (int64x4_second_int64 get) (int64x4_third_int64 get) (int64x4_fourth_int64 get);
    unboxed_int64_array_set_int64x4_unsafe unboxed_int64_array 1 i_6789;
    let get = unboxed_int64_array_get_int64x4_unsafe unboxed_int64_array 1 in
    eq (int64x4_first_int64 i_6789) (int64x4_second_int64 i_6789) (int64x4_third_int64 i_6789) (int64x4_fourth_int64 i_6789) (int64x4_first_int64 get) (int64x4_second_int64 get) (int64x4_third_int64 get) (int64x4_fourth_int64 get)
  ;;

  let () =
    let a = unboxed_int64_array () in
    let i_0 = i64x4 0L 0L 0L 0L in
    let fail a i =
      try
        let _ = unboxed_int64_array_get_int64x4 a i in
        let _ = unboxed_int64_array_set_int64x4 a i i_0 in
        Printf.printf "Did not fail on index %d\n" i
      with | Invalid_argument s when s = "index out of bounds" -> ()
    in
    fail a (-1);
    fail a 3;
    fail a 4;
    fail [||] 0;
    fail [|#0L|] 0;
    fail [|#0L|] 1;
    fail [|#0L|] (-1)
  ;;

  let () =
    let unboxed_nativeint_array = unboxed_nativeint_array () in
    let i_0123 = i64x4 0L 1L 2L 3L in
    let i_1234 = i64x4 1L 2L 3L 4L in
    let get = unboxed_nativeint_array_get_int64x4 unboxed_nativeint_array 0 in
    eq (int64x4_first_int64 i_0123) (int64x4_second_int64 i_0123) (int64x4_third_int64 i_0123) (int64x4_fourth_int64 i_0123) (int64x4_first_int64 get) (int64x4_second_int64 get) (int64x4_third_int64 get) (int64x4_fourth_int64 get);
    let get = unboxed_nativeint_array_get_int64x4 unboxed_nativeint_array 1 in
    eq (int64x4_first_int64 i_1234) (int64x4_second_int64 i_1234) (int64x4_third_int64 i_1234) (int64x4_fourth_int64 i_1234) (int64x4_first_int64 get) (int64x4_second_int64 get) (int64x4_third_int64 get) (int64x4_fourth_int64 get);

    let i_4567 = i64x4 4L 5L 6L 7L in
    let i_6789 = i64x4 6L 7L 8L 9L in
    unboxed_nativeint_array_set_int64x4 unboxed_nativeint_array 0 i_4567;
    let get = unboxed_nativeint_array_get_int64x4 unboxed_nativeint_array 0 in
    eq (int64x4_first_int64 i_4567) (int64x4_second_int64 i_4567) (int64x4_third_int64 i_4567) (int64x4_fourth_int64 i_4567) (int64x4_first_int64 get) (int64x4_second_int64 get) (int64x4_third_int64 get) (int64x4_fourth_int64 get);
    unboxed_nativeint_array_set_int64x4 unboxed_nativeint_array 1 i_6789;
    let get = unboxed_nativeint_array_get_int64x4 unboxed_nativeint_array 1 in
    eq (int64x4_first_int64 i_6789) (int64x4_second_int64 i_6789) (int64x4_third_int64 i_6789) (int64x4_fourth_int64 i_6789) (int64x4_first_int64 get) (int64x4_second_int64 get) (int64x4_third_int64 get) (int64x4_fourth_int64 get)
  ;;

  let () =
    let unboxed_nativeint_array = unboxed_nativeint_array () in
    let i_0123 = i64x4 0L 1L 2L 3L in
    let i_1234 = i64x4 1L 2L 3L 4L in
    let get = unboxed_nativeint_array_get_int64x4_unsafe unboxed_nativeint_array 0 in
    eq (int64x4_first_int64 i_0123) (int64x4_second_int64 i_0123) (int64x4_third_int64 i_0123) (int64x4_fourth_int64 i_0123) (int64x4_first_int64 get) (int64x4_second_int64 get) (int64x4_third_int64 get) (int64x4_fourth_int64 get);
    let get = unboxed_nativeint_array_get_int64x4_unsafe unboxed_nativeint_array 1 in
    eq (int64x4_first_int64 i_1234) (int64x4_second_int64 i_1234) (int64x4_third_int64 i_1234) (int64x4_fourth_int64 i_1234) (int64x4_first_int64 get) (int64x4_second_int64 get) (int64x4_third_int64 get) (int64x4_fourth_int64 get);

    let i_4567 = i64x4 4L 5L 6L 7L in
    let i_6789 = i64x4 6L 7L 8L 9L in
    unboxed_nativeint_array_set_int64x4_unsafe unboxed_nativeint_array 0 i_4567;
    let get = unboxed_nativeint_array_get_int64x4_unsafe unboxed_nativeint_array 0 in
    eq (int64x4_first_int64 i_4567) (int64x4_second_int64 i_4567) (int64x4_third_int64 i_4567) (int64x4_fourth_int64 i_4567) (int64x4_first_int64 get) (int64x4_second_int64 get) (int64x4_third_int64 get) (int64x4_fourth_int64 get);
    unboxed_nativeint_array_set_int64x4_unsafe unboxed_nativeint_array 1 i_6789;
    let get = unboxed_nativeint_array_get_int64x4_unsafe unboxed_nativeint_array 1 in
    eq (int64x4_first_int64 i_6789) (int64x4_second_int64 i_6789) (int64x4_third_int64 i_6789) (int64x4_fourth_int64 i_6789) (int64x4_first_int64 get) (int64x4_second_int64 get) (int64x4_third_int64 get) (int64x4_fourth_int64 get)
  ;;

  let () =
    let a = unboxed_nativeint_array () in
    let i_0 = i64x4 0L 0L 0L 0L in
    let fail a i =
      try
        let _ = unboxed_nativeint_array_get_int64x4 a i in
        let _ = unboxed_nativeint_array_set_int64x4 a i i_0 in
        Printf.printf "Did not fail on index %d\n" i
      with | Invalid_argument s when s = "index out of bounds" -> ()
    in
    fail a (-1);
    fail a 3;
    fail a 4;
    fail [||] 0;
    fail [|#0n|] 0;
    fail [|#0n|] 1;
    fail [|#0n|] (-1)
  ;;

  let () =
    let unboxed_int32_array = unboxed_int32_array () in
    let i_01234567 = i32x8 0l 1l 2l 3l 4l 5l 6l 7l in
    let i_23456789 = i32x8 2l 3l 4l 5l 6l 7l 8l 9l in
    let get = unboxed_int32_array_get_int32x8 unboxed_int32_array 0 in
    eq (int32x8_first_int64 i_01234567) (int32x8_second_int64 i_01234567) (int32x8_third_int64 i_01234567) (int32x8_fourth_int64 i_01234567) (int32x8_first_int64 get) (int32x8_second_int64 get) (int32x8_third_int64 get) (int32x8_fourth_int64 get);
    let get = unboxed_int32_array_get_int32x8 unboxed_int32_array 2 in
    eq (int32x8_first_int64 i_23456789) (int32x8_second_int64 i_23456789) (int32x8_third_int64 i_23456789) (int32x8_fourth_int64 i_23456789) (int32x8_first_int64 get) (int32x8_second_int64 get) (int32x8_third_int64 get) (int32x8_fourth_int64 get);

    let i_456789ab = i32x8 4l 5l 6l 7l 8l 9l 10l 11l in
    let i_6789abcd = i32x8 6l 7l 8l 9l 10l 11l 12l 13l in
    unboxed_int32_array_set_int32x8 unboxed_int32_array 0 i_456789ab;
    let get = unboxed_int32_array_get_int32x8 unboxed_int32_array 0 in
    eq (int32x8_first_int64 i_456789ab) (int32x8_second_int64 i_456789ab) (int32x8_third_int64 i_456789ab) (int32x8_fourth_int64 i_456789ab) (int32x8_first_int64 get) (int32x8_second_int64 get) (int32x8_third_int64 get) (int32x8_fourth_int64 get);
    unboxed_int32_array_set_int32x8 unboxed_int32_array 1 i_6789abcd;
    let get = unboxed_int32_array_get_int32x8 unboxed_int32_array 1 in
    eq (int32x8_first_int64 i_6789abcd) (int32x8_second_int64 i_6789abcd) (int32x8_third_int64 i_6789abcd) (int32x8_fourth_int64 i_6789abcd) (int32x8_first_int64 get) (int32x8_second_int64 get) (int32x8_third_int64 get) (int32x8_fourth_int64 get)
  ;;

  let () =
    let unboxed_int32_array = unboxed_int32_array () in
    let i_01234567 = i32x8 0l 1l 2l 3l 4l 5l 6l 7l in
    let i_23456789 = i32x8 2l 3l 4l 5l 6l 7l 8l 9l in
    let get = unboxed_int32_array_get_int32x8_unsafe unboxed_int32_array 0 in
    eq (int32x8_first_int64 i_01234567) (int32x8_second_int64 i_01234567) (int32x8_third_int64 i_01234567) (int32x8_fourth_int64 i_01234567) (int32x8_first_int64 get) (int32x8_second_int64 get) (int32x8_third_int64 get) (int32x8_fourth_int64 get);
    let get = unboxed_int32_array_get_int32x8_unsafe unboxed_int32_array 2 in
    eq (int32x8_first_int64 i_23456789) (int32x8_second_int64 i_23456789) (int32x8_third_int64 i_23456789) (int32x8_fourth_int64 i_23456789) (int32x8_first_int64 get) (int32x8_second_int64 get) (int32x8_third_int64 get) (int32x8_fourth_int64 get);

    let i_456789ab = i32x8 4l 5l 6l 7l 8l 9l 10l 11l in
    let i_6789abcd = i32x8 6l 7l 8l 9l 10l 11l 12l 13l in
    unboxed_int32_array_set_int32x8_unsafe unboxed_int32_array 0 i_456789ab;
    let get = unboxed_int32_array_get_int32x8_unsafe unboxed_int32_array 0 in
    eq (int32x8_first_int64 i_456789ab) (int32x8_second_int64 i_456789ab) (int32x8_third_int64 i_456789ab) (int32x8_fourth_int64 i_456789ab) (int32x8_first_int64 get) (int32x8_second_int64 get) (int32x8_third_int64 get) (int32x8_fourth_int64 get);
    unboxed_int32_array_set_int32x8_unsafe unboxed_int32_array 1 i_6789abcd;
    let get = unboxed_int32_array_get_int32x8_unsafe unboxed_int32_array 1 in
    eq (int32x8_first_int64 i_6789abcd) (int32x8_second_int64 i_6789abcd) (int32x8_third_int64 i_6789abcd) (int32x8_fourth_int64 i_6789abcd) (int32x8_first_int64 get) (int32x8_second_int64 get) (int32x8_third_int64 get) (int32x8_fourth_int64 get)
  ;;

  let () =
    let a = unboxed_int32_array () in
    let i_0 = i32x8 0l 0l 0l 0l 0l 0l 0l 0l in
    let fail a i =
      try
        let _ = unboxed_int32_array_get_int32x8 a i in
        let _ = unboxed_int32_array_set_int32x8 a i i_0 in
        Printf.printf "Did not fail on index %d\n" i
      with | Invalid_argument s when s = "index out of bounds" -> ()
    in
    fail a (-1);
    fail a 5;
    fail a 6;
    fail [||] 0;
    fail [|#0l|] 0;
    fail [|#0l|] 1;
    fail [|#0l|] 2;
    fail [|#0l|] 3;
    fail [|#0l;#1l|] 0;
    fail [|#0l;#1l|] 1;
    fail [|#0l;#1l|] 2;
    fail [|#0l;#1l|] 3;
    fail [|#0l;#1l;#2l|] 0;
    fail [|#0l;#1l;#2l|] 1;
    fail [|#0l;#1l;#2l|] 2;
    fail [|#0l;#1l;#2l|] 3;
    fail [|#0l|] (-1)
  ;;
end

module _ = Int_arrays(struct

  external int_array_get_int64x4 : int array -> int -> int64x4 = "%caml_int_array_get256"
  external int_array_get_int64x4_unsafe : int array -> int -> int64x4 = "%caml_int_array_get256u"

  external int_iarray_get_int64x4 : int iarray -> int -> int64x4 = "%caml_int_array_get256"
  external int_iarray_get_int64x4_unsafe : int iarray -> int -> int64x4 = "%caml_int_array_get256u"

  external int_array_set_int64x4 : int array -> int -> int64x4 -> unit = "%caml_int_array_set256"
  external int_array_set_int64x4_unsafe : int array -> int -> int64x4 -> unit = "%caml_int_array_set256u"

  external unboxed_int64_array_get_int64x4 : int64# array -> int -> int64x4 = "%caml_unboxed_int64_array_get256"
  external unboxed_int64_array_get_int64x4_unsafe : int64# array -> int -> int64x4 = "%caml_unboxed_int64_array_get256u"

  external unboxed_int64_array_set_int64x4 : int64# array -> int -> int64x4 -> unit = "%caml_unboxed_int64_array_set256"
  external unboxed_int64_array_set_int64x4_unsafe : int64# array -> int -> int64x4 -> unit = "%caml_unboxed_int64_array_set256u"

  external unboxed_nativeint_array_get_int64x4 : nativeint# array -> int -> int64x4 = "%caml_unboxed_nativeint_array_get256"
  external unboxed_nativeint_array_get_int64x4_unsafe : nativeint# array -> int -> int64x4 = "%caml_unboxed_nativeint_array_get256u"

  external unboxed_nativeint_array_set_int64x4 : nativeint# array -> int -> int64x4 -> unit = "%caml_unboxed_nativeint_array_set256"
  external unboxed_nativeint_array_set_int64x4_unsafe : nativeint# array -> int -> int64x4 -> unit = "%caml_unboxed_nativeint_array_set256u"

  external unboxed_int32_array_get_int32x8 : int32# array -> int -> int32x8 = "%caml_unboxed_int32_array_get256"
  external unboxed_int32_array_get_int32x8_unsafe : int32# array -> int -> int32x8 = "%caml_unboxed_int32_array_get256u"

  external unboxed_int32_array_set_int32x8 : int32# array -> int -> int32x8 -> unit = "%caml_unboxed_int32_array_set256"
  external unboxed_int32_array_set_int32x8_unsafe : int32# array -> int -> int32x8 -> unit = "%caml_unboxed_int32_array_set256u"

end)

module _ = Int_arrays(struct

  external int_array_get_int64x4 : int array -> int32# -> int64x4 = "%caml_int_array_get256_indexed_by_int32#"
  let int_array_get_int64x4 arr i = int_array_get_int64x4 arr (Stdlib_upstream_compatible.Int32_u.of_int i)
  external int_array_get_int64x4_unsafe : int array -> int32# -> int64x4 = "%caml_int_array_get256u_indexed_by_int32#"
  let int_array_get_int64x4_unsafe arr i = int_array_get_int64x4_unsafe arr (Stdlib_upstream_compatible.Int32_u.of_int i)

  external int_iarray_get_int64x4 : int iarray -> int32# -> int64x4 = "%caml_int_array_get256_indexed_by_int32#"
  let int_iarray_get_int64x4 arr i = int_iarray_get_int64x4 arr (Stdlib_upstream_compatible.Int32_u.of_int i)
  external int_iarray_get_int64x4_unsafe : int iarray -> int32# -> int64x4 = "%caml_int_array_get256u_indexed_by_int32#"
  let int_iarray_get_int64x4_unsafe arr i = int_iarray_get_int64x4_unsafe arr (Stdlib_upstream_compatible.Int32_u.of_int i)

  external int_array_set_int64x4 : int array -> int32# -> int64x4 -> unit = "%caml_int_array_set256_indexed_by_int32#"
  let int_array_set_int64x4 arr i v = int_array_set_int64x4 arr (Stdlib_upstream_compatible.Int32_u.of_int i) v
  external int_array_set_int64x4_unsafe : int array -> int32# -> int64x4 -> unit = "%caml_int_array_set256u_indexed_by_int32#"
  let int_array_set_int64x4_unsafe arr i v = int_array_set_int64x4_unsafe arr (Stdlib_upstream_compatible.Int32_u.of_int i) v

  external unboxed_int64_array_get_int64x4 : int64# array -> int32# -> int64x4 = "%caml_unboxed_int64_array_get256_indexed_by_int32#"
  let unboxed_int64_array_get_int64x4 arr i = unboxed_int64_array_get_int64x4 arr (Stdlib_upstream_compatible.Int32_u.of_int i)
  external unboxed_int64_array_get_int64x4_unsafe : int64# array -> int32# -> int64x4 = "%caml_unboxed_int64_array_get256u_indexed_by_int32#"
  let unboxed_int64_array_get_int64x4_unsafe arr i = unboxed_int64_array_get_int64x4_unsafe arr (Stdlib_upstream_compatible.Int32_u.of_int i)

  external unboxed_int64_array_set_int64x4 : int64# array -> int32# -> int64x4 -> unit = "%caml_unboxed_int64_array_set256_indexed_by_int32#"
  let unboxed_int64_array_set_int64x4 arr i v = unboxed_int64_array_set_int64x4 arr (Stdlib_upstream_compatible.Int32_u.of_int i) v
  external unboxed_int64_array_set_int64x4_unsafe : int64# array -> int32# -> int64x4 -> unit = "%caml_unboxed_int64_array_set256u_indexed_by_int32#"
  let unboxed_int64_array_set_int64x4_unsafe arr i v = unboxed_int64_array_set_int64x4_unsafe arr (Stdlib_upstream_compatible.Int32_u.of_int i) v

  external unboxed_nativeint_array_get_int64x4 : nativeint# array -> int32# -> int64x4 = "%caml_unboxed_nativeint_array_get256_indexed_by_int32#"
  let unboxed_nativeint_array_get_int64x4 arr i = unboxed_nativeint_array_get_int64x4 arr (Stdlib_upstream_compatible.Int32_u.of_int i)
  external unboxed_nativeint_array_get_int64x4_unsafe : nativeint# array -> int32# -> int64x4 = "%caml_unboxed_nativeint_array_get256u_indexed_by_int32#"
  let unboxed_nativeint_array_get_int64x4_unsafe arr i = unboxed_nativeint_array_get_int64x4_unsafe arr (Stdlib_upstream_compatible.Int32_u.of_int i)

  external unboxed_nativeint_array_set_int64x4 : nativeint# array -> int32# -> int64x4 -> unit = "%caml_unboxed_nativeint_array_set256_indexed_by_int32#"
  let unboxed_nativeint_array_set_int64x4 arr i v = unboxed_nativeint_array_set_int64x4 arr (Stdlib_upstream_compatible.Int32_u.of_int i) v
  external unboxed_nativeint_array_set_int64x4_unsafe : nativeint# array -> int32# -> int64x4 -> unit = "%caml_unboxed_nativeint_array_set256u_indexed_by_int32#"
  let unboxed_nativeint_array_set_int64x4_unsafe arr i v = unboxed_nativeint_array_set_int64x4_unsafe arr (Stdlib_upstream_compatible.Int32_u.of_int i) v

  external unboxed_int32_array_get_int32x8 : int32# array -> int32# -> int32x8 = "%caml_unboxed_int32_array_get256_indexed_by_int32#"
  let unboxed_int32_array_get_int32x8 arr i = unboxed_int32_array_get_int32x8 arr (Stdlib_upstream_compatible.Int32_u.of_int i)
  external unboxed_int32_array_get_int32x8_unsafe : int32# array -> int32# -> int32x8 = "%caml_unboxed_int32_array_get256u_indexed_by_int32#"
  let unboxed_int32_array_get_int32x8_unsafe arr i = unboxed_int32_array_get_int32x8_unsafe arr (Stdlib_upstream_compatible.Int32_u.of_int i)

  external unboxed_int32_array_set_int32x8 : int32# array -> int32# -> int32x8 -> unit = "%caml_unboxed_int32_array_set256_indexed_by_int32#"
  let unboxed_int32_array_set_int32x8 arr i v = unboxed_int32_array_set_int32x8 arr (Stdlib_upstream_compatible.Int32_u.of_int i) v
  external unboxed_int32_array_set_int32x8_unsafe : int32# array -> int32# -> int32x8 -> unit = "%caml_unboxed_int32_array_set256u_indexed_by_int32#"
  let unboxed_int32_array_set_int32x8_unsafe arr i v = unboxed_int32_array_set_int32x8_unsafe arr (Stdlib_upstream_compatible.Int32_u.of_int i) v

end)

module _ = Int_arrays(struct

  external int_array_get_int64x4 : int array -> int64# -> int64x4 = "%caml_int_array_get256_indexed_by_int64#"
  let int_array_get_int64x4 arr i = int_array_get_int64x4 arr (Stdlib_upstream_compatible.Int64_u.of_int i)
  external int_array_get_int64x4_unsafe : int array -> int64# -> int64x4 = "%caml_int_array_get256u_indexed_by_int64#"
  let int_array_get_int64x4_unsafe arr i = int_array_get_int64x4_unsafe arr (Stdlib_upstream_compatible.Int64_u.of_int i)

  external int_iarray_get_int64x4 : int iarray -> int64# -> int64x4 = "%caml_int_array_get256_indexed_by_int64#"
  let int_iarray_get_int64x4 arr i = int_iarray_get_int64x4 arr (Stdlib_upstream_compatible.Int64_u.of_int i)
  external int_iarray_get_int64x4_unsafe : int iarray -> int64# -> int64x4 = "%caml_int_array_get256u_indexed_by_int64#"
  let int_iarray_get_int64x4_unsafe arr i = int_iarray_get_int64x4_unsafe arr (Stdlib_upstream_compatible.Int64_u.of_int i)

  external int_array_set_int64x4 : int array -> int64# -> int64x4 -> unit = "%caml_int_array_set256_indexed_by_int64#"
  let int_array_set_int64x4 arr i v = int_array_set_int64x4 arr (Stdlib_upstream_compatible.Int64_u.of_int i) v
  external int_array_set_int64x4_unsafe : int array -> int64# -> int64x4 -> unit = "%caml_int_array_set256u_indexed_by_int64#"
  let int_array_set_int64x4_unsafe arr i v = int_array_set_int64x4_unsafe arr (Stdlib_upstream_compatible.Int64_u.of_int i) v

  external unboxed_int64_array_get_int64x4 : int64# array -> int64# -> int64x4 = "%caml_unboxed_int64_array_get256_indexed_by_int64#"
  let unboxed_int64_array_get_int64x4 arr i = unboxed_int64_array_get_int64x4 arr (Stdlib_upstream_compatible.Int64_u.of_int i)
  external unboxed_int64_array_get_int64x4_unsafe : int64# array -> int64# -> int64x4 = "%caml_unboxed_int64_array_get256u_indexed_by_int64#"
  let unboxed_int64_array_get_int64x4_unsafe arr i = unboxed_int64_array_get_int64x4_unsafe arr (Stdlib_upstream_compatible.Int64_u.of_int i)

  external unboxed_int64_array_set_int64x4 : int64# array -> int64# -> int64x4 -> unit = "%caml_unboxed_int64_array_set256_indexed_by_int64#"
  let unboxed_int64_array_set_int64x4 arr i v = unboxed_int64_array_set_int64x4 arr (Stdlib_upstream_compatible.Int64_u.of_int i) v
  external unboxed_int64_array_set_int64x4_unsafe : int64# array -> int64# -> int64x4 -> unit = "%caml_unboxed_int64_array_set256u_indexed_by_int64#"
  let unboxed_int64_array_set_int64x4_unsafe arr i v = unboxed_int64_array_set_int64x4_unsafe arr (Stdlib_upstream_compatible.Int64_u.of_int i) v

  external unboxed_nativeint_array_get_int64x4 : nativeint# array -> int64# -> int64x4 = "%caml_unboxed_nativeint_array_get256_indexed_by_int64#"
  let unboxed_nativeint_array_get_int64x4 arr i = unboxed_nativeint_array_get_int64x4 arr (Stdlib_upstream_compatible.Int64_u.of_int i)
  external unboxed_nativeint_array_get_int64x4_unsafe : nativeint# array -> int64# -> int64x4 = "%caml_unboxed_nativeint_array_get256u_indexed_by_int64#"
  let unboxed_nativeint_array_get_int64x4_unsafe arr i = unboxed_nativeint_array_get_int64x4_unsafe arr (Stdlib_upstream_compatible.Int64_u.of_int i)

  external unboxed_nativeint_array_set_int64x4 : nativeint# array -> int64# -> int64x4 -> unit = "%caml_unboxed_nativeint_array_set256_indexed_by_int64#"
  let unboxed_nativeint_array_set_int64x4 arr i v = unboxed_nativeint_array_set_int64x4 arr (Stdlib_upstream_compatible.Int64_u.of_int i) v
  external unboxed_nativeint_array_set_int64x4_unsafe : nativeint# array -> int64# -> int64x4 -> unit = "%caml_unboxed_nativeint_array_set256u_indexed_by_int64#"
  let unboxed_nativeint_array_set_int64x4_unsafe arr i v = unboxed_nativeint_array_set_int64x4_unsafe arr (Stdlib_upstream_compatible.Int64_u.of_int i) v

  external unboxed_int32_array_get_int32x8 : int32# array -> int64# -> int32x8 = "%caml_unboxed_int32_array_get256_indexed_by_int64#"
  let unboxed_int32_array_get_int32x8 arr i = unboxed_int32_array_get_int32x8 arr (Stdlib_upstream_compatible.Int64_u.of_int i)
  external unboxed_int32_array_get_int32x8_unsafe : int32# array -> int64# -> int32x8 = "%caml_unboxed_int32_array_get256u_indexed_by_int64#"
  let unboxed_int32_array_get_int32x8_unsafe arr i = unboxed_int32_array_get_int32x8_unsafe arr (Stdlib_upstream_compatible.Int64_u.of_int i)

  external unboxed_int32_array_set_int32x8 : int32# array -> int64# -> int32x8 -> unit = "%caml_unboxed_int32_array_set256_indexed_by_int64#"
  let unboxed_int32_array_set_int32x8 arr i v = unboxed_int32_array_set_int32x8 arr (Stdlib_upstream_compatible.Int64_u.of_int i) v
  external unboxed_int32_array_set_int32x8_unsafe : int32# array -> int64# -> int32x8 -> unit = "%caml_unboxed_int32_array_set256u_indexed_by_int64#"
  let unboxed_int32_array_set_int32x8_unsafe arr i v = unboxed_int32_array_set_int32x8_unsafe arr (Stdlib_upstream_compatible.Int64_u.of_int i) v

end)

module _ = Int_arrays(struct

  external int_array_get_int64x4 : int array -> nativeint# -> int64x4 = "%caml_int_array_get256_indexed_by_nativeint#"
  let int_array_get_int64x4 arr i = int_array_get_int64x4 arr (Stdlib_upstream_compatible.Nativeint_u.of_int i)
  external int_array_get_int64x4_unsafe : int array -> nativeint# -> int64x4 = "%caml_int_array_get256u_indexed_by_nativeint#"
  let int_array_get_int64x4_unsafe arr i = int_array_get_int64x4_unsafe arr (Stdlib_upstream_compatible.Nativeint_u.of_int i)

  external int_iarray_get_int64x4 : int iarray -> nativeint# -> int64x4 = "%caml_int_array_get256_indexed_by_nativeint#"
  let int_iarray_get_int64x4 arr i = int_iarray_get_int64x4 arr (Stdlib_upstream_compatible.Nativeint_u.of_int i)
  external int_iarray_get_int64x4_unsafe : int iarray -> nativeint# -> int64x4 = "%caml_int_array_get256u_indexed_by_nativeint#"
  let int_iarray_get_int64x4_unsafe arr i = int_iarray_get_int64x4_unsafe arr (Stdlib_upstream_compatible.Nativeint_u.of_int i)

  external int_array_set_int64x4 : int array -> nativeint# -> int64x4 -> unit = "%caml_int_array_set256_indexed_by_nativeint#"
  let int_array_set_int64x4 arr i v = int_array_set_int64x4 arr (Stdlib_upstream_compatible.Nativeint_u.of_int i) v
  external int_array_set_int64x4_unsafe : int array -> nativeint# -> int64x4 -> unit = "%caml_int_array_set256u_indexed_by_nativeint#"
  let int_array_set_int64x4_unsafe arr i v = int_array_set_int64x4_unsafe arr (Stdlib_upstream_compatible.Nativeint_u.of_int i) v

  external unboxed_int64_array_get_int64x4 : int64# array -> nativeint# -> int64x4 = "%caml_unboxed_int64_array_get256_indexed_by_nativeint#"
  let unboxed_int64_array_get_int64x4 arr i = unboxed_int64_array_get_int64x4 arr (Stdlib_upstream_compatible.Nativeint_u.of_int i)
  external unboxed_int64_array_get_int64x4_unsafe : int64# array -> nativeint# -> int64x4 = "%caml_unboxed_int64_array_get256u_indexed_by_nativeint#"
  let unboxed_int64_array_get_int64x4_unsafe arr i = unboxed_int64_array_get_int64x4_unsafe arr (Stdlib_upstream_compatible.Nativeint_u.of_int i)

  external unboxed_int64_array_set_int64x4 : int64# array -> nativeint# -> int64x4 -> unit = "%caml_unboxed_int64_array_set256_indexed_by_nativeint#"
  let unboxed_int64_array_set_int64x4 arr i v = unboxed_int64_array_set_int64x4 arr (Stdlib_upstream_compatible.Nativeint_u.of_int i) v
  external unboxed_int64_array_set_int64x4_unsafe : int64# array -> nativeint# -> int64x4 -> unit = "%caml_unboxed_int64_array_set256u_indexed_by_nativeint#"
  let unboxed_int64_array_set_int64x4_unsafe arr i v = unboxed_int64_array_set_int64x4_unsafe arr (Stdlib_upstream_compatible.Nativeint_u.of_int i) v

  external unboxed_nativeint_array_get_int64x4 : nativeint# array -> nativeint# -> int64x4 = "%caml_unboxed_nativeint_array_get256_indexed_by_nativeint#"
  let unboxed_nativeint_array_get_int64x4 arr i = unboxed_nativeint_array_get_int64x4 arr (Stdlib_upstream_compatible.Nativeint_u.of_int i)
  external unboxed_nativeint_array_get_int64x4_unsafe : nativeint# array -> nativeint# -> int64x4 = "%caml_unboxed_nativeint_array_get256u_indexed_by_nativeint#"
  let unboxed_nativeint_array_get_int64x4_unsafe arr i = unboxed_nativeint_array_get_int64x4_unsafe arr (Stdlib_upstream_compatible.Nativeint_u.of_int i)

  external unboxed_nativeint_array_set_int64x4 : nativeint# array -> nativeint# -> int64x4 -> unit = "%caml_unboxed_nativeint_array_set256_indexed_by_nativeint#"
  let unboxed_nativeint_array_set_int64x4 arr i v = unboxed_nativeint_array_set_int64x4 arr (Stdlib_upstream_compatible.Nativeint_u.of_int i) v
  external unboxed_nativeint_array_set_int64x4_unsafe : nativeint# array -> nativeint# -> int64x4 -> unit = "%caml_unboxed_nativeint_array_set256u_indexed_by_nativeint#"
  let unboxed_nativeint_array_set_int64x4_unsafe arr i v = unboxed_nativeint_array_set_int64x4_unsafe arr (Stdlib_upstream_compatible.Nativeint_u.of_int i) v

  external unboxed_int32_array_get_int32x8 : int32# array -> nativeint# -> int32x8 = "%caml_unboxed_int32_array_get256_indexed_by_nativeint#"
  let unboxed_int32_array_get_int32x8 arr i = unboxed_int32_array_get_int32x8 arr (Stdlib_upstream_compatible.Nativeint_u.of_int i)
  external unboxed_int32_array_get_int32x8_unsafe : int32# array -> nativeint# -> int32x8 = "%caml_unboxed_int32_array_get256u_indexed_by_nativeint#"
  let unboxed_int32_array_get_int32x8_unsafe arr i = unboxed_int32_array_get_int32x8_unsafe arr (Stdlib_upstream_compatible.Nativeint_u.of_int i)

  external unboxed_int32_array_set_int32x8 : int32# array -> nativeint# -> int32x8 -> unit = "%caml_unboxed_int32_array_set256_indexed_by_nativeint#"
  let unboxed_int32_array_set_int32x8 arr i v = unboxed_int32_array_set_int32x8 arr (Stdlib_upstream_compatible.Nativeint_u.of_int i) v
  external unboxed_int32_array_set_int32x8_unsafe : int32# array -> nativeint# -> int32x8 -> unit = "%caml_unboxed_int32_array_set256u_indexed_by_nativeint#"
  let unboxed_int32_array_set_int32x8_unsafe arr i v = unboxed_int32_array_set_int32x8_unsafe arr (Stdlib_upstream_compatible.Nativeint_u.of_int i) v

end)
