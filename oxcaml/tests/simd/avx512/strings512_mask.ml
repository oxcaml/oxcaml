open Stdlib

let () = Printexc.record_backtrace true

[@@@ocaml.warning "-unused-value-declaration"]
[@@@ocaml.warning "-unused-module"]

(* String, bytes and bigstring load/store primitives for AVX512 mask values
   ([mask]/[mask#], layout [mask]).  Masks are eight bytes wide and have no
   alignment requirement, so bounds checking behaves like the 64-bit scalar
   accessors, against which the loads are cross-checked. *)

external box_mask : mask# -> mask = "%box_mask"
external unbox_mask : mask -> mask# = "%unbox_mask"

external mask_of_int64 : int64 -> mask
  = "caml_vec512_unreachable" "caml_mask_of_int64"
[@@noalloc] [@@unboxed] [@@builtin]

external int64_of_mask : mask -> int64
  = "caml_vec512_unreachable" "caml_int64_of_mask"
[@@noalloc] [@@unboxed] [@@builtin]

external string_get64 : string -> int -> int64 = "%caml_string_get64u"
external bytes_get64 : bytes -> int -> int64 = "%caml_bytes_get64u"

let eq_mask (actual : mask) (expect : int64) =
  let actual = int64_of_mask actual in
  if actual <> expect then Printf.printf "%016Lx <> %016Lx\n" actual expect

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

(* ---- Bytes ---- *)

module Bytes_tests (Primitives : sig
  val get_mask : bytes -> int -> mask
  val get_mask_unsafe : bytes -> int -> mask
  val set_mask : bytes -> int -> mask -> unit
  val set_mask_unsafe : bytes -> int -> mask -> unit
  val extra_checks : bytes -> unit
end) =
struct
  open Primitives

  let data = Bytes.of_string test_data

  (* Loads at every valid offset agree with the 64-bit scalar load. *)
  let () =
    for i = 0 to Bytes.length data - 8 do
      eq_mask (get_mask data i) (bytes_get64 data i);
      eq_mask (get_mask_unsafe data i) (bytes_get64 data i)
    done
  ;;

  (* An eight-byte access is rejected within eight bytes of the end and at
     negative indices. *)
  let () =
    for i = Bytes.length data - 7 to Bytes.length data do
      assert_raises_out_of_bounds (fun () ->
        let _ = get_mask data i in
        ())
    done;
    assert_raises_out_of_bounds (fun () ->
      let _ = get_mask data (-1) in
      ())
  ;;

  (* Stores write exactly eight bytes. *)
  let () =
    let copy = Bytes.copy data in
    set_mask copy 5 (mask_of_int64 0x0123456789abcdefL);
    eq_mask (get_mask copy 5) 0x0123456789abcdefL;
    set_mask_unsafe copy 40 (mask_of_int64 0xf0e1d2c3b4a59687L);
    eq_mask (get_mask copy 40) 0xf0e1d2c3b4a59687L;
    Bytes.iteri
      (fun i c ->
        if (i < 5 || i > 12) && (i < 40 || i > 47)
        then assert (Char.equal c (Bytes.get data i)))
      copy;
    assert_raises_out_of_bounds (fun () ->
      set_mask copy (Bytes.length copy - 7) (mask_of_int64 1L));
    assert_raises_out_of_bounds (fun () ->
      set_mask copy (-1) (mask_of_int64 1L))
  ;;

  let () = extra_checks data
end

module _ = Bytes_tests (struct
  external get_mask : bytes -> int -> mask = "%caml_bytes_getmask"
  external get_mask_unsafe : bytes -> int -> mask = "%caml_bytes_getmasku"

  external set_mask : bytes -> int -> mask -> unit = "%caml_bytes_setmask"

  external set_mask_unsafe : bytes -> int -> mask -> unit
    = "%caml_bytes_setmasku"

  let extra_checks data =
    List.iter
      (fun index ->
        assert_raises_out_of_bounds (fun () ->
          let _ = get_mask data index in
          ());
        assert_raises_out_of_bounds (fun () ->
          set_mask data index (mask_of_int64 1L)))
      Int.[ min_int; add min_int one; sub zero one; max_int ]
  ;;
end)

(* Unboxed ([mask#]) variants of the primitives. *)
module _ = Bytes_tests (struct
  external get_mask_prim : bytes -> int -> mask# = "%caml_bytes_getmask#"

  let get_mask b i = box_mask (get_mask_prim b i)

  external get_mask_unsafe_prim : bytes -> int -> mask#
    = "%caml_bytes_getmasku#"

  let get_mask_unsafe b i = box_mask (get_mask_unsafe_prim b i)

  external set_mask_prim : bytes -> int -> mask# -> unit
    = "%caml_bytes_setmask#"

  let set_mask b i v = set_mask_prim b i (unbox_mask v)

  external set_mask_unsafe_prim : bytes -> int -> mask# -> unit
    = "%caml_bytes_setmasku#"

  let set_mask_unsafe b i v = set_mask_unsafe_prim b i (unbox_mask v)

  let extra_checks data =
    List.iter
      (fun index ->
        assert_raises_out_of_bounds (fun () ->
          let _ = get_mask data index in
          ());
        assert_raises_out_of_bounds (fun () ->
          set_mask data index (mask_of_int64 1L)))
      Int.[ min_int; add min_int one; sub zero one; max_int ]
  ;;
end)

module _ = Bytes_tests (struct
  external get_mask_prim : bytes -> int8# -> mask
    = "%caml_bytes_getmask_indexed_by_int8#"

  let get_mask b i = (get_mask_prim b (Stdlib_stable.Int8_u.of_int i))

  external get_mask_unsafe_prim : bytes -> int8# -> mask
    = "%caml_bytes_getmasku_indexed_by_int8#"

  let get_mask_unsafe b i = (get_mask_unsafe_prim b (Stdlib_stable.Int8_u.of_int i))

  external set_mask_prim : bytes -> int8# -> mask -> unit
    = "%caml_bytes_setmask_indexed_by_int8#"

  let set_mask b i v = set_mask_prim b (Stdlib_stable.Int8_u.of_int i) v

  external set_mask_unsafe_prim : bytes -> int8# -> mask -> unit
    = "%caml_bytes_setmasku_indexed_by_int8#"

  let set_mask_unsafe b i v = set_mask_unsafe_prim b (Stdlib_stable.Int8_u.of_int i) v

  let extra_checks data =
    List.iter
      (fun index ->
        let index = Stdlib_stable.Int8_u.of_int8 index in
        assert_raises_out_of_bounds (fun () ->
          let _ = get_mask_prim data index in
          ());
        assert_raises_out_of_bounds (fun () ->
          set_mask_prim data index (mask_of_int64 1L)))
      Stdlib_stable.Int8.[ min_int; add min_int one; sub zero one; max_int ]
  ;;
end)

module _ = Bytes_tests (struct
  external get_mask_prim : bytes -> int16# -> mask
    = "%caml_bytes_getmask_indexed_by_int16#"

  let get_mask b i = (get_mask_prim b (Stdlib_stable.Int16_u.of_int i))

  external get_mask_unsafe_prim : bytes -> int16# -> mask
    = "%caml_bytes_getmasku_indexed_by_int16#"

  let get_mask_unsafe b i = (get_mask_unsafe_prim b (Stdlib_stable.Int16_u.of_int i))

  external set_mask_prim : bytes -> int16# -> mask -> unit
    = "%caml_bytes_setmask_indexed_by_int16#"

  let set_mask b i v = set_mask_prim b (Stdlib_stable.Int16_u.of_int i) v

  external set_mask_unsafe_prim : bytes -> int16# -> mask -> unit
    = "%caml_bytes_setmasku_indexed_by_int16#"

  let set_mask_unsafe b i v = set_mask_unsafe_prim b (Stdlib_stable.Int16_u.of_int i) v

  let extra_checks data =
    List.iter
      (fun index ->
        let index = Stdlib_stable.Int16_u.of_int16 index in
        assert_raises_out_of_bounds (fun () ->
          let _ = get_mask_prim data index in
          ());
        assert_raises_out_of_bounds (fun () ->
          set_mask_prim data index (mask_of_int64 1L)))
      Stdlib_stable.Int16.[ min_int; add min_int one; sub zero one; max_int ]
  ;;
end)

module _ = Bytes_tests (struct
  external get_mask_prim : bytes -> int32# -> mask
    = "%caml_bytes_getmask_indexed_by_int32#"

  let get_mask b i = (get_mask_prim b (Stdlib_upstream_compatible.Int32_u.of_int i))

  external get_mask_unsafe_prim : bytes -> int32# -> mask
    = "%caml_bytes_getmasku_indexed_by_int32#"

  let get_mask_unsafe b i = (get_mask_unsafe_prim b (Stdlib_upstream_compatible.Int32_u.of_int i))

  external set_mask_prim : bytes -> int32# -> mask -> unit
    = "%caml_bytes_setmask_indexed_by_int32#"

  let set_mask b i v = set_mask_prim b (Stdlib_upstream_compatible.Int32_u.of_int i) v

  external set_mask_unsafe_prim : bytes -> int32# -> mask -> unit
    = "%caml_bytes_setmasku_indexed_by_int32#"

  let set_mask_unsafe b i v = set_mask_unsafe_prim b (Stdlib_upstream_compatible.Int32_u.of_int i) v

  let extra_checks data =
    List.iter
      (fun index ->
        let index = Stdlib_upstream_compatible.Int32_u.of_int32 index in
        assert_raises_out_of_bounds (fun () ->
          let _ = get_mask_prim data index in
          ());
        assert_raises_out_of_bounds (fun () ->
          set_mask_prim data index (mask_of_int64 1L)))
      Int32.[ min_int; add min_int one; sub zero one; max_int ]
  ;;
end)

module _ = Bytes_tests (struct
  external get_mask_prim : bytes -> int64# -> mask
    = "%caml_bytes_getmask_indexed_by_int64#"

  let get_mask b i = (get_mask_prim b (Stdlib_upstream_compatible.Int64_u.of_int i))

  external get_mask_unsafe_prim : bytes -> int64# -> mask
    = "%caml_bytes_getmasku_indexed_by_int64#"

  let get_mask_unsafe b i = (get_mask_unsafe_prim b (Stdlib_upstream_compatible.Int64_u.of_int i))

  external set_mask_prim : bytes -> int64# -> mask -> unit
    = "%caml_bytes_setmask_indexed_by_int64#"

  let set_mask b i v = set_mask_prim b (Stdlib_upstream_compatible.Int64_u.of_int i) v

  external set_mask_unsafe_prim : bytes -> int64# -> mask -> unit
    = "%caml_bytes_setmasku_indexed_by_int64#"

  let set_mask_unsafe b i v = set_mask_unsafe_prim b (Stdlib_upstream_compatible.Int64_u.of_int i) v

  let extra_checks data =
    List.iter
      (fun index ->
        let index = Stdlib_upstream_compatible.Int64_u.of_int64 index in
        assert_raises_out_of_bounds (fun () ->
          let _ = get_mask_prim data index in
          ());
        assert_raises_out_of_bounds (fun () ->
          set_mask_prim data index (mask_of_int64 1L)))
      Int64.[ min_int; add min_int one; sub zero one; max_int ]
  ;;
end)

module _ = Bytes_tests (struct
  external get_mask_prim : bytes -> nativeint# -> mask
    = "%caml_bytes_getmask_indexed_by_nativeint#"

  let get_mask b i = (get_mask_prim b (Stdlib_upstream_compatible.Nativeint_u.of_int i))

  external get_mask_unsafe_prim : bytes -> nativeint# -> mask
    = "%caml_bytes_getmasku_indexed_by_nativeint#"

  let get_mask_unsafe b i = (get_mask_unsafe_prim b (Stdlib_upstream_compatible.Nativeint_u.of_int i))

  external set_mask_prim : bytes -> nativeint# -> mask -> unit
    = "%caml_bytes_setmask_indexed_by_nativeint#"

  let set_mask b i v = set_mask_prim b (Stdlib_upstream_compatible.Nativeint_u.of_int i) v

  external set_mask_unsafe_prim : bytes -> nativeint# -> mask -> unit
    = "%caml_bytes_setmasku_indexed_by_nativeint#"

  let set_mask_unsafe b i v = set_mask_unsafe_prim b (Stdlib_upstream_compatible.Nativeint_u.of_int i) v

  let extra_checks data =
    List.iter
      (fun index ->
        let index = Stdlib_upstream_compatible.Nativeint_u.of_nativeint index in
        assert_raises_out_of_bounds (fun () ->
          let _ = get_mask_prim data index in
          ());
        assert_raises_out_of_bounds (fun () ->
          set_mask_prim data index (mask_of_int64 1L)))
      Nativeint.[ min_int; add min_int one; sub zero one; max_int ]
  ;;
end)

(* ---- String ---- *)

module String_tests (Primitives : sig
  val get_mask : string -> int -> mask
  val get_mask_unsafe : string -> int -> mask
  val extra_checks : string -> unit
end) =
struct
  open Primitives

  let () =
    for i = 0 to String.length test_data - 8 do
      eq_mask (get_mask test_data i) (string_get64 test_data i);
      eq_mask (get_mask_unsafe test_data i) (string_get64 test_data i)
    done
  ;;

  let () =
    for i = String.length test_data - 7 to String.length test_data do
      assert_raises_out_of_bounds (fun () ->
        let _ = get_mask test_data i in
        ())
    done;
    assert_raises_out_of_bounds (fun () ->
      let _ = get_mask test_data (-1) in
      ())
  ;;

  let () = extra_checks test_data
end

module _ = String_tests (struct
  external get_mask : string -> int -> mask = "%caml_string_getmask"
  external get_mask_unsafe : string -> int -> mask = "%caml_string_getmasku"

  let extra_checks data =
    List.iter
      (fun index ->
        assert_raises_out_of_bounds (fun () ->
          let _ = get_mask data index in
          ()))
      Int.[ min_int; add min_int one; sub zero one; max_int ]
  ;;
end)

(* Unboxed ([mask#]) variants of the primitives. *)
module _ = String_tests (struct
  external get_mask_prim : string -> int -> mask# = "%caml_string_getmask#"

  let get_mask b i = box_mask (get_mask_prim b i)

  external get_mask_unsafe_prim : string -> int -> mask#
    = "%caml_string_getmasku#"

  let get_mask_unsafe b i = box_mask (get_mask_unsafe_prim b i)

  let extra_checks data =
    List.iter
      (fun index ->
        assert_raises_out_of_bounds (fun () ->
          let _ = get_mask data index in
          ()))
      Int.[ min_int; add min_int one; sub zero one; max_int ]
  ;;
end)

module _ = String_tests (struct
  external get_mask_prim : string -> int8# -> mask
    = "%caml_string_getmask_indexed_by_int8#"

  let get_mask b i = (get_mask_prim b (Stdlib_stable.Int8_u.of_int i))

  external get_mask_unsafe_prim : string -> int8# -> mask
    = "%caml_string_getmasku_indexed_by_int8#"

  let get_mask_unsafe b i = (get_mask_unsafe_prim b (Stdlib_stable.Int8_u.of_int i))

  let extra_checks data =
    List.iter
      (fun index ->
        let index = Stdlib_stable.Int8_u.of_int8 index in
        assert_raises_out_of_bounds (fun () ->
          let _ = get_mask_prim data index in
          ()))
      Stdlib_stable.Int8.[ min_int; add min_int one; sub zero one; max_int ]
  ;;
end)

module _ = String_tests (struct
  external get_mask_prim : string -> int16# -> mask
    = "%caml_string_getmask_indexed_by_int16#"

  let get_mask b i = (get_mask_prim b (Stdlib_stable.Int16_u.of_int i))

  external get_mask_unsafe_prim : string -> int16# -> mask
    = "%caml_string_getmasku_indexed_by_int16#"

  let get_mask_unsafe b i = (get_mask_unsafe_prim b (Stdlib_stable.Int16_u.of_int i))

  let extra_checks data =
    List.iter
      (fun index ->
        let index = Stdlib_stable.Int16_u.of_int16 index in
        assert_raises_out_of_bounds (fun () ->
          let _ = get_mask_prim data index in
          ()))
      Stdlib_stable.Int16.[ min_int; add min_int one; sub zero one; max_int ]
  ;;
end)

module _ = String_tests (struct
  external get_mask_prim : string -> int32# -> mask
    = "%caml_string_getmask_indexed_by_int32#"

  let get_mask b i = (get_mask_prim b (Stdlib_upstream_compatible.Int32_u.of_int i))

  external get_mask_unsafe_prim : string -> int32# -> mask
    = "%caml_string_getmasku_indexed_by_int32#"

  let get_mask_unsafe b i = (get_mask_unsafe_prim b (Stdlib_upstream_compatible.Int32_u.of_int i))

  let extra_checks data =
    List.iter
      (fun index ->
        let index = Stdlib_upstream_compatible.Int32_u.of_int32 index in
        assert_raises_out_of_bounds (fun () ->
          let _ = get_mask_prim data index in
          ()))
      Int32.[ min_int; add min_int one; sub zero one; max_int ]
  ;;
end)

module _ = String_tests (struct
  external get_mask_prim : string -> int64# -> mask
    = "%caml_string_getmask_indexed_by_int64#"

  let get_mask b i = (get_mask_prim b (Stdlib_upstream_compatible.Int64_u.of_int i))

  external get_mask_unsafe_prim : string -> int64# -> mask
    = "%caml_string_getmasku_indexed_by_int64#"

  let get_mask_unsafe b i = (get_mask_unsafe_prim b (Stdlib_upstream_compatible.Int64_u.of_int i))

  let extra_checks data =
    List.iter
      (fun index ->
        let index = Stdlib_upstream_compatible.Int64_u.of_int64 index in
        assert_raises_out_of_bounds (fun () ->
          let _ = get_mask_prim data index in
          ()))
      Int64.[ min_int; add min_int one; sub zero one; max_int ]
  ;;
end)

module _ = String_tests (struct
  external get_mask_prim : string -> nativeint# -> mask
    = "%caml_string_getmask_indexed_by_nativeint#"

  let get_mask b i = (get_mask_prim b (Stdlib_upstream_compatible.Nativeint_u.of_int i))

  external get_mask_unsafe_prim : string -> nativeint# -> mask
    = "%caml_string_getmasku_indexed_by_nativeint#"

  let get_mask_unsafe b i = (get_mask_unsafe_prim b (Stdlib_upstream_compatible.Nativeint_u.of_int i))

  let extra_checks data =
    List.iter
      (fun index ->
        let index = Stdlib_upstream_compatible.Nativeint_u.of_nativeint index in
        assert_raises_out_of_bounds (fun () ->
          let _ = get_mask_prim data index in
          ()))
      Nativeint.[ min_int; add min_int one; sub zero one; max_int ]
  ;;
end)

(* ---- Bigstring ---- *)

open struct
  open Bigarray

  type bigstring = (char, int8_unsigned_elt, c_layout) Array1.t

  external bigstring_get64 : bigstring -> int -> int64
    = "%caml_bigstring_get64u"

  let bigstring_of_string s =
    let a = Array1.create char c_layout (String.length s) in
    for i = 0 to String.length s - 1 do
      a.{i} <- s.[i]
    done;
    a

  module Bigstring_tests (Primitives : sig
    val get_mask : bigstring -> int -> mask
    val get_mask_unsafe : bigstring -> int -> mask
    val set_mask : bigstring -> int -> mask -> unit
    val set_mask_unsafe : bigstring -> int -> mask -> unit
    val extra_checks : bigstring -> unit
  end) =
  struct
    open Primitives

    let data = bigstring_of_string test_data

    let () =
      for i = 0 to Array1.dim data - 8 do
        eq_mask (get_mask data i) (bigstring_get64 data i);
        eq_mask (get_mask_unsafe data i) (bigstring_get64 data i)
      done
    ;;

    let () =
      for i = Array1.dim data - 7 to Array1.dim data do
        assert_raises_out_of_bounds (fun () ->
          let _ = get_mask data i in
          ())
      done;
      assert_raises_out_of_bounds (fun () ->
        let _ = get_mask data (-1) in
        ())
    ;;

    let () =
      let copy = bigstring_of_string test_data in
      set_mask copy 5 (mask_of_int64 0x0123456789abcdefL);
      eq_mask (get_mask copy 5) 0x0123456789abcdefL;
      set_mask_unsafe copy 40 (mask_of_int64 0xf0e1d2c3b4a59687L);
      eq_mask (get_mask copy 40) 0xf0e1d2c3b4a59687L;
      for i = 0 to Array1.dim copy - 1 do
        if (i < 5 || i > 12) && (i < 40 || i > 47)
        then assert (Char.equal copy.{i} test_data.[i])
      done;
      assert_raises_out_of_bounds (fun () ->
        set_mask copy (Array1.dim copy - 7) (mask_of_int64 1L));
      assert_raises_out_of_bounds (fun () ->
        set_mask copy (-1) (mask_of_int64 1L))
    ;;

    let () = extra_checks data
  end

  module _ = Bigstring_tests (struct
    external get_mask : bigstring -> int -> mask = "%caml_bigstring_getmask"
    external get_mask_unsafe : bigstring -> int -> mask = "%caml_bigstring_getmasku"

    external set_mask : bigstring -> int -> mask -> unit = "%caml_bigstring_setmask"

    external set_mask_unsafe : bigstring -> int -> mask -> unit
      = "%caml_bigstring_setmasku"

    let extra_checks data =
      List.iter
        (fun index ->
          assert_raises_out_of_bounds (fun () ->
            let _ = get_mask data index in
            ());
          assert_raises_out_of_bounds (fun () ->
            set_mask data index (mask_of_int64 1L)))
        Int.[ min_int; add min_int one; sub zero one; max_int ]
    ;;
  end)


  (* Unboxed ([mask#]) variants of the primitives. *)
  module _ = Bigstring_tests (struct
    external get_mask_prim : bigstring -> int -> mask# = "%caml_bigstring_getmask#"

    let get_mask b i = box_mask (get_mask_prim b i)

    external get_mask_unsafe_prim : bigstring -> int -> mask#
      = "%caml_bigstring_getmasku#"

    let get_mask_unsafe b i = box_mask (get_mask_unsafe_prim b i)

    external set_mask_prim : bigstring -> int -> mask# -> unit
      = "%caml_bigstring_setmask#"

    let set_mask b i v = set_mask_prim b i (unbox_mask v)

    external set_mask_unsafe_prim : bigstring -> int -> mask# -> unit
      = "%caml_bigstring_setmasku#"

    let set_mask_unsafe b i v = set_mask_unsafe_prim b i (unbox_mask v)

    let extra_checks data =
      List.iter
        (fun index ->
          assert_raises_out_of_bounds (fun () ->
            let _ = get_mask data index in
            ());
          assert_raises_out_of_bounds (fun () ->
            set_mask data index (mask_of_int64 1L)))
        Int.[ min_int; add min_int one; sub zero one; max_int ]
    ;;
  end)


  module _ = Bigstring_tests (struct
    external get_mask_prim : bigstring -> int8# -> mask
      = "%caml_bigstring_getmask_indexed_by_int8#"

    let get_mask b i = (get_mask_prim b (Stdlib_stable.Int8_u.of_int i))

    external get_mask_unsafe_prim : bigstring -> int8# -> mask
      = "%caml_bigstring_getmasku_indexed_by_int8#"

    let get_mask_unsafe b i = (get_mask_unsafe_prim b (Stdlib_stable.Int8_u.of_int i))

    external set_mask_prim : bigstring -> int8# -> mask -> unit
      = "%caml_bigstring_setmask_indexed_by_int8#"

    let set_mask b i v = set_mask_prim b (Stdlib_stable.Int8_u.of_int i) v

    external set_mask_unsafe_prim : bigstring -> int8# -> mask -> unit
      = "%caml_bigstring_setmasku_indexed_by_int8#"

    let set_mask_unsafe b i v = set_mask_unsafe_prim b (Stdlib_stable.Int8_u.of_int i) v

    let extra_checks data =
      List.iter
        (fun index ->
          let index = Stdlib_stable.Int8_u.of_int8 index in
          assert_raises_out_of_bounds (fun () ->
            let _ = get_mask_prim data index in
            ());
          assert_raises_out_of_bounds (fun () ->
            set_mask_prim data index (mask_of_int64 1L)))
        Stdlib_stable.Int8.[ min_int; add min_int one; sub zero one; max_int ]
    ;;
  end)


  module _ = Bigstring_tests (struct
    external get_mask_prim : bigstring -> int16# -> mask
      = "%caml_bigstring_getmask_indexed_by_int16#"

    let get_mask b i = (get_mask_prim b (Stdlib_stable.Int16_u.of_int i))

    external get_mask_unsafe_prim : bigstring -> int16# -> mask
      = "%caml_bigstring_getmasku_indexed_by_int16#"

    let get_mask_unsafe b i = (get_mask_unsafe_prim b (Stdlib_stable.Int16_u.of_int i))

    external set_mask_prim : bigstring -> int16# -> mask -> unit
      = "%caml_bigstring_setmask_indexed_by_int16#"

    let set_mask b i v = set_mask_prim b (Stdlib_stable.Int16_u.of_int i) v

    external set_mask_unsafe_prim : bigstring -> int16# -> mask -> unit
      = "%caml_bigstring_setmasku_indexed_by_int16#"

    let set_mask_unsafe b i v = set_mask_unsafe_prim b (Stdlib_stable.Int16_u.of_int i) v

    let extra_checks data =
      List.iter
        (fun index ->
          let index = Stdlib_stable.Int16_u.of_int16 index in
          assert_raises_out_of_bounds (fun () ->
            let _ = get_mask_prim data index in
            ());
          assert_raises_out_of_bounds (fun () ->
            set_mask_prim data index (mask_of_int64 1L)))
        Stdlib_stable.Int16.[ min_int; add min_int one; sub zero one; max_int ]
    ;;
  end)


  module _ = Bigstring_tests (struct
    external get_mask_prim : bigstring -> int32# -> mask
      = "%caml_bigstring_getmask_indexed_by_int32#"

    let get_mask b i = (get_mask_prim b (Stdlib_upstream_compatible.Int32_u.of_int i))

    external get_mask_unsafe_prim : bigstring -> int32# -> mask
      = "%caml_bigstring_getmasku_indexed_by_int32#"

    let get_mask_unsafe b i = (get_mask_unsafe_prim b (Stdlib_upstream_compatible.Int32_u.of_int i))

    external set_mask_prim : bigstring -> int32# -> mask -> unit
      = "%caml_bigstring_setmask_indexed_by_int32#"

    let set_mask b i v = set_mask_prim b (Stdlib_upstream_compatible.Int32_u.of_int i) v

    external set_mask_unsafe_prim : bigstring -> int32# -> mask -> unit
      = "%caml_bigstring_setmasku_indexed_by_int32#"

    let set_mask_unsafe b i v = set_mask_unsafe_prim b (Stdlib_upstream_compatible.Int32_u.of_int i) v

    let extra_checks data =
      List.iter
        (fun index ->
          let index = Stdlib_upstream_compatible.Int32_u.of_int32 index in
          assert_raises_out_of_bounds (fun () ->
            let _ = get_mask_prim data index in
            ());
          assert_raises_out_of_bounds (fun () ->
            set_mask_prim data index (mask_of_int64 1L)))
        Int32.[ min_int; add min_int one; sub zero one; max_int ]
    ;;
  end)


  module _ = Bigstring_tests (struct
    external get_mask_prim : bigstring -> int64# -> mask
      = "%caml_bigstring_getmask_indexed_by_int64#"

    let get_mask b i = (get_mask_prim b (Stdlib_upstream_compatible.Int64_u.of_int i))

    external get_mask_unsafe_prim : bigstring -> int64# -> mask
      = "%caml_bigstring_getmasku_indexed_by_int64#"

    let get_mask_unsafe b i = (get_mask_unsafe_prim b (Stdlib_upstream_compatible.Int64_u.of_int i))

    external set_mask_prim : bigstring -> int64# -> mask -> unit
      = "%caml_bigstring_setmask_indexed_by_int64#"

    let set_mask b i v = set_mask_prim b (Stdlib_upstream_compatible.Int64_u.of_int i) v

    external set_mask_unsafe_prim : bigstring -> int64# -> mask -> unit
      = "%caml_bigstring_setmasku_indexed_by_int64#"

    let set_mask_unsafe b i v = set_mask_unsafe_prim b (Stdlib_upstream_compatible.Int64_u.of_int i) v

    let extra_checks data =
      List.iter
        (fun index ->
          let index = Stdlib_upstream_compatible.Int64_u.of_int64 index in
          assert_raises_out_of_bounds (fun () ->
            let _ = get_mask_prim data index in
            ());
          assert_raises_out_of_bounds (fun () ->
            set_mask_prim data index (mask_of_int64 1L)))
        Int64.[ min_int; add min_int one; sub zero one; max_int ]
    ;;
  end)


  module _ = Bigstring_tests (struct
    external get_mask_prim : bigstring -> nativeint# -> mask
      = "%caml_bigstring_getmask_indexed_by_nativeint#"

    let get_mask b i = (get_mask_prim b (Stdlib_upstream_compatible.Nativeint_u.of_int i))

    external get_mask_unsafe_prim : bigstring -> nativeint# -> mask
      = "%caml_bigstring_getmasku_indexed_by_nativeint#"

    let get_mask_unsafe b i = (get_mask_unsafe_prim b (Stdlib_upstream_compatible.Nativeint_u.of_int i))

    external set_mask_prim : bigstring -> nativeint# -> mask -> unit
      = "%caml_bigstring_setmask_indexed_by_nativeint#"

    let set_mask b i v = set_mask_prim b (Stdlib_upstream_compatible.Nativeint_u.of_int i) v

    external set_mask_unsafe_prim : bigstring -> nativeint# -> mask -> unit
      = "%caml_bigstring_setmasku_indexed_by_nativeint#"

    let set_mask_unsafe b i v = set_mask_unsafe_prim b (Stdlib_upstream_compatible.Nativeint_u.of_int i) v

    let extra_checks data =
      List.iter
        (fun index ->
          let index = Stdlib_upstream_compatible.Nativeint_u.of_nativeint index in
          assert_raises_out_of_bounds (fun () ->
            let _ = get_mask_prim data index in
            ());
          assert_raises_out_of_bounds (fun () ->
            set_mask_prim data index (mask_of_int64 1L)))
        Nativeint.[ min_int; add min_int one; sub zero one; max_int ]
    ;;
  end)
end
