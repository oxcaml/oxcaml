[@@@ocaml.warning "-unused-module"]

module I64 = Stdlib_upstream_compatible.Int64_u
module I32 = Stdlib_upstream_compatible.Int32_u

module Int64 = struct
  open Builtins.Int64

  let eq' x y = if x <> y then Printf.printf "%016Lx <> %016Lx\n" x y

  let () =
    eq' (andn 0b1010L 0b1100L) 0b0100L;
    eq' (bextr 0b1100L 0x0202L) 0b11L;
    eq' (blsi 0b1110L) 0b10L;
    eq' (blsmsk 0b1100L) 0b111L;
    eq' (blsr 0b1100L) 0b1000L;
    eq' (tzcnt 0b1100L) 2L;
    eq' (tzcnt 0L) 64L;
    eq' (lzcnt 0b1100L) 60L;
    eq' (lzcnt 0L) 64L;
    eq' (popcnt (-1L)) 64L;
    eq' (popcnt 0L) 0L;
    eq' (bzhi 0b1100L 3L) 0b100L;
    eq' (pdep 3L 4L) 0x4L;
    eq' (pdep 235L 522L) 0xAL;
    eq' (pext 3L 4L) 0x0L;
    eq' (pext 235L 522L) 0x3L;
    eq' (rorx 1 1L) Int64.min_int;
    eq' (sarx (-1L) 1L) (-1L);
    eq' (shrx (-1L) 1L) Int64.max_int;
    eq' (shlx 1L 1L) 2L

  external mulx : int64# -> int64# -> #(int64# * int64#) = "" "caml_bmi2_mulx_int64"
    [@@noalloc] [@@builtin]

  let () =
    let #(low,high) = mulx #2L #3L in
    eq' (I64.to_int64 low) 6L;
    eq' (I64.to_int64 high) 0L;
    let #(low,high) = mulx #0x8000000000000000L #2L in
    eq' (I64.to_int64 low) 0L;
    eq' (I64.to_int64 high) 1L
end

module Int32 = struct
  open Builtins.Int32

  let eq' x y = if x <> y then Printf.printf "%08lx <> %08lx\n" x y

  let () =
    eq' (andn 0b1010l 0b1100l) 0b0100l;
    eq' (bextr 0b1100l 0x0202l) 0b11l;
    eq' (blsi 0b1110l) 0b10l;
    eq' (blsmsk 0b1100l) 0b111l;
    eq' (blsr 0b1100l) 0b1000l;
    eq' (tzcnt 0b1100l) 2l;
    eq' (tzcnt 0l) 32l;
    eq' (lzcnt 0b1100l) 28l;
    eq' (lzcnt 0l) 32l;
    eq' (popcnt (-1l)) 32l;
    eq' (popcnt 0l) 0l;
    eq' (bzhi 0b1100l 3l) 0b100l;
    eq' (pdep 3l 4l) 0x4l;
    eq' (pdep 235l 522l) 0xAl;
    eq' (pext 3l 4l) 0x0l;
    eq' (pext 235l 522l) 0x3l;
    eq' (rorx 1 1l) Int32.min_int;
    eq' (sarx (-1l) 1l) (-1l);
    eq' (shrx (-1l) 1l) Int32.max_int;
    eq' (shlx 1l 1l) 2l

  (* Have to bind the return value as int64; the compiler doesn't know how to
     sign-extend pairs of smaller ints. *)
  external mulx : int32# -> int32# -> #(int64# * int64#) = "" "caml_bmi2_mulx_int32"
    [@@noalloc] [@@builtin]

  let mulx x y =
    let #(low,high) = mulx x y in
    #(I64.to_int32_u low,I64.to_int32_u high)

  let () =
    let #(low,high) = mulx #2l #3l in
    eq' (I32.to_int32 low) 6l;
    eq' (I32.to_int32 high) 0l;
    let #(low,high) = mulx #0x80000000l #2l in
    eq' (I32.to_int32 low) 0l;
    eq' (I32.to_int32 high) 1l
end
