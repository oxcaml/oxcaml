(* TEST
   compile_only = "true";
   flambda2;
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte with dump-simplify;
   check-fexpr-dump;
 *)

(* This whole file (with the .mli) should simplify down to nothing as we
   constant-fold all the tests. *)

module Int8 = struct
  type t = int8

  let size = 8

  external to_int : t -> int = "%int_of_int8"
  external format_int : string -> int -> string = "caml_format_int"
  let[@inline] to_string x = format_int "%d" (to_int x)

  let zero = 0s
  let one = 1s
  let minus_one = -1s
  let minus_two = -2s
  let min_int = 0x80s
  let max_int = 0x7Fs

  external leading_zeros : int8# -> int8# =
    "" "caml_int8_clz_untagged_to_untagged"
  [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

  external trailing_zeros : int8# -> int8# =
    "" "caml_int8_ctz_untagged_to_untagged"
  [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

  external popcount : int8# -> int8# =
    "" "caml_int8_popcnt_untagged_to_untagged"
  [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

  external untag : t -> int8# = "%int8#_of_int8"
  external tag_as_int : int8# -> int = "%int_of_int8#"

  let wrap f n = tag_as_int (f (untag n))
  [@@inline]

  let leading_zeros n = wrap leading_zeros n
  let trailing_zeros n = wrap trailing_zeros n
  let popcount n = wrap popcount n
end

module Int16 = struct
  type t = int16

  let size = 16

  external to_int : t -> int = "%int_of_int16"
  external format_int : string -> int -> string = "caml_format_int"
  let[@inline] to_string x = format_int "%d" (to_int x)

  let zero = 0S
  let one = 1S
  let minus_one = -1S
  let minus_two = -2S
  let min_int = 0x8000S
  let max_int = 0x7FFFS

  external leading_zeros : int16# -> int16# =
    "" "caml_int16_clz_untagged_to_untagged"
  [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

  external trailing_zeros : int16# -> int16# =
    "" "caml_int16_ctz_untagged_to_untagged"
  [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

  external popcount : int16# -> int16# =
    "" "caml_int16_popcnt_untagged_to_untagged"
  [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

  external untag : t -> int16# = "%int16#_of_int16"
  external tag_as_int : int16# -> int = "%int_of_int16#"

  let wrap f n = tag_as_int (f (untag n))
  [@@inline]

  let leading_zeros n = wrap leading_zeros n
  let trailing_zeros n = wrap trailing_zeros n
  let popcount n = wrap popcount n
end

module Int32 = struct
  type t = int32

  let size = 32

  external format : string -> int32 -> string = "caml_int32_format"
  let[@inline] to_string n = format "%d" n

  let zero = 0l
  let one = 1l
  let minus_one = -1l
  let minus_two = -2l
  let min_int = 0x80000000l
  let max_int = 0x7FFFFFFFl

  external leading_zeros : (t [@unboxed]) -> (int [@untagged]) =
    "" "caml_int32_clz_unboxed_to_untagged"
  [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

  external trailing_zeros : (t [@unboxed]) -> (int [@untagged]) =
    "" "caml_int32_ctz_unboxed_to_untagged"
  [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

  external popcount : (t [@unboxed]) -> (int [@untagged]) =
    "" "caml_int32_popcnt_unboxed_to_untagged"
  [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]
end

module Int64 = struct
  type t = int64

  let size = 64

  external format : string -> int64 -> string = "caml_int64_format"
  let[@inline] to_string n = format "%d" n

  let zero = 0L
  let one = 1L
  let minus_one = -1L
  let minus_two = -2L
  let min_int = 0x8000000000000000L
  let max_int = 0x7FFFFFFFFFFFFFFFL

  external leading_zeros : (t [@unboxed]) -> (int [@untagged]) =
    "" "caml_int64_clz_unboxed_to_untagged"
  [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

  external trailing_zeros : (t [@unboxed]) -> (int [@untagged]) =
    "" "caml_int64_ctz_unboxed_to_untagged"
  [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

  external popcount : (t [@unboxed]) -> (int [@untagged]) =
    "" "caml_int64_popcnt_unboxed_to_untagged"
  [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]
end

module Nativeint = struct
  type t = nativeint

  let size = Sys.word_size

  external format : string -> nativeint -> string = "caml_nativeint_format"
  let[@inline] to_string n = format "%d" n

  let zero = 0n
  let one = 1n
  let minus_one = -1n
  let minus_two = -2n
  external shift_left: nativeint -> int -> nativeint = "%nativeint_lsl"
  let min_int = shift_left 1n (size - 1)
  external sub: nativeint -> nativeint -> nativeint = "%nativeint_sub"
  let max_int = sub min_int 1n

  external leading_zeros : (t [@unboxed]) -> (int [@untagged]) =
    "" "caml_nativeint_clz_unboxed_to_untagged"
  [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

  external trailing_zeros : (t [@unboxed]) -> (int [@untagged]) =
    "" "caml_nativeint_ctz_unboxed_to_untagged"
  [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

  external popcount : (t [@unboxed]) -> (int [@untagged]) =
    "" "caml_nativeint_popcnt_unboxed_to_untagged"
  [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]
end

module Immediate = struct
  type t = int

  let size = Sys.int_size

  external format_int : string -> int -> string = "caml_format_int"
  let[@inline] to_string x = format_int "%d" x

  let zero = 0
  let one = 1
  let minus_one = -1
  let minus_two = -2
  external shift_right_logical : int -> int -> int = "%lsrint"
  let max_int = shift_right_logical (-1) 1
  external add : int -> int -> int = "%addint"
  let min_int = add max_int 1
end

module Naked_immediate = struct
  include Immediate

  external leading_zeros : (t [@untagged]) -> (int [@untagged]) =
    "" "caml_int_clz_untagged_to_untagged"
  [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

  external trailing_zeros : (t [@untagged]) -> (int [@untagged]) =
    "" "caml_int_ctz_untagged_to_untagged"
  [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

  external popcount : (t [@untagged]) -> (int [@untagged]) =
    "" "caml_int_popcnt_untagged_to_untagged"
  [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]
end

module Tagged_immediate = struct
  include Immediate

  external leading_zeros : t -> (int [@untagged]) =
    "" "caml_int_clz_tagged_to_untagged"
  [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

  external trailing_zeros : t -> (int [@untagged]) =
    "" "caml_int_ctz_tagged_to_untagged"
  [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

  external popcount : t -> (int [@untagged]) =
    "" "caml_int_popcnt_tagged_to_untagged"
  [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]
end

module Test(I : sig
  type t

  val to_string : t -> string

  val size : int
  val zero : t
  val one : t
  val minus_one : t
  val minus_two : t
  val min_int : t
  val max_int : t


  val leading_zeros : t -> int
  val trailing_zeros : t -> int
  val popcount : t -> int
end) = struct
  open I

  let[@inline] check name fn lhs rhs =
    let fn_lhs = (fn [@inlined hint]) lhs in
    if fn_lhs <> rhs
    then (Format.kasprintf [@inlined never]) failwith "%s(%s): expected %d but got %d" name (to_string lhs) fn_lhs rhs

  let[@inline] check_leading_zeros lhs rhs =
    check "leading_zeros" leading_zeros lhs rhs

  let[@inline] check_trailing_zeros lhs rhs =
    check "trailing_zeros" trailing_zeros lhs rhs

  let[@inline] check_popcount lhs rhs =
    check "popcount" popcount lhs rhs

  let () =
    check_leading_zeros zero size;
    check_leading_zeros one (size - 1);
    check_leading_zeros minus_one 0;
    check_leading_zeros minus_two 0;
    check_leading_zeros min_int 0;
    check_leading_zeros max_int 1;
    check_trailing_zeros zero size;
    check_trailing_zeros one 0;
    check_trailing_zeros minus_one 0;
    check_trailing_zeros minus_two 1;
    check_trailing_zeros min_int (size - 1);
    check_trailing_zeros max_int 0;
    check_popcount zero 0;
    check_popcount one 1;
    check_popcount minus_one size;
    check_popcount minus_two (size - 1);
    check_popcount min_int 1;
    check_popcount max_int (size - 1);
    ()
end
[@@inline]

module _ = Test(Int8)
module _ = Test(Int16)
module _ = Test(Int32)
module _ = Test(Int64)
module _ = Test(Nativeint)
module _ = Test(Naked_immediate)
module _ = Test(Tagged_immediate)
