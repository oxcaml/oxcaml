(* TEST
 modules = "test_atomic_builtins_stubs.c";
 arch_amd64;
 {
   flags = "-regalloc irc";
   native;
 }{
   flags = "-O3 -regalloc irc";
   native;
 }
*)

type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

external get64 : bigstring -> int -> int64 = "%caml_bigstring_get64"
external set64 : bigstring -> int -> int64 -> unit = "%caml_bigstring_set64"

external cas32 :
  bigstring -> (int[@untagged]) -> (int32[@unboxed]) ->
  (int32[@unboxed]) -> bool
  = "" "caml_bigstring_compare_and_swap_int32_unboxed"
  [@@noalloc] [@@builtin]

external cas64 :
  bigstring -> (int[@untagged]) -> (int64[@unboxed]) ->
  (int64[@unboxed]) -> bool
  = "" "caml_bigstring_compare_and_swap_int64_unboxed"
  [@@noalloc] [@@builtin]

(* Both instruction widths overwrite the accumulator on failure. *)
let[@inline never] cas32_twice memory expected =
  let first = cas32 memory 0 expected 0l in
  let second = cas32 memory 0 expected 0l in
  first, second

let[@inline never] cas64_twice memory expected =
  let first = cas64 memory 0 expected 0L in
  let second = cas64 memory 0 expected 0L in
  first, second

let () =
  let memory = Bigarray.Array1.create Bigarray.char Bigarray.c_layout 8 in
  set64 memory 0 5L;
  let first, second = cas32_twice memory (Sys.opaque_identity 2l) in
  assert (not first && not second);
  assert (Int64.equal (get64 memory 0) 5L);
  set64 memory 0 5L;
  let first, second = cas64_twice memory (Sys.opaque_identity 2L) in
  assert (not first && not second);
  assert (Int64.equal (get64 memory 0) 5L)
