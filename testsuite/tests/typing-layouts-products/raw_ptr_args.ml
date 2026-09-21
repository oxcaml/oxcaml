(* TEST
 modules = "raw_ptr_args_c.c";
 reference = "${test_source_directory}/raw_ptr_args.reference";
 flambda2;
 {
   native;
 }
 {
   bytecode;
 }
*)

(* Tests for the [@raw_ptr] attribute on external arguments: a fat pointer
   (unboxed pair of a [value] base and a [bits64] byte offset) is passed to
   C as a single raw pointer, base + offset. *)

external unbox_int64 : (int64[@local_opt]) -> int64_u = "%unbox_int64"

(* Our own C stubs *)

external fill
  :  (#(bytes * int64_u)[@raw_ptr])
  -> (int[@untagged])
  -> (int[@untagged])
  -> unit
  = "test_fill_bytecode" "test_fill_native"
  [@@noalloc]

external cmp
  :  (#(bytes * int64_u)[@raw_ptr])
  -> (#(bytes * int64_u)[@raw_ptr])
  -> (int[@untagged])
  -> (int[@untagged])
  = "test_cmp_bytecode" "test_cmp_native"
  [@@noalloc]

(* libc functions, called directly.  Their void* results are bound as
   nativeint_u and ignored; they must not be declared as returning [unit],
   which would treat the returned raw pointer as a [value]. *)

external memset
  :  (#(bytes * int64_u)[@raw_ptr])
  -> (int[@untagged])
  -> (int[@untagged])
  -> nativeint_u
  = "test_memset_bytecode" "memset"
  [@@noalloc]

external memmove
  :  (#(bytes * int64_u)[@raw_ptr])
  -> (#(bytes * int64_u)[@raw_ptr])
  -> (int[@untagged])
  -> nativeint_u
  = "test_memmove_bytecode" "memmove"
  [@@noalloc]

let print_bytes prefix b = Printf.printf "%s: %s\n" prefix (Bytes.to_string b)

let () =
  (* fill via our own stub, at an offset *)
  let b = Bytes.make 8 'A' in
  fill #(b, #2L) (Char.code 'x') 3;
  print_bytes "fill" b;
  (* fill with garbage in the top 12 (gap) bits of the offset: they must be
     ignored *)
  let junk_off = unbox_int64 (Int64.logor (Int64.shift_left 0xFFFL 52) 5L) in
  fill #(b, junk_off) (Char.code 'y') 2;
  print_bytes "fill-gap-bits" b;
  (* memset directly from libc *)
  let (_ : nativeint_u) = memset #(b, #0L) (Char.code '.') 8 in
  print_bytes "memset" b;
  (* overlapping memmove directly from libc: two fat-pointer arguments *)
  let m = Bytes.of_string "0123456789" in
  let (_ : nativeint_u) = memmove #(m, #2L) #(m, #0L) 5 in
  print_bytes "memmove" m;
  (* memcmp via our own (normalizing) stub *)
  let p = Bytes.of_string "abcdef" in
  let q = Bytes.of_string "abcxef" in
  Printf.printf "cmp-eq: %d\n" (cmp #(p, #0L) #(q, #0L) 3);
  Printf.printf "cmp-lt: %d\n" (cmp #(p, #2L) #(q, #2L) 3);
  Printf.printf "cmp-gt: %d\n" (cmp #(q, #2L) #(p, #2L) 3);
  (* same bytes at different offsets *)
  Printf.printf "cmp-self: %d\n" (cmp #(m, #0L) #(m, #2L) 3)

(* Verify the address computation exactly, without dereferencing: [ptr_id]
   returns the raw pointer the C function received.  The offsets are
   dynamic ([Sys.opaque_identity]), so the mask and add cannot be
   constant-folded; the 45-bit offset checks that only the top 12 (gap)
   bits are cleared. *)

external box_nativeint : nativeint_u -> (nativeint[@local_opt])
  = "%box_nativeint"

external ptr_id : (#(bytes * int64_u)[@raw_ptr]) -> nativeint_u
  = "test_ptr_id_bytecode" "test_ptr_id"
  [@@noalloc]

let () =
  let b = Bytes.make 1 'z' in
  let base = box_nativeint (ptr_id #(b, #0L)) in
  let off = Sys.opaque_identity 0x123_4567_89ABL in
  let p = box_nativeint (ptr_id #(b, unbox_int64 off)) in
  Printf.printf "ptr-id-45bit: %b\n"
    (p = Nativeint.add base (Int64.to_nativeint off));
  let junk = Sys.opaque_identity (Int64.logor (Int64.shift_left 0xFFFL 52) 7L) in
  let q = box_nativeint (ptr_id #(b, unbox_int64 junk)) in
  Printf.printf "ptr-id-gap: %b\n" (Nativeint.sub q base = 7n)
