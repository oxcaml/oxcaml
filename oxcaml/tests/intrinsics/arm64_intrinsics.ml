(* Test ARM64 intrinsics: memory fences, virtual timer counter, and CRC32C.
   These are ARM64 equivalents of the amd64 intrinsics recognized in
   cfg_selection.ml and simd_selection.ml. *)

(* Memory fences *)
external load_fence : unit -> unit = "caml_load_fence" "caml_load_fence"
[@@noalloc] [@@builtin]

external store_fence : unit -> unit = "caml_store_fence" "caml_store_fence"
[@@noalloc] [@@builtin]

external memory_fence : unit -> unit = "caml_memory_fence" "caml_memory_fence"
[@@noalloc] [@@builtin]

(* Virtual timer counter (ARM64 CNTVCT_EL0, equivalent of x86 RDTSC) *)
external rdtsc : unit -> (int[@untagged]) = "caml_rdtsc" "caml_rdtsc_unboxed"
[@@noalloc] [@@builtin]

(* CRC32C on untagged int *)
external int_crc32c : (int[@untagged]) -> (int[@untagged]) -> (int[@untagged])
  = "caml_sse42_int_untagged_crc_bc" "caml_sse42_int_untagged_crc"
[@@noalloc] [@@builtin]

let test_fences () =
  load_fence ();
  store_fence ();
  memory_fence ()

let[@inline never] test_rdtsc () =
  let t1 = rdtsc () in
  let t2 = rdtsc () in
  assert (t2 >= t1)

let test_crc32c () =
  (* CRC32C(init=0, data=0) = 0 *)
  assert (int_crc32c 0 0 = 0);
  (* Determinism *)
  let v1 = int_crc32c 0 42 in
  let v2 = int_crc32c 0 42 in
  assert (v1 = v2);
  (* Non-zero input produces non-zero CRC *)
  assert (int_crc32c 0 1 <> 0)

let () =
  test_fences ();
  test_rdtsc ();
  test_crc32c ()
