(* TEST
 reference = "${test_source_directory}/ext_ptr_atomic_bytecode.reference";
 bytecode;
*)

(* External pointer primitives cannot be implemented on bytecode (they
   dereference raw addresses), so they fail at runtime with a clear message.
   This mirrors [ext_ptr_bytecode.ml] for the atomic variants. *)

external atomic_load_ext_ptr :
  ('a : value_or_null).
  int64_u @ local -> 'a = "%unsafe_atomic_load_ext_ptr"

external atomic_set_ext_ptr :
  ('a : value_or_null).
  (int64_u[@local_opt]) -> 'a -> unit = "%unsafe_atomic_set_ext_ptr"

external atomic_exchange_ext_ptr :
  ('a : value_or_null).
  (int64_u[@local_opt]) -> 'a -> 'a = "%unsafe_atomic_exchange_ext_ptr"

external atomic_cas_ext_ptr :
  ('a : value_or_null).
  (int64_u[@local_opt]) -> 'a -> 'a -> bool = "%unsafe_atomic_cas_ext_ptr"

external atomic_compare_exchange_ext_ptr :
  ('a : value_or_null).
  (int64_u[@local_opt]) -> 'a -> 'a -> 'a
  = "%unsafe_atomic_compare_exchange_ext_ptr"

external atomic_fetch_add_ext_ptr :
  int64_u @ local -> int -> int = "%unsafe_atomic_fetch_add_ext_ptr"

external atomic_add_ext_ptr :
  int64_u @ local -> int -> unit = "%unsafe_atomic_add_ext_ptr"

external atomic_sub_ext_ptr :
  int64_u @ local -> int -> unit = "%unsafe_atomic_sub_ext_ptr"

external atomic_land_ext_ptr :
  int64_u @ local -> int -> unit = "%unsafe_atomic_land_ext_ptr"

external atomic_lor_ext_ptr :
  int64_u @ local -> int -> unit = "%unsafe_atomic_lor_ext_ptr"

external atomic_lxor_ext_ptr :
  int64_u @ local -> int -> unit = "%unsafe_atomic_lxor_ext_ptr"

let test name f =
  match f () with
  | () -> Printf.printf "%s: unexpectedly returned\n" name
  | exception Failure msg -> Printf.printf "%s: Failure: %s\n" name msg

let () =
  test "load" (fun () -> ignore (atomic_load_ext_ptr #0L : int));
  test "set" (fun () -> atomic_set_ext_ptr #0L 0);
  test "exchange" (fun () -> ignore (atomic_exchange_ext_ptr #0L 0 : int));
  test "cas" (fun () -> ignore (atomic_cas_ext_ptr #0L 0 1 : bool));
  test "compare_exchange"
    (fun () -> ignore (atomic_compare_exchange_ext_ptr #0L 0 1 : int));
  test "fetch_add" (fun () -> ignore (atomic_fetch_add_ext_ptr #0L 1 : int));
  test "add" (fun () -> atomic_add_ext_ptr #0L 1);
  test "sub" (fun () -> atomic_sub_ext_ptr #0L 1);
  test "land" (fun () -> atomic_land_ext_ptr #0L 1);
  test "lor" (fun () -> atomic_lor_ext_ptr #0L 1);
  test "lxor" (fun () -> atomic_lxor_ext_ptr #0L 1)
