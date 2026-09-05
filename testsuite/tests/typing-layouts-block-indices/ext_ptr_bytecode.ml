(* TEST
 reference = "${test_source_directory}/ext_ptr_bytecode.reference";
 include stdlib_stable;
 include stdlib_upstream_compatible;
 bytecode;
*)

(* External pointer primitives cannot be implemented on bytecode (they
   dereference raw addresses), so they fail at runtime with a clear message. *)

external get_ext_ptr
  : ('a : any).
  int64# @ local -> 'a @ local
  = "%unsafe_get_ext_ptr"
[@@layout_poly]

external set_ext_ptr
  : ('a : any).
  int64# @ local -> 'a @ local -> unit
  = "%unsafe_set_ext_ptr"
[@@layout_poly]

let () =
  (match (get_ext_ptr #0L : int) with
   | _ -> print_endline "unexpectedly returned from get_ext_ptr"
   | exception Failure msg -> Printf.printf "get: Failure: %s\n" msg);
  (match set_ext_ptr #0L 0 with
   | () -> print_endline "unexpectedly returned from set_ext_ptr"
   | exception Failure msg -> Printf.printf "set: Failure: %s\n" msg)
