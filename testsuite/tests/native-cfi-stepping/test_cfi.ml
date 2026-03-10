(* TEST
  arch_amd64;  (* CR sdolan: get this to pass on arm64 as well *)
  runtime5;
  flags = "-g";
  program = "${test_source_directory}/test_cfi";
  readonly_files = "test_cfi_stubs.c gdb_stepper.py";
  all_modules = "test_cfi_stubs.c test_cfi.ml";
  setup-ocamlopt.opt-build-env;
  ocamlopt.opt;
  script = "sh ${test_source_directory}/run_gdb.sh ${program}";
  script;
  check-program-output;
*)

external trace : (unit -> 'a) -> 'a = "trace_steps"

let () =
  (* Ensure uncaught exceptions aren't swallowed by the script (see run_gdb.sh) *)
  Printexc.set_uncaught_exception_handler (fun exn bt ->
    print_endline "LOG_BEGIN";
    print_endline ("Fatal error: exception " ^ Printexc.to_string exn);
    Printexc.print_raw_backtrace stdout bt;
    print_endline "LOG_END")

(* The first exception triggers allocation of the backtrace buffer, etc.
   Do that now, to avoid single-stepping through malloc *)
let () =
  try (Sys.opaque_identity (fun () -> raise Not_found)) ()
  with Not_found -> ()

let () =
  (* Exceptions raised from OCaml and caught by C *)
  let cond = Sys.opaque_identity (ref true) in
  match
    trace (fun () ->
      if !cond then raise Not_found; 42)
  with
  | _ -> assert false
  | exception Not_found -> ()

let () =
  (* Local allocations *)
  let n = trace (fun () -> let (r @ local) = ref 42 in (Sys.opaque_identity incr) r; !r) in
  assert (n = 43)

let () =
  (* Array bounds check failure *)
  let arr = Sys.opaque_identity [| |] in
  match
    trace (fun () -> arr.(42))
  with
  | _ -> assert false
  | exception (Invalid_argument _) -> ()

let () =
  (* C calls, both returning normally and raising *)
  let mkarrays () =
    let a1 = Array.make (Sys.opaque_identity 42) 'a' in
    let a2 = Array.make (Sys.opaque_identity (-3)) 'a' in
    a1, a2
  in
  match
    trace (fun () ->
      (* alloc C calls, including raising *)
      try mkarrays () |> fst
      with _ -> fst (mkarrays ()))
  with
  | _ -> assert false
  | exception (Invalid_argument _) -> ()

(* FIXME: noalloc C calls have correct CFI info, but gdb is unable to
   take backtraces past them when stack-switching is enabled *)

(*
(* This one fails in gdb 10 for unknown reasons, but passes in gdb 14.
   The CFI info looks correct at a glance, so it seems like a bug in gdb 10. *)
let zz =
  let[@inline never] fail () = int_of_string (Sys.opaque_identity "a") in
  trace (fun () ->
    try fail () |> ref |> (!)
    with _ -> 1)
*)

let () =
  print_string "LOG_BEGIN\nok\nLOG_END\n"
