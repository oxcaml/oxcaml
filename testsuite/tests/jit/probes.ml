(* TEST
 flags = "-I +compiler-libs -I +unix -I +dynlink -I +ocaml-jit -linkall -w -58";
 libraries = "unix dynlink ocamlcommon ocamlfrontend ocamloptcomp";
 libraries += " ocamlopttoplevel jit";
 probes;
 native;
*)

(* Toplevel phrases, loaded by the JIT, that have probes. Every unit that has
   a probe defines the base of the probe notes and the semaphore of the probe.
   The first phrase cannot be loaded, because of a primitive that does not
   exist: the others must not depend on what it defined, as they did on arm64
   for the base of the notes. Two of them have a probe of the same name,
   which is enabled in one and not in the other.

   Warning 58 is disabled because the [.cmx] files of some modules of the
   libraries are not installed. *)

let phrases =
  {|
module M = struct
  external missing : int -> int = "jit_test_missing_primitive"
  let f x = [%probe "jit_test_failed" (print_endline "handler")]; missing x
end;;
let f () =
  [%probe "jit_test" (print_endline "handler of f")];
  [%probe_is_enabled "jit_test"];;
let g () =
  [%probe "jit_test" ~enabled_at_init:true (print_endline "handler of g")];
  [%probe_is_enabled "jit_test"];;
let () =
  print_endline (string_of_bool (f ()));
  print_endline (string_of_bool (g ()));;
let h () =
  [%probe "jit_test_other" (print_endline "handler of h")];
  [%probe_is_enabled "jit_test_other"];;
let () = print_endline (string_of_bool (h ()));;
|}

let mentions_primitive msg =
  let name = "jit_test_missing_primitive" in
  let n = String.length name in
  let rec search i =
    i + n <= String.length msg
    && (String.equal (String.sub msg i n) name || search (i + 1))
  in
  search 0

let () =
  Clflags.set_target Clflags.Target.Native;
  Clflags.Opt_flag_handler.set Oxcaml_flags.opt_flag_handler;
  Jit.init_top ();
  Opttoploop.initialize_toplevel_env ();
  List.iter
    (fun phrase ->
      match Opttoploop.execute_phrase false Format.std_formatter phrase with
      | true -> ()
      | false -> print_endline "the phrase failed"
      | exception Failure msg when mentions_primitive msg ->
        print_endline "the phrase was not loaded: its primitive is missing"
      | exception exn -> print_endline (Printexc.to_string exn))
    (!Opttoploop.parse_use_file (Lexing.from_string phrases))
