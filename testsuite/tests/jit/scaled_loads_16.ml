(* TEST
 flags = "-I +compiler-libs -I +unix -I +dynlink -I +ocaml-jit -linkall -w -58";
 libraries = "unix dynlink ocamlcommon ocamlfrontend ocamloptcomp";
 libraries += " ocamlopttoplevel jit";
 arch_arm64;
 native;
*)

(* As the first phrases of scaled_loads.ml, for a read of 16 bits inside a
   string constant. It is only run on arm64: the x86_64 binary emitter cannot
   emit this read for the JIT.

   Warning 58 is disabled because the [.cmx] files of some modules of the
   libraries are not installed. *)

let phrases =
  {|
external get16 : string -> int -> int = "%caml_string_get16u";;
let () =
  print_int (get16 "abcdefghijklmnopqrstuvwxyz" 2);
  print_newline ();;
|}

let () =
  Clflags.set_target Clflags.Target.Native;
  Clflags.Opt_flag_handler.set Oxcaml_flags.opt_flag_handler;
  Jit.init_top ();
  Opttoploop.initialize_toplevel_env ();
  Clflags.dlcode := false;
  List.iter
    (fun phrase ->
      match Opttoploop.execute_phrase false Format.std_formatter phrase with
      | true -> ()
      | false -> print_endline "the phrase failed"
      | exception _ -> print_endline "the phrase raised an exception")
    (!Opttoploop.parse_use_file (Lexing.from_string phrases))
