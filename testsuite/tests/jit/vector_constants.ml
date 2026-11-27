(* TEST
 flags = "-I +compiler-libs -I +unix -I +dynlink -I +ocaml-jit -linkall -w -58";
 libraries = "unix dynlink ocamlcommon ocamlfrontend ocamloptcomp";
 libraries += " ocamlopttoplevel jit";
 native;
*)

(* Toplevel phrases, loaded by the JIT, that follow phrases with 128-bit
   vector constants.

   The vector primitives are taken from a signature, through a module that
   has nothing else, so that the phrases do not refer to the C stubs of the
   primitives, which the program does not contain.

   Warning 58 is disabled because the [.cmx] files of some modules of the
   libraries are not installed. *)

let phrases =
  {|
module type S = sig
  external dup : int64 -> int64x2# = "" "caml_int64x2_const1"
  [@@unboxed] [@@builtin] [@@noalloc]
  external low : int64x2# -> int64 = "" "caml_int64x2_low_to_int64"
  [@@unboxed] [@@builtin] [@@noalloc]
end;;
module V = (val (Obj.magic 0 : (module S)));;
let[@inline never] c () = V.dup 123456789L;;
let () = print_endline (Int64.to_string (V.low (c ())));;
let[@inline never] d () = V.dup 987654321L;;
let () = print_endline (Int64.to_string (V.low (d ())));;
|}

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
      | exception _ -> print_endline "the phrase raised an exception")
    (!Opttoploop.parse_use_file (Lexing.from_string phrases))
