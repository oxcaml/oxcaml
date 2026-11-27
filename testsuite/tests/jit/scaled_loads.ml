(* TEST
 flags = "-I +compiler-libs -I +unix -I +dynlink -I +ocaml-jit -linkall -w -58";
 libraries = "unix dynlink ocamlcommon ocamlfrontend ocamloptcomp";
 libraries += " ocamlopttoplevel jit";
 native;
*)

(* Toplevel phrases, loaded by the JIT, whose code loads from an address
   given by a symbol, which arm64 does with an offset that the instruction
   scales by the size of the access.

   The first phrases read 32 bits inside a string constant. They are compiled
   as code that is not position-independent, as [Eval] does, so that the
   string is addressed by its symbol.

   The others load [float32] and 128-bit vector constants from memory. The
   constants of one size of a phrase are next to each other, so all but the
   first are loaded with a non-zero offset. The vector primitives are taken
   from a signature, through a module that has nothing else, so that the
   phrases do not refer to the C stubs of the primitives, which the program
   does not contain.

   Warning 58 is disabled because the [.cmx] files of some modules of the
   libraries are not installed. *)

let string_reads =
  {|
external get32 : string -> int -> int32 = "%caml_string_get32u";;
let () =
  print_endline (Int32.to_string (get32 "abcdefghijklmnopqrstuvwxyz" 4));;
|}

let constants =
  {|
external add : float32 -> float32 -> float32 = "%addfloat32";;
external mul : float32 -> float32 -> float32 = "%mulfloat32";;
external to_float : float32 -> float = "%floatoffloat32";;
let print x = print_endline (string_of_float (to_float x));;
let f x = add (mul x 100.0s) 1000.0s;;
let () = print (f (Sys.opaque_identity 3.0s));;
let g x y = add (mul x 10000.0s) (add (mul y 100000.0s) 1000000.0s);;
let () = print (g (Sys.opaque_identity 2.0s) (Sys.opaque_identity 5.0s));;
module type S = sig
  external dup : int64 -> int64x2# = "" "caml_int64x2_const1"
  [@@unboxed] [@@builtin] [@@noalloc]
  external low : int64x2# -> int64 = "" "caml_int64x2_low_to_int64"
  [@@unboxed] [@@builtin] [@@noalloc]
end;;
module V = (val (Obj.magic 0 : (module S)));;
let () =
  let[@inline never] v b =
    if b then V.dup 123456789L else V.dup 987654321L
  in
  print_endline (Int64.to_string (V.low (v (Sys.opaque_identity true))));
  print_endline (Int64.to_string (V.low (v (Sys.opaque_identity false))));;
|}

let run phrases =
  List.iter
    (fun phrase ->
      match Opttoploop.execute_phrase false Format.std_formatter phrase with
      | true -> ()
      | false -> print_endline "the phrase failed"
      | exception _ -> print_endline "the phrase raised an exception")
    (!Opttoploop.parse_use_file (Lexing.from_string phrases))

let () =
  Clflags.set_target Clflags.Target.Native;
  Clflags.Opt_flag_handler.set Oxcaml_flags.opt_flag_handler;
  Jit.init_top ();
  Opttoploop.initialize_toplevel_env ();
  let dlcode = !Clflags.dlcode in
  Clflags.dlcode := false;
  run string_reads;
  Clflags.dlcode := dlcode;
  run constants
