(* TEST
 flags = "-I +compiler-libs -I +unix -I +dynlink -I +ocaml-jit -linkall -w -58";
 libraries = "unix dynlink ocamlcommon ocamlfrontend ocamloptcomp";
 libraries += " ocamlopttoplevel jit";
 native;
*)

(* A toplevel phrase that the JIT cannot load, because of a primitive that
   does not exist, followed by phrases that need what the first one defined:
   the generic functions for a function of 25 parameters ([caml_curry25] and
   its companions), which the program does not contain. Should the program
   come to contain them, the test says so and needs another number of
   parameters.

   Warning 58 is disabled because the [.cmx] files of some modules of the
   libraries are not installed. *)

let phrases =
  {|
module M = struct
  external missing : int -> int = "jit_test_missing_primitive"
  let sum25 a b c d e f g h i j k l m n o p q r s t u v w x y =
    missing
      (a + b + c + d + e + f + g + h + i + j + k + l + m + n + o + p + q + r
       + s + t + u + v + w + x + y)
end;;
let alt25 a b c d e f g h i j k l m n o p q r s t u v w x y =
  a - b + c - d + e - f + g - h + i - j + k - l + m - n + o - p + q - r + s
  - t + u - v + w - x + y;;
let () =
  let partial =
    (Sys.opaque_identity alt25)
      1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24
  in
  print_string "alt25 = ";
  print_int (partial 25);
  print_newline ();;
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
  if Option.is_some (Jit.jit_lookup_symbol "caml_curry25")
  then print_endline "the program contains caml_curry25";
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
        print_endline "the phrase was not loaded: its primitive is missing";
        if Option.is_some (Jit.jit_lookup_symbol "caml_curry25")
        then print_endline "but the JIT still has its caml_curry25"
      | exception exn -> print_endline (Printexc.to_string exn))
    (!Opttoploop.parse_use_file (Lexing.from_string phrases))
