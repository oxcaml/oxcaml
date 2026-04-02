(* TEST
   flambda2;
   compile_only = "true";
   ocamlopt_flags += " -flambda2-join-algorithm=n-way";
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte;
 *)

external __dummy2__ : unit -> 'a = "%opaque"
let a = ref false
let f () = __dummy2__ () a
let run ()  =
  for i = 0 to 1 do
    f ()
  done
let _ =
  run ()
