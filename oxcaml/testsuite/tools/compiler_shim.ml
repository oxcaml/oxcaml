(* Bytecode shim that forwards its command line to the sibling ".opt"
   executable.

   [make runtest-upstream-boot] (see Makefile.common-ox) installs this shim
   as ocamlc.byte, ocamlopt.byte and ocamllex.byte. In that mode only the
   boot compiler's native binaries are available, but ocamltest runs the
   ".byte" tools as bytecode under ocamlrun, so it needs an actual bytecode
   program; this one simply hands over to the native tool. *)

let () =
  let self = Sys.executable_name in
  let self =
    if Filename.extension self = ".byte" then Filename.remove_extension self
    else self
  in
  let args = List.tl (Array.to_list Sys.argv) in
  exit (Sys.command (Filename.quote_command (self ^ ".opt") args))
