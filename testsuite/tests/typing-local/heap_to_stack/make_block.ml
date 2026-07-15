(* TEST
   flags += "-O3";
   exit_status = "-6";
   native;
*)

external magic_local : 'a @ local -> 'a = "%identity"

let f x = exclave_ stack_ Some x

let[@inline never] bad z =
   let x = magic_local ((f [@inlined]) z) in x, x

let () = ignore (Sys.opaque_identity (bad 0))
