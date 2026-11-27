(* TEST
 modules = "startup_twice_main.c";
 {
   bytecode;
 }{
   native;
 }
*)

(* A setting that the runtime initializes when it reads its parameters. *)
let setting = "fiber_stack_size"

let changed = Gc.Tweak.get setting + 1

let change () = Gc.Tweak.set setting changed

let show () =
  Printf.printf "%s kept: %b\n%!" setting (Gc.Tweak.get setting = changed)

let () =
  Callback.register "change" change;
  Callback.register "show" show
