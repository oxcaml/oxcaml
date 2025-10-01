(* TEST
 reference = "${test_source_directory}/let_mutable_alpha.reference";
 include stdlib_upstream_compatible;
 flambda2;
 {
   flags = "-extension layouts_alpha -extension let_mutable";
   native;
 }{
   flags = "-extension layouts_alpha -extension let_mutable";
   bytecode;
 }{
   flags = "-extension layouts_alpha -extension let_mutable";
   javascript;
 }*)

type void : void
external unbox_unit : unit -> void = "%unbox_unit"

let () =
  let mutable u = unbox_unit () in
  let mutable v = unbox_unit () in
  u <- v;
  v <- u;
  print_endline "Hello, world!"

type t = #{ x: int; v: void; y: int32# }

let () =
  let mutable r = #{ x = 10; v = unbox_unit (); y = #20l } in
  r <- #{ x = 50; v = unbox_unit (); y = #60l };
  print_int r.#x;
  print_string " ";
  print_int (Stdlib_upstream_compatible.Int32_u.to_int r.#y);
  print_endline ""
