(* TEST
 include stdlib_upstream_compatible;
 flambda2;
 {
   flags = "-extension layouts_alpha -extension let_mutable";
   { expect; expect.opt; }
 }*)

type void : void
external unbox_unit : unit -> void = "%unbox_unit"

let _ =
  let mutable u = unbox_unit () in
  let mutable v = unbox_unit () in
  u <- v;
  v <- u;
  "Hello, world!"
[%%expect{|
type void : void
external unbox_unit : unit -> void = "%unbox_unit"
- : string = "Hello, world!"
|}]

type t = #{ x: int; v: void; y: int32# }

let _ =
  let mutable r = #{ x = 10; v = unbox_unit (); y = #20l } in
  r <- #{ x = 50; v = unbox_unit (); y = #60l };
  r.#x, Stdlib_upstream_compatible.Int32_u.to_int r.#y

[%%expect{|
type t = #{ x : int; v : void; y : int32#; }
- : int * int = (50, 60)
|}]
