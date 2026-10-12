(* TEST
   flags += " -extension mode_alpha";
   flags += " -extension mode_polymorphism_alpha";
   flags += " -extension mode_polymorphism_printing";
   expect.opt;
*)

module Allocation_can't_be_polymorphic = struct
  (* The partially-applied one-argument function is mode-polymorphic. *)
  let allocate_allocating_closure () () =
    let (state @ global) = ref None in
    state := Some 42
  (* Naively, we could abuse that mode polymorphism like so: *)
  let allocating_closure @ noalloc_strict = allocate_allocating_closure ()
end
[%%expect{|
CR wsturgeon for wsturgeon: this has to fail
|}]
