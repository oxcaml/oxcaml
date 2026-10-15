(* [open]ing an unmentionable dep directly is rejected like any other
   mention of it. *)

module Inst = Bundle_foo_lib.Make (P_int) ()

open Inst.Foo__

let () = print_endline (A.hello (P_int.create ()))
