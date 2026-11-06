(* Naming a dep after [open]ing a named instance: opening a module path goes
   through [Env.add_components], which copies the instance's component table
   rather than its signature, and must preserve visibility as well. *)

module Inst = Bundle_foo_lib.Make (P_int) ()

open Inst

let () = print_endline (Foo__A.hello (P_int.create ()))
