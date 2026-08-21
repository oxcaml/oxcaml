(* Consumer for the [arg_block_extract] bundle.  Applies [Bundle.Make]
   with concrete [P_int] and [R_int] and prints
   [Inst.Nested_r.describe], which internally goes through
   [Foo_r(R)(R_impl)]: at runtime the bundle applies [R_impl(P)] and
   passes ITS ARG BLOCK (not its whole main block) as [Foo_r]'s [R]. *)

module Inst = Bundle.Make (P_int) (R_int) ()

let () = print_endline (Inst.Nested_r.describe ())
