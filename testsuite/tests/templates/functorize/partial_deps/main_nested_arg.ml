(* Consumer for the [nested_arg] bundle.  Applies [Bundle.Make] with
   concrete [P_int] and [Q_int] and prints [Inst.Nested_arg.describe],
   which composes strings from [Nested_arg] and (via the compound alias)
   [Foo_q]. *)

module Inst = Bundle.Make (P_int) (Q_int) ()

let () = print_endline Inst.Nested_arg.describe
