module Inst = Bundle.Make (P_int) ()

let () = print_endline Inst.Nested_arg.describe
