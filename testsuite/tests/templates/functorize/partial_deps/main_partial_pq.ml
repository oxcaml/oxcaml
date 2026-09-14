module Inst = Bundle.Make (P_int) ()

let () = print_endline (Inst.Partial_pq.describe (P_int.create ()))
