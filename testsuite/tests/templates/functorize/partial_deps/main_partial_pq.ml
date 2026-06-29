(* Consumer for the [partial_pq] bundle.  [Pair_pq] is parameterised by
   [P] and [Q]; [Partial_pq]'s reference [Pair_pq(Q)(Q_impl)] fills [Q]
   but leaves [P] unfilled — the elaborated form has both [visible_args]
   and [hidden_args].  The [P] threads all the way through to
   [Pair_pq]'s [name] function via the composed instance. *)

module Inst = Bundle.Make (P_int) ()

let () = print_endline (Inst.Partial_pq.describe (P_int.create ()))
