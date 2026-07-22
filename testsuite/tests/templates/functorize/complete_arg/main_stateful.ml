(* [P_stateful] is parameterised by [A]; [Bar_q] references
   [Pair_pq[P:P_stateful[A:A_impl]]{Q}] via a complete arg to [Pair_pq].
   The functorizer must resolve [P_stateful[A:A_impl]] to a [Pgetglobal]
   of that specific compilation unit — so the counter is shared between
   direct access via [Static] and access via the bundle. *)

module Static = P_stateful(A)(A_impl) [@jane.non_erasable.instances]

let () =
  assert (Static.get_count () = 0);
  Static.inc_count ();
  assert (Static.get_count () = 1);

  let module Inst = Bundle.Make (Q_int) () in
  Inst.Bar_q.bump ();
  assert (Static.get_count () = 2);

  Static.inc_count ();
  assert (Static.get_count () = 3);

  Inst.Bar_q.bump ();
  assert (Static.get_count () = 4);

  print_endline "OK"
