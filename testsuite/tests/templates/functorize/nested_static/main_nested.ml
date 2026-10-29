(* [Static] is the pre-instantiated [Stateful[P:P_int]] compilation
   unit.  The bundle's [Wrap] reaches [Stateful] through
   [Bar[P:P_int]{Q}], whose [Stateful[P:P]] runtime slot specialises
   to the same complete instance [Stateful[P:P_int]]; the functorizer
   resolves it to that global unit rather than binding a fresh copy
   inside [Make] (see the sketch in test_byte.ml).  So both routes
   below observe one shared counter. *)

module Static = Stateful(P)(P_int) [@jane.non_erasable.instances]

let () =
  assert (Static.get_count () = 0);
  Static.inc_count ();
  assert (Static.get_count () = 1);

  let module Inst = Bundle.Make (Q_int) () in
  (* Bundle observes the same counter. *)
  assert (Inst.Wrap.foo_count () = 1);

  (* Mutation through the bundle observed at top level. *)
  Inst.Wrap.foo_bump ();
  assert (Static.get_count () = 2);

  print_endline "OK"
