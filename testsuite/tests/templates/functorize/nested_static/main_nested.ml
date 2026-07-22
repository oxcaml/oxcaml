(* [Bar] is parameterised by [P] and [Q]; its body uses [Stateful] (which
   is parameterised by [P] only), so [Stateful] appears as a runtime
   parameter of [Bar].  [Wrap] references [Bar[P:P_int]{Q}].  When we
   functorize [Wrap], the [Rp_main_module_block Stateful[P:P]] slot of
   [Bar]'s instantiating functor is substituted (via
   [visible_arg_map = {P:P_int}]) to [Stateful[P:P_int]] — now complete.
   The functorizer resolves this to a [Pgetglobal] of the
   pre-instantiated [Stateful[P:P_int]] compilation unit, so its
   counter is a shared global instance. *)

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
