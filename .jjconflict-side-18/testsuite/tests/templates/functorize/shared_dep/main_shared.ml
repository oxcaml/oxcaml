(* [Bar1] and [Bar2] both depend on [Stateful] (parameterised by [P]).
   Inside a single [Bundle.Make (P_int) ()] application, [Stateful[P:P]]
   should be [bind_local_instance]'d once and shared between [Bar1] and
   [Bar2] via the [module_map] cache — not rebuilt for each. *)

let () =
  let module Inst = Bundle.Make (P_int) () in
  assert (Inst.Bar1.peek () = 0);
  assert (Inst.Bar2.peek () = 0);
  Inst.Bar1.bump ();
  (* [Bar2] observes the same counter as [Bar1] — one shared [Stateful]. *)
  assert (Inst.Bar2.peek () = 1);
  Inst.Bar2.bump ();
  assert (Inst.Bar1.peek () = 2);
  print_endline "OK"
