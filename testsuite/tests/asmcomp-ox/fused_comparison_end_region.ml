(* TEST
   stack-allocation;
   flambda2;
   ocamlopt_flags = "-O3 -use-ssa";
   native;
*)

(* [v == p] is computed before the end of [g]'s region. If it were fused into
   the branch at the end of [test], [v] would be a GC root during [Gc.minor ()]
   after its region has ended. [w] reuses the storage of [v], so the GC would
   read [w]'s second field as the header of [v]: 384 is encoded as 0x301, with
   the colour of an unmarked local block, which the GC would overwrite. *)

let[@inline always] g (p : int * int) =
  let v = Sys.opaque_identity (stack_ (fst p, snd p)) in
  v == p

let[@inline never] test (p : int * int) =
  let c = g p in
  let w = Sys.opaque_identity (stack_ (fst p, snd p, fst p, fst p)) in
  Gc.minor ();
  let _, field1, _, _ = w in
  if c then -field1 else field1

let () = assert (test (Sys.opaque_identity (0, 384)) = 384)
