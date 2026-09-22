(* TEST
 include stdlib_stable;
 include stdlib_upstream_compatible;
 flags = "-extension layouts_beta";
 { bytecode; }
 { native; }
 { flags += "-Oclassic"; native; }
*)

open Stdlib_stable
open Stdlib_upstream_compatible

type part = { text : string; number : float# }
type record = { prefix : int; mutable part : part#; suffix : string }

let[@inline never] make () =
  { prefix = 1; part = #{ text = "before"; number = #2.0 }; suffix = "end" }

let () =
  let r = make () in
  let p : part# ptr = Ptr.of_idx r (.part) in
  Ptr.unsafe_set p #{ text = "pointer"; number = #3.0 };
  assert ((Ptr.unsafe_get p).#text = "pointer");
  let a : part# addr = Addr.of_idx r (.part) in
  Addr.set a #{ text = "address"; number = #4.0 };
  assert ((Addr.get a).#text = "address");
  assert (Float_u.to_float (Addr.get a).#number = 4.0);
  assert (r.suffix = "end");
  let i = Idx_mut.compose_imm (.part) (.number) in
  Addr.set (Addr.of_idx r i) #5.0;
  assert (Float_u.to_float r.part.#number = 5.0);
  let pi : string ptr_imm = Ptr_imm.of_idx r (.suffix) in
  let ai : string addr_imm = Addr_imm.of_idx r (.suffix) in
  assert (Ptr_imm.unsafe_get pi = "end");
  assert (Addr_imm.get ai = "end")

let[@inline never] retained () =
  let r = make () in
  Addr.of_idx r (.part)

let () =
  let a = retained () in
  Gc.full_major ();
  Gc.compact ();
  assert ((Addr.get a).#text = "before");
  Addr.set a #{ text = String.make 8 'x'; number = #6.0 };
  Gc.full_major ();
  assert ((Addr.get a).#text = "xxxxxxxx")

let () =
  let xs = [|#1.0; #2.0|] in
  let i = Idx_mut.unsafe_create_into_array 1 in
  let p = Ptr.of_idx xs i in
  Ptr.unsafe_set p #7.0;
  assert (Float_u.to_float (Ptr.unsafe_get p) = 7.0);
  let xs = [:"a"; "b":] in
  let i = Idx_imm.unsafe_create_into_iarray 1 in
  assert (Addr_imm.get (Addr_imm.of_idx xs i) = "b")

let () =
  let r = stack_ { prefix = 0;
                  part = #{ text = "local"; number = #1.0 };
                  suffix = "end" } in
  let a = Addr.of_idx r (.part) in
  Addr.set a #{ text = "updated"; number = #2.0 };
  assert ((Addr.get a).#text = "updated")
