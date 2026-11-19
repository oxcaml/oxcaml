(* TEST
 {
   bytecode;
 }{
   flags = "-regalloc irc";
   native;
 }{
   flags = "-O3 -regalloc irc";
   native;
 }
*)

let r = Atomic.make 1
let () = assert (Atomic.get r = 1)

let () = Atomic.set r 2
let () = assert (Atomic.get r = 2)

let () = assert (Atomic.exchange r 3 = 2)

let () = assert (Atomic.compare_and_set r 3 4 = true)
let () = assert (Atomic.get r = 4)

let () = assert (Atomic.compare_and_set r 3 (-4) = false)
let () = assert (Atomic.get r = 4 )

let () = assert (Atomic.compare_and_set r 3 4 = false)

let () = assert (Atomic.fetch_and_add r 2 = 4)
let () = assert (Atomic.get r = 6)

let () = assert (Atomic.fetch_and_add r (-2) = 6)
let () = assert (Atomic.get r = 4)

let () = assert ((Atomic.incr r; Atomic.get r) = 5)

let () = assert ((Atomic.decr r; Atomic.get r) = 4)

let () = assert ((Atomic.add r 3; Atomic.get r) = 7)
let () = assert ((Atomic.sub r 3; Atomic.get r) = 4)
let () = assert ((Atomic.logand r 2; Atomic.get r) = 0)
let () = assert ((Atomic.logor r 2; Atomic.get r) = 2)
let () = assert ((Atomic.logxor r 3; Atomic.get r) = 1)

let () =
  let r = Atomic.make 0 in
  let cur = Atomic.get r in
  ignore (Atomic.set r (cur + 1), Atomic.set r (cur - 1));
  assert (Atomic.get r <> cur)

let () =
  let r = Atomic.make 0 in
  let cur = Atomic.get r in
  ignore (Atomic.incr r, Atomic.decr r);
  assert (Atomic.get r = cur)

(* Test operations on the contents field of the atomic *)

let () = assert (Atomic.Loc.get [%atomic.loc r.contents] = 1)

let () = assert ((Atomic.Loc.set [%atomic.loc r.contents] 5; Atomic.get r) = 5)

let () = assert ((Atomic.Loc.incr [%atomic.loc r.contents]; Atomic.get r) = 6)

(* A failed CAS must not overwrite an expected value used by a later CAS.
   Keep the two operations together, with no calls between them. *)

let[@inline never] cas_twice (a : int Atomic.t) expected =
  let first = Atomic.compare_and_set a expected 0 in
  let second = Atomic.compare_and_set a expected 0 in
  first, second

let[@inline never] cas_bool_twice (a : bool Atomic.t) expected =
  let first = Atomic.compare_and_set a expected true in
  let second = Atomic.compare_and_set a expected true in
  first, second

type atomic_record = { mutable field : int [@atomic] }

let[@inline never] cas_field_twice a expected =
  let first = Atomic.Loc.compare_and_set [%atomic.loc a.field] expected 0 in
  let second = Atomic.Loc.compare_and_set [%atomic.loc a.field] expected 0 in
  first, second

let[@inline never] cas_loop (a : int Atomic.t) expected last =
  let successes = ref 0 in
  for _ = 0 to last do
    if Atomic.compare_and_set a expected 0 then incr successes;
    Atomic.incr a
  done;
  !successes

let () =
  let a = Atomic.make (Sys.opaque_identity 5) in
  assert (cas_twice a (Sys.opaque_identity 2) = (false, false));
  assert (Atomic.get a = 5);
  assert (cas_twice a (Sys.opaque_identity 5) = (true, false));
  assert (Atomic.get a = 0)

let () =
  let a = Atomic.make (Sys.opaque_identity true) in
  assert (cas_bool_twice a (Sys.opaque_identity false) = (false, false));
  assert (Atomic.get a)

let () =
  let a = { field = Sys.opaque_identity 5 } in
  assert (cas_field_twice a (Sys.opaque_identity 2) = (false, false));
  assert (a.field = 5)

let () =
  let a = Atomic.make (Sys.opaque_identity 3) in
  assert (cas_loop a (Sys.opaque_identity 4) (Sys.opaque_identity 3) = 1);
  assert (Atomic.get a = 3)

(* Test primitives with non-immediate types *)

let a = ref 1
let r = Atomic.make a
let () = assert (Atomic.get r == a)

let b = ref 2
let () = Atomic.set r b
let () = assert (Atomic.get r == b)

let c = ref 3
let () = assert (Atomic.exchange r c == b)

let d = ref 4
let () = assert (Atomic.compare_and_set r c d = true)
let () = assert (Atomic.get r == d)

let e = ref (-4)
let () = assert (Atomic.compare_and_set r c e = false)
let () = assert (Atomic.get r == d)

let () = assert (Atomic.compare_and_set r c d = false)
