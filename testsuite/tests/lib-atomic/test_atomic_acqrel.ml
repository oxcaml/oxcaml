(* TEST
   multicore;
   native;
   bytecode;
*)

(* Acquire loads and release stores, on [Atomic.t] and on [Atomic.Loc.t]. *)

external get_acquire : 'a Atomic.t -> 'a = "%atomic_load_acquire"
external set_release : 'a Atomic.t -> 'a -> unit = "%atomic_store_release"
external loc_get_acquire : 'a Atomic.Loc.t @ local -> 'a
  = "%atomic_load_acquire_loc"
external loc_set_release : 'a Atomic.Loc.t @ local -> 'a -> unit
  = "%atomic_store_release_loc"

type 'a r = { filler : unit; mutable x : 'a [@atomic] }

(* Immediates *)

let () =
  let a = Atomic.make 1 in
  assert (get_acquire a = 1);
  set_release a 2;
  assert (get_acquire a = 2);
  assert (Atomic.get a = 2);
  Atomic.set a 3;
  assert (get_acquire a = 3)

let () =
  let r = { filler = (); x = 1 } in
  assert (loc_get_acquire [%atomic.loc r.x] = 1);
  loc_set_release [%atomic.loc r.x] 2;
  assert (loc_get_acquire [%atomic.loc r.x] = 2);
  assert (r.x = 2);
  r.x <- 3;
  assert (loc_get_acquire [%atomic.loc r.x] = 3)

(* Pointers: the release store must run the write barrier, so storing a young
   block into an old atomic and then running a minor GC must keep the value
   alive. *)

let () =
  let a = Atomic.make "old" in
  Gc.full_major ();
  set_release a (String.concat "" ["you"; "ng"]);
  Gc.minor ();
  assert (String.equal (get_acquire a) "young")

let () =
  let r = { filler = (); x = "old" } in
  Gc.full_major ();
  loc_set_release [%atomic.loc r.x] (String.concat "" ["you"; "ng"]);
  Gc.minor ();
  assert (String.equal (loc_get_acquire [%atomic.loc r.x]) "young")

(* Message passing: data written before a release store is visible to a
   thread that observes the store with an acquire load. *)

let[@alert "-unsafe_parallelism"] () =
  let iterations = 1000 in
  let data = ref 0 in
  let flag = Atomic.make 0 in
  let producer =
    Domain.spawn (fun () ->
      for i = 1 to iterations do
        data := i;
        set_release flag i;
        while get_acquire flag = i do Domain.cpu_relax () done
      done)
  in
  for i = 1 to iterations do
    while get_acquire flag <> i do Domain.cpu_relax () done;
    assert (!data = i);
    set_release flag 0
  done;
  Domain.join producer
