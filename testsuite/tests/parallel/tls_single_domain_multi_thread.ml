(* TEST
 include systhreads;
 hassysthreads;
 { bytecode; }
 { native; }
*)

module TLS = Thread.TLS

let check_tls () =
  let k1 = TLS.new_key (fun () -> 10) in
  let k2 = TLS.new_key (fun () -> 1.0) in
  TLS.set k1 100;
  TLS.set k2 200.0;
  let v1 = TLS.get k1 in
  let v2 = TLS.get k2 in
  assert (v1 = 100);
  assert (v2 = 200.0);
  Gc.major ()

let check_tls_reuse () =
  let k1 = TLS.new_key (fun () -> 100) in
  let k2 = TLS.new_key (fun () -> 200) in
  let threads = Array.init 4 (fun _ -> Thread.create(fun () ->
    TLS.set k1 31415;
    TLS.set k2 27182;
    assert (TLS.get k1 = 31415);
    assert (TLS.get k2 = 27182)) ()) in
  Array.iter Thread.join threads;
  Gc.full_major ();
  let threads2 = Array.init 4 (fun _ -> Thread.create(fun () ->
    assert(TLS.get k1 = 100);
    assert(TLS.get k2 = 200)) ()) in
  Array.iter Thread.join threads2

let check_tls_split () =
  let k = TLS.new_key
    ~split_from_parent:(fun i ->
      let i = Atomic.fetch_and_add i 1 in
      (fun () -> Atomic.make i))
    (fun () -> Atomic.make 0) in
  let threads = Array.init 4 (fun idx -> Thread.create(fun () ->
    assert (Atomic.get (TLS.get k) = idx)) ()) in
  Array.iter Thread.join threads

let _ =
  let threads = Array.init 3 (fun _ -> Thread.create(check_tls) ()) in
  check_tls ();
  Array.iter Thread.join threads;
  check_tls_reuse ();
  check_tls_split ()
