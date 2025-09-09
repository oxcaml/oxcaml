(* TEST
 include systhreads;
 hassysthreads;
 { bytecode; }
 { native; }
*)

let check_tls () =
  let k1 = Thread.TLS.new_key (fun () -> 10) in
  let k2 = Thread.TLS.new_key (fun () -> 1.0) in
  Thread.TLS.set k1 100;
  Thread.TLS.set k2 200.0;
  let v1 = Thread.TLS.get k1 in
  let v2 = Thread.TLS.get k2 in
  assert (v1 = 100);
  assert (v2 = 200.0);
  Gc.major ()

let check_tls_reuse () =
  let k1 = Thread.TLS.new_key (fun () -> 100) in
  let k2 = Thread.TLS.new_key (fun () -> 200) in
  let threads = Array.init 4 (fun _ -> Thread.create(fun () ->
    Thread.TLS.set k1 31415;
    Thread.TLS.set k2 27182;
    assert (Thread.TLS.get k1 = 31415);
    assert (Thread.TLS.get k2 = 27182)) ()) in
  Array.iter Thread.join threads;
  Gc.full_major ();
  let threads2 = Array.init 4 (fun _ -> Thread.create(fun () ->
    assert(Thread.TLS.get k1 = 100);
    assert(Thread.TLS.get k2 = 200)) ()) in
  Array.iter Thread.join threads2

let check_tls_split () =
  let k = Thread.TLS.new_key
    ~split_from_parent:(fun i ->
      let i = Atomic.fetch_and_add i 1 in
      (fun () -> Atomic.make i))
    (fun () -> Atomic.make 0) in
  let threads = Array.init 4 (fun idx -> Thread.create(fun () ->
    assert (Atomic.get (Thread.TLS.get k) = idx)) ()) in
  Array.iter Thread.join threads

let _ =
  let threads = Array.init 3 (fun _ -> Thread.create(check_tls) ()) in
  check_tls ();
  Array.iter Thread.join threads;
  check_tls_reuse ();
  check_tls_split ()
