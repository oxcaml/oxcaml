(* TEST
 include systhreads;
 hassysthreads;
 multicore;
 { bytecode; }
 { native; }
*)

module TLS = Thread.TLS

let k1 = TLS.new_key (fun () -> 10)
let k2 = TLS.new_key (fun () -> 1.0)

let check_tls () =
  TLS.set k1 100;
  TLS.set k2 200.0;
  let v1 = TLS.get k1 in
  let v2 = TLS.get k2 in
  assert (v1 = 100);
  assert (v2 = 200.0);
  Gc.major ()

let k1 = TLS.new_key (fun () -> 100)
let k2 = TLS.new_key (fun () -> 200)

let check_tls_domain_reuse () =
  let domains = Array.init 4 (fun _ -> Thread.create(fun () ->
    TLS.set k1 31415;
    TLS.set k2 27182;
    assert (TLS.get k1 = 31415);
    assert (TLS.get k2 = 27182)) ()) in
  Array.iter Thread.join domains;
  Gc.full_major ();
  let domains2 = Array.init 4 (fun _ -> Thread.create(fun () ->
    assert(TLS.get k1 = 100);
    assert(TLS.get k2 = 200)) ()) in
  Array.iter Thread.join domains2

let _ =
  let domains = Array.init 3 (fun _ -> Domain.spawn(check_tls)) in
  check_tls ();
  Array.iter Domain.join domains;
  let domains = Array.init 3 (fun _ -> Domain.spawn(check_tls_domain_reuse)) in
  check_tls_domain_reuse ();
  Array.iter Domain.join domains

let k = TLS.new_key
  ~split_from_parent:(fun i ->
    let i = Atomic.fetch_and_add i 1 in
    (fun () -> Atomic.make i))
  (fun () -> Atomic.make 0)

let check_tls_split dom_idx =
  assert (Atomic.get (TLS.get k) = dom_idx);
  let threads = Array.init 4 (fun thd_idx -> Thread.create(fun () ->
    assert (Atomic.get (TLS.get k) = dom_idx + thd_idx)) ()) in
  Array.iter Thread.join threads

let _ =
  let domains = Array.init 3 (fun idx -> Domain.spawn(fun () -> check_tls_split idx)) in
  check_tls_split 3;
  Array.iter Domain.join domains
