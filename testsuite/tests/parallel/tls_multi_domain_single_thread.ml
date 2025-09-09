(* TEST
 flags += "-alert -do_not_spawn_domains -alert -unsafe_multidomain";
 runtime5;
 multidomain;
 { bytecode; }
 { native; }
*)

external has_tls_state
    : unit -> bool @@ portable = "caml_thread_has_tls_state"

let () = assert (not (has_tls_state ()))

let check_tls () =
  let k1 = Domain.TLS.new_key (fun () -> 10) in
  let k2 = Domain.TLS.new_key (fun () -> 1.0) in
  Domain.TLS.set k1 100;
  Domain.TLS.set k2 200.0;
  let v1 = Domain.TLS.get k1 in
  let v2 = Domain.TLS.get k2 in
  assert (v1 = 100);
  assert (v2 = 200.0);
  Gc.major ()

let check_tls_domain_reuse () =
  let k1 = Domain.TLS.new_key (fun () -> 100) in
  let k2 = Domain.TLS.new_key (fun () -> 200) in
  let domains = Array.init 4 (fun _ -> Domain.spawn(fun _ ->
    Domain.TLS.set k1 31415;
    Domain.TLS.set k2 27182;
    assert (Domain.TLS.get k1 = 31415);
    assert (Domain.TLS.get k2 = 27182))) in
  Array.iter Domain.join domains;
  Gc.full_major ();
  let domains2 = Array.init 4 (fun _ -> Domain.spawn(fun _ ->
    assert(Domain.TLS.get k1 = 100);
    assert(Domain.TLS.get k2 = 200))) in
  Array.iter Domain.join domains2

let _ =
  let domains = Array.init 3 (fun _ -> Domain.spawn(check_tls)) in
  check_tls ();
  Array.iter Domain.join domains;
  check_tls_domain_reuse ()

let k = Domain.Safe.TLS.new_key
  ~split_from_parent:(fun i ->
    let i = Atomic.fetch_and_add i 1 in
    (fun () -> Atomic.make i))
  (fun () -> Atomic.make 0)

let check_tls_split dom_idx =
  assert (Atomic.get (Domain.TLS.get k) = dom_idx)

let _ =
  let domains = Array.init 3 (fun idx -> Domain.spawn(fun () -> check_tls_split idx)) in
  check_tls_split 3;
  Array.iter Domain.join domains
