(* TEST
 multicore;
 { bytecode; }
 { native; }
*)

module TLS = Domain.Safe.TLS

let get (type a : value mod many portable contended) (k : a ref TLS.key) : a =
  (TLS.access k (fun r -> { Modes.Global.global = !r })).global
let set (type a : value mod many portable contended) (k : a ref TLS.key) (v : a) =
  TLS.access k (fun r -> r := v) [@nontail]

let check_tls () =
  let k1 = TLS.new_key (fun () -> ref 10) in
  let k2 = TLS.new_key (fun () -> ref 1.0) in
  set k1 100;
  set k2 200.0;
  let v1 = get k1 in
  let v2 = get k2 in
  assert (v1 = 100);
  assert (v2 = 200.0);
  Gc.major ()

let check_tls_domain_reuse () =
  let k1 = TLS.new_key (fun () -> ref 100) in
  let k2 = TLS.new_key (fun () -> ref 200) in
  let domains = Array.init 4 (fun _ -> Domain.spawn(fun _ ->
    set k1 31415;
    set k2 27182;
    assert (get k1 = 31415);
    assert (get k2 = 27182))) in
  Array.iter Domain.join domains;
  Gc.full_major ();
  let domains2 = Array.init 4 (fun _ -> Domain.spawn(fun _ ->
    assert(get k1 = 100);
    assert(get k2 = 200))) in
  Array.iter Domain.join domains2

let _ =
  let domains = Array.init 3 (fun _ -> Domain.spawn(check_tls)) in
  check_tls ();
  Array.iter Domain.join domains;
  check_tls_domain_reuse ()

let k = TLS.new_key
  ~split_from_parent:(fun i ->
    let i = Atomic.fetch_and_add !i 1 in
    (fun () -> ref (Atomic.make i)))
  (fun () -> ref (Atomic.make 0))

let check_tls_split dom_idx =
  assert (Atomic.get (get k) = dom_idx)

let _ =
  let domains = Array.init 3 (fun idx -> Domain.spawn(fun () -> check_tls_split idx)) in
  check_tls_split 3;
  Array.iter Domain.join domains
