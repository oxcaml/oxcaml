(* TEST
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

let _ = check_tls ()
