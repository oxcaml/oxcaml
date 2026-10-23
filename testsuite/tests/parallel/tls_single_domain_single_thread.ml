(* TEST
 { bytecode; }
 { native; }
*)

module TLS = Domain.Safe.TLS

let check_tls () =
  let k1 = TLS.new_key (fun () -> 10) in
  let k2 = TLS.new_key (fun () -> 1.0) in
  TLS.set k1 100;
  TLS.set k2 200.0;
  let v1 = TLS.get k1 in
  let v2 = TLS.get k2 in
  assert (v1 = 100);
  assert (v2 = 200.0);
  (* [set] must not run the initializer, even when growing the TLS array. *)
  let keys = Array.init 100 (fun _ ->
    TLS.new_key (fun () -> failwith "unexpected initializer")) in
  Array.iteri (fun i k -> TLS.set k i) keys;
  Array.iteri (fun i k -> assert (i = TLS.get k)) keys;
  Gc.major ()

let _ = check_tls ()
