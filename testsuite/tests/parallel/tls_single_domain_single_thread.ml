(* TEST
  flags += "-alert -unsafe_multidomain";
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

let _ = check_tls ()
