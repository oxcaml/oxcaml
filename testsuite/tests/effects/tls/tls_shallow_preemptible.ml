(* TEST
   flags += "-alert -unsafe_multidomain -alert -unsafe_effects";
   { bytecode; }
   { native; }
*)

(* A shallow preemptible fiber owns TLS state from creation: split keys are
   applied from the creator's state at [Preemptible.fiber], not at
   resumption, and the fiber keeps its own state across resumptions. A
   non-preemptible shallow fiber shares its resumer's state. *)

open Effect

type _ Effect.t += Ping : unit Effect.t

let split_key =
  Domain.TLS.new_key ~split_from_parent:(fun parent -> parent * 2)
    (fun () -> -1)

let sh_handler k2ref =
  { Shallow.Preemptible.retc = Fun.id; exnc = raise;
    effc = (fun (type c) (e : c Effect.t) ->
      match e with
      | Ping ->
        Some (fun (k2 : (c, _) Shallow.Preemptible.continuation) ->
          (* [c] is [unit] here (from matching [Ping]), but the equation
             cannot escape into [k2ref]'s type. *)
          k2ref :=
            Some
              (Obj.magic k2 : (unit, unit) Shallow.Preemptible.continuation))
      | _ -> None);
    tickc = (fun () -> Continue) }

let () =
  Domain.TLS.set split_key 5;
  let k = Shallow.Preemptible.fiber (fun () ->
      Printf.printf "first resume sees: %d\n" (Domain.TLS.get split_key);
      Domain.TLS.set split_key 1000;
      perform Ping;
      Printf.printf "second resume sees: %d\n" (Domain.TLS.get split_key))
  in
  (* The fiber's state was split at creation: changing the creator's value
     before the first resumption must not affect it. *)
  Domain.TLS.set split_key 6;
  let k2ref = ref None in
  Shallow.Preemptible.continue_with k () (sh_handler k2ref);
  assert (Domain.TLS.get split_key = 6);
  Domain.TLS.set split_key 7;
  (match !k2ref with
   | Some k2 -> Shallow.Preemptible.continue_with k2 () (sh_handler (ref None))
   | None -> assert false);
  assert (Domain.TLS.get split_key = 7);

  (* A non-preemptible shallow fiber shares its resumer's state. *)
  let k = Shallow.fiber (fun () ->
      assert (Domain.TLS.get split_key = 7);
      Domain.TLS.set split_key 8)
  in
  Shallow.continue_with k ()
    { Shallow.retc = Fun.id; exnc = raise;
      effc = (fun (type c) (_ : c Effect.t) -> None) };
  assert (Domain.TLS.get split_key = 8);
  print_endline "OK"
