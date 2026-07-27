(* TEST
   flags += "-alert -unsafe_multidomain -alert -unsafe_effects";
   { bytecode; }
   { native; }
*)

(* A shallow fiber becomes a TLS owner at its first preemptible resumption:
   split keys are applied exactly once (from the resumer's state at that
   point), and the fiber keeps its own state on later resumptions. *)

open Effect

type _ Effect.t += Ping : unit Effect.t

let split_key =
  Domain.TLS.new_key ~split_from_parent:(fun parent -> parent * 2)
    (fun () -> -1)

let sh_handler k2ref =
  { Shallow.Preemptible.retc = Fun.id; exnc = raise;
    effc = (fun (type c) (e : c Effect.t) ->
      match e with
      | Ping -> Some (fun (k2 : (c, _) Shallow.continuation) ->
          (* [c] is [unit] here (from matching [Ping]), but the equation
             cannot escape into [k2ref]'s type. *)
          k2ref := Some (Obj.magic k2 : (unit, unit) Shallow.continuation))
      | _ -> None);
    tickc = (fun () -> Continue) }

let () =
  let k = Shallow.fiber (fun () ->
      Printf.printf "first resume sees: %d\n" (Domain.TLS.get split_key);
      Domain.TLS.set split_key 1000;
      perform Ping;
      Printf.printf "second resume sees: %d\n" (Domain.TLS.get split_key))
  in
  Domain.TLS.set split_key 5;
  let k2ref = ref None in
  Shallow.Preemptible.continue_with k () (sh_handler k2ref);
  (* Change the resumer's value in between: the fiber must keep its own
     state rather than being re-split. *)
  Domain.TLS.set split_key 7;
  (match !k2ref with
   | Some k2 -> Shallow.Preemptible.continue_with k2 () (sh_handler (ref None))
   | None -> assert false);
  assert (Domain.TLS.get split_key = 7);
  print_endline "OK"
