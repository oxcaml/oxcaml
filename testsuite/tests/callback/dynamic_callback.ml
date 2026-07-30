(* TEST
 modules = "dynamic_callback_.c";
 {
   bytecode;
 }{
   native;
 }
*)

(* Runs [f] on a fresh stack, so that the dynamic bindings of the caller are
   only reachable through the parent stack, which callbacks from C do not
   see. *)
let on_fresh_stack f =
  Effect.Deep.match_with f ()
    { retc = (fun v -> v);
      exnc = (fun e -> raise e);
      effc = (fun (type a) (_ : a Effect.t) -> None) }

external call_from_c : (unit -> unit) -> unit = "dynamic_callback_call"

let print_dyn d =
  match Dynamic.get d with
  | This x -> Int.to_string x
  | Null -> "null"

let () =
  let d = Dynamic.make () in
  Dynamic.with_temporarily d 17 ~f:(fun () ->
    on_fresh_stack (fun () ->
      Printf.printf "before callback [expect 17]: %s\n" (print_dyn d);
      call_from_c (fun () ->
        Printf.printf "inside callback [expect null]: %s\n" (print_dyn d));
      Printf.printf "after callback [expect 17]: %s\n" (print_dyn d)))
