(* TEST
   flambda2;
   flags += "-flambda2-reaper -reaper-local-fields";
   { native; check-program-output; }
 *)

let[@inline never] go t b =
  let handler_opt =
    if b then
      None
    else
      Some (fun () ->
        (* This rebinding ensures we have different names
           for the value slots in those two functions, but is
           not necessary for the operation of this test. *)
        let u = t + 0 in
        (* We create a function which has [any_usage], but whose
           value slot [u] is used. For the use to be tracked, we need
           to see the source of the function. *)
         (fun () -> u)
      )
  in
  match handler_opt with
  | None -> assert false
  | Some handler ->
    (* Here, using variant unboxing (and the fact that the poison
       values it produces are constants and as such, [any_source], 
       we have [handler] which has any_source, making it necessary
       to see the usages of [handler] (and thus, of [t], which is the
       only used field) to track its source. *)
    (fun () -> handler ())

(* At this point, we need to see a use for [t] to track its source,
   but we need to see a source to track its use. This deadlocks, and
   we miss the fact that [t] actually has a source in the inner
   functions. Thus, the below code produces an invalid. *)
let n = (go 0 false) ()