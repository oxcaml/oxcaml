(* TEST
   flambda2;
   flags += "-flambda2-reaper";
   { native; }		
 *)		
		
[@@@warning "-ignored-extra-argument"]

external __dummy2__ : unit -> 'a = "%opaque"
external __ignore__ : _ -> unit = "%ignore"

let eval, eval2 =
  let[@inline always] evaluation () =
    let bind f () = f [@@inline never] [@@local never] in
    let extend_new_tid = __dummy2__ () in
    let rec eval_app () =
      bind (fun _ -> extend_new_tid (eval_app ()))
        [@@inline never] [@@local never]
    in
    fun () ->
      __ignore__ eval_app;
      ()
  in
  evaluation (), evaluation ()
