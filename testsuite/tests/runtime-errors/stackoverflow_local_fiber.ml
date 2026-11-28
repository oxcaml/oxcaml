(* TEST
 no-stack-checks;
 native;
*)

(* Guard-page stack overflow inside a fiber that holds live local
   allocations. This is a regression test concerning stack alignment.
   See #7088. *)

external opaque_local : 'a @ local -> 'a @ local = "%opaque"

let rec go n =
  let box @ local = opaque_local (n, n) in
  let r = 1 + go (n + 1) in
  ignore (Sys.opaque_identity (fst box));
  r

let () =
  (try
     Sys.with_async_exns (fun () ->
       Effect.Deep.match_with (fun () -> ignore (go 0)) ()
         { retc = (fun () -> ());
           exnc = (fun e -> raise e);
           effc = (fun (type a) (_ : a Effect.t) -> None) })
   with Stack_overflow -> print_endline "Stack overflow caught")
