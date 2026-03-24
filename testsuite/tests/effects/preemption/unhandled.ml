(* TEST
   modules = "preemption_util.ml";
   include unix;
   hasunix;
   runtime5;
   poll_insertion;
   flags += "-alert -unsafe_multidomain -w -21";
   { native; }
*)

open Preemption_util

let _ = Sys.opaque_identity (Effect.Preemption : unit Effect.t)

(** Test that unhandled preemption just resumes execution in all cases *)

let alloc () =
  let r @ global =
    ref (
      "this is a string",
      "this is another string",
      Sys.opaque_identity (ref 42)
    )
  in
  Sys.opaque_identity r
;;

let () =
  with_preemption_setup ~interval:0.001 ~repeating:true (fun () ->
    let accu = ref [] in
    for i = 1 to 100_000 do
      let r = alloc () in
      accu := r :: !accu;
      List.iter (fun r ->
        let (s1, s2, r2) = !r in
        if not (s1 = "this is a string") then failwith ("s1 = " ^ s1);
        assert (s2 = "this is another string");
        assert (!r2 = 42);
      ) !accu;
    done)
