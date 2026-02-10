(* TEST
   include unix;
   hasunix;
   runtime5;
   poll_insertion;
   flags += "-alert -unsafe_multidomain -w -21";
   { native; }
*)

open Effect
open Effect.Deep

let _ = Sys.opaque_identity (Preemption : unit t)

(** Test that unhandled preemption just resumes execution in all cases *)

external preempt_self : unit -> unit = "caml_domain_preempt_self" [@@noalloc]

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
  (* Trigger a preemption every 0.001s *)
  let _ = Unix.setitimer ITIMER_REAL {it_interval = 0.001; it_value = 0.001} in
  let _ = Sys.set_signal Sys.sigalrm (Signal_handle (fun _ -> preempt_self ())) in

  (* Loop for a while while allocating *)
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
  done
