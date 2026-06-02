(* TEST
 runtime5;
 native;
*)

(* When a reperform reaches the "no parent handler" (unhandled) path, the
   runtime atomically consumes the continuation via caml_continuation_use_noexc
   before switching stacks. If the continuation has already been consumed,
   caml_continuation_use_noexc returns NULL and we should raise
   [Continuation_already_resumed] instead of proceeding with a NULL stack.

   This test sets up that scenario by calling [reperform] twice on the same
   continuation from within an unhandled-effect-catching [with_stack]. The
   first [reperform] raises [Unhandled] via the unhandled path, consuming the
   continuation as a side effect. The second [reperform] uses the same (now
   NULL) continuation and must raise [Continuation_already_resumed].

   This is not something that can actually be hit by the stdlib, so we have to
   use the effect primitives directly.
*)

open Effect

type _ t += E : unit t

type ('a, 'x, 'b) cont
type last_fiber [@@immediate]

external with_stack :
  ('x -> 'b) ->
  (exn -> 'b) ->
  ('c t -> ('c, 'x, 'b) cont -> last_fiber -> 'b) ->
  ('d -> 'x) ->
  'd ->
  'b = "%with_stack"

external reperform :
  'a t -> ('a, _, 'b) cont -> last_fiber -> 'b = "%reperform"

let () =
  Printexc.record_backtrace false;
  let saw_unhandled = ref false in
  let saw_already_resumed = ref false in
  let effc (type a) (eff : a t) (k : (a, _, _) cont) (lf : last_fiber) =
    match eff with
    | E ->
        (try reperform E k lf
         with Unhandled _ -> saw_unhandled := true);
        (try reperform E k lf
         with Continuation_already_resumed -> saw_already_resumed := true);
        ()
    | _ -> reperform eff k lf
  in
  with_stack
    (fun () -> ())
    (fun e -> raise e)
    effc
    (fun () -> perform E)
    ();
  Printf.printf "first reperform raised Unhandled: %b\n" !saw_unhandled;
  Printf.printf "second reperform raised Continuation_already_resumed: %b\n"
    !saw_already_resumed
