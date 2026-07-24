(* TEST
  include eval;
  flags = "-extension runtime_metaprogramming";
  no-address-sanitizer;
  { native; }
*)

#syntax quotations on

(* Exceptions raised from eval'd code: at the time the raise traverses
   the stack, the eval'd unit is on the call chain. F.2 (stack RA scan)
   inside the unloadable code's frames must darken the corresponding
   Code_block via the back-pointer at [retaddr - 1]. After the raise
   has unwound past the eval'd frame and the closure is no longer
   reachable, the unit should unload. *)

let report label =
  let r = Eval.unloadable_units_registered_total () in
  let u = Eval.unloadable_units_unloaded_total () in
  Printf.printf "%s: registered=%d unloaded=%d live=%d\n"
    label r u (r - u)

let[@inline never] raise_from_eval () =
  let f =
    Eval.eval
      <[
        fun n ->
          if n > 0 then raise (Failure ("oops " ^ string_of_int n));
          Printf.printf "f %d returned normally\n" n
      ]>
  in
  (try f 7 with Failure s -> Printf.printf "caught: %s\n" s);
  (try f 0 with Failure s -> Printf.printf "caught: %s\n" s);
  (try f 13 with Failure s -> Printf.printf "caught: %s\n" s)

let[@inline never] caller_in_eval_then_raise () =
  (* Eval'd code calls back into non-eval'd code which raises. The raise
     traverses the eval'd unit's frame, so the frame's UNLOADABLE bit
     must steer the stack walk through F.2 properly. *)
  let f =
    Eval.eval
      <[
        fun (cb : int -> unit) x ->
          cb x;
          Printf.printf "(cb returned)\n"
      ]>
  in
  let cb x =
    if x > 0 then raise (Failure (string_of_int x))
    else Printf.printf "cb called with %d\n" x
  in
  (try f cb 0 with Failure s -> Printf.printf "caught nested: %s\n" s);
  (try f cb 99 with Failure s -> Printf.printf "caught nested: %s\n" s)

let () =
  report "start";
  raise_from_eval ();
  Gc.compact ();
  report "after raise_from_eval + compact";

  caller_in_eval_then_raise ();
  Gc.compact ();
  report "after caller_in_eval_then_raise + compact"
