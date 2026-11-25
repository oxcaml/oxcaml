(* TEST
   flambda2;
   stack-allocation;
   unset OCAMLPARAM;
   flags += "-flambda2-reaper -no-experimental-reaper-escape -reaper-debug-flags=nostamps";
   script = "sh ${test_source_directory}/compare-with-annotated.sh escape_tail_cycle";
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte with dump-reaper;
   script;
   run;
   check-program-output;
 *)

(* [escape_tail_cycle] with the annotations the escape analysis infers (see
   [compare-with-annotated.sh]). *)
external local_stack_offset : unit -> int = "caml_local_stack_offset"

let[@inline never] sum_pair (local_ (p : int * int)) = fst p + snd p

let peak = ref 0

module M : sig
  val run : int -> int
end = struct
  let rec ping n acc =
    if n = 0
    then (
      peak := local_stack_offset ();
      acc, acc)
    else
      let local_ tmp = n, acc in
      pong (n - 1) (sum_pair tmp land 0xffff)

  and pong n acc =
    if n = 0
    then (
      peak := local_stack_offset ();
      acc, n)
    else
      let local_ tmp = acc, n in
      ping (n - 1) (sum_pair tmp land 0xffff)

  let[@inline never] run n =
    let p = ping n 0 in
    (sum_pair p [@nontail])
end

let () =
  let before = local_stack_offset () in
  let r = M.run 1_000_000 in
  (Printf.printf [@inlined never]) "%d; stack growth %d\n" r (!peak - before)
