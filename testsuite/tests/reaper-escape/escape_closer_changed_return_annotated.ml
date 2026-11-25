(* TEST
   flambda2;
   stack-allocation;
   flags += "-flambda2-reaper -reaper-debug-flags=nostamps";
   script = "sh ${test_source_directory}/compare-with-annotated.sh escape_closer_changed_return";
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte with dump-reaper;
   script;
   run;
   check-program-output;
 *)

(* [escape_closer_changed_return] with the annotations the escape analysis infers (see
   [compare-with-annotated.sh]). *)
let[@inline never] sum_pair (local_ (p : int * int)) = fst p + snd p

let r = ref 0

module M : sig
  val run : int -> int
end = struct
  let[@inline never] unused_result x y =
    let local_ t = x, y in
    let s = sum_pair t in
    if s > 10
    then 1
    else if s > 5
    then (
      incr r;
      2)
    else 3

  let[@inline never] pair_result x y =
    let local_ t = x, y in
    let s = sum_pair t in
    if s > 10 then exclave_ (s, 1) else if s > 5 then exclave_ (2, s) else 3, 4

  let[@inline never] opt_result x y =
    let local_ t = x, y in
    let s = sum_pair t in
    if s > 10 then None else if s > 5 then exclave_ Some s else Some 0

  let[@inline never] run n =
    ignore (unused_result n n);
    ignore (unused_result 1 1);
    ignore (unused_result 3 3);
    let a, b = pair_result n n in
    let c, d = pair_result 3 3 in
    let e, f = pair_result 1 1 in
    let o = match opt_result n n with None -> 100 | Some v -> v in
    let o2 = match opt_result 3 3 with None -> 100 | Some v -> v in
    let o3 = match opt_result 1 1 with None -> 100 | Some v -> v in
    !r + a + (2 * b) + (3 * c) + (4 * d) + (5 * e) + (6 * f) + o + o2 + o3
end

let () = (Printf.printf [@inlined never]) "%d\n" (M.run 10)
