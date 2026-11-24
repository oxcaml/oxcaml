(* TEST
   flambda2;
   stack-allocation;
   flags += "-flambda2-reaper -reaper-debug-flags=nostamps";
   script = "sh ${test_source_directory}/compare-with-annotated.sh escape_shared_code";
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte with dump-reaper;
   script;
   run;
   check-program-output;
 *)

(* [escape_shared_code] with the annotations the escape analysis infers (see
   [compare-with-annotated.sh]). *)
let[@inline never] sum7 (t : int * int * int * int * int * int * int) =
  let a, b, c, d, e, f, g = t in
  a + b + c + d + e + f + g

let[@inline never] sum_local (local_ p : int * int) = fst p + snd p

let[@inline never] clobber n =
  let local_ big = n + 1, n + 2, n + 3, n + 4, n + 5, n + 6, n + 7 in
  let local_ big2 = n + 1, n + 2, n + 3, n + 4, n + 5, n + 6, n + 7 in
  (sum7 big [@nontail]) + (sum7 big2 [@nontail])

let[@inline] mk a = fun y z -> y + z + a

let[@inline never] apply_local (f : int -> int -> int) = (f 1 2 [@nontail])

let[@inline never] use1 a =
  let h = mk a in
  (apply_local h [@nontail])

let[@inline never] use2 a = mk a

let r : (int -> int) ref = ref (fun x -> x)

let[@inline never] ext () =
  let h = Sys.opaque_identity (use2 100) in
  h 5

let[@inline never] outer n =
  let local_ t = n, n in
  let p = ext () in
  r := p;
  (sum_local t [@nontail])

let () =
  let a = use1 3 in
  let b = outer 4 in
  let c = clobber 5 in
  (Printf.printf [@inlined never]) "%d %d %d %d\n" a b c (!r 7)
