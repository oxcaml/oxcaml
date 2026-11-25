(* TEST
   flambda2;
   stack-allocation;
   flags += "-flambda2-reaper -reaper-debug-flags=nostamps";
   script = "sh ${test_source_directory}/compare-with-annotated.sh escape_partial_application_region";
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte with dump-reaper;
   script;
   run;
   check-program-output;
 *)

(* [escape_partial_application_region] with the annotations the escape analysis infers (see
   [compare-with-annotated.sh]). *)
let[@inline never] sum_pair (local_ (p : int * int)) = fst p + snd p

let[@inline never] sum7 (local_ (t : int * int * int * int * int * int * int)) =
  let a, b, c, d, e, f, g = t in
  a + b + c + d + e + f + g

let[@inline never] papp_exported (f : int -> int -> int) m =
  let tmp = m, m + 1 in
  let k = f (sum_pair tmp) in
  let _ = sum_pair tmp in
  k

module M : sig
  val run : int -> int
end = struct
  let[@inline never] papp (local_ (f : int -> int -> int)) m =
    exclave_
      (let tmp = m, m + 1 in
       let k = f (sum_pair tmp) in
       let _ = sum_pair tmp in
       k)

  let[@inline never] run n =
    let local_ h a b = a + b + n in
    let local_ k = papp h n in
    let h2 a b = (a * b) + n in
    let k2 = papp_exported h2 n in
    let local_ big = n + 1, n + 2, n + 3, n + 4, n + 5, n + 6, n + 7 in
    k 2 + k2 3 + sum7 big
end

let () = (Printf.printf [@inlined never]) "%d\n" (M.run 10)
