(* TEST
   flambda2;
   stack-allocation;
   unset OCAMLPARAM;
   flags += "-flambda2-reaper -no-experimental-reaper-escape -reaper-debug-flags=nostamps";
   script = "sh ${test_source_directory}/compare-with-annotated.sh escape_partial_application_argument";
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte with dump-reaper;
   script;
   run;
   check-program-output;
 *)

(* [escape_partial_application_argument] with the annotations the escape analysis infers (see
   [compare-with-annotated.sh]). *)
let[@inline never] sum7 (local_ (t : int * int * int * int * int * int * int)) =
  let a, b, c, d, e, f, g = t in
  a + b + c + d + e + f + g

let[@inline never] sum7_local
    (local_ (t : int * int * int * int * int * int * int)) =
  let a, b, c, d, e, f, g = t in
  a + b + c + d + e + f + g

let[@inline never] clobber n =
  let local_ big = n + 1, n + 2, n + 3, n + 4, n + 5, n + 6, n + 7 in
  let local_ big2 = n + 1, n + 2, n + 3, n + 4, n + 5, n + 6, n + 7 in
  (sum7_local big [@nontail]) + (sum7_local big2 [@nontail])

module M : sig
  val mk : int -> int -> int

  val mk2 : int -> int -> int

  val use : int -> int
end = struct
  let[@inline never] h a (b : int * int) c = a + fst b + snd b + c

  let[@inline never] papp (f : int -> int * int -> int -> int) x p =
    (f x p [@nontail])

  let[@inline never] papp_local
      (local_ (f : int -> local_ int * int -> int -> int)) x (local_ p) =
    exclave_ f x p

  let[@inline never] mk n =
    let p = n, n + 1 in
    (papp h n p [@nontail])

  let[@inline never] mk2 n =
    let p = n, n + 1 in
    let h2 a (b : int * int) c = a + fst b + snd b + c + n in
    (papp h2 n p [@nontail])

  let[@inline never] use n =
    let local_ p = n, n + 1 in
    let local_ h3 a (local_ (b : int * int)) c = a + fst b + snd b + c + n in
    let local_ k = papp_local h3 n p in
    let local_ big = n + 1, n + 2, n + 3, n + 4, n + 5, n + 6, n + 7 in
    let s = sum7 big in
    (k 5 [@nontail]) + s
end

let () =
  let g = M.mk 10 in
  let g2 = M.mk2 10 in
  let u = M.use 10 in
  let c = clobber 100 in
  (Printf.printf [@inlined never]) "%d %d %d %d\n" c (g 5) (g2 5) u
