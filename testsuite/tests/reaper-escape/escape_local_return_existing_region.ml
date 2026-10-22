(* TEST
   flambda2;
   stack-allocation;
   flags += "-flambda2-reaper -X reaper-escape=1 -reaper-debug-flags=nostamps";
   { native with dump-reaper; check-fexpr-dump; }
 *)

(* All calls to [mk] are known, but its call site is inside a pre-existing
   region which closes before [after] uses the result. Changing [mk] to return
   locally would therefore free its result too early. This checks the
   call-result counterpart of [escape_type_checker_region]. *)
let[@inline never] sum_pair (p : int * int) = fst p + snd p

let[@inline never] sum_local (local_ p : int * int) = fst p + snd p

let[@inline always] after p n =
  let q = n, n + 1 in
  let r = n + 2, n + 3 in
  sum_pair q + sum_pair r + sum_pair p

module M : sig
  val f : int -> int
end = struct
  let[@inline never] mk x = x + 1, x + 2

  let[@inline never] f x =
    let local_ l = x, x in
    let n = sum_local l in
    after (mk x) n
end

let () = (Printf.printf [@inlined never]) "%d\n" (M.f 10)
