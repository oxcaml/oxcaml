(* TEST
   flambda2;
   flags += "-flambda2-reaper -X reaper-escape=1 -reaper-debug-flags=nostamps";
   { native with dump-reaper; check-fexpr-dump; }
 *)

(* The pair returned by [these] is unboxed by the reaper, so the option stored
   in it travels to [test] as a field: it must not be allocated in a region of
   [these]. *)
let[@inline never] use o = match o with Some v -> v | None -> 0

let[@inline never] sum_pair (p : int * int) = fst p + snd p

module M : sig
  val test : int -> int
end = struct
  let[@inline never] these x = x + 1, Some x

  let[@inline never] test x =
    let n, o = these x in
    let p = n + 1, n + 2 in
    n + use o + sum_pair p
end

let () = (Printf.printf [@inlined never]) "%d\n" (M.test 41)
