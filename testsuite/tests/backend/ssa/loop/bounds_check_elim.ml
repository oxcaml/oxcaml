(* TEST
 flags = "-O3 -ssa-bounds-check-elim";
 native;
*)

(* Exercises range-based bounds-check elimination. The loops below have checks
   that the pass can prove redundant, but correctness must not depend on that:
   in particular an index that is genuinely out of bounds must still raise
   [Invalid_argument], and a check that only *looks* redundant (guarded on a
   path that reconverges) must not be removed. *)

(* Classic in-bounds iteration: [i] ranges over [0, length). *)
let[@inline never] sum_for (a : int array) =
  let s = ref 0 in
  for i = 0 to Array.length a - 1 do
    s := !s + a.(i)
  done;
  !s

(* Explicit dominating guard [0 <= i < len]; the access check is redundant. *)
let[@inline never] guarded (a : int array) (i : int) =
  let len = Array.length a in
  if i >= 0 && i < len then a.(i) else -1

(* Reconverging guard: the [i >= len] branch has a side effect and then falls
   through to the access, so [i < len] does NOT hold on entry to [a.(i)]. The
   check must survive and raise for [i >= len]. *)
let[@inline never] reconverging (a : int array) (n : int) =
  let len = Array.length a in
  let s = ref 0 in
  (try
     for i = 0 to n - 1 do
       if i >= len then ignore (Sys.opaque_identity i);
       s := !s + a.(i)
     done
   with Invalid_argument _ -> s := !s + 999);
  !s

let () =
  let a = Array.init 50 (fun i -> i) in
  Printf.printf "sum_for=%d\n" (sum_for a);
  Printf.printf "guarded=%d %d\n" (guarded a 10) (guarded a 100);
  (* length 3, n 5: reads a.(0..2) then a.(3) raises -> caught, +999 *)
  let short = [| 10; 20; 30 |] in
  Printf.printf "reconverging=%d\n" (reconverging short 5)
