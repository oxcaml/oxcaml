(* TEST
   flambda2;
   flags += "-flambda2-reaper -X reaper-escape=1";
   { native; check-program-output; }
 *)

(* A submodule with a signature: the signature says nothing about
   locality, so everything is global at the boundary. Checks whether
   a signature is needed to force Heap, or whether unannotated code is
   already Heap. *)
module M : sig
  val sum_pair : int * int -> int
  val f : int -> int -> int
end = struct
  let[@inline never] sum_pair (p : int * int) = fst p + snd p

  let[@inline never] f x y =
    let p = (x, y) in
    (sum_pair p [@nontail])
end

let () = Printf.printf "%d\n" (M.f 3 4)
