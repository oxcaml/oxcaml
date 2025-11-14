(* TEST
 flambda2;
 native;
*)

module type S = sig
  val x : int -> int
  val y : int
end

module Make(X : S) = struct
  let x = X.x
end

module M = Make
    (struct
      let x, y = (fun x -> x), 5
    end)
