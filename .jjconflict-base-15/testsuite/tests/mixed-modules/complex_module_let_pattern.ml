(* TEST
 flambda2;
 native;
*)

(* This test doesn't actually have much to do with mixed modules. It exercises
   an uncommon transformation, where module-level lets are transformed to static
   raise/catch pairs when their patterns aren't simple. The layout associated
   with the result of this catch was wrong before mixed modules, and we had to
   fix it but initially got it wrong in a different way that hit a scary
   _runtime_ flambda2 invalid code error. This test mainly exists to make sure
   that uncommon transformation is exercised in the test suite. *)

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
