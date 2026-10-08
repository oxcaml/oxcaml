(* Tests showing how [zero_alloc] information from a functor parameter's
   signature can be used in checking the functor's body.

   For more comprehensive tests of using zero_alloc information from signatures,
   see [test_signatures_separate_{a,b}.ml]. *)

(* Most basic use case. *)
module type S_basic = sig
  val f : int -> int [@@zero_alloc]
end

module F_basic (X : S_basic) = struct
  let[@zero_alloc] g x = X.f x
end

(* A non-strict assumption won't help you with a strict check. *)
module F_strict_bad (X : S_basic) = struct
  let[@zero_alloc strict] g x = X.f x
end

module type S_partial = sig
  val id : 'a -> 'a [@@zero_alloc partial]

  val add : int -> int -> int [@@zero_alloc partial]
end

module F_partial (X : S_partial) = struct
  (* compiles: *)
  let[@zero_alloc] partial x = X.add x

  (* compiles: *)
  let[@zero_alloc] full x y = X.add x y

  (* fails to compile: *)
  let[@zero_alloc] over h x = X.id h x
end
