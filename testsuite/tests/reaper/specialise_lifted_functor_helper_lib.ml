module type S = sig
  val f : int -> int
end

(* Inlining [Make] must specialise its hidden [@inline never] helper on [X.f],
   without inlining the helper itself. *)
module[@inline] Make (X : S) : sig
  val apply_twice : int -> int
end = struct
  let[@inline never] helper x = X.f (X.f x)

  let[@inline] apply_twice x = helper x
end
