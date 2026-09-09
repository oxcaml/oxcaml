module type S = sig
  val f : int -> int
end

(* Inlining [Make] in another unit must specialise its hidden
   non-tail-recursive [loop] on [X.f], even after the reaper has lifted it. *)
module[@inline] Make (X : S) : sig
  val map : int list @ local -> int list @ local
end = struct
  let[@inline available] rec loop (l @ local) = exclave_
    match l with
    | [] -> []
    | x :: xs ->
      let y = X.f x in
      let ys = loop xs in
      y :: ys

  let[@inline] map (l @ local) = exclave_ loop l
end
