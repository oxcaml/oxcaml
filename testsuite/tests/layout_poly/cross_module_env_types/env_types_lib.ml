let[@inline never] f x = x + 1

let[@inline never] poly_ top x = #(f 1, x)

module F = functor (X : sig @@ dynamic
  val x : int
end @ static) -> struct
  let[@inline never] g x = x + X.x
  let r = Sys.opaque_identity (X.x * 10)
  let[@inline never] poly_ h x = #(f (g r), x, X.x)
end

module M1 = F (struct let x = Sys.opaque_identity 3 end)

module M2 = F (struct let x = Sys.opaque_identity 4 end)
