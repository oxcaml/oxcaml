module type S = sig
  type t
  val x : t
  val i : int
end

type w = W : (module S with type t = string) -> w

type v = V : (module S with type t = int * string) -> v

type ('a, 'b) u = U : (module S with type t = 'a * 'b) -> ('a, 'b) u
