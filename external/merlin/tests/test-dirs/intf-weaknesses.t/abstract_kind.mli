module M : sig
  kind_ k

  type t : k
end

type u
type v : M.k
