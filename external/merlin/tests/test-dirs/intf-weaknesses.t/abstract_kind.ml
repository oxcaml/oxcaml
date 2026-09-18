module M : sig
  kind_ k

  type t : k
end = struct
  kind_ k = value

  type t = int
end

type u = { field : M.t }

type v : M.k mod portable
