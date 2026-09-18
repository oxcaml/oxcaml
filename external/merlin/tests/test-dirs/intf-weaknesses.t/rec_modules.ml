module rec Tree : sig
  type t =
    | Leaf
    | Node of Forest.t
end = struct
  type t =
    | Leaf
    | Node of Forest.t
end

and Forest : sig
  type t = { trees : Tree.t list }
end = struct
  type t = { trees : Tree.t list }
end

type tree = Tree.t
type forest = Forest.t
