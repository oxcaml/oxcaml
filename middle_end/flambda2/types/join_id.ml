type t = int

module T0 = struct
  let compare = Numeric_types.Int.compare

  let equal = Numeric_types.Int.equal

  let hash = Numeric_types.Int.hash

  let print = Numeric_types.Int.print
end

include T0

module T = struct
  type nonrec t = t

  include T0
end

module Tree = Patricia_tree.Make (struct
  let print = print
end)

module Set = Tree.Set
module Map = Tree.Map

let create =
  let cnt = ref 0 in
  fun () ->
    incr cnt;
    !cnt
