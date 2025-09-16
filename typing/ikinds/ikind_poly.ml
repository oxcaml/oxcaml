module Rigid = struct
  type t =
    | Param of int
    | Constr_arg of Path.t * int
end

module Node = struct
  type t =
    | Leaf of Axis_lattice.t
    | Branch of
        { rigid : Rigid.t;
          lo : t;
          hi : t
        }
end

type t =
  { base : Node.t;
    coeffs : Node.t array
  }
