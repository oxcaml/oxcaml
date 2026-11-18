module Type = struct
  type t =
    | EmptyModule
    | Module of
        { ty : t
        ; etc : t
        }
    | Type of
        { kind : kind
        ; etc : t
        }
    | Val of
        { ty : expr
        ; etc : t
        }

  and expr =
    | Arrow of
        { domain : expr
        ; codomain : expr
        }
    | Constr of
        { constr : constr
        ; args : expr list
        }
    | Poly of
        { body : expr
        }
    | TypeVar of
        { de_bruijn : int
        }

  and kind =
    | Abstract

  and constr =
    | Predef of predef
    | Var of { de_bruijn : int }

  and predef =
    | Bool
    | Int
    | Option
end

module rec Expr : sig
  module Scalar_operation : sig
    module Binary : sig
      module Integral : sig
        type t =
          | Mod
      end

      type t =
        | Integral of Integral.t
    end

    type t =
      | Binary of
          { operation : Binary.t
          ; lhs : Expr.t
          ; rhs : Expr.t
          }
  end

  type t =
    | Apply of
        { func : t
        ; arg : t
        }
    | Block of
        { tag : int
        ; fields : t list
        }
    | Field of
        { block : t
        ; index : int
        }
    | Function of
        { body : t
        }
    | LetBinding of
        { bound : t
        ; body : t
        }
    | PhysicalEquality of
        { lhs : t
        ; rhs : t
        }
    | ScalarOperation of
        { operation : Scalar_operation.t
        }
    | Switch of
        { scrutinee : t
        ; n_consts : int
        ; n_blocks : int
        ; consts : switch_case list
        ; blocks : switch_case list
        ; failaction : t option
        }
    | UpstreamCompilationUnit of
        { name : string
        }
    | Value of
        { value : Value.t
        }
    | Var of
        { de_bruijn : int
        }

  and switch_case =
    { case : int
    ; body_if_matched : t
    }
end = Expr

and Value : sig
  type t =
    | Immediate of
        { bits_above_lsb : int
        }
    | Indirection of
        { indirection : indirection
        }

  and indirection =
    | Immutable of
        { block : block
        }

  and block =
    { tag : int
    ; contents : contents
    }

  and contents =
    | Values of
        { values : t list
        }
    | Bytes of
        { bytes : int list
        }
    | Closure of
        { body : Expr.t
        }
end = Value
