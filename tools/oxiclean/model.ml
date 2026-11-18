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

  and kind =
    | Abstract

  and constr =
    | Predef of predef
    | Var of { de_bruijn : int }

  and predef =
    | Int
    | Option
end

module rec Expression : sig
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
          ; lhs : Expression.t
          ; rhs : Expression.t
          }
  end

  type t =
    | Apply of
        { func : t
        ; args : t list
        }
    | Block of
        { tag : int
        ; fields : t list
        }
    | Field of
        { block : t
        ; index : int
        }
    | Function of closure
    | LetBinding of
        { bound : t
        ; body : t
        }
    | PhysicalEquality of
        { negated : bool
        ; lhs : t
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
        { path : string list
        }
    | Value of
        { value : Value.t
        }
    | Var of
        { de_bruijn : int
        }

  and closure =
    { n_params : int
    ; body : t
    }

  and switch_case =
    { case : int
    ; body_if_matched : t
    }
end = Expression

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
    | Closure of Expression.closure
end = Value
