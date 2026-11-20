type module_statics =
  | Compilation_unit of
      { name : Global_module.Name.t
      }
  | Module_field of
      { within : module_statics
      ; index : int
      }
  | Module_var of
      { de_bruijn : int
      }

type module_type =
  { absent : module_item_absent_at_runtime list
  ; present : module_item_present_at_runtime list
  }

and type_decl =
  { arity : int
  ; manifest : val_type option
  }

and module_item_absent_at_runtime =
  | Module_type of module_type option
  | Val_type of type_decl

and module_item_present_at_runtime =
  | Module of module_type (* ...and not just an alias like [module M = N] *)
  | Val of val_type

and val_type =
  | Arrow of
      { domain : val_type
      ; codomain : val_type
      }
  | Constr of
      { constr : type_constr
      ; args : val_type list
      }
  | First_class_module of
      { signature : module_type
      }
  | Poly of
      { body : val_type
      }
  | Type_var of
      { de_bruijn : int
      }

and type_constr =
  | Type_constr_field of
      { within : module_statics
      ; index : int
      }
  | Predef of predef

and predef =
  | Bool
  | Int
  | Option

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
    | Compilation_unit of
        { name : Global_module.Name.t
        }
    | Field of
        { within : t
        ; index : int
        }
    | Function of
        { body : t
        }
    | Let_binding of
        { bound : t
        ; body : t
        }
    | Physical_equality of
        { lhs : t
        ; rhs : t
        }
    | Scalar_operation of
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
