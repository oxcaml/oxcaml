module Name : sig
  type t

  val of_string : string -> t

  val to_string : t -> string
end

module NumberTy : sig
  module Base : sig
    type t =
      | Float
      | Float32
      | Int
      | Nativeint
      | Int64
      | Int32
      | Int16
      | Int8

    val all : t list

    val equal : t -> t -> bool

    val to_module : t -> string

    val is_floating_point : t -> bool
  end

  type t =
    { base : Base.t;
      unboxed : bool
    }

  val boxed : Base.t -> t

  val unboxed : Base.t -> t

  val equal : t -> t -> bool

  val to_module : t -> string

  val to_string : ?no_hash:bool -> t -> string

  val converter_name : from:t -> to_:t -> string

  val is_floating_point : t -> bool

  val all : t list
end

module Number : sig
  type t =
    | Float of int64
    | Float32 of int32
    | Int of int
    | Nativeint of Nativeint.t
    | Int64 of int64
    | Int32 of int32
    | Int16 of int
    | Int8 of int

  val of_integral_bits : NumberTy.Base.t -> int64 -> t

  val to_code : t -> Parsetree.expression
end

module Ty : sig
  type t =
    | Number of NumberTy.t
    | Array of NumberTy.t * int list
    | Record of record
    | Bool

  and record =
    { id : int;
      fields : field list;
      unboxed : bool
    }

  and field =
    { index : int;
      ty : t;
      is_mutable : bool
    }

  val equal : t -> t -> bool
  val record_name : record -> string
  val field_name : record -> field -> string
  val to_code : t -> Parsetree.core_type
end

module Binding : sig
  type t =
    { name : Name.t;
      ty : Ty.t;
      is_mutable : bool
    }
end

module Bin_op : sig
  type t =
    | Add
    | Sub
    | Mul
    | Bit_and
    | Bit_or
    | Bit_xor
    | Shift_left
    | Shift_right
    | Shift_right_logical
    | Eq
    | Lt
    | Le
    | Gt
    | Ge
    | And
    | Or

  val ops_for_ty : Ty.t -> t list

  val to_code :
    Ty.t ->
    t ->
    Parsetree.expression ->
    Parsetree.expression ->
    Parsetree.expression
end

module Expr : sig
  type t =
    | Const of Number.t
    | Read of place
    | Array_literal of t list
    | Array_make of
        { dimensions : int list;
          init_name : Name.t;
          init : t
        }
    | Record of Ty.record * t list
    | Record_update of Ty.record * t * Ty.field * t
    | Record_convert of
        { from : Ty.record;
          to_unboxed : bool;
          source_name : Name.t;
          expr : t
        }
    | Opaque of t
    | Bin_op of
        { ty : Ty.t;
          op : Bin_op.t;
          lhs : t;
          rhs : t
        }
    | Convert of
        { expr : t;
          from : NumberTy.t;
          to_ : NumberTy.t
        }
    | Call_toplevel of
        { fun_name : Name.t;
          args : t list
        }

  and place =
    | Variable of Name.t
    | Field of place * Ty.record * Ty.field
    | Element of place * t list

  val convert_num :
    Parsetree.expression ->
    from:NumberTy.t ->
    to_:NumberTy.t ->
    Parsetree.expression

  val to_code : t -> Parsetree.expression
  val place_to_code : place -> Parsetree.expression
  val assignment_to_code : place -> t -> Parsetree.expression
end

module Place : sig
  type t = Expr.place =
    | Variable of Name.t
    | Field of t * Ty.record * Ty.field
    | Element of t * Expr.t list

  val to_code : t -> Parsetree.expression
end

module Statement : sig
  type t =
    | Assign of Place.t * Expr.t
    | Seq of t list
    | If of Expr.t * t * t
    | Let of Binding.t * Expr.t * t
    | Bounded_loop of
        { var : Name.t;
          init : Expr.t;
          bound : Expr.t;
          stride : int;
          body : t
        }

  val let_binding :
    Binding.t -> Parsetree.expression -> Parsetree.expression ->
    Parsetree.expression

  val sequence : t -> t -> t
  val to_code : t -> Parsetree.expression
end

module Inline : sig
  type t =
    | Never
    | Always
    | Default
end

module Function : sig
  type t =
    { name : Name.t;
      params : Binding.t list;
      inline : Inline.t;
      body : Statement.t;
      return_ty : Ty.t;
      result : Expr.t
    }

  val to_code : t -> Parsetree.structure_item
end
