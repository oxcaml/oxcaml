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
    val is_float : t -> bool
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
  val is_float : t -> bool
  val all : t list
end

module Number : sig
  type t =
    | Float of float
    | Float32 of float
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
    | Bool

  val equal : t -> t -> bool
end

module Bin_op : sig
  type t =
    | Add
    | Sub
    | Mul
    | Eq
    | And
    | Or

  val ops_for_ty : Ty.t -> t list
  val to_code : Ty.t -> t -> Parsetree.expression
end

module Expr : sig
  type t =
    | Const of Number.t
    | Var of Name.t
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

  val convert_num :
    Parsetree.expression ->
    from:NumberTy.t ->
    to_:NumberTy.t ->
    Parsetree.expression
  val to_code : t -> Parsetree.expression
end

module Statement : sig
  type t =
    | Assign of Name.t * Expr.t
    | Seq of t list
    | If of Expr.t * t * t
    | Let_mutable of Name.t * Expr.t * t
    | Bounded_loop of Name.t * int * t

  val let_mutable :
    Name.t ->
    Parsetree.expression ->
    Parsetree.expression ->
    Parsetree.expression
  val sequence : t -> t -> t
  val to_code : t -> Parsetree.expression
end

module Function : sig
  type t =
    { name : Name.t;
      params : (Name.t * Ty.t) list;
      body : Statement.t;
      return_ty : NumberTy.t;
      result : Expr.t
    }

  val to_code : t -> Parsetree.structure_item
end
