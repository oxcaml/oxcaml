(** Symbolic description of a value as a projection out of a function parameter
    (or [my_closure]).

    For example, the result of [%get_tag] applied to the untagged form of the
    first parameter is described by [get_tag (untag (param 0))]. *)

type source =
  | Param of int
  | My_closure

type projection =
  | Get_tag of projection
  | Untag of projection
  | Is_int of projection
  | Block_load of
      { field : Target_ocaml_int.t;
        projection : projection
      }
  | Identity

type t =
  { source : source;
    projection : projection
  }

val param : int -> t

val my_closure : t

val get_tag : t -> t

val untag : t -> t

val is_int : t -> t

val block_load : Target_ocaml_int.t -> t -> t

val equal : t -> t -> bool

val compare : t -> t -> int

val print : Format.formatter -> t -> unit
