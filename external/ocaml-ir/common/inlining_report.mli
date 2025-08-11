module Path : sig
  type t [@@deriving sexp]

  include sig
    [@@@ocaml.warning "-32"]

    include Sexplib0.Sexpable.S with type t := t
  end
  [@@ocaml.doc "@inline"] [@@merlin.hide]

  type element =
    | Compilation_unit of string
    | Module of string
    | Class of string
    | Unknown
    | Call of
        { fn : t
        ; loc : Location.Simple.t
        }
    | Fundecl of
        { name : string
        ; loc : Location.Simple.t
        }
  [@@deriving sexp]

  include sig
    [@@@ocaml.warning "-32"]

    val sexp_of_element : element -> Sexplib0.Sexp.t
    val element_of_sexp : Sexplib0.Sexp.t -> element
  end
  [@@ocaml.doc "@inline"] [@@merlin.hide]

  module Map : sig
    include Map.S with type key = element

    val t_of_sexp : (Sexplib.Sexp.t -> 'a) -> Sexplib.Sexp.t -> 'a t
    val sexp_of_t : ('a -> Sexplib.Sexp.t) -> 'a t -> Sexplib.Sexp.t
  end

  val add : t -> element -> t
  val empty : string -> t
  val location : t -> Location.Simple.t option
end

module Call_decision : sig
  type t =
    | Reference of Path.t
    | Unavailable
    | Inlined
    | Not_inlined
  [@@deriving sexp]

  include sig
    [@@@ocaml.warning "-32"]

    include Sexplib0.Sexpable.S with type t := t
  end
  [@@ocaml.doc "@inline"] [@@merlin.hide]

  val compare : t -> t -> int
end

module Fundecl_decision : sig
  type t =
    | Cannot_be_inlined
    | Inlinable
    | Must_be_inlined
  [@@deriving sexp]

  include sig
    [@@@ocaml.warning "-32"]

    include Sexplib0.Sexpable.S with type t := t
  end
  [@@ocaml.doc "@inline"] [@@merlin.hide]

  val compare : t -> t -> int
end

module Tree : sig
  type element =
    | Scope of t
    | Call of
        { decision_description : string
        ; decision : Call_decision.t
        ; inlined : t
        }
    | Fundecl of
        { decision_description : string
        ; decision : Fundecl_decision.t
        ; body : t
        }

  and t = element Path.Map.t [@@deriving sexp]

  include sig
    [@@@ocaml.warning "-32"]

    val sexp_of_element : element -> Sexplib0.Sexp.t
    val sexp_of_t : t -> Sexplib0.Sexp.t
    val element_of_sexp : Sexplib0.Sexp.t -> element
    val t_of_sexp : Sexplib0.Sexp.t -> t
  end
  [@@ocaml.doc "@inline"] [@@merlin.hide]

  val empty : t
  val insert : key:Path.element -> element:element -> t -> t
  val get_function_call : path:Path.t -> t -> element option
  val get_function_def : path:Path.t -> t -> element option
  val find_all_function_calls : under:Path.t -> t -> (Path.t * Call_decision.t) list
  val find_all_function_defs : under:Path.t -> t -> (Path.t * Fundecl_decision.t) list
end

type t =
  | Org_mode of { content : string }
  | Tree of Tree.t
[@@deriving sexp]

include sig
  [@@@ocaml.warning "-32"]

  include Sexplib0.Sexpable.S with type t := t
end
[@@ocaml.doc "@inline"] [@@merlin.hide]

val print : Format.formatter -> t -> unit
