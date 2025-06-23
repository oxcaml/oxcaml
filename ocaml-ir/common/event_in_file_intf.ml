module type S  =
  sig
    type data
    type inner = {
      pos: int ;
      data: data }[@@deriving sexp_of]
    include
      sig
        [@@@ocaml.warning "-32"]
        val sexp_of_inner : inner -> Sexplib0.Sexp.t
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
    type type_ =
      | Start 
      | End [@@deriving sexp_of]
    include
      sig
        [@@@ocaml.warning "-32"]
        val sexp_of_type_ : type_ -> Sexplib0.Sexp.t
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
    type t = (type_ * inner)[@@deriving sexp_of]
    include
      sig [@@@ocaml.warning "-32"] val sexp_of_t : t -> Sexplib0.Sexp.t end
    [@@ocaml.doc "@inline"][@@merlin.hide ]
    val sort : data list -> t list
  end
