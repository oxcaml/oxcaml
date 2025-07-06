module Make : functor
  (M : sig
     type t [@@deriving sexp_of]

     include sig
       [@@@ocaml.warning "-32"]

       val sexp_of_t : t -> Sexplib0.Sexp.t
     end
     [@@ocaml.doc "@inline"] [@@merlin.hide]

     val compare : t -> t -> int
     val byte_offset_within_file : t -> (int * int) option
   end)
  -> sig
  val highlight
    :  marker:(M.t -> string)
    -> stop:string
    -> start_newline:(int -> string)
    -> content:string
    -> Format.formatter
    -> M.t list
    -> unit
end
