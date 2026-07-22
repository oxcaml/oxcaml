(* Type-directed program generator. *)

module Mode : sig
  type t =
    | Soundness
    | Completeness

  val to_string : t -> string

  val of_string : string -> t option
end

module Sample : sig
  (* A single program carrying both annotations on each function under test: [@
     noalloc_strict] drives the frontend Allocation-axis check, and [@zero_alloc
     strict] enables the backend check. Both checks must see the same program
     because the mode annotation changes inference (it forces every allocation
     in the function to be local). *)
  type t = { source : string }

  val to_string : t -> string
end

val generate : mode:Mode.t -> seed:int -> Sample.t
