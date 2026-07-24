(* Type-directed program generator. *)

module Mode : sig
  type t =
    | Soundness
    | Completeness

  val to_string : t -> string

  val of_string : string -> t option
end

module Sample : sig
  type t =
    { prelude_source : string;
          (* the prelude alone (type declarations + values / functions), for the
             oracle's gate compile *)
      source : string (* the full program: prelude + main function *)
    }

  val to_string : t -> string
end

(* [max_decls] bounds the number of type declarations per program (count drawn
   uniformly from [min 2 max_decls .. max_decls]; must be at least 1).
   [allow_assume] enables the [@zero_alloc assume] lane on prelude functions
   (experimental; see [gen_annot] in gen.ml). *)
val generate :
  max_decls:int -> allow_assume:bool -> mode:Mode.t -> seed:int -> Sample.t
