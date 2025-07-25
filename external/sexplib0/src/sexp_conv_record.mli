module Kind : sig
  (** A GADT specifying how to parse a record field. See documentation for
      [ppx_sexp_conv]. *)
  type (_, _) t =
    | Default : (unit -> 'a) -> ('a, Sexp.t -> 'a) t
    | Omit_nil : ('a, Sexp.t -> 'a) t
    | Required : ('a, Sexp.t -> 'a) t
    | Sexp_array : ('a array, Sexp.t -> 'a) t
    | Sexp_bool : (bool, unit) t
    | Sexp_list : ('a list, Sexp.t -> 'a) t
    | Sexp_option : ('a option, Sexp.t -> 'a) t
end

module Fields : sig
  (** A GADT specifying record fields. *)
  type _ t =
    | Empty : unit t
    | Field :
        { name : string
        ; kind : ('a, 'conv) Kind.t
        ; conv : 'conv
        ; rest : 'b t
        }
        -> ('a * 'b) t
end

(** Parses a record from a sexp that must be a list of fields.

    Uses [caller] as the source for error messages. Parses using the given [field]s. Uses
    [index_of_field] to look up field names found in sexps. If [allow_extra_fields] is
    true, extra fields are allowed and discarded without error. [create] is used to
    construct the final returned value.
*)
val record_of_sexp
  :  caller:string
  -> fields:'a Fields.t
  -> index_of_field:(string -> int)
  -> allow_extra_fields:bool
  -> create:('a -> 'b)
  -> Sexp.t
  -> 'b

(** Like [record_of_sexp], but for a list of sexps with no [List] wrapper. Used, for
    example, to parse arguments to a variant constructor with an inlined record argument.
    Reports [context] for parse errors when no more specific sexp is applicable. *)
val record_of_sexps
  :  caller:string
  -> context:Sexp.t
  -> fields:'a Fields.t
  -> index_of_field:(string -> int)
  -> allow_extra_fields:bool
  -> create:('a -> 'b)
  -> Sexp.t list
  -> 'b
