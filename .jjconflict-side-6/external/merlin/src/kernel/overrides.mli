(** Decodes [[@@@merlin]] override attribute into a list and provides [find] to find an
    [Override.t] given a [Lexing.position].

    The general structure of a [[@@@merlin]] attribute is a list of records pairing
    a [Location.t] with a payload. The [[@@@merlin.document]] attribute can be used, for
    example, to override merlin's [Document] behavior.

    The expected structure of a general [[@@@merlin]]'s payload is as follows:
    {|
      [
        {
          "location" = {
            "loc_start" = { pos_fname = "filename.ml"; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 }
            "loc_end" = { pos_fname = "filename.ml"; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 }
            "loc_ghost" = false
          },
          "payload" = <payload>
        };
        ...
      ]
    |}
    Each individual element of the list is stored as an [Override.t], and the full list
    is stored as a [t].
*)

(** Constants for override attribute names *)
module Attribute_name : sig
  type _ t = Document : string t | Locate : Lexing.position t
end

module Override : sig
  type 'a t

  val payload : 'a t -> 'a
end

type 'a t

(** Constructs a [t] from a [Mreader.parsetree]. An error is returned on unexpected
    AST node structures and parsing errors.

    If there are multiple [@@@merlin.X] attributes (of the same .X), they will be merged. *)
val get_overrides :
  attribute_name:'a Attribute_name.t -> Mreader.parsetree -> 'a t

(** Finds the first [Override.t] that [cursor] is enclosed in. *)
val find : 'a t -> cursor:Lexing.position -> 'a Override.t option
