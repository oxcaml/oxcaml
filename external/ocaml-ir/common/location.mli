module Position :
sig
  type t = {
    pos_fname: string ;
    pos_lnum: int ;
    pos_bol: int ;
    pos_cnum: int }[@@deriving sexp]
  include
    sig
      [@@@ocaml.warning "-32"]
      include Sexplib0.Sexpable.S with type  t :=  t
    end[@@ocaml.doc "@inline"][@@merlin.hide ]
end
module Simple :
sig
  type t = {
    loc_start: Position.t ;
    loc_end: Position.t }[@@deriving sexp]
  include
    sig
      [@@@ocaml.warning "-32"]
      include Sexplib0.Sexpable.S with type  t :=  t
    end[@@ocaml.doc "@inline"][@@merlin.hide ]
  val fname : t -> string
  val compare : t -> t -> int
  val non_empty : t -> bool[@@ocaml.doc
                             " Returns true if the range spans >0 characters "]
  val keep_file_basenames : t -> t[@@ocaml.doc
                                    " Returns a Location with its positions modified such that [pos_fname] is equal to\n      [Filename.basename pos_fname] "]
  val of_warnings_loc : Warnings.loc -> t
  val of_debuginfo : Debuginfo.t -> t
  val print : Format.formatter -> t -> unit
  module Map :
  sig
    include Map.S with type  key =  t
    val t_of_sexp : (Sexplib.Sexp.t -> 'a) -> Sexplib.Sexp.t -> 'a t
    val sexp_of_t : ('a -> Sexplib.Sexp.t) -> 'a t -> Sexplib.Sexp.t
  end
end[@@ocaml.doc " Location is an interval between two positions "]
module With_inlined_frames :
sig
  type t[@@deriving sexp]
  include
    sig
      [@@@ocaml.warning "-32"]
      include Sexplib0.Sexpable.S with type  t :=  t
    end[@@ocaml.doc "@inline"][@@merlin.hide ]
  val compare : t -> t -> int
  val in_file : t -> Simple.t
  val of_simple : Simple.t -> t
  val update_in_file : f:(Simple.t -> Simple.t) -> t -> t
  val of_debuginfo : Debuginfo.t -> t
  val of_debuginfo' : Debuginfo.item list -> t
  val is_inlined : t -> bool
  val is_inlined_within_file : t -> bool
  val get_first_declaration : t -> Simple.t
  val print : Format.formatter -> t -> unit
  val for_testing : Simple.t list -> t
  val to_string : t -> string
end
module Pattern :
sig
  type t
  val of_string : string -> t option
  val matches : With_inlined_frames.t -> t -> bool
  val print : Format.formatter -> t -> unit
end[@@ocaml.doc " Pattern to match inlined frames with "]
module Inferred :
sig
  type precision =
    | Exact
    | Approximate [@@deriving (sexp, compare)]
  include
    sig
      [@@@ocaml.warning "-32"]
      val sexp_of_precision : precision -> Sexplib0.Sexp.t
      val precision_of_sexp : Sexplib0.Sexp.t -> precision
      val compare_precision :
        precision -> ((precision)[@merlin.hide ]) -> int
    end[@@ocaml.doc "@inline"][@@merlin.hide ]
  type t = {
    precision: precision ;
    loc: With_inlined_frames.t }[@@deriving (sexp, compare)]
  include
    sig
      [@@@ocaml.warning "-32"]
      include Sexplib0.Sexpable.S with type  t :=  t
        val compare : t -> t -> int
    end[@@ocaml.doc "@inline"][@@merlin.hide ]
  val extract_byte_offset_within_file : t -> (int * int)
  val extract_filename : t -> string
  val extract : t -> Simple.t
  module For_testing :
  sig val create : string -> (int * int * int) -> (int * int * int) -> t end
end
