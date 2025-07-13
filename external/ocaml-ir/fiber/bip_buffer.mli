[@@@ocaml.text " Simon Cooke's Bip_buffer "]

module Blit : sig
  type ('src, 'dst) t =
    src:'src -> src_pos:int -> dst:'dst -> dst_pos:int -> len:int -> unit
end

module Slice : sig
  type t =
    { pos : int
    ; len : int
    }
end

type 'a t
[@@ocaml.doc
  " a bip buffer with some underlying container of bytes indexed by a continuous range of\n\
  \    integers that starts from 0. "]

val max_available : _ t -> int
  [@@ocaml.doc
    " [max_available t] returns the maximum available contiguous write size the buffer can\n\
    \    accept "]

val best_available : _ t -> int
  [@@ocaml.doc
    " [best_available t] returns the best available contiguous write size the buffer can\n\
    \    accept. If all writes are smaller than [best_available t], it is guaranteed \
     that no\n\
    \    space will be wasted. "]

val is_empty : _ t -> bool
  [@@ocaml.doc " [is_empty t] true if there are no bytes available to read in [t] "]

val length : _ t -> int
  [@@ocaml.doc " [length t] returns the number of bytes readable in [t] "]

val buffer : 'a t -> 'a
  [@@ocaml.doc " [buffer t] returns the underlying buffer of [t] for reading/writing "]

val create : 'a -> len:int -> 'a t
  [@@ocaml.doc
    " [create buf ~len] creates a new buffer with underlying storage [buf] of length \
     [len] "]

val junk : _ t -> len:int -> unit
  [@@ocaml.doc
    " [junk t ~len] discards [len] from the front of the buffer. Usually called after\n\
    \    reading "]

val peek : _ t -> Slice.t option
  [@@ocaml.doc
    " [peek t] returns the next contiguous readable buffer slice as [Some _]. If [t] is\n\
    \    empty, [None] is returned. Once a portion of this slice is read, [junk] should be\n\
    \    called. "]

val reserve : _ t -> len:int -> int option
val commit : _ t -> len:int -> unit

val unused_space : _ t -> int
  [@@ocaml.doc
    " Total amount of free space available in the buffer. Not all of it may be usable. To\n\
    \    reclaim it, call [compress] "]

val compress : 'a t -> ('a, 'a) Blit.t -> unit
  [@@ocaml.doc
    " [compress t blit] will try to compress the buffer with 2 blit operations. Use\n\
    \    [unused_space t] to asses how useful this will be. "]

val resize : 'a t -> ('a, 'a) Blit.t -> 'a -> len:int -> unit
  [@@ocaml.doc
    " [resize t blit buf ~len] will create a new buffer with the same data. The old buffer\n\
    \    is then emptied and can be reused "]

val pp : (Format.formatter -> 'a * Slice.t -> unit) -> Format.formatter -> 'a t -> unit

module Bytes : sig
  type nonrec t = Bytes.t t

  val resize : t -> len:int -> unit
  val compress : t -> unit
  val pp : Format.formatter -> t -> unit

  module Writer : sig
    [@@@ocaml.text
    " This module will automatically resize/compress the buffer when space runs out. If\n\
    \        you want to make sure the buffer doesn't grow, make sure the writes are \
     within\n\
    \        [max_available] or [best_available]. "]

    val add_char : t -> char -> unit
    val add_string : t -> string -> unit
    val add_substring : t -> string -> pos:int -> len:int -> unit
    val add_bytes : t -> Bytes.t -> unit
    val add_subbytes : t -> Bytes.t -> pos:int -> len:int -> unit
  end
end
