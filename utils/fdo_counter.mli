(** Structured FDO counters. Function entries and named module instantiations
    survive body edits; AST positions additionally identify their Lambda body.
*)

type edge =
  | Then
  | Else
  | Switch_case of int
  | Callsite

(** A 32-bit hash of a position, with the low bit replaced by the function-entry
    tag. *)
module Hash : sig
  type t = private int32

  (** For readers of the on-disk formats. *)
  val of_int32 : int32 -> t

  val equal : t -> t -> bool

  (** Unsigned ordering, as used by profile indexes. *)
  val compare : t -> t -> int

  val is_function_entry : t -> bool

  module Tbl : Hashtbl.S with type key = t
end

(** A 32-bit hash of a function's Lambda body (see [Fdo_fingerprint]): the
    positions of a body are numbered in it, so the counters at them are only
    comparable between builds of the same body. *)
module Function_body_hash : sig
  type t = private int32

  val of_int32 : int32 -> t

  val equal : t -> t -> bool
end

(** The [Fdo_prehash.t] a position's or function id's [hash] is taken from:
    computed once, by the constructors below, from its components and the
    prehashes of the positions and function ids it contains. *)
type prehash

type function_id = private
  | Function of
      { unmangled_name : string;
        discriminator : int;
        prehash : prehash
      }
  | Specialized of
      { unspecialized : function_id;
        specialization_site : position;
        prehash : prehash
      }

and position = private
  | Function_entry of function_id
  | Position of
      { function_id : function_id;
        function_body_hash : Function_body_hash.t;
        ast_pos : int;
        edge : edge;
        prehash : prehash
      }
  | Instantiation_site of
      { module_initializer : function_id;
        prehash : prehash
      }
      (** The named module initializer, possibly itself specialized. It has no
          body hash. *)

val function_id : unmangled_name:string -> discriminator:int -> function_id

val specialized :
  unspecialized:function_id -> specialization_site:position -> function_id

val function_entry : function_id -> position

val position :
  function_id:function_id ->
  function_body_hash:Function_body_hash.t ->
  ast_pos:int ->
  edge:edge ->
  position

val instantiation_site : function_id -> position

val hash_function_id : function_id -> Hash.t

val hash_position : position -> Hash.t

type t =
  { position : position;
    inlining_stack : position list  (** call sites, innermost first *)
  }

val inline : t -> at:t -> t

(** Rename the outermost function identity rather than adding inlining depth. *)
val specialize : t -> at:t -> t

val is_function_entry : position -> bool

val equal : t -> t -> bool

(** Canonical, unambiguous renderings, for name metadata and dumps. *)
val function_id_to_string : function_id -> string

val position_to_string : position -> string

val to_string : t -> string

(** The hashes of a counter's position and inlining stack, innermost first. *)
type hashed = Hash.t list

val hash : t -> hashed

(** [add_all existing counters] appends the [counters] not already in
    [existing]. *)
val add_all : t list -> t list -> t list
