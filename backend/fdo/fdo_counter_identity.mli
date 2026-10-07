(******************************************************************************
 *                                  OxCaml                                    *
 *                               Jane Street                                  *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2026 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

(** Stable identities for source-position FDO counters.

    A {!Definition_id.t} names a function or a module binding by its source
    scope path and its occurrence among definitions with the same path, rather
    than by its symbol, which changes with unrelated edits. A copy made by
    specialization is named after the original and the call site that made it.

    A {!Position.t} is a point within a definition: a function entry, a numbered
    edge or call site in a function body, or a functor application binding a
    named module. Interior positions are numbered within a particular Lambda
    body, so they also carry a hash of that body; the other kinds do not depend
    on any body.

    Metadata and profiles store only {!Hash.t}s, computed structurally from the
    components of each position (see {!Fdo_prehash}). *)

(** An edge out of a branch, or a call site. *)
module Edge : sig
  type t =
    | Then
    | Else
    | Switch_case of int
    | Callsite
end

(** A position hash. Bit 0 marks function entries for call-target lookup. *)
module Hash : sig
  type t

  (** Read a hash from metadata or a profile. *)
  val of_int32 : int32 -> t

  (** Return the on-disk representation. *)
  val to_int32 : t -> int32

  (** Compare hash values for equality. *)
  val equal : t -> t -> bool

  (** Unsigned ordering, as used by profile indexes. *)
  val compare : t -> t -> int

  (** Test the function-entry tag. *)
  val is_function_entry : t -> bool

  module Tbl : Hashtbl.S with type key = t
end

(** A Lambda body fingerprint, guarding the numbering of interior positions. *)
module Function_body_hash : sig
  type t

  (** Wrap a body fingerprint. *)
  val of_int32 : int32 -> t

  (** Return the on-disk representation. *)
  val to_int32 : t -> int32

  (** Compare body fingerprints for equality. *)
  val equal : t -> t -> bool
end

(** A function or a module binding: its original definition, or a copy of it
    made by specialization. *)
module rec Definition_id : sig
  type t

  (** Identify an original definition by name and occurrence, independently of
      its body. *)
  val create_original : demangled_name:string -> discriminator:int -> t

  (** Identify a copy made at a specialization site. *)
  val create_specialized :
    unspecialized:t -> specialization_site:Position.t -> t

  (** The original name, or [None] for a specialized copy. *)
  val demangled_name : t -> string option

  (** The hash of the entry position of the function with this id. *)
  val hash : t -> Hash.t

  (** Compare identities structurally, not by hash. *)
  val equal : t -> t -> bool

  (** Canonical, unambiguous spelling for metadata and dumps. *)
  val to_string : t -> string
end

and Position : sig
  type t

  (** The entry of the given function, independent of its body. *)
  val create_function_entry : Definition_id.t -> t

  (** A numbered edge or call site in the given function's Lambda body. *)
  val create_interior :
    function_id:Definition_id.t ->
    function_body_hash:Function_body_hash.t ->
    ast_pos:int ->
    edge:Edge.t ->
    t

  (** The functor application binding the given module, independent of the
      enclosing body. *)
  val create_instantiation_site : Definition_id.t -> t

  (** The containing function, or the bound module of an instantiation site. *)
  val definition_id : t -> Definition_id.t

  (** [None] for function entries and named module instantiations. *)
  val function_body_hash : t -> Function_body_hash.t option

  (** Whether this position denotes a function entry. *)
  val is_function_entry : t -> bool

  (** The tagged hash used in metadata and profiles. *)
  val hash : t -> Hash.t

  (** Structural equality. *)
  val equal : t -> t -> bool

  (** Canonical, unambiguous spelling for metadata and dumps. *)
  val to_string : t -> string

  (** The position corresponding to [t] in a copy of its definition. [at] is the
      call site that made the copy, followed by its inlining context, innermost
      first. A site that is not a call site is a fatal error. *)
  val specialize : t -> at:t list -> t
end
