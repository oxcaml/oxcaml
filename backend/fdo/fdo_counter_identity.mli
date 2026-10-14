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

    Metadata and profiles store only {!Position_hash.t}s, computed structurally
    from the components of each position (see {!Fdo_prehash}). *)

(** A position hash. Bit 0 marks function entries for call-target lookup. *)
module Position_hash : sig
  type t

  (** Read a hash from metadata or a profile. *)
  val of_int32 : int32 -> t

  (** Return the on-disk representation. *)
  val to_int32 : t -> int32

  val equal : t -> t -> bool

  (** Unsigned ordering, as used by profile indexes. *)
  val compare : t -> t -> int

  val is_function_entry : t -> bool

  module Tbl : Hashtbl.S with type key = t
end

(** A Lambda body fingerprint, guarding the numbering of interior positions. *)
module Body_hash : sig
  type t

  val of_int32 : int32 -> t

  (** Return the on-disk representation. *)
  val to_int32 : t -> int32

  val equal : t -> t -> bool

  val to_string : t -> string

  val print : Format.formatter -> t -> unit
end

(** A string that represents a name (like a demangled name, though not
    necessarily), with a discriminator to tell apart those with the same string
    (e.g. due to shadowing). *)
module Name : sig
  type t

  val create : name_str:string -> discriminator:int -> t

  val equal : t -> t -> bool

  val compare : t -> t -> int

  val to_string : t -> string
end

(** A function: its original definition, or a copy of it made by specialization
    (see {!Position.specialize}). *)
module Function : sig
  type t

  (** Identify an original function by name, independently of its body. *)
  val create_original : Name.t -> t

  (** The name of the original function, shared by all its copies. *)
  val name : t -> Name.t

  val entry_position_hash : t -> Position_hash.t

  val equal : t -> t -> bool

  val compare : t -> t -> int

  val to_string : t -> string
end

(** A point within a function: its entry, an edge out of a branch, or a call.
    The unit's toplevel code counts as a function. *)
module Position : sig
  type t

  (** The entry of the given function, independent of its body. *)
  val create_function_entry : Function.t -> t

  (** The edge to arm [arm] of the [index]th construct numbered in the given
      function's Lambda body. *)
  val create_branch :
    function_:Function.t -> body_hash:Body_hash.t -> index:int -> arm:int -> t

  (** The call that is the [index]th construct numbered in the given function's
      Lambda body. *)
  val create_call :
    function_:Function.t -> body_hash:Body_hash.t -> index:int -> t

  (** The [index]th call evaluated in the [let] or [module] binding [binder] of
      a structure in the given function (the unit's toplevel code, a functor
      body, or a function with a [let module]), independent of any body. *)
  val create_module_level_call :
    function_:Function.t -> binder:Name.t -> index:int -> t

  (** The function containing the position. *)
  val function_ : t -> Function.t

  (** [None] for function entries and module-level calls. *)
  val body_hash : t -> Body_hash.t option

  val is_function_entry : t -> bool

  val is_call : t -> bool

  val hash : t -> Position_hash.t

  val equal : t -> t -> bool

  val compare : t -> t -> int

  val to_string : t -> string

  (** The position corresponding to [t] in a copy of its function. [at] is the
      call that made the copy, followed by its inlining context, innermost
      first. A position in [at] that is not a call is a fatal error. *)
  val specialize : t -> at:t list -> t
end
