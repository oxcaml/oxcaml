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

(** The format of the [".debug_fdo_metadata"] section, shared by its encoder
    ([Fdo_metadata_encode]) and its decoder.

    The section is a concatenation of fragments, one per compilation unit (the
    linker concatenates them):

    {v
    fragment:
      "FDOM"            4 bytes
      length            u64    bytes after this field, to skip the fragment
      version           uleb
      counters          uleb n, then n x (hash u32, context uleb)
      functions         uleb n, then n x function
      bodies            uleb n, then n x (function hash u32, body hash u32)
      names             uleb n, then n x (hash u32, .debug_str offset u32)
      dropped           uleb n, then n x (reason u8, counter uleb)

    function:
      start             u64    absolute address
      length            uleb   bytes of code
      entries           uleb n, then n x entry   (not sorted by offset)

    entry:
      offset            uleb   from the function's start
      flags             u8
      operand           uleb   by action, see below
      counter           uleb   if the action has one

    flags:
       7  6  5  4  3  2  1  0
      +--------+--------+-----+
      |0  0  0 | action |t  f |   t, f: on taken, on fallthrough
      +--------+--------+-----+

      action                code  t f   operand
      Count_taken             2   1 0
      Count_fallthrough       2   0 1
      Count_executed          2   1 1
      Count_call              0   1 0   call length
      Count_tail_call         1   1 0
      Count_indirect_jump     4   1 0   target offset from the function's start
      Unwind_to_handler       3   1 1   (and no counter)
    v}

    The counters of the fragment are in a table (see [Counter_table]), to which
    the entries and the dropped counters refer by index. A call's return address
    is its address plus its length, so returns need no entries. The names are
    recorded with [-fdo-names] and the dropped counters with
    [-fdo-dropped-counters]; the tables are empty otherwise. *)

val section_name : string

val magic : string

val version : int

(** The operand of an action, which precedes its counter. *)
module Operand : sig
  type 'address t =
    | Return_address of 'address
        (** Of a call, encoded as the length of the call instruction. *)
    | Target of 'address
        (** Of an indirect jump, encoded as its offset from the start of the
            function. *)
end

(** What a decoder does when the instruction at an entry's address executes.
    Every counter is the complete stack of the counter within its function
    (static context); the decoder appends the dynamic context of the callers.
    ['address] is how addresses are represented: as labels in the encoder, as
    integers in the decoder. *)
module Action : sig
  type 'address t =
    | Count_taken of Fdo_counter.Hashed.t
        (** Counts when the instruction branches. *)
    | Count_fallthrough of Fdo_counter.Hashed.t
        (** Counts when the instruction executes without branching. *)
    | Count_executed of Fdo_counter.Hashed.t
        (** Counts whenever the instruction executes. *)
    | Count_call of
        { return_address : 'address;
          counter : Fdo_counter.Hashed.t
        }
        (** Counts the call, and pushes a frame of dynamic context that a return
            to [return_address] pops. *)
    | Count_tail_call of Fdo_counter.Hashed.t
        (** Counts the call, and extends the dynamic context without a frame to
            return to. *)
    | Count_indirect_jump of
        { target : 'address;
          counter : Fdo_counter.Hashed.t
        }  (** Counts when the jump lands at [target]. *)
    | Unwind_to_handler
        (** An exception handler: unwinds the dynamic context to the nearest
            frame pushed by a call in the handler's machine function, or clears
            it if there is none. *)

  val counter : _ t -> Fdo_counter.Hashed.t option

  val operand : 'address t -> 'address Operand.t option
end

(** The kind of an action, without its operand and counter. *)
module Kind : sig
  type t =
    | Count_taken
    | Count_fallthrough
    | Count_executed
    | Count_call
    | Count_tail_call
    | Count_indirect_jump
    | Unwind_to_handler

  val of_action : _ Action.t -> t

  (** Whether the action applies when the instruction branches. *)
  val on_taken : t -> bool

  (** Whether the action applies when the instruction executes without
      branching. *)
  val on_fallthrough : t -> bool

  (** Whether the action has a counter. *)
  val has_counter : t -> bool

  (** The operand of the actions of this kind, if they have one. *)
  val operand : t -> unit Operand.t option

  (** The action of this kind with these parts, or [None] if they are not those
      of the kind (see [operand] and [has_counter]). *)
  val to_action :
    t ->
    operand:'address Operand.t option ->
    counter:Fdo_counter.Hashed.t option ->
    'address Action.t option

  (** The flags byte of an entry. *)
  val to_flags : t -> int

  (** [None] for a byte that is no valid flags. *)
  val of_flags : int -> t option
end

(** An instruction with an action. *)
module Entry : sig
  type 'address t =
    { address : 'address;
      action : 'address Action.t
    }
end

module Function : sig
  type 'address t =
    { start : 'address;
      finish : 'address;  (** The address just after the function's code. *)
      entries : 'address Entry.t list  (** In metadata order. *)
    }
end

(** What the section records, of a fragment or of the whole section. *)
type 'address t =
  { functions : 'address Function.t list;  (** In metadata order. *)
    body_hashes : Fdo_counter.Body_hash.t Fdo_counter.Position_hash.Tbl.t;
        (** The body hash of every compiled instrumented function, by the hash
            of its entry position. *)
    names : string Fdo_counter.Position_hash.Tbl.t;
    dropped : (Fdo_dropped_counters.Reason.t * Fdo_counter.Hashed.t) list
        (** In metadata order. *)
  }

(** The counters of a fragment. Each is recorded as its innermost position and
    its context: the counter of the call site that its code was inlined at,
    which precedes it in the table (the inlining stack of a counter is the
    counter of that call site). So a context is recorded once however many
    counters have it.

    {v context:   0 for none, k for the counter at index k - 1 v} *)
module Counter_table : sig
  module Node : sig
    type t =
      { hash : Fdo_counter.Position_hash.t;
        context : int option  (** The index of the context's counter. *)
      }
  end

  (** The table of the counters of a fragment, as the encoder fills it. *)
  module Builder : sig
    type t

    val create : unit -> t

    (** The index of the counter, which is added with its contexts if absent. *)
    val add : t -> Fdo_counter.Hashed.t -> int

    (** In index order. *)
    val nodes : t -> Node.t list
  end

  (** The table, as the decoder reads it. *)
  type t

  (** [None] if the context of a node does not precede it. *)
  val of_nodes : Node.t list -> t option

  (** The counter at an index, if any. *)
  val find : t -> int -> Fdo_counter.Hashed.t option
end
