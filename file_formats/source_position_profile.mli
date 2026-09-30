(******************************************************************************
 *                                  OxCaml                                    *
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

(** An FDO profile of execution counts per counter.

    A counter ({!Fdo_counter.t}) is a position (a function entry, a branch edge
    or a call site) and the call sites it was inlined through, innermost first.
    Their separate hash encodings produce 32-bit keys. The profile is a forest
    of inverse tries: each root holds the count summed over all contexts;
    walking down the trie refines it by increasingly long prefixes of the
    inlining stack.

    Counters are not stored directly, only their hashes. A node counts
    separately the paths that end at it (recorded with no further context) and
    those that continue into its children, so that a query can tell how much of
    what it passes on the way down it cannot account for.

    The profile also indexes the call graph: for each call site level, the entry
    counters of the functions that calls from it reached ({!call_targets}),
    whether inlined or explicit. This is the inverse of the first level of the
    function-entry tries (identified by bit 0 of their root hashes). The counts
    stay in the trie, under the callee's entry counter refined by the call site
    and its inlining context like any other counter, so a consumer lists the
    candidates and then walks the trie from each with as much context as it has
    ({!count_for_deepest_context}).

    The on-disk format is designed to be queried in place, without parsing: trie
    nodes reference their children by file offset through hash-sorted entry
    arrays, so a query reads only the entries it searches. Loading validates the
    header eagerly but the trie lazily, as it is read, so a memory-mapped
    profile has most of its pages never touched. *)

type t

(** Raised, with a message, on any problem reading or validating the profile: by
    {!load} for problems in the header or the root index, and by any query or
    traversal that reads a malformed part of the trie (validation is lazy). The
    profile is deliberately validated strictly: a malformed profile signals that
    feedback-directed optimization is broken and should be surfaced loudly
    rather than silently ignored. *)
exception Error of string

(** The magic number at the start of the on-disk format (its last byte is the
    format version). *)
val magic_number : string

(** {2 Reading and querying} *)

(** [load ~filename] opens a profile and validates its header. Raises {!Error}
    if the file cannot be opened, has the wrong magic number or version, or has
    a malformed header or root index (including trailing bytes). The trie itself
    is validated lazily: queries and traversals raise {!Error} when they read a
    malformed part.

    The file is memory-mapped when a mapper has been registered with
    {!register_mmap}, so that queries only fault in the pages they touch;
    otherwise, or if mapping fails, it is read into memory. *)
val load : filename:string -> t

type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

(** Register how to memory-map a file. Mapping needs [Unix], which this library
    cannot depend on; the native compiler driver registers a mapper at startup.
*)
val register_mmap : (string -> bigstring) -> unit

(** What the profile says about a counter's execution count: [lower] is what it
    recorded for exactly this counter, [upper] adds what might also be the
    counter's, and [estimate] is the likeliest value in between, when there is a
    basis for one. The three agree when the trie has the whole inlining context
    and every path through the nodes on the way carried at least that much
    context. Paths that ended along the way with less context (the profiled
    build inlined differently, or lost the dynamic call context) may or may not
    be the counter's: they widen the interval, and the estimate assumes they
    split among the contexts like the paths that continued. Where a context
    level is missing, the count is 0 except for those paths (estimate 0), and,
    if the profiled build did not have the level's position (its function
    changed body), for the paths continuing into the other contexts, which might
    be the counter's under its old position (no estimate). A position the trie
    has no root for was never executed if the profiled build had it ([0, 0]),
    and is unknown otherwise ([0, infinity], no estimate). *)
type bound =
  { lower : float;
    upper : float;
    estimate : float option
  }

val count : t -> Fdo_counter.t -> bound

(** The recorded count for a counter. Returns 0 if any part of the context is
    absent; unobserved and never-executed counters are not distinguished. *)
val recorded_count : t -> Fdo_counter.t -> int64

(** [count_for_deepest_context t ~root ~context], for level hashes, walks the
    trie from the root [root] along [context] as far as the profile has nodes
    and returns the count there: the count for the longest recorded prefix of
    the context, i.e. refined by the context the profile knows and aggregated
    over the rest. 0 when there is no such root. *)
val count_for_deepest_context :
  t -> root:Fdo_counter.Hash.t -> context:Fdo_counter.hashed -> int64

(** Whether the profiled build compiled the function, and with the same Lambda
    body hash. [Unknown_function] means the function did not exist under this
    id; [Changed_body] means its interior counters cannot apply. A zero count
    under [Same_body] is a real measurement of zero. *)
type body_status =
  | Unknown_function
  | Same_body
  | Changed_body

val body_status :
  t ->
  function_id:Fdo_counter.function_id ->
  function_body_hash:Fdo_counter.Function_body_hash.t ->
  body_status

(** Iterate the body index in unsigned hash order. *)
val iter_bodies :
  t ->
  f:
    (hash:Fdo_counter.Hash.t ->
    function_body_hash:Fdo_counter.Function_body_hash.t ->
    unit) ->
  unit

(** The hashes of the entry counters of the functions that calls from the call
    site [callsite] (the level of the call site, without inlining context)
    reached. *)
val call_targets : t -> Fdo_counter.position -> Fdo_counter.Hash.t list

(** Pre-order iteration over every trie node, in deterministic (unsigned hash)
    order: the position hash on the node's incoming edge, its depth (roots have
    depth 1), its count and how much of it is from paths ending at the node.
    Intended for dumping and debugging; doubles as a deep validation of the
    profile (raising {!Error} on malformed parts, since validation is lazy). *)
val iter :
  t ->
  f:
    (hash:Fdo_counter.Hash.t ->
    depth:int ->
    count:int64 ->
    ending:int64 ->
    unit) ->
  unit

(** Iteration over the call-target index, in deterministic (unsigned hash) order
    of call sites, then of callees: the hash of the call site level and of the
    callee's entry counter. Intended for dumping and debugging. *)
val iter_call_targets :
  t ->
  f:(callsite:Fdo_counter.Hash.t -> callee:Fdo_counter.Hash.t -> unit) ->
  unit

(** {2 Writing}

    Used by the profile producer and by tests. *)

module Writer : sig
  type profile := t

  type t

  val create : unit -> t

  (** [add_counter t ~counter ~count] adds [count] to every trie node along
      [counter]. *)
  val add_counter : t -> counter:Fdo_counter.t -> count:int64 -> unit

  (** {!add_counter} for a counter given as level hashes (as recorded in an
      executable's FDO metadata). *)
  val add_hashed_stack : t -> hashes:Fdo_counter.hashed -> count:int64 -> unit

  (** Record a compiled function's body hash. Conflicting body hashes for one
      function are rejected. *)
  val add_body :
    t ->
    hash:Fdo_counter.Hash.t ->
    function_body_hash:Fdo_counter.Function_body_hash.t ->
    unit

  (** Serialize the accumulated forest and derive its call-target index. The
      output is deterministic (children are ordered by hash). *)
  val write : t -> filename:string -> unit

  (** In-memory counterpart of {!write} followed by {!load}, for testing. *)
  val to_profile : t -> profile
end
