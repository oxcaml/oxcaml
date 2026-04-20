(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2015--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* The following is a "big endian" implementation. *)

type key = int

external int_clz : int -> (int[@untagged])
  = "caml_int_clz_tagged_to_tagged" "caml_int_clz_tagged_to_untagged"
[@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

(* A bit-packed pair of a bit [b] and prefix [p] matching the beginning
   (big-endian) of every key in a subtree, up to bit [b].

   The bit [b] is represented as a bitmask with only [b] set. This makes testing
   an individual bit very cheap.

   The prefix is represented as a sequence of bits matched by the beginning
   (big-endian) of every key in a subtree. It has some length, represented as
   the first [bit] after the entire prefix.

   This is represented as the logical "or" of the bit and the prefix. The
   representation is:
 *)
(*
 *           ____ bit [b] is the least significant bit
 *          /
 * <prefix>100..00
 * \______/
 *   arbitrary prefix leading to [b]
 *)
type prefix_and_bit = int

let[@inline always] unpack prefix_and_bit =
  (* This computes the least significant bit in [0]. *)
  let bit = prefix_and_bit land -prefix_and_bit in
  let prefix = prefix_and_bit lxor bit in
  prefix, bit

let[@inline always] pack prefix bit = prefix lor bit

let zero_bit i bit = i land bit = 0

(* Most significant 1 bit *)
let highest_bit x = 1 lsl (62 - int_clz x)

(* Highest bit at which [prefix0] and [prefix1] differ *)
let branching_bit prefix0 prefix1 = highest_bit (prefix0 lxor prefix1)

(* Keep only the bits strictly higher than [i] *)
let mask i bit = i land -(bit lsl 1)

(* Does [i] match [prefix], whose length is [bit]? In other words, does [i]
   match [prefix] at every position strictly higher than [bit]? *)
let match_prefix i prefix bit = mask i bit = prefix

let match_prefix_and_bit i prefix_and_bit =
  (* CR bclement: There might be better ways to compute this, such as [mask (i
     lxor prefix_and_bit) (prefix_and_bit land -prefix_and_bit) = 0] which would
     avoid a [xor], but it's not clear that the assembly is better due to this
     operating on tagged integers. *)
  let prefix, bit = unpack prefix_and_bit in
  match_prefix i prefix bit

let higher bit0 bit1 =
  (* We need to do _unsigned_ int comparison on bits.

     The only bit <= 0 (where signed and unsigned comparison would differ) is
     0x4000..., which becomes 0x3fff... when subtracting 1, and all other bits
     are > 0. *)
  bit0 - 1 > bit1 - 1

(* Is [prefix0], of length [bit0], a sub-prefix of [prefix1], of length
   [bit1]? *)
let includes_prefix prefix0 bit0 prefix1 bit1 =
  higher bit0 bit1 && match_prefix prefix1 prefix0 bit0

(* Provides a total ordering over [(prefix, bit)] pairs. Not otherwise
   specified. (Only useful for implementing [compare], which is similarly
   loosely specified.) *)
let compare_prefix prefix0 bit0 prefix1 bit1 =
  (* Signed comparison is fine here, so long as it's a total ordering *)
  let c = compare bit0 bit1 in
  if c = 0 then compare prefix0 prefix1 else c

type empty = [`Empty]

type leaf = [`Leaf]

type branch = [`Branch]

type non_empty =
  [ leaf
  | branch ]

(* A tree structure that will be used to implement a datatype, either sets or
   maps. Many algorithms operate identically on sets and maps, so they are
   implemented in the functor [Tree_operations] over this module type. *)
module type Tree = sig
  (* A Patricia tree. For a prefix P, we write that the tree has prefix P if
     every node in the tree has a key that matches P. (Note that a tree with
     prefix P also has any sub-prefix of P. For instance, if the tree has prefix
     011, it also has prefix 01.) *)
  type 'a t

  type (_, _) tree

  (* A witness that ['a] is a valid type for a value stored in the tree. Maps
     will allow ['a] to be any value but sets will only allow [unit]. *)
  type 'a is_value [@@immediate]

  (* Deduce that ['a] is a value type from a pre-existing ['a t]. *)
  val is_value_of : 'a t -> 'a is_value

  (* An empty tree. Since it has no nodes, it is safe to treat it as having any
     prefix. *)
  val empty : 'a is_value -> 'a t

  val of_tree : ('a, _) tree -> 'a t

  (* A tree containing a single key-value pair. It has the entire key as a
     prefix. *)
  val leaf : 'a is_value -> key -> 'a -> ('a, [< non_empty > `Leaf]) tree

  (* A tree with the given prefix, the length of the prefix, and two subtrees.
     If the prefix is P, we require that [t0] has prefix P0 and [t1] has prefix
     P1 (note that this is big-endian notation). For efficiency, [t0] and [t1]
     are assumed to be non-empty. *)
  val branch :
    prefix_and_bit ->
    ('a, non_empty) tree ->
    ('a, non_empty) tree ->
    ('a, [< non_empty > `Branch]) tree

  val leaf_key : ('a, leaf) tree -> key

  val leaf_datum : ('a, leaf) tree -> 'a

  val branch_prefix_and_bit : ('a, branch) tree -> prefix_and_bit

  val branch0 : ('a, branch) tree -> ('a, non_empty) tree

  val branch1 : ('a, branch) tree -> ('a, non_empty) tree

  type 'a tree_descr =
    | Leaf of ('a, leaf) tree
    | Branch of ('a, branch) tree

  val tree_descr : ('a, non_empty) tree -> 'a tree_descr

  (* A view on a given node, corresponding to which of [empty], [leaf], or
     [branch] constructed it. Passing the fields back in as arguments will
     construct an identical tree. *)
  type 'a descr =
    | Empty
    | Non_empty of ('a, non_empty) tree

  val descr : 'a t -> 'a descr

  module Binding : sig
    type 'a t

    val create : key -> 'a -> 'a t

    val key : _ t -> key

    val value : 'a is_value -> 'a t -> 'a
  end

  module Callback : sig
    type ('a, 'b) t

    val of_func : 'a is_value -> (key -> 'a -> 'b) -> ('a, 'b) t

    val call : ('a, 'b) t -> key -> 'a -> 'b
  end

  (* CR bclement: This module (and the modules below) are used as workarounds to
     compensate for the lack of function specialisation: instead of specialising
     at the function level, we inline at the module level. *)
  module Merge_callback : sig
    type ('a, 'b, 'c) t

    val call_union : ('a, 'b, 'c) t -> key -> 'a -> 'b -> 'c option

    val call_diff : ('a, 'b, 'c) t -> key -> 'a -> 'b -> 'c option
  end

  module Inter_callback : sig
    type ('a, 'b, 'c) t

    val call : ('a, 'b, 'c) t -> key -> 'a -> 'b -> 'c
  end

  module Compare_callback : sig
    type ('a, 'b) t

    val call : ('a, 'b) t -> 'a -> 'b -> int
  end

  module Equal_callback : sig
    type ('a, 'b) t

    val call : ('a, 'b) t -> 'a -> 'b -> bool
  end

  module Split_callback : sig
    type ('a, 'b) t

    val call : ('a, 'b) t -> 'a -> 'b
  end
end

module Set0 = struct
  type (_, _) tree =
    | Empty : (unit, empty) tree
    | Leaf : key -> (unit, [< non_empty > `Leaf]) tree
    | Branch :
        prefix_and_bit * (unit, non_empty) tree * (unit, non_empty) tree
        -> (unit, [< non_empty > `Branch]) tree

  let leaf_key (type a) (Leaf elt : (a, leaf) tree) = elt

  let leaf_datum (type a) (Leaf _ : (a, leaf) tree) : a = ()

  let branch_prefix_and_bit (type a)
      (Branch (prefix_and_bit, _, _) : (a, branch) tree) =
    prefix_and_bit

  let branch0 (type a) (Branch (_, t0, _) : (a, branch) tree) :
      (a, non_empty) tree =
    t0

  let branch1 (type a) (Branch (_, _, t1) : (a, branch) tree) :
      (a, non_empty) tree =
    t1

  type 'a tree_descr =
    | Leaf of ('a, leaf) tree
    | Branch of ('a, branch) tree

  let tree_descr (type a) (tree : (a, non_empty) tree) : a tree_descr =
    match tree with
    | Leaf _ as tree -> Leaf tree
    | Branch _ as tree -> Branch tree

  type 'a descr =
    | Empty
    | Non_empty of ('a, non_empty) tree

  type 'a t = Tree : ('a, _) tree -> 'a t [@@unboxed]

  let[@inline] of_tree t = Tree t

  let descr (type a) (Tree tree : a t) : a descr =
    match tree with
    | Empty -> Empty
    | Leaf _ as tree -> Non_empty tree
    | Branch _ as tree -> Non_empty tree

  type 'a is_value = Unit : unit is_value

  let[@inline always] is_value_of (type a) (Tree t : a t) : a is_value =
    (* Crucially, this compiles down to just [Unit], making this function cost
       nothing. *)
    match t with
    | Empty -> Unit
    | Leaf _ -> Unit
    | Branch _ -> Unit

  let[@inline always] empty (type a) (Unit : a is_value) : a t = Tree Empty

  let[@inline always] leaf (type a) (Unit : a is_value) elt (() : a) :
      (a, _) tree =
    Leaf elt

  let[@inline always] branch (type a) prefix_and_bit (t0 : (a, non_empty) tree)
      (t1 : (a, non_empty) tree) : (a, _) tree =
    let Unit = is_value_of (Tree t0) in
    Branch (prefix_and_bit, t0, t1)

  module Binding = struct
    type _ t = key

    let[@inline always] create i _ = i

    let[@inline always] key i = i

    let[@inline always] value (type a) (Unit : a is_value) (_i : a t) : a = ()
  end

  module Callback = struct
    type (_, 'b) t = key -> 'b

    let[@inline always] of_func (type a) (Unit : a is_value)
        (f : key -> a -> 'b) key =
      (f [@inlined hint]) key ()

    let[@inline always] call f key _ = f key
  end

  module Merge_callback = struct
    type (_, _, 'c) t = 'c is_value

    let[@inline always] call_union (type a) (Unit : a is_value) _ _ _ : a option
        =
      Some ()

    let[@inline always] call_diff (type a) (Unit : a is_value) _ _ _ : a option
        =
      None
  end

  module Inter_callback = struct
    type (_, _, 'c) t = 'c is_value

    let[@inline always] call (type a) (Unit : a is_value) _ _ _ : a = ()
  end

  module Compare_callback = struct
    type (_, _) t = unit

    let[@inline always] call _ _ _ = 0
  end

  module Equal_callback = struct
    type (_, _) t = unit

    let[@inline always] call _ _ _ = true
  end

  module Split_callback = struct
    type ('a, 'b) t = True : 'a is_value -> ('a, bool) t [@@unboxed]

    let[@inline always] call (type a b) (True Unit : (a, b) t) (() : a) : b =
      true
  end
end

module _ : Tree = Set0

module Map0 = struct
  type (+!_, _) tree =
    | Empty : ('a, empty) tree
    | Leaf : key * 'a -> ('a, [< non_empty > `Leaf]) tree
    | Branch :
        prefix_and_bit * ('a, non_empty) tree * ('a, non_empty) tree
        -> ('a, [< non_empty > `Branch]) tree

  type +!_ t = Tree : ('a, _) tree -> 'a t [@@unboxed]

  let[@inline always] of_tree t = Tree t

  type _ is_value = Any : 'a is_value

  let[@inline always] is_value_of _ = Any

  let[@inline always] empty Any = Tree Empty

  let[@inline always] leaf Any i d = Leaf (i, d)

  let[@inline always] branch prefix_and_bit t0 t1 =
    Branch (prefix_and_bit, t0, t1)

  let leaf_key (Leaf (key, _) : (_, leaf) tree) = key

  let leaf_datum (Leaf (_, datum) : (_, leaf) tree) = datum

  let branch_prefix_and_bit (type a)
      (Branch (prefix_and_bit, _, _) : (a, branch) tree) =
    prefix_and_bit

  let branch0 (type a) (Branch (_, t0, _) : (a, branch) tree) = t0

  let branch1 (type a) (Branch (_, _, t1) : (a, branch) tree) = t1

  type 'a tree_descr =
    | Leaf of ('a, leaf) tree
    | Branch of ('a, branch) tree

  let tree_descr (type a) (tree : (a, non_empty) tree) : a tree_descr =
    match tree with
    | Leaf _ as tree -> Leaf tree
    | Branch _ as tree -> Branch tree

  type 'a descr =
    | Empty
    | Non_empty of ('a, non_empty) tree

  let descr (type a) (Tree tree : a t) : a descr =
    match tree with
    | Empty -> Empty
    | Leaf _ as tree -> Non_empty tree
    | Branch _ as tree -> Non_empty tree

  module Binding = struct
    type 'a t = key * 'a

    let[@inline always] create i d = i, d

    let[@inline always] key (i, _d) = i

    let[@inline always] value Any (_i, d) = d
  end

  module Callback = struct
    type ('a, 'b) t = key -> 'a -> 'b

    let[@inline always] call f i d = f i d

    let[@inline always] of_func Any f = f
  end

  module Merge_callback = struct
    type ('a, 'b, 'c) t = key -> 'a -> 'b -> 'c option

    let[@inline always] call_union f key t t' = f key t t'

    let[@inline always] call_diff f key t t' = f key t t'
  end

  module Inter_callback = struct
    type ('a, 'b, 'c) t = key -> 'a -> 'b -> 'c

    let[@inline always] call f key t t' = f key t t'
  end

  module Compare_callback = struct
    type ('a, 'b) t = 'a -> 'b -> int

    let[@inline always] call f t t' = f t t'
  end

  module Equal_callback = struct
    type ('a, 'b) t = 'a -> 'b -> bool

    let[@inline always] call f t t' = f t t'
  end

  module Split_callback = struct
    type (_, _) t = Option : ('a, 'a option) t

    let[@inline always] call (type a b) (Option : (a, b) t) (a : a) : b = Some a
  end
end

module _ : Tree = Map0

module Tree_operations (Tree : Tree) : sig
  open! Tree

  val is_empty : 'a t -> bool

  val singleton : 'a is_value -> key -> 'a -> 'a t

  val mem : key -> 'a t -> bool

  val add : key -> 'a -> 'a t -> 'a t

  val replace : key -> ('a -> 'a) -> 'a t -> 'a t

  val update : key -> ('a option -> 'a option) -> 'a t -> 'a t

  val remove : key -> 'a t -> 'a t

  val union : ('a, 'a, 'a) Merge_callback.t -> 'a t -> 'a t -> 'a t

  val union_sharing : ('a, 'a, 'a) Merge_callback.t -> 'a t -> 'a t -> 'a t

  val union_shared : ('a, 'a, 'a) Merge_callback.t -> 'a t -> 'a t -> 'a t

  val union_total : (key -> 'a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t

  val union_total_shared : (key -> 'a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t

  val union_left_biased : 'a t -> 'a t -> 'a t

  val union_right_biased : 'a t -> 'a t -> 'a t

  val update_many :
    (key -> 'a option -> 'b -> 'a option) -> 'a t -> 'b t -> 'a t

  val subset : 'a t -> 'a t -> bool

  val find : key -> 'a t -> 'a

  val inter :
    'c is_value -> ('a, 'b, 'c) Inter_callback.t -> 'a t -> 'b t -> 'c t

  val inter_domain_is_non_empty : 'a t -> 'b t -> bool

  val diff_domains : 'a t -> 'b t -> 'a t

  val diff : ('a, 'b, 'a) Merge_callback.t -> 'a t -> 'b t -> 'a t

  val diff_sharing : ('a, 'b, 'a) Merge_callback.t -> 'a t -> 'b t -> 'a t

  val diff_shared : ('a, 'a, 'a) Merge_callback.t -> 'a t -> 'a t -> 'a t

  val cardinal : _ t -> int

  val iter : ('a, unit) Callback.t -> 'a t -> unit

  val fold : ('a, 'b -> 'b) Callback.t -> 'a t -> 'b -> 'b

  val for_all : ('a, bool) Callback.t -> 'a t -> bool

  val exists : ('a, bool) Callback.t -> 'a t -> bool

  val filter : ('a, bool) Callback.t -> 'a t -> 'a t

  val partition : ('a, bool) Callback.t -> 'a t -> 'a t * 'a t

  val choose : 'a t -> 'a Binding.t

  val choose_opt : 'a t -> 'a Binding.t option

  val min_binding : 'a t -> 'a Binding.t

  val min_binding_opt : 'a t -> 'a Binding.t option

  val max_binding : 'a t -> 'a Binding.t

  val max_binding_opt : 'a t -> 'a Binding.t option

  val equal : ('a, 'a) Equal_callback.t -> 'a t -> 'a t -> bool

  val compare : ('a, 'a) Compare_callback.t -> 'a t -> 'a t -> int

  val split :
    found:('a, 'b) Split_callback.t ->
    not_found:'b ->
    key ->
    'a t ->
    'a t * 'b * 'a t

  val to_list : 'a t -> 'a Binding.t list

  val merge :
    'c is_value ->
    (key -> 'a option -> 'b option -> 'c option) ->
    'a t ->
    'b t ->
    'c t

  val find_opt : key -> 'a t -> 'a option

  val get_singleton : 'a t -> 'a Binding.t option

  val map : 'b is_value -> ('a -> 'b) -> 'a t -> 'b t

  val map_sharing : ('a -> 'a) -> 'a t -> 'a t

  val mapi : 'b is_value -> ('a, 'b) Callback.t -> 'a t -> 'b t

  val filter_map : 'b is_value -> (key -> 'a -> 'b option) -> 'a t -> 'b t

  val filter_map_sharing : (key -> 'a -> 'a option) -> 'a t -> 'a t

  type 'a iterator

  val iterator : 'a t -> 'a iterator

  val current : 'a iterator -> 'a Binding.t option

  val advance : 'a iterator -> 'a iterator

  val seek : 'a iterator -> key -> 'a iterator

  val to_seq : 'a t -> 'a Binding.t Seq.t

  val of_list : 'a is_value -> 'a Binding.t list -> 'a t

  val map_keys : (key -> key) -> 'a t -> 'a t

  val valid : 'a t -> bool
end = struct
  include Tree

  let is_value_of_tree t = is_value_of (of_tree t) [@@inline always]

  let leaf_descr leaf = leaf_key leaf, leaf_datum leaf [@@inline always]

  let leaf_binding leaf = Binding.create (leaf_key leaf) (leaf_datum leaf)
  [@@inline always]

  let branch_descr branch =
    branch_prefix_and_bit branch, branch0 branch, branch1 branch
  [@@inline always]

  (* A relaxed version of [Tree.branch], allowing [t0] and/or [t1] to be empty.
     It still requires that [t0] and [t1] have prefix [P0] and [P1],
     respectively, where [P] is the bits in [prefix] lower than [bit]. *)
  let branch prefix_and_bit t0 t1 =
    match (descr [@inlined hint]) t0, (descr [@inlined hint]) t1 with
    | Empty, _ -> t1
    | _, Empty -> t0
    | Non_empty t0, Non_empty t1 -> of_tree (Tree.branch prefix_and_bit t0 t1)
  [@@inline always]

  let branch_right_nonempty prefix_and_bit t0 t1 =
    match (descr [@inlined hint]) t0 with
    | Empty -> t1
    | Non_empty t0 -> Tree.branch prefix_and_bit t0 t1
  [@@inline always]

  let branch_left_nonempty prefix_and_bit t0 t1 =
    match (descr [@inlined hint]) t1 with
    | Empty -> t0
    | Non_empty t1 -> Tree.branch prefix_and_bit t0 t1
  [@@inline always]

  let branch_non_empty prefix_and_bit t0 t1 =
    (Tree.branch [@inlined hint]) prefix_and_bit t0 t1
  [@@inline always]

  let is_empty t = match descr t with Empty -> true | Non_empty _ -> false
  [@@inline always]

  let singleton iv i d = of_tree (leaf iv i d) [@@inline always]

  let rec mem_tree i t =
    match tree_descr t with
    | Leaf l -> leaf_key l = i
    | Branch b ->
      let prefix, bit = unpack (branch_prefix_and_bit b) in
      if not (match_prefix i prefix bit)
      then false
      else if zero_bit i bit
      then mem_tree i (branch0 b)
      else mem_tree i (branch1 b)

  let mem i t =
    match descr t with Empty -> false | Non_empty tree -> mem_tree i tree

  (* Join two subtrees whose prefixes are disjoint (neither includes the other)
     but otherwise arbitrary. Assumes that [t0] has prefix [prefix0] and [t1]
     has prefix [prefix1]. (Most functions that take a prefix also take a bit
     representing the length of the prefix, but in this case, the lengths don't
     matter: the prefixes must differ at some shorter length anyway.) *)
  let join_non_empty prefix0 t0 prefix1 t1 =
    let bit = branching_bit prefix0 prefix1 in
    let prefix_and_bit = pack (mask prefix0 bit) bit in
    let t0, t1 = if zero_bit prefix0 bit then t0, t1 else t1, t0 in
    branch_non_empty prefix_and_bit t0 t1
  [@@inline always]

  let join prefix0 t0 prefix1 t1 =
    match descr t0, descr t1 with
    | _, Empty -> t0
    | Empty, _ -> t1
    | Non_empty t0, Non_empty t1 ->
      of_tree (join_non_empty prefix0 t0 prefix1 t1)
  [@@inline always]

  (* CR mshinwell: This is now [add_or_replace], like [Map] *)
  let rec add_tree i d t : (_, non_empty) tree =
    let iv = is_value_of_tree t in
    let[@local] join prefix = join_non_empty i (leaf iv i d) prefix t in
    match tree_descr t with
    | Leaf l ->
      let j = leaf_key l in
      if i = j then leaf iv i d else join j
    | Branch b ->
      let prefix_and_bit = branch_prefix_and_bit b in
      let prefix, bit = unpack prefix_and_bit in
      if match_prefix i prefix bit
      then
        let t0 = branch0 b in
        let t1 = branch1 b in
        if zero_bit i bit
        then branch_non_empty prefix_and_bit (add_tree i d t0) t1
        else branch_non_empty prefix_and_bit t0 (add_tree i d t1)
      else join prefix

  let add i d t : _ t =
    let iv = is_value_of t in
    match descr t with
    | Empty -> of_tree (leaf iv i d)
    | Non_empty t -> of_tree (add_tree i d t)

  let rec replace_tree key f t =
    let iv = is_value_of_tree t in
    match tree_descr t with
    | Leaf l ->
      let key' = leaf_key l in
      if key = key'
      then
        let datum = f (leaf_datum l) in
        leaf iv key datum
      else t
    | Branch b ->
      let prefix_and_bit = branch_prefix_and_bit b in
      let prefix, bit = unpack prefix_and_bit in
      if match_prefix key prefix bit
      then
        let t0 = branch0 b in
        let t1 = branch1 b in
        if zero_bit key bit
        then branch_non_empty prefix_and_bit (replace_tree key f t0) t1
        else branch_non_empty prefix_and_bit t0 (replace_tree key f t1)
      else t

  let replace key f t =
    let iv = is_value_of t in
    match descr t with
    | Empty -> empty iv
    | Non_empty t -> of_tree (replace_tree key f t)

  let rec update_tree key f t =
    let iv = is_value_of_tree t in
    let[@local] join prefix =
      match f None with
      | None -> of_tree t
      | Some datum -> of_tree (join_non_empty key (leaf iv key datum) prefix t)
    in
    match tree_descr t with
    | Leaf l ->
      let key' = leaf_key l in
      if key = key'
      then
        match f (Some (leaf_datum l)) with
        | None -> empty iv
        | Some datum -> of_tree (leaf iv key datum)
      else join key'
    | Branch b ->
      let prefix_and_bit = branch_prefix_and_bit b in
      let prefix, bit = unpack prefix_and_bit in
      if match_prefix key prefix bit
      then
        let t0 = branch0 b in
        let t1 = branch1 b in
        if zero_bit key bit
        then
          of_tree
            (branch_right_nonempty prefix_and_bit (update_tree key f t0) t1)
        else
          of_tree
            (branch_left_nonempty prefix_and_bit t0 (update_tree key f t1))
      else join prefix

  let update key f t =
    let iv = is_value_of t in
    match descr t with
    | Empty -> (
      match f None with
      | None -> empty iv
      | Some datum -> of_tree (leaf iv key datum))
    | Non_empty t -> update_tree key f t

  let rec remove_tree i t =
    let iv = is_value_of_tree t in
    match tree_descr t with
    | Leaf l -> if i = leaf_key l then empty iv else of_tree t
    | Branch b ->
      let prefix_and_bit = branch_prefix_and_bit b in
      let prefix, bit = unpack prefix_and_bit in
      if match_prefix i prefix bit
      then
        let t0, t1 = branch0 b, branch1 b in
        if zero_bit i bit
        then
          of_tree (branch_right_nonempty prefix_and_bit (remove_tree i t0) t1)
        else of_tree (branch_left_nonempty prefix_and_bit t0 (remove_tree i t1))
      else of_tree t

  let remove i t =
    let iv = is_value_of t in
    match descr t with
    | Empty -> empty iv
    | Non_empty tree -> remove_tree i tree

  (* [pattern_match_pair t0 t1 ~join ~leaf ~branch] deconstructs two trees
     simultaneously.

     - [join descr0 descr1] is called when both trees are disjoint. This might
     be because they are non-empty trees with incompatible prefixes, or because
     one (or both) is empty.

     - [leaf i d0 d1] is called when both trees are are singletons with the same
     key [i] and data [d0] and [d1], respectively.

     - [branch ?t00 ?t01 ?t10 ?t11 prefix bit] is called when at least one tree
     is a [Branch] and the domains of the trees intersect. The [prefix] and
     [bit] correspond to the highest position where at least one of the trees
     has a branch: it is guaranteed that [t0 = branch prefix bit t00 t01] and
     [t1 = branch prefix bit t10 t11], where the missing optional arguments are
     treated as empty.

     {b Note}: The [join], [leaf], and [branch] functions are aggressively
     inlined. *)
  (* CR-someday bclement (and lmaurer): We could turn this into a functor over
     three [Tree] instances and get arbitrary combinations of taking and
     returning sets and maps. *)
  let[@inline always] pattern_match_pair t0 t1 ~join ~leaf
      ~(branch : ?t00:_ -> ?t01:_ -> ?t10:_ -> ?t11:_ -> prefix_and_bit -> _) =
    match tree_descr t0, tree_descr t1 with
    (* Leaf/Leaf cases *)
    | Leaf l0, Leaf l1 ->
      let i0, i1 = leaf_key l0, leaf_key l1 in
      if i0 = i1
      then (leaf [@inlined hint]) i0 (leaf_datum l0) (leaf_datum l1)
      else (join [@inlined hint]) i0 i1
    (* Leaf/Branch cases *)
    | Leaf l0, Branch b1 ->
      let i = leaf_key l0 in
      let prefix_and_bit = branch_prefix_and_bit b1 in
      let prefix, bit = unpack prefix_and_bit in
      if match_prefix i prefix bit
      then
        let t10, t11 = branch0 b1, branch1 b1 in
        if zero_bit i bit
        then (branch [@inlined hint]) prefix_and_bit ~t00:t0 ~t10 ~t11
        else (branch [@inlined hint]) prefix_and_bit ~t01:t0 ~t10 ~t11
      else (join [@inlined hint]) i prefix
    | Branch b0, Leaf l1 ->
      let i = leaf_key l1 in
      let prefix_and_bit = branch_prefix_and_bit b0 in
      let prefix, bit = unpack prefix_and_bit in
      if match_prefix i prefix bit
      then
        let t00, t01 = branch0 b0, branch1 b0 in
        if zero_bit i bit
        then (branch [@inlined hint]) prefix_and_bit ~t00 ~t01 ~t10:t1
        else (branch [@inlined hint]) prefix_and_bit ~t00 ~t01 ~t11:t1
      else (join [@inlined hint]) prefix i
    (* Branch/Branch case *)
    | Branch b0, Branch b1 ->
      let prefix_and_bit0, t00, t01 = branch_descr b0 in
      let prefix_and_bit1, t10, t11 = branch_descr b1 in
      if prefix_and_bit0 = prefix_and_bit1
      then (branch [@inlined hint]) prefix_and_bit0 ~t00 ~t01 ~t10 ~t11
      else
        let prefix0, bit0 = unpack prefix_and_bit0 in
        let prefix1, bit1 = unpack prefix_and_bit1 in
        if includes_prefix prefix0 bit0 prefix1 bit1
        then
          if zero_bit prefix1 bit0
          then (branch [@inlined hint]) prefix_and_bit0 ~t00 ~t01 ~t10:t1
          else (branch [@inlined hint]) prefix_and_bit0 ~t00 ~t01 ~t11:t1
        else if includes_prefix prefix1 bit1 prefix0 bit0
        then
          if zero_bit prefix0 bit1
          then (branch [@inlined hint]) prefix_and_bit1 ~t00:t0 ~t10 ~t11
          else (branch [@inlined hint]) prefix_and_bit1 ~t01:t0 ~t10 ~t11
        else (join [@inlined hint]) prefix0 prefix1

  (* The following [phys_eq_XXX] helpers are used by [pattern_match_pair_merge]
     below to exploit physical equality.

     They have a slightly complicated interface in order for
     [pattern_match_pair_merge] to have signature ['a t -> 'b t -> 'c t]. *)

  let no_phys_eq_shortcut _iv _t0 _t1 = None

  let phys_eq_shortcut_union _iv t0 t1 =
    if t0 == t1 then Some (of_tree t0) else None

  let phys_eq_shortcut_diff iv t0 t1 =
    if t0 == t1 then Some (empty iv) else None

  let no_phys_eq_check_branch ~orig_t:_ ~orig_t0:_ ~orig_t1:_ _t0 _t1 = None

  let phys_eq_check_branch ~orig_t ~orig_t0 ~orig_t1 t0 t1 =
    if t0 == of_tree orig_t0 && t1 == of_tree orig_t1 then Some orig_t else None

  let no_phys_eq_check_leaf ~orig_t:_ ~orig_d:_ _d = None

  let phys_eq_check_leaf ~orig_t ~orig_d d =
    if d == orig_d then Some orig_t else None

  (* Perform a pattern-matching on two trees simultaneously, and reconstructs a
     new tree with similar shape (as in the [merge] function).

     [phys_eq_shortcut] is called before performing the actual pattern-matching
     and can be used to implement fast paths based on physical equality (e.g.
     for [union] or [diff] operations).

     [phys_eq_check_branch_left], [phys_eq_check_branch_right],
     [phys_eq_check_leaf_left] and [phys_eq_check_leaf_right] are called when
     reconstructing a new tree and can be used to enforce sharing. The [_left]
     variants take precedence over the [_right] variants.

     [only_left] (resp. [only_right]) is called on sub-trees that of [t0] (resp.
     [t1]) whose intersection with [t1] (resp. [t0]) is empty. Note that the
     sub-tree itself might be empty. It must return a tree whose keys are a
     subset of its argument's keys.

     [both_sides] is called on pairs of sub-tree of [t0] and [t1] that may have
     a non-empty intersection (and will typically call into the same
     [pattern_match_pair_merge] recursively). It must return a tree whose keys
     are present in at least one of its arguments.

     {b Note}: All functions that are named arguments are aggressively inlined,
     as they are expected to be inline anonymous functions, but the [combine]
     argument is not, as it is expected to be passed directly from the user in
     most cases. *)
  let[@inline always] pattern_match_pair_merge
      ?(phys_eq_shortcut = no_phys_eq_shortcut)
      ?(phys_eq_check_branch_left = no_phys_eq_check_branch)
      ?(phys_eq_check_branch_right = no_phys_eq_check_branch)
      ?(phys_eq_check_leaf_left = no_phys_eq_check_leaf)
      ?(phys_eq_check_leaf_right = no_phys_eq_check_leaf) ~only_left ~only_right
      ~both_sides iv combine t0 t1 =
    match (phys_eq_shortcut [@inlined hint]) iv t0 t1 with
    | Some t' -> t'
    | None ->
      pattern_match_pair t0 t1
        ~leaf:(fun i d0 d1 ->
          (* NB: [combine] does not have an [@inlined hint] annotation because
             it is expected that this is the merge function passed from the user
             (e.g. argument of [merge] or [union]). *)
          match combine i d0 d1 with
          | None -> empty iv
          | Some d -> (
            match
              (phys_eq_check_leaf_left [@inlined hint]) ~orig_t:t0 ~orig_d:d0 d
            with
            | Some t' -> of_tree t'
            | None -> (
              match
                (phys_eq_check_leaf_right [@inlined hint]) ~orig_t:t1 ~orig_d:d1
                  d
              with
              | Some t' -> of_tree t'
              | None -> of_tree (leaf iv i d))))
        ~join:(fun prefix0 prefix1 ->
          join prefix0
            ((only_left [@inlined hint]) t0)
            prefix1
            ((only_right [@inlined hint]) t1))
        ~branch:(fun ?t00 ?t01 ?t10 ?t11 prefix_and_bit ->
          (* We expect all of the constructors for the arguments to be
             statically known here, so the matches below should get simplified
             to a single path depending on context. *)
          let both_sides' t0 t1 =
            match t0, t1 with
            | None, None -> empty iv
            | Some t0, None -> (only_left [@inlined hint]) t0
            | None, Some t1 -> (only_right [@inlined hint]) t1
            | Some t0, Some t1 -> (both_sides [@inlined hint]) t0 t1
              [@@inline always]
          in
          let t0' = both_sides' t00 t10 in
          let t1' = both_sides' t01 t11 in
          let[@local] branch () = branch prefix_and_bit t0' t1' in
          let[@local] branch1 () =
            match t10, t11 with
            | None, _ | _, None -> branch ()
            | Some t10, Some t11 -> (
              match
                (phys_eq_check_branch_right [@inlined hint]) ~orig_t:t1
                  ~orig_t0:t10 ~orig_t1:t11 t0' t1'
              with
              | Some t' -> of_tree t'
              | None -> branch ())
          in
          let[@local] branch0 () =
            match t00, t01 with
            | None, _ | _, None -> branch1 ()
            | Some t00, Some t01 -> (
              match
                (phys_eq_check_branch_left [@inlined hint]) ~orig_t:t0
                  ~orig_t0:t00 ~orig_t1:t01 t0' t1'
              with
              | Some t' -> of_tree t'
              | None -> branch1 ())
          in
          branch0 ())

  let toplevel_union nonempty_union t0 t1 =
    match descr t0 with
    | Empty -> t1
    | Non_empty t0' -> (
      match descr t1 with
      | Empty -> t0
      | Non_empty t1' -> nonempty_union t0' t1')
  [@@inline always]

  let rec union_tree f t0 t1 =
    let iv = is_value_of_tree t0 in
    pattern_match_pair_merge
      ~only_left:(fun t0 -> of_tree t0)
      ~only_right:(fun t1 -> of_tree t1)
      ~both_sides:(fun t0 t1 -> union_tree f t0 t1)
      iv
      (fun[@inline] k t t' -> Merge_callback.call_union f k t t')
      t0 t1

  let union f t0 t1 = toplevel_union (union_tree f) t0 t1

  (* [_sharing] functions are guaranteed to share with their first argument
     only.

     Some [_sharing] functions also share with their second argument as an
     optimisation, when possible. *)
  let pattern_match_pair_merge_sharing =
    pattern_match_pair_merge ~phys_eq_check_branch_left:phys_eq_check_branch
      ~phys_eq_check_leaf_left:phys_eq_check_leaf

  let rec union_sharing_tree f t0 t1 =
    let iv = is_value_of_tree t0 in
    pattern_match_pair_merge_sharing
      ~phys_eq_check_branch_right:phys_eq_check_branch
      ~phys_eq_check_leaf_right:phys_eq_check_leaf
      ~only_left:(fun t0 -> of_tree t0)
      ~only_right:(fun t1 -> of_tree t1)
      ~both_sides:(fun t0 t1 -> union_sharing_tree f t0 t1)
      iv
      (fun[@inline] k t t' -> Merge_callback.call_union f k t t')
      t0 t1

  let union_sharing f t0 t1 = toplevel_union (union_sharing_tree f) t0 t1

  let rec union_shared_tree f t0 t1 =
    let iv = is_value_of_tree t0 in
    pattern_match_pair_merge_sharing ~phys_eq_shortcut:phys_eq_shortcut_union
      ~phys_eq_check_branch_right:phys_eq_check_branch
      ~phys_eq_check_leaf_right:phys_eq_check_leaf
      ~only_left:(fun t0 -> of_tree t0)
      ~only_right:(fun t1 -> of_tree t1)
      ~both_sides:(fun t0 t1 -> union_shared_tree f t0 t1)
      iv
      (fun[@inline] k t t' -> Merge_callback.call_union f k t t')
      t0 t1

  let union_shared f t0 t1 = toplevel_union (union_shared_tree f) t0 t1

  let rec union_total_tree f t0 t1 =
    let iv = is_value_of_tree t0 in
    pattern_match_pair_merge
      ~only_left:(fun t0 -> of_tree t0)
      ~only_right:(fun t1 -> of_tree t1)
      ~both_sides:(fun t0 t1 -> union_total_tree f t0 t1)
      iv
      (fun[@inline] k t t' -> Some (f k t t'))
      t0 t1

  let union_total f t0 t1 = toplevel_union (union_total_tree f) t0 t1

  let rec union_total_shared_tree f t0 t1 =
    let iv = is_value_of_tree t0 in
    pattern_match_pair_merge_sharing ~phys_eq_shortcut:phys_eq_shortcut_union
      ~phys_eq_check_branch_right:phys_eq_check_branch
      ~phys_eq_check_leaf_right:phys_eq_check_leaf
      ~only_left:(fun t0 -> of_tree t0)
      ~only_right:(fun t1 -> of_tree t1)
      ~both_sides:(fun t0 t1 -> union_total_shared_tree f t0 t1)
      iv
      (fun[@inline] k t t' -> Some (f k t t'))
      t0 t1

  let union_total_shared f t0 t1 =
    toplevel_union (union_total_shared_tree f) t0 t1

  let rec union_left_biased_tree t0 t1 =
    let iv = is_value_of_tree t0 in
    pattern_match_pair_merge_sharing ~phys_eq_shortcut:phys_eq_shortcut_union
      ~phys_eq_check_branch_right:phys_eq_check_branch
      ~phys_eq_check_leaf_right:phys_eq_check_leaf
      ~only_left:(fun t0 -> of_tree t0)
      ~only_right:(fun t1 -> of_tree t1)
      ~both_sides:(fun t0 t1 -> union_left_biased_tree t0 t1)
      iv
      (fun[@inline] _k t _t' -> Some t)
      t0 t1

  let union_left_biased t0 t1 = toplevel_union union_left_biased_tree t0 t1

  let rec union_right_biased_tree t0 t1 =
    let iv = is_value_of_tree t0 in
    pattern_match_pair_merge_sharing ~phys_eq_shortcut:phys_eq_shortcut_union
      ~phys_eq_check_branch_right:phys_eq_check_branch
      ~phys_eq_check_leaf_right:phys_eq_check_leaf
      ~only_left:(fun t0 -> of_tree t0)
      ~only_right:(fun t1 -> of_tree t1)
      ~both_sides:(fun t0 t1 -> union_right_biased_tree t0 t1)
      iv
      (fun[@inline] _k _t t' -> Some t')
      t0 t1

  let union_right_biased t0 t1 = toplevel_union union_right_biased_tree t0 t1

  let rec diff_tree f t0 t1 =
    let iv = is_value_of_tree t0 in
    pattern_match_pair_merge
      ~only_left:(fun t0 -> of_tree t0)
      ~only_right:(fun _ -> empty iv)
      ~both_sides:(fun t0 t1 -> diff_tree f t0 t1)
      iv
      (fun[@inline] k t t' -> Merge_callback.call_diff f k t t')
      t0 t1

  let toplevel_diff nonempty_diff t0 t1 =
    match descr t0 with
    | Empty -> empty (is_value_of t0)
    | Non_empty t0' -> (
      match descr t1 with Empty -> t0 | Non_empty t1' -> nonempty_diff t0' t1')
  [@@inline always]

  let diff f t0 t1 = toplevel_diff (diff_tree f) t0 t1

  let rec diff_sharing_tree f t0 t1 =
    let iv = is_value_of_tree t0 in
    pattern_match_pair_merge_sharing
      ~only_left:(fun t0 -> of_tree t0)
      ~only_right:(fun _ -> empty iv)
      ~both_sides:(fun t0 t1 -> diff_sharing_tree f t0 t1)
      iv
      (fun[@inline] k t t' -> Merge_callback.call_diff f k t t')
      t0 t1

  let diff_sharing f t0 t1 = toplevel_diff (diff_sharing_tree f) t0 t1

  let rec diff_shared_tree f t0 t1 =
    let iv = is_value_of_tree t0 in
    pattern_match_pair_merge_sharing ~phys_eq_shortcut:phys_eq_shortcut_diff
      ~only_left:(fun t0 -> of_tree t0)
      ~only_right:(fun _ -> empty iv)
      ~both_sides:(fun t0 t1 -> diff_shared_tree f t0 t1)
      iv
      (fun[@inline] k t t' -> Merge_callback.call_diff f k t t')
      t0 t1

  let diff_shared f t0 t1 = toplevel_diff (diff_shared_tree f) t0 t1

  (* CR mshinwell: rename to subset_domain and inter_domain? *)

  let rec subset_tree t0 t1 =
    match tree_descr t0, tree_descr t1 with
    | Branch _, Leaf _ -> false
    | Leaf l, _ -> mem_tree (leaf_key l) t1
    | Branch b0, Branch b1 ->
      let prefix_and_bit0, t00, t01 = branch_descr b0 in
      let prefix_and_bit1, t10, t11 = branch_descr b1 in
      if prefix_and_bit0 = prefix_and_bit1
      then subset_tree t00 t10 && subset_tree t01 t11
      else
        let prefix0, bit0 = unpack prefix_and_bit0 in
        let prefix1, bit1 = unpack prefix_and_bit1 in
        if includes_prefix prefix1 bit1 prefix0 bit0
        then
          if zero_bit prefix0 bit1
          then subset_tree t0 t10
          else subset_tree t0 t11
        else false

  let subset t0 t1 =
    match descr t0, descr t1 with
    | Empty, _ -> true
    | Non_empty _, Empty -> false
    | Non_empty t0, Non_empty t1 -> subset_tree t0 t1

  (* CR lmaurer: Should use [raise_notrace] internally *)
  let rec find_tree i t =
    match tree_descr t with
    | Leaf l -> if leaf_key l = i then leaf_datum l else raise Not_found
    | Branch b ->
      let prefix, bit = unpack (branch_prefix_and_bit b) in
      if not (match_prefix i prefix bit)
      then raise Not_found
      else if zero_bit i bit
      then find_tree i (branch0 b)
      else find_tree i (branch1 b)

  let find i t =
    match descr t with
    | Empty -> raise Not_found
    | Non_empty tree -> find_tree i tree

  let rec inter_tree iv f t0 t1 =
    match tree_descr t0, tree_descr t1 with
    | Leaf l0, _ -> (
      let i = leaf_key l0 in
      match find_tree i t1 with
      | exception Not_found -> empty iv
      | d1 ->
        let d0 = leaf_datum l0 in
        of_tree (leaf iv i (Inter_callback.call f i d0 d1)))
    | _, Leaf l1 -> (
      let i = leaf_key l1 in
      match find_tree i t0 with
      | exception Not_found -> empty iv
      | d0 ->
        let d1 = leaf_datum l1 in
        of_tree (leaf iv i (Inter_callback.call f i d0 d1)))
    | Branch b0, Branch b1 ->
      let prefix_and_bit0, t00, t01 = branch_descr b0 in
      let prefix_and_bit1, t10, t11 = branch_descr b1 in
      if prefix_and_bit0 = prefix_and_bit1
      then
        branch prefix_and_bit0 (inter_tree iv f t00 t10)
          (inter_tree iv f t01 t11)
      else
        let prefix0, bit0 = unpack prefix_and_bit0 in
        let prefix1, bit1 = unpack prefix_and_bit1 in
        if includes_prefix prefix0 bit0 prefix1 bit1
        then
          if zero_bit prefix1 bit0
          then inter_tree iv f t00 t1
          else inter_tree iv f t01 t1
        else if includes_prefix prefix1 bit1 prefix0 bit0
        then
          if zero_bit prefix0 bit1
          then inter_tree iv f t0 t10
          else inter_tree iv f t0 t11
        else empty iv

  let inter iv f t0 t1 =
    match descr t0, descr t1 with
    | Empty, _ -> empty iv
    | _, Empty -> empty iv
    | Non_empty t0, Non_empty t1 -> inter_tree iv f t0 t1

  let rec inter_domain_is_non_empty_tree t0 t1 =
    match tree_descr t0, tree_descr t1 with
    | Leaf l, _ -> mem_tree (leaf_key l) t1
    | _, Leaf l -> mem_tree (leaf_key l) t0
    | Branch b0, Branch b1 ->
      let prefix_and_bit0, t00, t01 = branch_descr b0 in
      let prefix_and_bit1, t10, t11 = branch_descr b1 in
      if prefix_and_bit0 = prefix_and_bit1
      then
        inter_domain_is_non_empty_tree t00 t10
        || inter_domain_is_non_empty_tree t01 t11
      else
        let prefix0, bit0 = unpack prefix_and_bit0 in
        let prefix1, bit1 = unpack prefix_and_bit1 in
        if includes_prefix prefix0 bit0 prefix1 bit1
        then
          if zero_bit prefix1 bit0
          then inter_domain_is_non_empty_tree t00 t1
          else inter_domain_is_non_empty_tree t01 t1
        else if includes_prefix prefix1 bit1 prefix0 bit0
        then
          if zero_bit prefix0 bit1
          then inter_domain_is_non_empty_tree t0 t10
          else inter_domain_is_non_empty_tree t0 t11
        else false

  let inter_domain_is_non_empty t0 t1 =
    match descr t0, descr t1 with
    | Empty, _ | _, Empty -> false
    | Non_empty t0, Non_empty t1 -> inter_domain_is_non_empty_tree t0 t1

  (* CR bclement: this could probably be removed now that we have a more generic
     [diff]. *)
  let rec diff_domains_tree t0 t1 =
    let iv = is_value_of_tree t0 in
    match tree_descr t0, tree_descr t1 with
    | Leaf l, _ -> if mem_tree (leaf_key l) t1 then empty iv else of_tree t0
    | _, Leaf l -> remove_tree (leaf_key l) t0
    | Branch b0, Branch b1 ->
      let prefix_and_bit0, t00, t01 = branch_descr b0 in
      let prefix_and_bit1, t10, t11 = branch_descr b1 in
      if prefix_and_bit0 = prefix_and_bit1
      then
        branch prefix_and_bit0
          (diff_domains_tree t00 t10)
          (diff_domains_tree t01 t11)
      else
        let prefix0, bit0 = unpack prefix_and_bit0 in
        let prefix1, bit1 = unpack prefix_and_bit1 in
        if includes_prefix prefix0 bit0 prefix1 bit1
        then
          if zero_bit prefix1 bit0
          then branch prefix_and_bit0 (diff_domains_tree t00 t1) (of_tree t01)
          else branch prefix_and_bit0 (of_tree t00) (diff_domains_tree t01 t1)
        else if includes_prefix prefix1 bit1 prefix0 bit0
        then
          if zero_bit prefix0 bit1
          then diff_domains_tree t0 t10
          else diff_domains_tree t0 t11
        else of_tree t0

  let diff_domains t0 t1 =
    match descr t0, descr t1 with
    | Empty, _ -> empty (is_value_of t0)
    | _, Empty -> t0
    | Non_empty tree0, Non_empty tree1 -> diff_domains_tree tree0 tree1

  let rec cardinal_tree tree =
    match tree_descr tree with
    | Leaf _ -> 1
    | Branch b -> cardinal_tree (branch0 b) + cardinal_tree (branch1 b)

  let cardinal t =
    match descr t with Empty -> 0 | Non_empty tree -> cardinal_tree tree

  let[@inline always] order_branches prefix_and_bit t0 t1 =
    (* [t0] is ordered first, unless the bit encoded in [prefix_and_bit] is
       negative (i.e. it is the most significant bit), in which case [t1] must
       be ordered first.

       The most significant bit must have an empty prefix, so shifting it to the
       left is always zero.

       All other bits are non-zero, so all other [prefix_and_bit] have a
       non-zero bit that is not the most significant. *)
    if prefix_and_bit lsl 1 = 0 then t1, t0 else t0, t1

  let[@inline always] order_branches' branch =
    order_branches
      (branch_prefix_and_bit branch)
      (branch0 branch) (branch1 branch)

  let rec unsigned_iter f t =
    match tree_descr t with
    | Leaf l -> Callback.call f (leaf_key l) (leaf_datum l)
    | Branch b ->
      unsigned_iter f (branch0 b);
      unsigned_iter f (branch1 b)

  let iter f t =
    match descr t with
    | Empty -> ()
    | Non_empty tree -> (
      match tree_descr tree with
      | Leaf l -> Callback.call f (leaf_key l) (leaf_datum l)
      | Branch b ->
        let t0, t1 = order_branches' b in
        unsigned_iter f t0;
        unsigned_iter f t1)

  let rec unsigned_fold f t acc =
    match tree_descr t with
    | Leaf l -> Callback.call f (leaf_key l) (leaf_datum l) acc
    | Branch b -> unsigned_fold f (branch1 b) (unsigned_fold f (branch0 b) acc)

  let fold f t acc =
    match descr t with
    | Empty -> acc
    | Non_empty tree -> (
      match tree_descr tree with
      | Leaf l -> Callback.call f (leaf_key l) (leaf_datum l) acc
      | Branch b ->
        let t0, t1 = order_branches' b in
        unsigned_fold f t1 (unsigned_fold f t0 acc))

  let rec unsigned_for_all p t =
    match tree_descr t with
    | Leaf l -> Callback.call p (leaf_key l) (leaf_datum l)
    | Branch b ->
      unsigned_for_all p (branch0 b) && unsigned_for_all p (branch1 b)

  let for_all p t =
    match descr t with
    | Empty -> true
    | Non_empty tree -> (
      match tree_descr tree with
      | Leaf l -> Callback.call p (leaf_key l) (leaf_datum l)
      | Branch b ->
        let t0, t1 = order_branches' b in
        unsigned_for_all p t0 && unsigned_for_all p t1)

  let rec unsigned_exists p t =
    match tree_descr t with
    | Leaf leaf -> Callback.call p (leaf_key leaf) (leaf_datum leaf)
    | Branch b -> unsigned_exists p (branch0 b) || unsigned_exists p (branch1 b)

  let exists p t =
    match descr t with
    | Empty -> false
    | Non_empty tree -> (
      match tree_descr tree with
      | Leaf leaf -> Callback.call p (leaf_key leaf) (leaf_datum leaf)
      | Branch b ->
        let t0, t1 = order_branches' b in
        unsigned_exists p t0 || unsigned_exists p t1)

  let filter p t =
    let rec loop tree =
      let iv = is_value_of_tree tree in
      match tree_descr tree with
      | Leaf leaf ->
        if Callback.call p (leaf_key leaf) (leaf_datum leaf)
        then of_tree tree
        else empty iv
      | Branch b ->
        let prefix_and_bit, t0, t1 = branch_descr b in
        branch prefix_and_bit (loop t0) (loop t1)
    in
    match descr t with Empty -> t | Non_empty tree -> loop tree

  (* CR-someday lmaurer: Make this O(n) rather than O(n log n). *)
  let partition p t =
    let empty = empty (is_value_of t) in
    match descr t with
    | Empty -> empty, empty
    | Non_empty tree ->
      let rec loop ((true_, false_) as acc) tree =
        match tree_descr tree with
        | Leaf leaf ->
          let i, d = leaf_descr leaf in
          if Callback.call p i d
          then add i d true_, false_
          else true_, add i d false_
        | Branch branch -> loop (loop acc (branch0 branch)) (branch1 branch)
      in
      loop (empty, empty) tree

  let choose t =
    match descr t with
    | Empty -> raise Not_found
    | Non_empty tree ->
      let rec loop tree =
        match tree_descr tree with
        | Leaf leaf -> leaf_binding leaf
        | Branch branch -> loop (branch0 branch)
      in
      loop tree

  let choose_opt t =
    match choose t with exception Not_found -> None | choice -> Some choice

  let min_binding t =
    match descr t with
    | Empty -> raise Not_found
    | Non_empty tree -> (
      let rec loop tree =
        match tree_descr tree with
        | Leaf leaf -> leaf_binding leaf
        | Branch branch -> loop (branch0 branch)
      in
      match tree_descr tree with
      | Leaf leaf -> leaf_binding leaf
      | Branch branch ->
        let t0, _ = order_branches' branch in
        loop t0)

  let min_binding_opt t =
    match min_binding t with exception Not_found -> None | min -> Some min

  let max_binding t =
    match descr t with
    | Empty -> raise Not_found
    | Non_empty tree -> (
      let rec loop tree =
        match tree_descr tree with
        | Leaf leaf -> leaf_binding leaf
        | Branch branch -> loop (branch1 branch)
      in
      match tree_descr tree with
      | Leaf leaf -> leaf_binding leaf
      | Branch branch ->
        let _, t1 = order_branches' branch in
        loop t1)

  let max_binding_opt t =
    match max_binding t with exception Not_found -> None | max -> Some max

  let rec equal_tree f t0 t1 =
    if t0 == t1
    then true
    else
      match tree_descr t0, tree_descr t1 with
      | Leaf l0, Leaf l1 ->
        leaf_key l0 = leaf_key l1
        && Equal_callback.call f (leaf_datum l0) (leaf_datum l1)
      | Branch b0, Branch b1 ->
        if branch_prefix_and_bit b0 = branch_prefix_and_bit b1
        then
          equal_tree f (branch0 b0) (branch0 b1)
          && equal_tree f (branch1 b0) (branch1 b1)
        else false
      | (Leaf _ | Branch _), _ -> false

  let equal f t0 t1 =
    match descr t0, descr t1 with
    | Empty, Empty -> true
    | Empty, Non_empty _ | Non_empty _, Empty -> false
    | Non_empty t0, Non_empty t1 -> equal_tree f t0 t1

  let rec compare_tree f t0 t1 =
    if t0 == t1
    then 0
    else
      match tree_descr t0, tree_descr t1 with
      | Leaf l0, Leaf l1 ->
        let i, j = leaf_key l0, leaf_key l1 in
        let c = if i = j then 0 else if i < j then -1 else 1 in
        if c <> 0
        then c
        else Compare_callback.call f (leaf_datum l0) (leaf_datum l1)
      | Branch b0, Branch b1 ->
        let prefix_and_bit0 = branch_prefix_and_bit b0 in
        let prefix_and_bit1 = branch_prefix_and_bit b1 in
        let prefix0, bit0 = unpack prefix_and_bit0 in
        let prefix1, bit1 = unpack prefix_and_bit1 in
        let c = compare_prefix prefix0 bit0 prefix1 bit1 in
        if c = 0
        then
          let c = compare_tree f (branch0 b0) (branch0 b1) in
          if c = 0 then compare_tree f (branch1 b0) (branch1 b1) else c
        else c
      | Leaf _, Branch _ -> 1
      | Branch _, Leaf _ -> -1

  let compare f t0 t1 =
    match descr t0, descr t1 with
    | Empty, Empty -> 0
    | Empty, Non_empty _ -> 1
    | Non_empty _, Empty -> -1
    | Non_empty t0, Non_empty t1 -> compare_tree f t0 t1

  (* All entries in [t] have the same sign -- either all are non-negative or all
     are negative.

     [i] might be any value. *)
  let same_sign_split ~found ~not_found i t =
    let rec loop t =
      match tree_descr t with
      | Leaf l ->
        let iv = is_value_of_tree t in
        let j = leaf_key l in
        if i = j
        then empty iv, Split_callback.call found (leaf_datum l), empty iv
        else if j < i
        then of_tree t, not_found, empty iv
        else empty iv, not_found, of_tree t
      | Branch b ->
        let prefix_and_bit = branch_prefix_and_bit b in
        let prefix, bit = unpack prefix_and_bit in
        if match_prefix i prefix bit
        then
          let t0, t1 = branch0 b, branch1 b in
          if zero_bit i bit
          then
            let lt, mem, gt = loop t0 in
            lt, mem, of_tree (branch_right_nonempty prefix_and_bit gt t1)
          else
            let lt, mem, gt = loop t1 in
            of_tree (branch_left_nonempty prefix_and_bit t0 lt), mem, gt
        else if i < prefix
        then empty (is_value_of_tree t), not_found, of_tree t
        else of_tree t, not_found, empty (is_value_of_tree t)
    in
    loop t

  let split ~found ~not_found i t =
    match descr t with
    | Empty -> empty (is_value_of t), not_found, empty (is_value_of t)
    | Non_empty t -> (
      match tree_descr t with
      | Branch b when branch_prefix_and_bit b lsl 1 = 0 ->
        let prefix_and_bit, t0, t1 = branch_descr b in
        (* prefix is necessarily empty *)
        if i < 0
        then
          let lt, mem, gt = same_sign_split ~found ~not_found i t1 in
          lt, mem, of_tree (branch_left_nonempty prefix_and_bit t0 gt)
        else
          let lt, mem, gt = same_sign_split ~found ~not_found i t0 in
          of_tree (branch_right_nonempty prefix_and_bit lt t1), mem, gt
      | Leaf _ | Branch _ ->
        (same_sign_split [@inlined hint]) ~found ~not_found i t)

  let to_list t =
    let rec loop acc t =
      match tree_descr t with
      | Leaf l -> leaf_binding l :: acc
      | Branch b -> loop (loop acc (branch1 b)) (branch0 b)
    in
    match descr t with
    | Empty -> []
    | Non_empty t -> (
      match tree_descr t with
      | Leaf l -> [leaf_binding l]
      | Branch b ->
        let t0, t1 = order_branches' b in
        loop (loop [] t1) t0)

  (* [merge_left] and [merge_right] are just [filter_map] under another name,
     but they are written this way to avoid allocating extra closures. We can
     replace them with [filter_map] once we have function specialization. *)

  let[@inline always] leaf_or_empty iv i datum_opt =
    match datum_opt with
    | None -> empty iv
    | Some datum -> of_tree (leaf iv i datum)

  let rec merge_left iv f t0 =
    match (tree_descr [@inlined hint]) t0 with
    | Leaf l ->
      let i, d = leaf_descr l in
      leaf_or_empty iv i (f i (Some d) None)
    | Branch b ->
      let prefix_and_bit, t00, t01 = branch_descr b in
      branch prefix_and_bit (merge_left iv f t00) (merge_left iv f t01)

  let rec merge_right iv f t1 =
    match (tree_descr [@inlined hint]) t1 with
    | Leaf l ->
      let i, d = leaf_descr l in
      leaf_or_empty iv i (f i None (Some d))
    | Branch b ->
      let prefix_and_bit, t10, t11 = branch_descr b in
      branch prefix_and_bit (merge_right iv f t10) (merge_right iv f t11)

  let rec merge_tree iv f t0 t1 =
    pattern_match_pair_merge
      ~only_left:(fun t0 -> merge_left iv f t0)
      ~only_right:(fun t1 -> merge_right iv f t1)
      ~both_sides:(fun t0 t1 -> merge_tree iv f t0 t1)
      iv
      (fun[@inline always] i d0 d1 -> f i (Some d0) (Some d1))
      t0 t1

  let merge iv f t0 t1 =
    match descr t0, descr t1 with
    | Empty, Empty -> empty iv
    | Empty, Non_empty t1 -> merge_right iv f t1
    | Non_empty t0, Empty -> merge_left iv f t0
    | Non_empty t0, Non_empty t1 -> merge_tree iv f t0 t1

  let find_opt t key =
    match find t key with exception Not_found -> None | datum -> Some datum

  let get_singleton t =
    match descr t with
    | Empty -> None
    | Non_empty t -> (
      match tree_descr t with
      | Branch _ -> None
      | Leaf l -> Some (leaf_binding l))

  let rec map_tree iv f t =
    match tree_descr t with
    | Leaf l -> leaf iv (leaf_key l) (f (leaf_datum l))
    | Branch b ->
      let prefix_and_bit, t0, t1 = branch_descr b in
      branch_non_empty prefix_and_bit (map_tree iv f t0) (map_tree iv f t1)

  let map iv f t =
    match descr t with
    | Empty -> empty iv
    | Non_empty t -> of_tree (map_tree iv f t)

  let rec map_sharing_tree f t =
    let iv = is_value_of_tree t in
    match tree_descr t with
    | Leaf l ->
      let k, v = leaf_descr l in
      let v' = f v in
      if v == v' then t else leaf iv k v'
    | Branch b ->
      let prefix_and_bit, t0, t1 = branch_descr b in
      let t0' = map_sharing_tree f t0 in
      let t1' = map_sharing_tree f t1 in
      if t0' == t0 && t1' == t1
      then t
      else branch_non_empty prefix_and_bit t0' t1'

  let map_sharing f t =
    match descr t with
    | Empty -> empty (is_value_of t)
    | Non_empty t -> of_tree (map_sharing_tree f t)

  let rec mapi_tree iv f t =
    match tree_descr t with
    | Leaf l ->
      let key, datum = leaf_descr l in
      leaf iv key (Callback.call f key datum)
    | Branch b ->
      let prefix_and_bit, t0, t1 = branch_descr b in
      branch_non_empty prefix_and_bit (mapi_tree iv f t0) (mapi_tree iv f t1)

  let mapi iv f t =
    match descr t with
    | Empty -> empty iv
    | Non_empty t -> of_tree (mapi_tree iv f t)

  let rec filter_map_tree iv f t =
    match tree_descr t with
    | Leaf l -> (
      let k, d = leaf_descr l in
      match f k d with None -> empty iv | Some d' -> of_tree (leaf iv k d'))
    | Branch b ->
      let prefix_and_bit, t0, t1 = branch_descr b in
      branch prefix_and_bit (filter_map_tree iv f t0) (filter_map_tree iv f t1)

  let filter_map iv f t =
    match descr t with
    | Empty -> empty iv
    | Non_empty t -> filter_map_tree iv f t

  (* See comment about [merge_right]; this is just a specialized version of
     [filter_map] *)
  let rec update_many_right iv f t1 =
    match (tree_descr [@inlined hint]) t1 with
    | Leaf l ->
      let k = leaf_key l in
      leaf_or_empty iv k (f k None (leaf_datum l))
    | Branch b ->
      let prefix_and_bit, t10, t11 = branch_descr b in
      branch prefix_and_bit
        (update_many_right iv f t10)
        (update_many_right iv f t11)

  let rec update_many_tree f t0 t1 =
    let iv = is_value_of_tree t0 in
    pattern_match_pair_merge
      ~only_left:(fun t0 -> of_tree t0)
      ~only_right:(fun t1 -> update_many_right iv f t1)
      ~both_sides:(fun t0 t1 -> update_many_tree f t0 t1)
      iv
      (fun[@inline always] k d0 d1 -> f k (Some d0) d1)
      t0 t1

  let update_many f t0 t1 =
    match descr t0, descr t1 with
    | _, Empty -> t0
    | Empty, Non_empty t1 -> update_many_right (is_value_of t0) f t1
    | Non_empty t0, Non_empty t1 -> update_many_tree f t0 t1

  let rec filter_map_sharing_tree f t =
    let iv = is_value_of_tree t in
    match tree_descr t with
    | Leaf l -> (
      let k, d = leaf_descr l in
      match f k d with
      | None -> empty iv
      | Some d' when d == d' -> of_tree t
      | Some d' -> of_tree (leaf iv k d'))
    | Branch b ->
      let prefix_and_bit, t0, t1 = branch_descr b in
      let t0' = filter_map_sharing_tree f t0 in
      let t1' = filter_map_sharing_tree f t1 in
      if t0' == of_tree t0 && t1' == of_tree t1
      then of_tree t
      else branch prefix_and_bit t0' t1'

  let filter_map_sharing f t =
    let iv = is_value_of t in
    match descr t with
    | Empty -> empty iv
    | Non_empty t -> filter_map_sharing_tree f t

  (* NB: an iterator [Next (binding, rest)] is positioned on the binding
     [binding] and then will iterate on the trees in [rest] in order.

     The trees in ['a t] are {b never} at top-level; in particular, they never
     contain branches with a negative bit and we can safely use signed
     comparison in all the functions below (except for [iterator], which is
     called on a top-level tree). *)

  type 'a iterator =
    | Done
    | Next of 'a Binding.t * ('a, non_empty) tree list

  (* NB: We rely on [rest] not containing a top-level patricia tree to use
     signed comparison. *)
  let rec iterator0 t rest =
    match tree_descr t with
    | Leaf l -> Next (leaf_binding l, rest)
    | Branch b -> iterator0 (branch0 b) (branch1 b :: rest)

  let iterator t =
    match descr t with
    | Empty -> Done
    | Non_empty t -> (
      match tree_descr t with
      | Leaf l -> Next (leaf_binding l, [])
      | Branch b ->
        let t0, t1 = order_branches' b in
        iterator0 t0 [t1])

  let current it = match it with Done -> None | Next (b, _) -> Some b

  let advance it =
    match it with
    | Done | Next (_, []) -> Done
    | Next (_, t :: rest) -> iterator0 t rest

  (* NB: We rely on [rest] not containing a top-level patricia tree to use
     signed comparison. *)
  let rec seek0 k t rest =
    match tree_descr t, rest with
    | Leaf l, _ when k <= leaf_key l -> Next (leaf_binding l, rest)
    | Branch b, _ when match_prefix_and_bit k (branch_prefix_and_bit b) ->
      let prefix_and_bit = branch_prefix_and_bit b in
      let _, bit = unpack prefix_and_bit in
      let t1 = branch1 b in
      if zero_bit k bit
      then seek0 k (branch0 b) (t1 :: rest)
      else seek0 k t1 rest
    | Branch b, _
      when let prefix, _ = unpack (branch_prefix_and_bit b) in
           k <= prefix ->
      iterator0 (branch0 b) (branch1 b :: rest)
    | (Leaf _ | Branch _), [] -> Done
    | (Leaf _ | Branch _), t' :: rest' -> seek0 k t' rest'

  let seek it k =
    match it with
    | Done -> Done
    | Next (b, rest) -> (
      if k <= Binding.key b
      then it
      else match rest with [] -> Done | t' :: rest' -> seek0 k t' rest')

  let to_seq t =
    let rec aux acc () =
      match acc with
      | [] -> Seq.Nil
      | t0 :: r -> (
        match tree_descr t0 with
        | Leaf l -> Seq.Cons (leaf_binding l, aux r)
        | Branch b -> aux (branch0 b :: branch1 b :: r) ())
    in
    fun () ->
      match descr t with
      | Empty -> Seq.Nil
      | Non_empty t -> (
        match tree_descr t with
        | Leaf l -> Seq.Cons (leaf_binding l, aux [])
        | Branch b ->
          let t0, t1 = order_branches' b in
          aux [t0; t1] ())

  let[@inline always] of_list iv l =
    List.fold_left
      (fun map b -> add (Binding.key b) (Binding.value iv b) map)
      (empty iv) l

  let map_keys f t =
    let iv = is_value_of t in
    fold (Callback.of_func iv (fun i d acc -> add (f i) d acc)) t (empty iv)

  let valid t =
    let rec check_deep prefix bit t =
      match tree_descr t with
      | Leaf l ->
        let i = leaf_key l in
        (bit = 0 && prefix = i) || match_prefix i prefix bit
      | Branch b ->
        let prefix_and_bit', t0, t1 = branch_descr b in
        let prefix', bit' = unpack prefix_and_bit' in
        (* CR-someday lmaurer: Should check that [bit'] has a POPCOUNT of 1 *)
        let prefix0 =
          (* This should be a no-op, since [prefix'] should already have a zero
             here *)
          prefix' land lnot bit'
        in
        let prefix1 = prefix' lor bit' in
        let bit0 = bit' lsr 1 in
        let bit1 = bit0 in
        prefix0 = prefix'
        && (bit = bit' || higher bit bit')
        && bit <> 0
        && match_prefix prefix' prefix bit
        && check_deep prefix0 bit0 t0 && check_deep prefix1 bit1 t1
    in
    match descr t with Empty -> true | Non_empty t -> check_deep 0 min_int t
end
[@@inline always]

module Set = struct
  type elt = key

  type t = unit t0

  and 'a t0 = 'a Set0.t

  module Ops = Tree_operations (Set0)
  include Ops

  let empty = Set0.empty Unit

  let singleton i = Ops.singleton Unit i ()

  let add i t = Ops.add i () t

  (* CR bclement: This should just be `Ops.union (fun _ _ _ -> Some ())` once we
     have function specialisation, see the comment on
     {!module-Merge_callback}. *)
  let union t0 t1 = Ops.union (Set0.is_value_of t0) t0 t1

  let union_shared t0 t1 = Ops.union_shared (Set0.is_value_of t0) t0 t1

  let union_sharing t0 t1 = Ops.union_sharing (Set0.is_value_of t0) t0 t1

  let diff = Ops.diff_domains

  (* CR bclement: This should just be `Ops.diff_sharing (fun _ _ _ -> None)`
     once we have function specialisation, see the comment on
     {!module-Merge_callback}. *)
  let diff_sharing t0 t1 = Ops.diff_sharing (Set0.is_value_of t0) t0 t1

  let diff_shared t0 t1 = Ops.diff_shared (Set0.is_value_of t0) t0 t1

  let disjoint t0 t1 = not (Ops.inter_domain_is_non_empty t0 t1)

  let inter t0 t1 = Ops.inter Unit (Set0.is_value_of t0) t0 t1

  let rec union_list ts =
    match ts with [] -> empty | t :: ts -> union t (union_list ts)

  let filter_map f t =
    let rec loop f acc (t : (_, non_empty) Set0.tree) =
      match t with
      | Set0.Leaf i -> ( match f i with None -> acc | Some j -> add j acc)
      | Set0.Branch (_, t0, t1) -> loop f (loop f acc t0) t1
    in
    match Set0.descr t with Empty -> empty | Non_empty t -> loop f empty t

  let elements = Ops.to_list

  let min_elt = Ops.min_binding

  let min_elt_opt = Ops.min_binding_opt

  let max_elt = Ops.max_binding

  let max_elt_opt = Ops.max_binding_opt

  (* CR bclement: This should just be `Ops.equal (fun _ _ -> true)` once we have
     function specialisation, see the comment on {!module-Merge_callback}. *)
  let equal t0 t1 = Ops.equal () t0 t1

  (* CR bclement: This should just be `Ops.compare (fun _ _ -> 0)` once we have
     function specialisation, see the comment on {!module-Merge_callback}. *)
  let compare t0 t1 = Ops.compare () t0 t1

  (* CR bclement: This should just be `Ops.split ~found:(fun _ -> true)` once we
     have function specialisation, see the comment on
     {!module-Merge_callback}. *)
  let split i t =
    Ops.split ~found:(True (Set0.is_value_of t)) ~not_found:false i t

  let find elt t = if mem elt t then elt else raise Not_found

  let of_list l = Ops.of_list Unit l

  let map f t = Ops.map_keys f t
end

module Map = struct
  include Map0
  module Ops = Tree_operations (Map0)
  include Ops

  let empty = empty Any

  let singleton i d = Ops.singleton Any i d

  let inter f t0 t1 = Ops.inter Any f t0 t1

  (* CR bclement: This should just be `Ops.split ~found:(fun a -> Some a)` once
     we have function specialisation, see the comment on
     {!module-Merge_callback}. *)
  let split i t = Ops.split ~found:Option ~not_found:None i t

  let bindings s = Ops.to_list s

  let map f t = Ops.map Any f t

  let mapi f t = Ops.mapi Any f t

  let filter_map f t = Ops.filter_map Any f t

  let of_list l = Ops.of_list Any l

  let merge f t0 t1 = Ops.merge Any f t0 t1

  (* CR-someday lmaurer: This should be doable as a fast map operation if we
     generalize [Ops.map] by letting the returned tree be built by a different
     [Tree] module *)
  let keys map = fold (fun k _ set -> Set.add k set) map Set.empty

  let data t = List.map snd (bindings t)

  (* CR-someday lmaurer: See comment on [keys] *)
  let of_set f set = Set.fold (fun e map -> add e (f e) map) set empty
end

type set = Set.t

type +!'a map = 'a Map.t

module Make (X : sig
  val print : Format.formatter -> key -> unit
end) =
struct
  module Set = struct
    include Set
    module Elt = X

    let [@ocamlformat "disable"] print ppf s =
      let elts ppf s = iter (fun e -> Format.fprintf ppf "@ %a" Elt.print e) s in
      Format.fprintf ppf "@[<1>{@[<1>%a@ @]}@]" elts s

    let to_string s = Format.asprintf "%a" print s
  end

  module Map = struct
    include Map
    module Key = X

    type nonrec key = key

    module Set = Set

    let [@ocamlformat "disable"] print_debug print_datum ppf t =
      let rec pp ppf (t : (_, non_empty) tree) =
        match t with
        | Leaf (k, v) -> Format.fprintf ppf "@[<hv 1>(%x@ %a)@]" k print_datum v
        | Branch (k, l, r) ->
          Format.fprintf ppf "@[<hv 1>(branch@ %x@ %a@ %a)@]" k pp l
            pp r
      in
      match descr t with
      | Empty -> Format.pp_print_string ppf "()"
      | Non_empty t -> pp ppf t

    let[@inline always] disjoint_union ?eq ?print t1 t2 =
      ignore print;
      if t1 == t2
      then t1
      else
        let fail key =
          Misc.fatal_errorf
            "Patricia_tree.disjoint_union: key %a is in intersection" Key.print
            key
        in
        union
          (fun key datum1 datum2 ->
            match eq with
            | None -> fail key
            | Some eq -> if eq datum1 datum2 then Some datum1 else fail key)
          t1 t2

    let [@ocamlformat "disable"] print print_datum ppf t =
      if is_empty t then
        Format.fprintf ppf "{}"
      else
        Format.fprintf ppf "@[<hov 1>{%a}@]"
          (Format.pp_print_list ~pp_sep:Format.pp_print_space
             (fun ppf (key, datum) ->
                Format.fprintf ppf "@[<hov 1>(%a@ %a)@]"
                  Key.print key print_datum datum))
          (bindings t)
  end
end
[@@inline always]
