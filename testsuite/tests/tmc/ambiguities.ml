(* TEST
   expect;
*)
type 'a tree =
  | Leaf of 'a
  | Node of 'a tree * 'a tree

[%%expect {|
type 'a tree = Leaf of 'a | Node of 'a tree * 'a tree
|}]

module Ambiguous = struct
  let[@tail_mod_cons] rec map f = function
    | Leaf v -> Leaf (f v)
    | Node (left, right) -> Node (map f left, map f right)
end

[%%expect
{|
Line 4, characters 28-58:
4 |     | Node (left, right) -> Node (map f left, map f right)
                                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: "[@tail_mod_cons]": this constructor application may be TMC-transformed
       in several different ways. Please disambiguate by adding an explicit
       "[@tailcall]" attribute to the call that should be made tail-recursive,
       or a "[@tailcall false]" attribute on calls that should not be
       transformed.
Line 4, characters 34-44:
4 |     | Node (left, right) -> Node (map f left, map f right)
                                      ^^^^^^^^^^
  This call could be annotated.
Line 4, characters 46-57:
4 |     | Node (left, right) -> Node (map f left, map f right)
                                                  ^^^^^^^^^^^
  This call could be annotated.
|}]

module Positive_disambiguation = struct
  let[@tail_mod_cons] rec map f = function
    | Leaf v -> Leaf (f v)
    | Node (left, right) -> Node (map f left, (map [@tailcall]) f right)
end

[%%expect
{|
module Positive_disambiguation :
  sig val map : ('a -> 'b) -> 'a tree -> 'b tree end
|}]

module Negative_disambiguation = struct
  let[@tail_mod_cons] rec map f = function
    | Leaf v -> Leaf (f v)
    | Node (left, right) -> Node ((map [@tailcall false]) f left, map f right)
end

[%%expect
{|
module Negative_disambiguation :
  sig val map : ('a -> 'b) -> 'a tree -> 'b tree end
|}]

module Positive_and_negative_disambiguation = struct
  (* in-depth disambiguations *)
  type 'a t =
    | N
    | C of 'a t * ('a t * 'a t)

  let[@tail_mod_cons] rec map1 f l =
    match l with
    | N -> N
    | C (a, (b, c)) ->
      C ((map1 [@tailcall]) f a, ((map1 [@tailcall false]) f b, map1 f c))

  let[@tail_mod_cons] rec map2 f l =
    match l with
    | N -> N
    | C (a, (b, c)) ->
      C ((map2 [@tailcall false]) f a, ((map2 [@tailcall]) f b, map2 f c))
end

[%%expect
{|
module Positive_and_negative_disambiguation :
  sig
    type 'a t = N | C of 'a t * ('a t * 'a t)
    val map1 : 'a -> 'b t -> 'c t
    val map2 : 'a -> 'b t -> 'c t
  end
|}]

module Long_before_and_after = struct
  type 'a tree =
    | Leaf of 'a
    | Node of 'a tree * 'a tree * 'a tree * 'a tree * 'a tree

  let[@tail_mod_cons] rec map f = function
    | Leaf v -> Leaf (f v)
    | Node (t1, t2, t3, t4, t5) ->
      (* manual unfolding *)
      Node (map f t1, map f t2, (map [@tailcall]) f t3, map f t4, map f t5)

  let () =
    assert (
      map succ (Node (Leaf 0, Leaf 1, Leaf 2, Leaf 3, Leaf 4))
      = Node (Leaf 1, Leaf 2, Leaf 3, Leaf 4, Leaf 5))
end

[%%expect
{|
module Long_before_and_after :
  sig
    type 'a tree =
        Leaf of 'a
      | Node of 'a tree * 'a tree * 'a tree * 'a tree * 'a tree
    val map : ('a -> 'b) -> 'a tree -> 'b tree
  end
|}]

module Deep_nesting_nonambiguous = struct
  type 'a tree =
    | Leaf of 'a
    | Node of 'a tree * ('a tree * ('a tree * ('a tree * 'a tree)))

  let[@tail_mod_cons] rec map f = function
    | Leaf v -> Leaf (f v)
    | Node (t1, (t2, (t3, (t4, t5)))) ->
      Node (map f t1, (map f t2, ((map [@tailcall]) f t3, (map f t4, map f t5))))

  let () =
    assert (
      map succ (Node (Leaf 0, (Leaf 1, (Leaf 2, (Leaf 3, Leaf 4)))))
      = Node (Leaf 1, (Leaf 2, (Leaf 3, (Leaf 4, Leaf 5)))))
end

[%%expect
{|
module Deep_nesting_nonambiguous :
  sig
    type 'a tree =
        Leaf of 'a
      | Node of 'a tree * ('a tree * ('a tree * ('a tree * 'a tree)))
    val map : ('a -> 'b) -> 'a tree -> 'b tree
  end
|}]

module Deep_nesting_ambiguous = struct
  type 'a tree =
    | Leaf of 'a
    | Node of 'a tree * ('a tree * ('a tree * ('a tree * 'a tree)))

  let[@tail_mod_cons] rec map f = function
    | Leaf v -> Leaf (f v)
    | Node (t1, (t2, (t3, (t4, t5)))) ->
      Node (map f t1, (map f t2, (map f t3, (map f t4, map f t5))))

  let () =
    assert (
      map succ (Node (Leaf 0, (Leaf 1, (Leaf 2, (Leaf 3, Leaf 4)))))
      = Node (Leaf 1, (Leaf 2, (Leaf 3, (Leaf 4, Leaf 5)))))
end

[%%expect
{|
Line 9, characters 6-67:
9 |       Node (map f t1, (map f t2, (map f t3, (map f t4, map f t5))))
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: "[@tail_mod_cons]": this constructor application may be TMC-transformed
       in several different ways. Please disambiguate by adding an explicit
       "[@tailcall]" attribute to the call that should be made tail-recursive,
       or a "[@tailcall false]" attribute on calls that should not be
       transformed.
Line 9, characters 12-20:
9 |       Node (map f t1, (map f t2, (map f t3, (map f t4, map f t5))))
                ^^^^^^^^
  This call could be annotated.
Line 9, characters 23-31:
9 |       Node (map f t1, (map f t2, (map f t3, (map f t4, map f t5))))
                           ^^^^^^^^
  This call could be annotated.
Line 9, characters 34-42:
9 |       Node (map f t1, (map f t2, (map f t3, (map f t4, map f t5))))
                                      ^^^^^^^^
  This call could be annotated.
Line 9, characters 45-53:
9 |       Node (map f t1, (map f t2, (map f t3, (map f t4, map f t5))))
                                                 ^^^^^^^^
  This call could be annotated.
Line 9, characters 55-63:
9 |       Node (map f t1, (map f t2, (map f t3, (map f t4, map f t5))))
                                                           ^^^^^^^^
  This call could be annotated.
|}]

module Disjunctions_ambiguous = struct
  type t =
    | Leaf of int
    | Node of t * t

  (** [shift ~flip:false k t] shifts all the leaves of [t] by [k].
     When [~flip:true], leaves of even level are shifted by k,
     leaves of odd level by (-k) *)
  let[@tail_mod_cons] rec shift ~flip k = function
    | Leaf n -> Leaf (n + k)
    | Node (left, right) ->
      (* This example contains several ambiguous TMC calls per constructor argument:
         the two subcalls of each arguments are *both* in TMC position, and annotating
         either of them is enough to fix the ambiguity error. *)
      Node
        ( (if flip then shift ~flip (-k) left else shift ~flip k left),
          if flip then shift ~flip (-k) right else shift ~flip k right )
end

[%%expect
{|
Lines 15-17, characters 6-72:
15 | ......Node
16 |         ( (if flip then shift ~flip (-k) left else shift ~flip k left),
17 |           if flip then shift ~flip (-k) right else shift ~flip k right )
Error: "[@tail_mod_cons]": this constructor application may be TMC-transformed
       in several different ways. Please disambiguate by adding an explicit
       "[@tailcall]" attribute to the call that should be made tail-recursive,
       or a "[@tailcall false]" attribute on calls that should not be
       transformed.
Line 16, characters 24-45:
16 |         ( (if flip then shift ~flip (-k) left else shift ~flip k left),
                             ^^^^^^^^^^^^^^^^^^^^^
  This call could be annotated.
Line 16, characters 51-69:
16 |         ( (if flip then shift ~flip (-k) left else shift ~flip k left),
                                                        ^^^^^^^^^^^^^^^^^^
  This call could be annotated.
Line 17, characters 23-45:
17 |           if flip then shift ~flip (-k) right else shift ~flip k right )
                            ^^^^^^^^^^^^^^^^^^^^^^
  This call could be annotated.
Line 17, characters 51-70:
17 |           if flip then shift ~flip (-k) right else shift ~flip k right )
                                                        ^^^^^^^^^^^^^^^^^^^
  This call could be annotated.
|}]

module Disjunctions_disambiguated = struct
  type t =
    | Leaf of int
    | Node of t * t

  let[@tail_mod_cons] rec shift ~flip k = function
    | Leaf n -> Leaf (n + k)
    | Node (left, right) ->
      Node
        ( (if flip then shift ~flip (-k) left else shift ~flip k left),
          if flip
          then shift ~flip (-k) right
          else (shift [@tailcall]) ~flip k right )
end

[%%expect
{|
module Disjunctions_disambiguated :
  sig
    type t = Leaf of int | Node of t * t
    val shift : flip:bool -> int -> t -> t
  end
|}]

module Disjunctions_ambiguous_again = struct
  type t =
    | Leaf of int
    | Node of t * t

  let[@tail_mod_cons] rec shift ~flip k = function
    | Leaf n -> Leaf (n + k)
    | Node (left, right) ->
      Node
        ( (if flip
          then (shift [@tailcall]) ~flip (-k) left
          else shift ~flip k left),
          if flip
          then shift ~flip (-k) right
          else (shift [@tailcall]) ~flip k right )
end

[%%expect
{|
Lines 9-15, characters 6-50:
 9 | ......Node
10 |         ( (if flip
11 |           then (shift [@tailcall]) ~flip (-k) left
12 |           else shift ~flip k left),
13 |           if flip
14 |           then shift ~flip (-k) right
15 |           else (shift [@tailcall]) ~flip k right )
Error: "[@tail_mod_cons]": this constructor application may be TMC-transformed
       in several different ways. Only one of the arguments may become a TMC
       call, but several arguments contain calls that are explicitly marked
       as tail-recursive. Please fix the conflict by reviewing and fixing the
       conflicting annotations.
Line 11, characters 15-50:
11 |           then (shift [@tailcall]) ~flip (-k) left
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  This call is explicitly annotated.
Line 15, characters 15-48:
15 |           else (shift [@tailcall]) ~flip k right )
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  This call is explicitly annotated.
|}]
