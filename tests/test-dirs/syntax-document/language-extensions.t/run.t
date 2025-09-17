Syntax-Documentation Test - Language-extentions
  $ alias syn_doc="$MERLIN single syntax-document -position "

Recursive definition of values
  $ cat > main.ml << EOF
  > let rec name1 = 1 :: name2 and name2 = 2 :: name1
  > EOF

on rec
  $ syn_doc 1:6 \
  > -filename ./main.ml < ./main.ml | jq '.value.name'
  "Recursive value definition"

Recursive modules
  $ cat > rec-modules.ml << EOF
  > module rec A : sig
  >   type t = Leaf of string | Node of ASet.t
  >   val compare: t -> t -> int
  > end = struct
  > type t = Leaf of string | Node of ASet.t
  > let compare t1 t2 =
  >   match (t1, t2) with
  >    | (Leaf s1, Leaf s2) -> Stdlib.compare s1 s2
  >    | (Leaf _, Node _) -> 1
  >    | (Node _, Leaf _) -> -1
  >    | (Node n1, Node n2) -> ASet.compare n1 n2
  > end
  > and ASet : Set.S with type elt = A.t
  > = Set.Make(A)
  > module B = struct
  >    module rec A : sig
  >      type t
  >      val empty: t
  >    end = struct
  >      type t = Empty | Node of int * t * t
  >      let empty = Empty
  >    end
  >  end
  > EOF
on rec
  $ syn_doc 1:9 \
  > -filename ./rec-modules.ml < ./rec-modules.ml | jq '.value.name'
  "Recursive module"
On type t = Leaf of stri...
  $ syn_doc 5:5 \
  > -filename ./rec-modules.ml < ./rec-modules.ml | jq '.value.name'
  "Variant Type"
On rec .. Nested recurvise module
  $ syn_doc 16:12 \
  > -filename ./rec-modules.ml < ./rec-modules.ml | jq '.value.name'
  "Recursive module"


Recovering the type of a module
  $ cat > rec-mod-type.ml << EOF 
  > module type MYHASH = sig
  >   include module type of struct include Hashtbl end
  >   val replace: ('a, 'b) t -> 'a -> 'b -> unit
  > end
  > module MySet : module type of Set = struct
  > end
  > EOF
on module type of..
  $ syn_doc 2:23 \
  > -filename ./rec-mod-type.ml < ./rec-mod-type.ml | jq '.value.name'
  "Recovering module type"
on module type of..
  $ syn_doc 5:28 \
  > -filename ./rec-mod-type.ml < ./rec-mod-type.ml | jq '.value.name'
  "Recovering module type"


// Signature Substitutions
  $ cat > sig-subs.ml << EOF
  > module type Printable = sig
  >    type t
  >    val print : Format.formatter -> t -> unit
  >  end
  >  module type Comparable = sig
  >    type t
  >    val compare : t -> t -> int
  >  end
  >  module type PrintableComparable = sig
  >    include Printable
  >    include Comparable with type t := t
  >  end
  >  module type S = sig
  >    type t
  >    module Sub : sig
  >      type outer := t
  >      type t
  >      val to_outer : t -> outer
  >    end
  >  end
  >  module type ENDO = sig
  >    module type T
  >    module F: T -> T
  >  end
  > module Endo(X: sig module type T end): ENDO with module type T = X.T =
  >  struct
  >      module type T = X.T
  >      module F(X:T) = X
  >  end
  > EOF
// Destructive substitutions
On 'with':
  $ syn_doc 11:24 -filename ./sig-subs.ml < ./sig-subs.ml | jq '.value'
  "No documentation found"

On 'type t :='
  $ syn_doc 11:27 -filename ./sig-subs.ml < ./sig-subs.ml | jq '.value.name'
  "Destructive substitution"
  $ syn_doc 11:32 -filename ./sig-subs.ml < ./sig-subs.ml | jq '.value.name'
  "Destructive substitution"
  $ syn_doc 11:34 -filename ./sig-subs.ml < ./sig-subs.ml | jq '.value.name'
  "Destructive substitution"
On '... t'
  $ syn_doc 11:37 -filename ./sig-subs.ml < ./sig-subs.ml | jq '.value'
  "No documentation found"
On type t
  $ syn_doc 2:9 -filename ./sig-subs.ml < ./sig-subs.ml | jq '.value.name'
  "Abstract Type"
// Local substitutions
  $ syn_doc 16:12 -filename ./sig-subs.ml < ./sig-subs.ml | jq '.value.name'
  "Local substitution"
// Module type substitutions
  $ syn_doc 25:57 \
  > -filename ./sig-subs.ml < ./sig-subs.ml | jq '.value.name'
  "Module substitution"


// Types
  $ cat > types.ml << EOF 
  > type a1 = ..
  > type a2 = A
  > type a3 = |
  > type a4 = {x: int}
  > type a5 = int
  > EOF
on type a1..
  $ syn_doc 1:5 \
  > -filename ./types.ml < ./types.ml | jq '.value.name'
  "Extensible Variant Type"
on type a2..
  $ syn_doc 2:5 \
  > -filename ./types.ml < ./types.ml | jq '.value.name'
  "Variant Type"
on type a3..
  $ syn_doc 3:6 \
  > -filename ./types.ml < ./types.ml | jq '.value.name'
  "Empty Variant Type"
on type a4..
  $ syn_doc 4:5 \
  > -filename ./types.ml < ./types.ml | jq '.value.name'
  "Record Type"
on type a5..
  $ syn_doc 5:5 \
  > -filename ./types.ml < ./types.ml | jq '.value'
  "No documentation found"

// Private types
// Extensible
  $ cat > p-types.ml << EOF
  > type b1 = private ..
  > type b2 = private A
  > type b3 = private A of int
  > type b4 = private { x:int }
  > type b5 = private int 
  > module N : sig
  >   type t = private int
  >   val of_int: int -> t
  >   val to_int: t -> int
  > end = struct
  >   type t = int
  >   let of_int n = assert (n >= 0); n
  >   let to_int n = n
  > end
  > EOF
on type b1..
  $ syn_doc 1:14 \
  > -filename ./p-types.ml < ./p-types.ml | jq '.value.name'
  "Private Extensible Variant Type"
on type b2..
  $ syn_doc 2:14 \
  > -filename ./private-types.ml < ./p-types.ml | jq '.value.name'
  "Private Variant Type"
on type b3..
  $ syn_doc 3:14 \
  > -filename ./p-types.ml < ./p-types.ml | jq '.value.name'
  "Private Variant Type"
on type b4..
  $ syn_doc 4:14 \
  > -filename ./p-types.ml < ./p-types.ml | jq '.value.name'
  "Private Record Type"
on b5..
  $ syn_doc 5:14 \
  > -filename ./p-types.ml < ./p-types.ml | jq '.value.name'
  "Private Type Abbreviation"
on type t = private int..
  $ syn_doc 7:14 \
  > -filename ./p-types.ml < ./p-types.ml | jq '.value.name'
  "Private Type Abbreviation"
on type t = int..
  $ syn_doc 11:7 \
  > -filename ./p-types.ml < ./p-types.ml | jq '.value'
  "No documentation found"

// Locally abstract data types
  $ cat > locally-abstract-dt.ml << EOF 
  > let f = fun (type t) (x: t) -> x = x
  > let sort_uniq (type s) (cmp : s -> s -> int) =
  >   let module S = Set.Make(struct type t = s let compare = cmp end) in
  >   fun l ->
  >     S.elements (List.fold_right S.add l S.empty)
  > EOF
// Locally abstract data types
on type t..
  $ syn_doc 1:17 \
  > -filename ./locally-abstract-dt.ml < ./locally-abstract-dt.ml | jq '.value.name'
  "Locally Abstract Type"
On fun..
  $ syn_doc 1:9 \
  > -filename ./locally-abstract-dt.ml < ./locally-abstract-dt.ml | jq '.value'
  "No documentation found"
On x
  $ syn_doc 1:39 \
  > -filename ./locally-abstract-dt.ml < ./locally-abstract-dt.ml | jq '.value'
  "No documentation found"


// First class Modules
  $ cat > first-class-modules.ml << EOF
  > type picture = { x : int; y : int }
  > module type DEVICE = sig
  >   val draw : picture -> unit
  > end
  > let devices : (string, (module DEVICE)) Hashtbl.t = Hashtbl.create 17
  > module SVG = struct end
  > module PNG = struct end
  > let _svg = Hashtbl.add devices "SVG" (module SVG : DEVICE)
  > let _png = Hashtbl.add devices "PNG" (module PNG : SVG)
  > let sort (type s) (module Set : Set.S with type elt = s) l =
  >   Set.elements (List.fold_right Set.add l Set.empty)
  > let make_set (type s) cmp =
  >   let module S = Set.Make(struct
  >     type t = s
  >     let compare = cmp
  >   end) in
  >   (module S : Set.S with type elt = s)
  > EOF
on type picture
  $ syn_doc 1:6 \
  > -filename ./first-class-modules.ml < ./first-class-modules.ml | jq '.value.name'
  "Record Type"
on module SVG..
  $ syn_doc 6:6 \
  > -filename ./first-class-modules.ml < ./first-class-modules.ml | jq '.value'
  "No documentation found"
on (module SVG : DEVICE)
  $ syn_doc 8:43 \
  > -filename ./first-class-modules.ml < ./first-class-modules.ml | jq '.value.name'
  "First class module"
on (module PNG : SVG)
  $ syn_doc 9:43 \
  > -filename ./first-class-modules.ml < ./first-class-modules.ml | jq '.value'
  "No documentation found"
on type t = s..
  $ syn_doc 14:10 \
  > -filename ./first-class-modules.ml < ./first-class-modules.ml | jq '.value'
  "No documentation found"
on (module S : Set.S with type elt = s)
  $ syn_doc 17:2 \
  > -filename ./first-class-modules.ml < ./first-class-modules.ml | jq '.value.name'
  "First class module"

  $ call_syntax_doc_and_extract_field () {
  >   file="$1"
  >   line="$2"
  >   col="$3"
  >   field="$4"
  >   
  >   # Print the line, with a ^ underneath pointing at the character
  >   sed -n "${line}p" "$file"
  >   printf "%*s^\n" "$col" ""
  >   
  >   # Call merlin on the position
  >   "$MERLIN" single syntax-document -position  "$line:$col" -filename "$file" < "$file" \
  >     | jq "if (.value | type) == \"string\" then .value else .value.$field end" -r
  > }

  $ syn_doc_name () {
  >   call_syntax_doc_and_extract_field "$1" "$2" "$3" name
  > }

  $ syn_doc_desc () {
  >   call_syntax_doc_and_extract_field "$1" "$2" "$3" description
  > }

Convenience function to ensure we haven't made any syntax errors.
(This is especially convenient for oxcaml language features, as their syntax is volatile.)

  $ syntax_errors () {
  >   "$MERLIN" single errors -filename "$1" < "$1" | jq '.value[] | select(.type == "parser")'
  > }

// Modes
# CR-someday: Add raw modes (and @?) to typedtree so this information can be recovered

  $ cat > modes.ml << EOF
  > module type S = sig
  >   type t = foo @ local -> bar @ portable
  > end
  > let (f @ stateless) (x : int @ contended) = (_ : _ @ contended)
  > let x : int @ local = 10
  > EOF

  $ syntax_errors modes.ml

  $ syn_doc_name modes.ml 2 15
    type t = foo @ local -> bar @ portable
                 ^
  No documentation found

  $ syn_doc_name modes.ml 2 19
    type t = foo @ local -> bar @ portable
                     ^
  No documentation found

  $ syn_doc_name modes.ml 2 30
    type t = foo @ local -> bar @ portable
                                ^
  No documentation found

  $ syn_doc_name modes.ml 2 35
    type t = foo @ local -> bar @ portable
                                     ^
  No documentation found

  $ syn_doc_name modes.ml 4 7
  let (f @ stateless) (x : int @ contended) = (_ : _ @ contended)
         ^
  No documentation found

  $ syn_doc_name modes.ml 4 12
  let (f @ stateless) (x : int @ contended) = (_ : _ @ contended)
              ^
  No documentation found

  $ syn_doc_name modes.ml 4 29
  let (f @ stateless) (x : int @ contended) = (_ : _ @ contended)
                               ^
  No documentation found

  $ syn_doc_name modes.ml 4 33
  let (f @ stateless) (x : int @ contended) = (_ : _ @ contended)
                                   ^
  No documentation found

  $ syn_doc_name modes.ml 4 51
  let (f @ stateless) (x : int @ contended) = (_ : _ @ contended)
                                                     ^
  No documentation found

  $ syn_doc_name modes.ml 4 58
  let (f @ stateless) (x : int @ contended) = (_ : _ @ contended)
                                                            ^
  No documentation found

  $ syn_doc_name modes.ml 5 12
  let x : int @ local = 10
              ^
  No documentation found

  $ syn_doc_name modes.ml 5 17
  let x : int @ local = 10
                   ^
  No documentation found

  $ syn_doc_desc modes.ml 5 17
  let x : int @ local = 10
                   ^
  No documentation found

// Modalities
# CR-someday: Add raw modalities (and @@?) to typedtree so this information can be recovered

  $ cat > modalities.ml << EOF
  > module type S = sig @@ portable
  >   val foo : int -> int @@ stateless
  > end
  > external id : 'a -> 'a @@ portable = "%identity"
  > type t = { foo : int @@ contended }
  > EOF

  $ syntax_errors modalities.ml

  $ syn_doc_name modalities.ml 1 21
  module type S = sig @@ portable
                       ^
  No documentation found

  $ syn_doc_name modalities.ml 1 23
  module type S = sig @@ portable
                         ^
  No documentation found

  $ syn_doc_name modalities.ml 2 23
    val foo : int -> int @@ stateless
                         ^
  No documentation found

  $ syn_doc_name modalities.ml 2 28
    val foo : int -> int @@ stateless
                              ^
  No documentation found

  $ syn_doc_name modalities.ml 4 24
  external id : 'a -> 'a @@ portable = "%identity"
                          ^
  No documentation found

  $ syn_doc_name modalities.ml 4 28
  external id : 'a -> 'a @@ portable = "%identity"
                              ^
  No documentation found

  $ syn_doc_name modalities.ml 5 22
  type t = { foo : int @@ contended }
                        ^
  Record Type

  $ syn_doc_name modalities.ml 5 28
  type t = { foo : int @@ contended }
                              ^
  Record Type

# CR-someday: Since modalities aren't yet supported, this is falling through to the record
# case
  $ syn_doc_desc modalities.ml 5 28
  type t = { foo : int @@ contended }
                              ^
  Defines variants with a fixed set of fields

// Kinds

  $ cat > kinds.ml << EOF
  > type ('a : immediate) t : value mod portable with 'a @@ global
  > module type S = sig
  >   val id : ('a : value). 'a -> 'a
  >   val id2 : ('a : value) -> ('a : value)
  > end
  > let f (x : (_ : value)) = (x : (_ : value))
  > type t : float64 mod everything
  > EOF

  $ syntax_errors kinds.ml

  $ syn_doc_name kinds.ml 1 16
  type ('a : immediate) t : value mod portable with 'a @@ global
                  ^
  Kind abbreviation

  $ syn_doc_name kinds.ml 1 28
  type ('a : immediate) t : value mod portable with 'a @@ global
                              ^
  Kind abbreviation

  $ syn_doc_name kinds.ml 1 33
  type ('a : immediate) t : value mod portable with 'a @@ global
                                   ^
  `mod` keyword (in a kind)

  $ syn_doc_name kinds.ml 1 40
  type ('a : immediate) t : value mod portable with 'a @@ global
                                          ^
  Mod-bound

  $ syn_doc_name kinds.ml 1 47
  type ('a : immediate) t : value mod portable with 'a @@ global
                                                 ^
  `with` keyword (in a kind)

  $ syn_doc_name kinds.ml 1 51
  type ('a : immediate) t : value mod portable with 'a @@ global
                                                     ^
  with-type

  $ syn_doc_name kinds.ml 1 53
  type ('a : immediate) t : value mod portable with 'a @@ global
                                                       ^
  `@@` keyword (in a kind)

  $ syn_doc_name kinds.ml 1 57
  type ('a : immediate) t : value mod portable with 'a @@ global
                                                           ^
  Modality

  $ syn_doc_name kinds.ml 3 20
    val id : ('a : value). 'a -> 'a
                      ^
  Kind abbreviation

  $ syn_doc_name kinds.ml 4 20
    val id2 : ('a : value) -> ('a : value)
                      ^
  Kind abbreviation

  $ syn_doc_name kinds.ml 4 37
    val id2 : ('a : value) -> ('a : value)
                                       ^
  Kind abbreviation

  $ syn_doc_name kinds.ml 6 19
  let f (x : (_ : value)) = (x : (_ : value))
                     ^
  Kind abbreviation

  $ syn_doc_name kinds.ml 6 36
  let f (x : (_ : value)) = (x : (_ : value))
                                      ^
  Kind abbreviation

  $ syn_doc_name kinds.ml 7 13
  type t : float64 mod everything
               ^
  Kind abbreviation

  $ syn_doc_name kinds.ml 7 28
  type t : float64 mod everything
                              ^
  Mod-bound

  $ syn_doc_desc kinds.ml 7 13
  type t : float64 mod everything
               ^
  The layout of types represented by a 64-bit machine float.

  $ syn_doc_desc kinds.ml 7 28
  type t : float64 mod everything
                              ^
  Synonym for "global aliased many contended portable unyielding immutable stateless external_", convenient for describing immediates.

  $ syn_doc_desc kinds.ml 1 40
  type ('a : immediate) t : value mod portable with 'a @@ global
                                          ^
  Values of types of this kind can cross to `portable` from weaker modes.

// include functor

  $ cat > include_functor.ml << EOF
  > module type F = functor (S : sig end) -> sig end
  > module F (S : sig end) = struct end
  > module M : sig
  >   include functor F
  > end = struct
  >   include functor F
  > end
  > EOF

  $ syntax_errors include_functor.ml

  $ syn_doc_name include_functor.ml 4 2
    include functor F
    ^
  include functor

  $ syn_doc_name include_functor.ml 4 13
    include functor F
               ^
  include functor

  $ syn_doc_name include_functor.ml 4 18
    include functor F
                    ^
  No documentation found

  $ syn_doc_name include_functor.ml 6 2
    include functor F
    ^
  include functor

  $ syn_doc_name include_functor.ml 6 13
    include functor F
               ^
  include functor

  $ syn_doc_name include_functor.ml 6 18
    include functor F
                    ^
  No documentation found

// local allocations

  $ cat > local.ml << EOF
  > let f x =
  >   let _ = stack_ Some x in
  >   exclave_ Some x
  > let f x = g x [@nontail]
  > EOF

  $ syntax_errors local.ml

  $ syn_doc_name local.ml 2 13
    let _ = stack_ Some x in
               ^
  stack_

  $ syn_doc_name local.ml 2 18
    let _ = stack_ Some x in
                    ^
  No documentation found

  $ syn_doc_name local.ml 3 4
    exclave_ Some x
      ^
  exclave_

  $ syn_doc_name local.ml 3 13
    exclave_ Some x
               ^
  No documentation found

  $ syn_doc_name local.ml 4 20
  let f x = g x [@nontail]
                      ^
  nontail annotation

// zero-alloc annotations

  $ cat > zero_alloc.ml << EOF
  > let[@zero_alloc] f x = x
  > let[@zero_alloc opt] f x = x
  > let[@zero_alloc assume] f x = x
  > let[@zero_alloc strict] f x = x
  > let f x =
  >   (g[@zero_alloc assume]) x
  > module type S = sig
  >   val[@zero_alloc] f : int -> int
  >   val[@zero_alloc arity 1] f : t
  > end
  > external id : 'a -> 'a = "%identity" [@@noalloc]
  > let[@zero_alloc assume_unless_opt] f x = x
  > EOF

  $ syntax_errors zero_alloc.ml

  $ syn_doc_name zero_alloc.ml 1 10
  let[@zero_alloc] f x = x
            ^
  Zero-alloc annotation

  $ syn_doc_name zero_alloc.ml 2 10
  let[@zero_alloc opt] f x = x
            ^
  Zero-alloc opt annotation

  $ syn_doc_name zero_alloc.ml 2 18
  let[@zero_alloc opt] f x = x
                    ^
  Zero-alloc opt annotation

  $ syn_doc_name zero_alloc.ml 3 10
  let[@zero_alloc assume] f x = x
            ^
  Zero-alloc assume annotation

  $ syn_doc_name zero_alloc.ml 3 18
  let[@zero_alloc assume] f x = x
                    ^
  Zero-alloc assume annotation

  $ syn_doc_name zero_alloc.ml 4 10
  let[@zero_alloc strict] f x = x
            ^
  Zero-alloc strict annotation

  $ syn_doc_name zero_alloc.ml 4 18
  let[@zero_alloc strict] f x = x
                    ^
  Zero-alloc strict annotation

  $ syn_doc_name zero_alloc.ml 6 3
    (g[@zero_alloc assume]) x
     ^
  No documentation found

  $ syn_doc_name zero_alloc.ml 6 10
    (g[@zero_alloc assume]) x
            ^
  Zero-alloc assume annotation

  $ syn_doc_name zero_alloc.ml 6 18
    (g[@zero_alloc assume]) x
                    ^
  Zero-alloc assume annotation

  $ syn_doc_name zero_alloc.ml 8 10
    val[@zero_alloc] f : int -> int
            ^
  Zero-alloc annotation

  $ syn_doc_name zero_alloc.ml 9 13
    val[@zero_alloc arity 1] f : t
               ^
  Zero-alloc arity annotation

  $ syn_doc_name zero_alloc.ml 9 21
    val[@zero_alloc arity 1] f : t
                       ^
  Zero-alloc arity annotation

  $ syn_doc_name zero_alloc.ml 9 24
    val[@zero_alloc arity 1] f : t
                          ^
  Zero-alloc arity annotation

  $ syn_doc_name zero_alloc.ml 11 44
  external id : 'a -> 'a = "%identity" [@@noalloc]
                                              ^
  Noalloc annotation
  $ syn_doc_name zero_alloc.ml 12 10
  let[@zero_alloc assume_unless_opt] f x = x
            ^
  Zero-alloc assume_unless_opt annotation

  $ syn_doc_name zero_alloc.ml 12 18
  let[@zero_alloc assume_unless_opt] f x = x
                    ^
  Zero-alloc assume_unless_opt annotation

// inlining annotations

  $ cat > inlining.ml << EOF
  > let[@inline always] f x = x
  > let[@inline never] f x = x
  > let[@inline available] f x = x
  > let[@inline] f x = x
  > let () = (f [@inlined always]) 0
  > let () = (f [@inlined never]) 0
  > let () = (f [@inlined hint]) 0
  > let () = (f [@inlined]) 0
  > let () = (f [@loop always]) 0
  > let () = (f [@loop never]) 0
  > let () = (f [@loop]) 0
  > let () = (f [@unrolled 10]) 0
  > EOF

  $ syntax_errors inlining.ml

  $ syn_doc_name inlining.ml 1 10
  let[@inline always] f x = x
            ^
  Inline always annotation

  $ syn_doc_name inlining.ml 1 15
  let[@inline always] f x = x
                 ^
  Inline always annotation

  $ syn_doc_name inlining.ml 2 10
  let[@inline never] f x = x
            ^
  Inline never annotation

  $ syn_doc_name inlining.ml 2 13
  let[@inline never] f x = x
               ^
  Inline never annotation

  $ syn_doc_name inlining.ml 3 10
  let[@inline available] f x = x
            ^
  Inline available annotation

  $ syn_doc_name inlining.ml 3 13
  let[@inline available] f x = x
               ^
  Inline available annotation

  $ syn_doc_name inlining.ml 4 10
  let[@inline] f x = x
            ^
  Inline always annotation

  $ syn_doc_name inlining.ml 5 17
  let () = (f [@inlined always]) 0
                   ^
  Inlined always annotation

  $ syn_doc_name inlining.ml 5 25
  let () = (f [@inlined always]) 0
                           ^
  Inlined always annotation

  $ syn_doc_name inlining.ml 6 17
  let () = (f [@inlined never]) 0
                   ^
  Inlined never annotation

  $ syn_doc_name inlining.ml 6 25
  let () = (f [@inlined never]) 0
                           ^
  Inlined never annotation

  $ syn_doc_name inlining.ml 7 17
  let () = (f [@inlined hint]) 0
                   ^
  Inlined hint annotation

  $ syn_doc_name inlining.ml 7 25
  let () = (f [@inlined hint]) 0
                           ^
  Inlined hint annotation

  $ syn_doc_name inlining.ml 8 17
  let () = (f [@inlined]) 0
                   ^
  Inlined always annotation

  $ syn_doc_name inlining.ml 9 17
  let () = (f [@loop always]) 0
                   ^
  Loop always annotation

  $ syn_doc_name inlining.ml 9 22
  let () = (f [@loop always]) 0
                        ^
  Loop always annotation

  $ syn_doc_name inlining.ml 10 17
  let () = (f [@loop never]) 0
                   ^
  Loop never annotation

  $ syn_doc_name inlining.ml 10 22
  let () = (f [@loop never]) 0
                        ^
  Loop never annotation

  $ syn_doc_name inlining.ml 11 17
  let () = (f [@loop]) 0
                   ^
  Loop always annotation

  $ syn_doc_name inlining.ml 12 18
  let () = (f [@unrolled 10]) 0
                    ^
  unrolled annotation

  $ syn_doc_name inlining.ml 12 24
  let () = (f [@unrolled 10]) 0
                          ^
  unrolled annotation

// module strengthening

  $ cat > module_strengthening.ml << EOF
  > module type S = sig end
  > module M = struct end
  > module type S = S with M
  > EOF

  $ syntax_errors module_strengthening.ml

  $ syn_doc_name module_strengthening.ml 3 16
  module type S = S with M
                  ^
  No documentation found

  $ syn_doc_name module_strengthening.ml 3 20
  module type S = S with M
                      ^
  Module strengthening

  $ syn_doc_name module_strengthening.ml 3 23
  module type S = S with M
                         ^
  No documentation found

Validate that docstrings, URLs, and levels are being created correctly

  $ cat > validate.ml << EOF
  > let rec name1 = 1 :: name2 and name2 = 2 :: name1
  > type t : value
  > EOF

  $ $MERLIN single syntax-document -position 1:6 -filename validate.ml < validate.ml | jq .value
  {
    "name": "Recursive value definition",
    "description": "Supports a certain class of recursive definitions of non-functional values.",
    "url": "https://ocaml.org/manual/5.2/letrecvalues.html",
    "level": "simple"
  }

  $ $MERLIN single syntax-document -position 2:11 -filename validate.ml < validate.ml | jq .value
  {
    "name": "Kind abbreviation",
    "description": "The kind of ordinary OCaml types",
    "url": "https://oxcaml.org/documentation/kinds/syntax/",
    "level": "advanced"
  }
