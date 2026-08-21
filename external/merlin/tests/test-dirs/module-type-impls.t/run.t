The [module-type-impls] query answers from the compiler facts recorded in the
configured indexes.  Compile test programs with the compiler under test so the
resulting artifacts contain the facts channel.

  $ print-results () {
  >   local module_type="${1-}"
  >   jq -r --arg module_type "$module_type" '
  >     def position: "\(.line):\(.col)";
  >     .value.status,
  >     ([.value.implementations[]
  >       | select(($module_type == "") or (.target == $module_type))]
  >      | sort_by([.start.line,
  >                 .start.col,
  >                 .end.line,
  >                 .end.col,
  >                 (.name // ""),
  >                 (.check // .kind // "")])
  >      | .[]
  >      | [(.name // "<anon>"),
  >         (.start | position),
  >         (.end | position),
  >         (.check // .kind // "<none>")]
  >      | join(" "))'
  > }

  $ impls-of () {
  >   local module_type="$1"
  >   cat > main.ml
  >   "$MERLIN_TEST_OCAML_PATH/bin/ocamlc" -bin-annot -c main.ml || return
  >   ocaml-index aggregate main.cmt -o module-types.ocaml-index || return
  >   $MERLIN single module-type-impls \
  >     -index-file ./module-types.ocaml-index \
  >     -filename ./main.ml < ./main.ml \
  >     | print-results "$module_type"
  > }

Every module checked against [S] is returned.  Named modules carry their UID
and source name in the raw response; expression-based implementation sites are
shown as [<anon>].

  $ impls-of S <<'EOF'
  > module type S = sig
  >   type t
  > end
  >
  > module M : S = struct
  >   type t = int
  > end
  >
  > module N : S = struct
  >   type t = string
  > end
  > EOF
  complete
  M 5:7 5:8 annotation
  N 9:7 9:8 annotation

Nested module-type aliases retain their dependency on the top-level [S].  Both
the module that defines the alias and a module ascribed to it are affected.

  $ impls-of S <<'EOF'
  > module type S = sig
  >   type u
  > end
  >
  > module type Outer = sig
  >   module type Inner = S
  > end
  >
  > module O : Outer = struct
  >   module type Inner = S
  > end
  >
  > module P : O.Inner = struct
  >   type u = bool
  > end
  > EOF
  complete
  O 9:7 9:8 annotation
  P 13:7 13:8 annotation

Module-type aliases can form a chain before reaching an implementation.

  $ impls-of S <<'EOF'
  > module type S = sig
  >   type t
  > end
  >
  > module type Alias = S
  > module type Alias_of_alias = Alias
  >
  > module M : Alias_of_alias = struct
  >   type t = int
  > end
  > EOF
  complete
  M 8:7 8:8 annotation

Including a module type should retain the relationship with the included
module type.

  $ impls-of S <<'EOF'
  > module type S = sig
  >   type t
  > end
  >
  > module type Extended = sig
  >   include S
  >   val make : unit -> t
  > end
  >
  > module M : Extended = struct
  >   type t = int
  >   let make () = 0
  > end
  > EOF
  complete
  M 10:7 10:8 annotation

Including the result of a functor application combines include, application,
projection, and alias contexts.

  $ impls-of S <<'EOF'
  > module type S = sig
  >   type t
  > end
  >
  > module Make (X : sig type t end) = struct
  >   module Result : S with type t = X.t = struct
  >     type t = X.t
  >   end
  > end
  >
  > module Argument = struct
  >   type t = int
  > end
  >
  > module Reexported = struct
  >   include Make (Argument)
  > end
  >
  > module Alias = Reexported.Result
  > EOF
  complete
  Result 6:9 6:15 annotation

A module type obtained through [module type of] should preserve the provenance
of the module whose type was inspected.

  $ impls-of S <<'EOF'
  > module type S = sig
  >   type t
  >   val value : t
  > end
  >
  > module Prototype : S = struct
  >   type t = int
  >   let value = 0
  > end
  >
  > module type Derived = module type of struct
  >   include Prototype
  > end
  >
  > module Copy : Derived = struct
  >   type t = Prototype.t
  >   let value = Prototype.value
  > end
  > EOF
  complete
  Prototype 6:7 6:16 annotation
  Copy 15:7 15:11 annotation

Destructive module-type substitution should connect the substituted signature
member to the replacement module type.

  $ impls-of S <<'EOF'
  > module type S = sig
  >   type t
  > end
  >
  > module type Carrier = sig
  >   module type Element
  >   module Value : Element
  > end
  >
  > module type Specialized =
  >   Carrier with module type Element := S
  >
  > module M : Specialized = struct
  >   module Value : S = struct
  >     type t = int
  >   end
  > end
  > EOF
  complete
  M 13:7 13:8 annotation
  Value 14:9 14:14 annotation

  $ impls-of S <<'EOF'
  > module type S = sig
  >   type t
  > end
  >
  > module type Carrier = sig
  >   module type Element
  >   module Value : Element
  > end
  >
  > module type Specialized =
  >   Carrier with module type Element := S
  >
  > module M : Specialized = struct
  >   module Value = struct
  >     type t = int
  >   end
  > end
  > EOF
  complete
  M 13:7 13:8 annotation

Repeated applications of an applicative functor exercise congruence and
deduplication of application contexts.

  $ impls-of S <<'EOF'
  > module type S = sig
  >   type t
  > end
  >
  > module type Argument = sig
  >   type t
  > end
  >
  > module Make (X : Argument) : S with type t = X.t = struct
  >   type t = X.t
  > end
  >
  > module A = struct
  >   type t = int
  > end
  >
  > module First = Make (A)
  > module Second = Make (A)
  > EOF
  complete
  <anon> 9:27 11:3 annotation

Projecting a result from an applied functor combines application and projection
contexts.

  $ impls-of S <<'EOF'
  > module type S = sig
  >   val value : int
  > end
  >
  > module type Argument = sig
  >   val value : int
  > end
  >
  > module Make (X : Argument) = struct
  >   module Result : S = struct
  >     let value = X.value
  >   end
  > end
  >
  > module A = struct
  >   let value = 1
  > end
  >
  > module Built = Make (A)
  > module Projected = Built.Result
  > EOF
  complete
  Result 10:9 10:15 annotation

Functor applications with anonymous arguments should still produce stable
query results.

  $ impls-of S <<'EOF'
  > module type S = sig
  >   val value : int
  > end
  >
  > module Make (X : sig val value : int end) : S = struct
  >   let value = X.value
  > end
  >
  > module M = Make (struct
  >   let value = 1
  > end)
  > EOF
  complete
  <anon> 5:42 7:3 annotation

Packing and unpacking a module crosses the first-class module boundary.

  $ impls-of S <<'EOF'
  > module type S = sig
  >   type t
  >   val value : t
  > end
  >
  > module Original : S = struct
  >   type t = int
  >   let value = 0
  > end
  >
  > let packed = (module Original : S)
  > module Unpacked = (val packed : S)
  > EOF
  complete
  Original 6:7 6:15 annotation
  <anon> 11:21 11:29 package

Mutually recursive modules put multiple annotations in the same recursive
group.

  $ impls-of S <<'EOF'
  > module type S = sig
  >   val value : unit -> int
  > end
  >
  > module rec Left : S = struct
  >   let value () = Right.value ()
  > end
  > and Right : S = struct
  >   let value () = 1
  > end
  > EOF
  complete
  Left 5:11 5:15 annotation
  Right 8:4 8:9 annotation

A higher-order functor receives an applicative functor, applies it inside its
body, and exposes the result through a second application context.

  $ impls-of S <<'EOF'
  > module type S = sig
  >   type t
  > end
  >
  > module type Argument = sig
  >   type t
  > end
  >
  > module type Producer =
  >   functor (X : Argument) -> S with type t = X.t
  >
  > module Base (X : Argument) : S with type t = X.t = struct
  >   type t = X.t
  > end
  >
  > module Apply (F : Producer) (X : Argument) : S with type t = X.t =
  >   F (X)
  >
  > module A = struct
  >   type t = int
  > end
  >
  > module Result = Apply (Base) (A)
  > EOF
  partial
  Base 12:7 12:11 argument
  <anon> 12:27 14:3 annotation
  <anon> 16:43 17:7 annotation

Independently repeated applications of a functor returning another functor
should converge on the same nested result family.

  $ impls-of S <<'EOF'
  > module type S = sig
  >   type t
  > end
  >
  > module type Argument = sig
  >   type t
  > end
  >
  > module Outer (X : Argument) = struct
  >   module Inner (Y : Argument) : S with type t = X.t * Y.t = struct
  >     type t = X.t * Y.t
  >   end
  > end
  >
  > module A = struct
  >   type t = int
  > end
  >
  > module B = struct
  >   type t = string
  > end
  >
  > module Partial = Outer (A)
  > module Via_partial = Partial.Inner (B)
  > module Partial_again = Outer (A)
  > module Via_repeated = Partial_again.Inner (B)
  > EOF
  complete
  <anon> 10:30 12:5 annotation

A functor parameter carries both a module-type member and a module checked
against that member; the result reexports the member under a new projection.

  $ impls-of S <<'EOF'
  > module type S = sig
  >   type t
  > end
  >
  > module type Input = sig
  >   module type T = S
  >   module Value : T
  > end
  >
  > module Consume (X : Input) = struct
  >   module type T = X.T
  >   module Copy : T = X.Value
  > end
  >
  > module A = struct
  >   module type T = S
  >   module Value : T = struct
  >     type t = int
  >   end
  > end
  >
  > module Built = Consume (A)
  > module Alias = Built.Copy
  > EOF
  complete
  A 15:7 15:8 argument
  Value 17:9 17:14 annotation

[module type of] follows a projection from an applicative functor result, then
the captured type constrains another alias of that projection.

  $ impls-of S <<'EOF'
  > module type S = sig
  >   type t
  > end
  >
  > module type Argument = sig
  >   type t
  > end
  >
  > module Make (X : Argument) = struct
  >   module Witness : S with type t = X.t = struct
  >     type t = X.t
  >   end
  > end
  >
  > module A = struct
  >   type t = int
  > end
  >
  > module Built = Make (A)
  > module type Snapshot = module type of Built.Witness
  > module Copy : Snapshot = Built.Witness
  > EOF
  complete
  Witness 10:9 10:16 annotation
  Copy 21:7 21:11 annotation

Multiple nested [with module] constraints force two signature projections to
the same implementation before the constrained signature is implemented.

  $ impls-of S <<'EOF'
  > module type S = sig
  >   type t
  > end
  >
  > module Concrete = struct
  >   type t = int
  > end
  >
  > module type Container = sig
  >   module Selected : S
  >   module Nested : sig
  >     module Item : S
  >   end
  > end
  >
  > module type Fixed =
  >   Container
  >   with module Selected = Concrete
  >    and module Nested.Item = Concrete
  >
  > module M : Fixed = struct
  >   module Selected = Concrete
  >   module Nested = struct
  >     module Item = Concrete
  >   end
  > end
  > EOF
  complete
  Concrete 5:7 5:15 annotation
  Concrete 5:7 5:15 annotation
  M 21:7 21:8 annotation
  Nested 23:9 23:15 annotation

Two signature includes form a diamond whose leaves independently refer to the
same module type; the implementation relies on member pairing rather than
direct annotations.

  $ impls-of S <<'EOF'
  > module type S = sig
  >   type t
  > end
  >
  > module type Left = sig
  >   module L : S
  > end
  >
  > module type Right = sig
  >   module R : S
  > end
  >
  > module type Diamond = sig
  >   include Left
  >   include Right
  > end
  >
  > module M : Diamond = struct
  >   module L = struct
  >     type t = int
  >   end
  >   module R = struct
  >     type t = string
  >   end
  > end
  > EOF
  complete
  M 18:7 18:8 annotation
  L 19:9 19:10 annotation
  R 22:9 22:10 annotation

Including a doubly applied functor with anonymous arguments anchors the result
at an unnamed site while exporting an alias of [S] used afterward.

  $ impls-of S <<'EOF'
  > module type S = sig
  >   type t
  > end
  >
  > module type Result = sig
  >   module type T = S
  > end
  >
  > module Build
  >     (X : sig type t end)
  >     (Y : sig type u end) : Result = struct
  >   module type T = S
  > end
  >
  > include Build
  >     (struct type t = int end)
  >     (struct type u = string end)
  >
  > module M : T = struct
  >   type t = int * string
  > end
  > EOF
  complete
  <anon> 11:25 13:3 annotation

Generative applications of the same partially applied functor must remain
distinct while their projected result modules retain the same family.

  $ impls-of S <<'EOF'
  > module type S = sig
  >   type t
  > end
  >
  > module type Argument = sig
  >   type t
  > end
  >
  > module Make (X : Argument) () = struct
  >   module Result : S with type t = X.t = struct
  >     type t = X.t
  >   end
  > end
  >
  > module A = struct
  >   type t = int
  > end
  >
  > module First = Make (A) ()
  > module Second = Make (A) ()
  > module First_result = First.Result
  > module Second_result = Second.Result
  > EOF
  complete
  Result 10:9 10:15 annotation

Alias-preserving and alias-removing forms of [module type of] derive signatures
from the same module and are both used in later annotations.

  $ impls-of S <<'EOF'
  > module type S = sig
  >   type t
  > end
  >
  > module Base = struct
  >   module Inner : S = struct
  >     type t = int
  >   end
  > end
  >
  > module type Preserved = module type of struct
  >   include Base
  > end
  >
  > module type Removed = module type of struct
  >   include Base
  > end [@remove_aliases]
  >
  > module P : Preserved = Base
  > module R : Removed = struct
  >   module Inner = Base.Inner
  > end
  > EOF
  partial
  Inner 6:9 6:14 annotation
  Inner 21:9 21:14 annotation

An implementation ascribed to a functor module type joins parameter members,
result members, aliases, and the eventual application instance.

  $ impls-of S <<'EOF'
  > module type S = sig
  >   type t
  > end
  >
  > module type Input = sig
  >   module type T = S
  >   module Value : T
  > end
  >
  > module type Transformer =
  >   functor (X : Input) -> sig
  >     module type T = X.T
  >     module Value : T
  >   end
  >
  > module Transform : Transformer = functor (X : Input) -> struct
  >   module type T = X.T
  >   module Value : T = X.Value
  > end
  >
  > module A = struct
  >   module type T = S
  >   module Value : T = struct
  >     type t = int
  >   end
  > end
  >
  > module Result = Transform (A)
  > module Alias = Result.Value
  > EOF
  complete
  Transform 16:7 16:16 annotation
  A 21:7 21:8 argument
  Value 23:9 23:14 annotation
