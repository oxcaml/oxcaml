The [module-type-impls] query answers from the compiler facts recorded in the
configured indexes.  Compile test programs with the compiler under test so the
resulting artifacts contain the facts channel.

Every expected block below is the contract: it states exactly the correct
answer for its case.  A failing diff is an open defect; inspect it, decide
what the correct answer is, and fix the implementation until that answer is
produced.  Never promote a failure into the expectations.

  $ print_results () {
  >   local module_type="${1-}"
  >   jq -r --arg module_type "$module_type" '
  >     def position: "\(.line):\(.col)";
  >     (.value.targets[]
  >      | select(.target == $module_type)
  >      | .status),
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

  $ impls_of () {
  >   local module_type="$1"
  >   cat > main.ml
  >   $OCAMLC -bin-annot -c main.ml || return
  >   ocaml-index aggregate main.cmt -o module-types.ocaml-index || return
  >   $MERLIN single module-type-impls \
  >     -index-file ./module-types.ocaml-index \
  >     -filename ./main.ml < ./main.ml \
  >     | print_results "$module_type"
  > }

Multi-file scenarios follow one shape: compile the files in dependency order,
aggregate their artifacts into an index, then query one module type by the
position of its declaration, computed from the source so the tests never
hard-code coordinates.

  $ setup_index () {
  >   local artifacts=()
  >   for file in "$@"; do
  >     $OCAMLC -bin-annot -c "$file" || return
  >     case "$file" in
  >       *.mli) artifacts+=("${file%.mli}.cmti") ;;
  >       *) artifacts+=("${file%.ml}.cmt") ;;
  >     esac
  >   done
  >   ocaml-index aggregate "${artifacts[@]}" -o project.ocaml-index
  > }

  $ position_of_module_type () {
  >   awk -v name="$1" '
  >     { column = index($0, "module type " name)
  >       if (column) { printf "%d:%d", NR, column + 11; exit } }' "$2"
  > }

  $ impls_of_module_type () {
  >   local name="$1" file="$2" target="${3-$1}"
  >   $MERLIN single module-type-impls \
  >     -position "$(position_of_module_type "$name" "$file")" \
  >     -index-file ./project.ocaml-index \
  >     -filename "./$file" < "./$file" \
  >     | print_results "$target"
  > }

Every module checked against [S] is returned.  Named modules carry their UID
and source name in the raw response; expression-based implementation sites are
shown as [<anon>].

  $ impls_of S <<EOF
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

Nested module-type aliases retain their dependency on the top-level [S]: a
module ascribed to the nested alias implements [S].  The module that merely
provides the alias as a member does not.

  $ impls_of S <<EOF
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
  P 13:7 13:8 annotation

Module-type aliases can form a chain before reaching an implementation.

  $ impls_of S <<EOF
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

  $ impls_of S <<EOF
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

An anonymous ascription checks its module members just like a named binding
does.  The member [N] implements [S], not the containing structure.

  $ impls_of S <<'EOF'
  > module type S = sig
  >   type t
  > end
  > 
  > include (struct
  >   module N = struct
  >     type t = bool
  >   end
  > end : sig module N : S end)
  > EOF
  complete
  N 6:9 6:10 annotation

Including the result of a functor application combines include, application,
projection, and alias contexts.

  $ impls_of S <<EOF
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

  $ impls_of S <<EOF
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

  $ impls_of S <<EOF
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
  Value 14:9 14:14 annotation

  $ impls_of S <<EOF
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
  Value 14:9 14:14 annotation

Destructive type substitution removes a requirement of the original signature.
A module checked against the resulting empty signature does not implement [S].

  $ impls_of S <<'EOF'
  > module type S = sig
  >   type t
  > end
  > 
  > module type Removed = S with type t := int
  > 
  > module Gone : Removed = struct end
  > EOF
  complete

  $ impls_of S <<'EOF'
  > module type S = sig
  >   val value : int
  > end
  > 
  > module type Base = sig
  >   include S
  >   type t
  > end
  > 
  > module type Removed = Base with type t := int
  > 
  > module M : Removed = struct
  >   let value = 0
  > end
  > EOF
  complete
  M 12:7 12:8 annotation

  $ impls_of S <<'EOF'
  > module type S = sig val value : int end
  > 
  > module Outer = struct
  >   module type Alias = S
  >   module type Base = sig
  >     include Alias
  >     type t
  >   end
  >   module type Reduced = Base with type t := int
  >   module M : Reduced = struct let value = 0 end
  > end
  > EOF
  complete
  M 10:9 10:10 annotation

Repeated applications of an applicative functor exercise congruence and
deduplication of application contexts.

  $ impls_of S <<EOF
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

  $ impls_of S <<EOF
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

  $ impls_of S <<EOF
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

Passing a module to a functor checks its members against the parameter's
signature.  [A.M] implements [S] even without a direct annotation on [M];
[A] itself does not implement [S].

  $ impls_of S <<'EOF'
  > module type S = sig
  >   type t
  > end
  > 
  > module type Outer = sig
  >   module M : S
  > end
  > 
  > module A = struct
  >   module M = struct
  >     type t = int
  >   end
  > end
  > 
  > module F (X : Outer) = struct end
  > 
  > module R = F (A)
  > EOF
  complete
  M 10:9 10:10 argument

  $ impls_of S <<'EOF'
  > module type S = sig
  >   type t
  > end
  > 
  > module type Outer = sig
  >   module M : S
  > end
  > 
  > module F (X : Outer) = struct end
  > 
  > module R = F (struct
  >   module M = struct
  >     type t = int
  >   end
  > end)
  > EOF
  complete
  M 12:9 12:10 argument

  $ impls_of S <<'EOF'
  > module type S = sig val x : int end
  > module type U = sig val y : bool end
  > module A = struct module type T = S end
  > module B = struct module type T = U end
  > module F (X : sig module type T end) (Y : sig module M : X.T end) = struct end
  > module Arg_a = struct module M = struct let x = 1 end end
  > module Arg_b = struct module M = struct let y = true end end
  > module First = F (A) (Arg_a)
  > module Second = F (B) (Arg_b)
  > EOF
  partial
  M 6:29 6:30 argument

  $ $MERLIN single module-type-impls \
  >   -index-file ./module-types.ocaml-index \
  >   -filename ./main.ml < ./main.ml \
  >   | print_results U
  partial
  M 7:29 7:30 argument

A nested argument member implements [S] when its signature is reached through
a module-type alias declared inside the parameter signature.  [A.N.M]
implements [S]; neither [A] nor [A.N] does.

  $ impls_of S <<'EOF'
  > module type S = sig val x : int end
  > module F (X : sig
  >   module type T = sig module M : S end
  >   module N : T
  > end) = struct end
  > module A = struct
  >   module type T = sig module M : S end
  >   module N = struct
  >     module M = struct let x = 1 end
  >   end
  > end
  > module R = F (A)
  > EOF
  complete
  M 9:11 9:12 argument

Packing and unpacking a module crosses the first-class module boundary.

  $ impls_of S <<EOF
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

  $ impls_of S <<EOF
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

  $ impls_of S <<EOF
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
  complete
  <anon> 12:27 14:3 annotation
  <anon> 16:43 17:7 annotation

Independently repeated applications of a functor returning another functor
should converge on the same nested result family.

  $ impls_of S <<EOF
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

  $ impls_of S <<EOF
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
  Value 17:9 17:14 annotation

[module type of] follows a projection from an applicative functor result, then
the captured type constrains another alias of that projection.

  $ impls_of S <<EOF
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

  $ impls_of S <<EOF
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

A [with module] constraint on a member does not make the containing module
implement that member's type.  [Concrete] and [M.N] implement [S], but [M]
does not provide the required type [t].

  $ impls_of S <<'EOF'
  > module type S = sig
  >   type t
  > end
  > 
  > module Concrete : S = struct
  >   type t = int
  > end
  > 
  > module type Outer = sig
  >   module N : S
  > end
  > 
  > module type Fixed = Outer with module N = Concrete
  > 
  > module M : Fixed = struct
  >   module N = Concrete
  > end
  > EOF
  complete
  Concrete 5:7 5:15 annotation
  N 16:9 16:10 annotation

A [with module type] constraint determines the type implemented by a member
whose annotation refers to that module type.  The constraint is specific to
each instance: [A.M] implements [S], while [B.M] implements [U].

  $ impls_of S <<'EOF'
  > module type S = sig val x : int end
  > module type U = sig val y : bool end
  > module type Outer = sig
  >   module type T
  >   module M : T
  > end
  > module A : Outer with module type T = S = struct
  >   module type T = S
  >   module M = struct let x = 1 end
  > end
  > module B : Outer with module type T = U = struct
  >   module type T = U
  >   module M = struct let y = true end
  > end
  > EOF
  complete
  M 9:9 9:10 annotation

  $ $MERLIN single module-type-impls \
  >   -index-file ./module-types.ocaml-index \
  >   -filename ./main.ml < ./main.ml \
  >   | print_results U
  complete
  M 13:9 13:10 annotation

Two signature includes form a diamond whose leaves independently refer to the
same module type; the implementation relies on member pairing rather than
direct annotations.

  $ impls_of S <<EOF
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
  L 19:9 19:10 annotation
  R 22:9 22:10 annotation

Including a doubly applied functor exports an alias of [S]; a module ascribed
to the exported alias implements [S].  The functor body that merely provides
the alias as a member does not.

  $ impls_of S <<EOF
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
  M 19:7 19:8 annotation

Generative applications of the same partially applied functor must remain
distinct while their projected result modules retain the same family.

  $ impls_of S <<EOF
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

  $ impls_of S <<EOF
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

  $ impls_of S <<EOF
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
  Value 23:9 23:14 annotation

With [-position], the query answers for exactly one module type: the
innermost module-type declaration enclosing the position.  The buffer is
still what identifies the declaration, so the selection is deterministic.

  $ cat > one.ml <<'EOF'
  > module type S = sig
  >   type t
  > end
  > module type Outer = sig
  >   module type Inner = S
  > end
  > module M : S = struct
  >   type t = int
  > end
  > EOF
  $ $OCAMLC -bin-annot -c one.ml
  $ ocaml-index aggregate one.cmt -o one.ocaml-index
  $ $MERLIN single module-type-impls \
  >   -position 1:13 \
  >   -index-file ./one.ocaml-index \
  >   -filename ./one.ml < ./one.ml \
  >   | jq -r '.value.targets[].target'
  S

Inside [Outer]'s body the innermost enclosing declaration is [Outer.Inner],
not [Outer].

  $ $MERLIN single module-type-impls \
  >   -position 5:14 \
  >   -index-file ./one.ocaml-index \
  >   -filename ./one.ml < ./one.ml \
  >   | jq -r '.value.targets[].target'
  Outer.Inner

A position enclosed by no module-type declaration is an explicit failure,
never an empty answer.

  $ $MERLIN single module-type-impls \
  >   -position 8:2 \
  >   -index-file ./one.ocaml-index \
  >   -filename ./one.ml < ./one.ml \
  >   | jq -r '"\(.class): \(.value)"'
  failure: No module-type declaration at this position

The [_intf.ml] pattern: a signature lives in its own unit and an [.mli]
includes it.  Selecting [S] by position in [foo_intf.ml] is how the module
type [Foo_intf.S] is specified, and the implementers of the signature it
denotes are the module annotated with it directly and the unit whose
interface includes it.

  $ cat > foo_intf.ml <<'EOF'
  > module type S = sig
  >   type t
  > end
  > EOF
  $ cat > foo.mli <<'EOF'
  > include Foo_intf.S
  > EOF
  $ cat > foo.ml <<'EOF'
  > type t = int
  > EOF
  $ cat > bar.ml <<'EOF'
  > module Another : Foo_intf.S = struct
  >   type t = { mutable field : string }
  > end
  > EOF
  $ setup_index foo_intf.ml foo.mli foo.ml bar.ml
  $ impls_of_module_type S foo_intf.ml
  complete
  Foo 0:-1 0:-1 interface
  Another 1:7 1:14 annotation

A module bound in an expression implements a module type like a structure
binding does: the check is attributed to the binding's own declaration, so it
is reported under the binding's name and position.

  $ impls_of S <<'EOF'
  > module type S = sig
  >   type t
  > end
  > 
  > let f () =
  >   let module Local : S = struct
  >     type t = int
  >   end in
  >   ()
  > EOF
  complete
  Local 6:13 6:18 annotation

The scope of a [let module] is the bound module expression alone.  A module
type declared inside a binding is a member of that binding only, not of an
earlier sibling, so an annotation against it still joins its declaration.

  $ impls_of S <<'EOF'
  > module type S = sig
  >   type t
  > end
  > 
  > let f () =
  >   let module M = struct
  >     type t = int
  >   end in
  >   let module Holder = struct
  >     module type T = S
  >   end in
  >   let module Impl : Holder.T = struct
  >     type t = int
  >   end in
  >   ()
  > EOF
  complete
  Impl 12:13 12:17 annotation

A module unpacked by a pattern is checked against its package type just like
a module packed by an expression, so both sites implement [S].

  $ impls_of S <<'EOF'
  > module type S = sig
  >   type t
  > end
  > 
  > module M = struct
  >   type t = int
  > end
  > 
  > let packed = (module M : S)
  > 
  > let unpack (module X : S) = ()
  > EOF
  complete
  <anon> 9:21 9:22 package
  <anon> 11:19 11:20 package

The result signature of a functor declared in an [.mli] is what a client's
applications instantiate: a client checked against [F(A).T] implements the
[S] that [T] aliases, and the argument implements the parameter's [S].

  $ cat > ifun.mli <<'EOF'
  > module type S = sig
  >   type t
  > end
  > module F (X : S) : sig
  >   module type T = S
  > end
  > EOF
  $ cat > ifun.ml <<'EOF'
  > module type S = sig
  >   type t
  > end
  > module F (X : S) = struct
  >   module type T = S
  >   type witness = X.t
  > end
  > EOF
  $ cat > fclient.ml <<'EOF'
  > module A = struct
  >   type t = int
  > end
  > 
  > module Z : Ifun.F(A).T = struct
  >   type t = int
  > end
  > EOF
  $ setup_index ifun.mli ifun.ml fclient.ml
  $ impls_of_module_type S ifun.mli
  complete
  A 1:7 1:8 argument
  Z 5:7 5:8 annotation

A declaration nested inside a module-type body is paired with the [.mli]'s
declaration during the interface check, exactly like its toplevel siblings,
so implementations checked in the [.ml] surface when the query resolves the
buffer's [Container.Local] declaration.

  $ cat > cont.mli <<'EOF'
  > module type S = sig
  >   type t
  > end
  > module type Container = sig
  >   module type Local = S
  >   module Member : S
  > end
  > module C : Container
  > EOF
  $ cat > cont.ml <<'EOF'
  > module type S = sig
  >   type t
  > end
  > module type Container = sig
  >   module type Local = S
  >   module Member : S
  > end
  > module C : Container = struct
  >   module type Local = S
  >   module Member = struct
  >     type t = int
  >   end
  > end
  > module Impl : C.Local = struct
  >   type t = int
  > end
  > EOF
  $ setup_index cont.mli cont.ml
  $ impls_of_module_type Local cont.mli Container.Local
  complete
  Impl 14:7 14:11 annotation

A partial artifact has no facts channel.  Combining it with an artifact that
has facts must not turn an unavailable answer into a complete empty answer.
The availability is the same whether the artifacts are indexed separately
or aggregated together.

  $ cat > channel.ml <<'EOF'
  > module type S = sig val x : int end
  > EOF
  $ cat > incomplete.ml <<'EOF'
  > module M : Channel.S = struct let x = 1 end
  > let broken : int = true
  > EOF
  $ $OCAMLC -bin-annot -c channel.ml
  $ $OCAMLC -bin-annot -c incomplete.ml > incomplete.log 2>&1
  [2]
  $ ocaml-index aggregate channel.cmt -o channel.ocaml-index
  $ ocaml-index aggregate incomplete.cmt -o incomplete.ocaml-index
  $ $MERLIN single module-type-impls \
  >   -index-file ./channel.ocaml-index \
  >   -index-file ./incomplete.ocaml-index \
  >   -filename ./channel.ml < ./channel.ml \
  >   | print_results S
  unavailable

  $ ocaml-index aggregate channel.cmt incomplete.cmt -o combined.ocaml-index
  $ $MERLIN single module-type-impls \
  >   -index-file ./combined.ocaml-index \
  >   -filename ./channel.ml < ./channel.ml \
  >   | print_results S
  unavailable

A missing channel also survives merging existing indexes, with the missing
channel first rather than last.

  $ ocaml-index aggregate incomplete.ocaml-index channel.ocaml-index \
  >   -o merged.ocaml-index
  $ $MERLIN single module-type-impls \
  >   -index-file ./merged.ocaml-index \
  >   -filename ./channel.ml < ./channel.ml \
  >   | print_results S
  unavailable

A name that is not a module-type declaration of the buffer selects nothing:
the query only ever answers for the buffer's own declarations, identified by
their uids.

  $ impls_of Nonexistent <<'EOF'
  > module type S = sig
  >   type t
  > end
  > EOF
