Module-type implementation queries using indexed compiler facts.

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
  >   local module_type
  >   cat > main.ml
  >   $OCAMLC -bin-annot -c main.ml || return
  >   ocaml-index aggregate main.cmt -o module-types.ocaml-index || return
  >   for module_type in "$@"; do
  >     $MERLIN single module-type-impls \
  >       -index-file ./module-types.ocaml-index \
  >       -filename ./main.ml < ./main.ml \
  >       | print_results "$module_type"
  >   done
  > }

  $ setup_index () (
  >   for file in "$@"; do
  >     $OCAMLC -bin-annot -c "$file" || exit
  >     shift
  >     case "$file" in
  >       *.mli) set -- "$@" "${file%.mli}.cmti" ;;
  >       *) set -- "$@" "${file%.ml}.cmt" ;;
  >     esac
  >   done
  >   ocaml-index aggregate "$@" -o project.ocaml-index
  > )

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

Named implementations of [S].

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

A nested alias of [S] makes [P] an implementer; declaring the alias in [O]
does not.

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

Chains of module-type aliases.

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

Includes preserve module-type requirements.

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

An anonymous ascription makes the member [N] an implementer of [S].

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

Including a functor result and aliasing its member.

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

[module type of] preserves the original module's requirements.

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

  $ impls_of S <<EOF
  > module type S = sig
  >   type t = int
  > end
  > 
  > module M1 : S = struct
  >   type t = int
  > end
  > 
  > module M2 = struct
  >   include M1
  >   type u = t
  > end
  > 
  > module M3 : (module type of M2) = struct
  >   type t = int
  >   type u = int
  > end
  > EOF
  complete
  M1 5:7 5:9 annotation
  M3 14:7 14:9 annotation

Requirements survive successive includes, including anonymous structures.

  $ impls_of S <<'EOF'
  > module type S = sig val value : int end
  > module Original : S = struct let value = 0 end
  > module Forwarded = struct include Original end
  > module Extended = struct
  >   include struct include Forwarded end
  >   let extra = true
  > end
  > module Copy : module type of Extended = Extended
  > EOF
  complete
  Original 2:7 2:15 annotation
  Copy 8:7 8:11 annotation

Shadowing an included declaration replaces its requirements.

  $ impls_of S <<'EOF'
  > module type S = sig type t = int val value : int end
  > module Original : S = struct type t = int let value = 0 end
  > module Changed_value = struct
  >   include Original
  >   let value = true
  > end
  > module Value_copy : module type of Changed_value = Changed_value
  > module Changed_type = struct
  >   include Original
  >   type t = string
  > end
  > module Type_copy : module type of Changed_type = Changed_type
  > module Changed_by_include = struct
  >   include Original
  >   include struct let value = true end
  > end
  > module Include_copy : module type of Changed_by_include = Changed_by_include
  > EOF
  complete
  Original 2:7 2:15 annotation

Declarations in different namespaces do not shadow each other.

  $ impls_of S <<'EOF'
  > module type S = sig type t = int val value : int end
  > module Original : S = struct type t = int let value = 0 end
  > let result =
  >   let module Extended = struct
  >     include Original
  >     let t = true
  >     type value = bool
  >   end in
  >   let module Copy : module type of Extended = Extended in
  >   Copy.value
  > EOF
  complete
  Original 2:7 2:15 annotation
  Copy 9:13 9:17 annotation

Destructive module-type substitution.

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
  >   val foo : t
  > 
  >   val bar : t -> unit
  > end
  > 
  > module U : S = struct
  >   type t = string
  > 
  >   let foo = ""
  > 
  >   let bar _s = ()
  > end
  > 
  > module type Subbed = S with type t := int
  > 
  > module Impl : Subbed = struct
  >   let foo = 0
  >   let bar _i = ()
  > end
  > EOF
  complete
  U 8:7 8:8 annotation
  Impl 18:7 18:11 annotation

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

Requirements survive destructive substitution, even when it removes every
declaration.

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
  Gone 7:7 7:11 annotation

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

Repeated applications of an applicative functor are deduplicated.

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

Projecting a member from a functor result.

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

Functor applications with anonymous arguments.

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

Passing [A] to a functor makes its member [A.M] an implementer of [S].

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

  $ impls_of S U <<'EOF'
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
  partial
  M 7:29 7:30 argument

A parameter signature's nested alias makes [A.N.M] an implementer of [S].

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

Packing and unpacking modules.

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

Mutually recursive modules.

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

Higher-order functor applications.

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

Repeated applications of a functor returning another functor.

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

Reexporting a functor parameter's module type and module.

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

[module type of] applied to a projected functor result.

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

Nested [with module] constraints.

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

A [with module] constraint makes [Concrete] and [M.N] implementers of [S].

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

[with module type] constraints apply per instance: [A.M] implements [S], and
[B.M] implements [U].

  $ impls_of S U <<'EOF'
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
  complete
  M 13:9 13:10 annotation

Diamond-shaped signature includes with unannotated implementations.

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

An alias exported by a doubly applied functor makes [M] an implementer of [S].

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

Generative applications remain distinct while sharing a module-type family.

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

Alias-preserving and alias-removing forms of [module type of].

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

Ascription to a functor module type.

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

[-position] selects the innermost enclosing module-type declaration.

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

Inside [Outer.Inner], the selected declaration is [Outer.Inner].

  $ $MERLIN single module-type-impls \
  >   -position 5:14 \
  >   -index-file ./one.ocaml-index \
  >   -filename ./one.ml < ./one.ml \
  >   | jq -r '.value.targets[].target'
  Outer.Inner

A position outside any module-type declaration fails.

  $ $MERLIN single module-type-impls \
  >   -position 8:2 \
  >   -index-file ./one.ocaml-index \
  >   -filename ./one.ml < ./one.ml \
  >   | jq -r '"\(.class): \(.value)"'
  failure: No module-type declaration at this position

The [_intf.ml] pattern: [Foo] implements [S] through its interface;
[Another] has a direct annotation.

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

A local module is reported at its binding.

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

A local module type belongs to its enclosing binding, not an earlier
sibling.

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

Module packing and unpacking patterns both implement the package type.

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

Functor argument and result requirements declared in an [.mli].

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

Querying [Container.Local] in an [.mli] finds implementations in the [.ml].

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

Duplicate filenames: resolve both named ([Uid]) and anonymous ([Location])
implementations.

  $ mkdir -p path-resolution/query path-resolution/left path-resolution/right
  $ path_test_root="$(cd path-resolution && pwd -P)"
  $ cat > path-resolution/query/contracts.ml <<'EOF'
  > module type S = sig
  >   val value : string
  > end
  > EOF
  $ for directory in left right; do
  >   cat > "path-resolution/$directory/shared.ml" <<EOF
  > module Named : Contracts.S = struct let value = "$directory" end
  > let packed = (module struct let value = "$directory" end : Contracts.S)
  > EOF
  > done

Distinct compilation-unit names and rooted indexes. Check-site and
implementation paths must agree.

  $ index_source () (
  >   local directory="$1" unit="$2" source="$3"
  >   cd "$path_test_root/$directory" || exit
  >   $OCAMLC -bin-annot -I ../query -c -o "$unit.cmo" "$source" || exit
  >   ocaml-index aggregate "$unit.cmt" --root "./$directory" --rewrite-root \
  >     -o stanza.ocaml-index
  > )
  $ path_impls () {
  >   local file="$1"
  >   $MERLIN single module-type-impls \
  >     -position "$(position_of_module_type S "$file")" \
  >     -filename "$file" < "$file" \
  >     | jq -r --arg root "$path_test_root/" '
  >       def node_kind: if has("uid") then "Uid" else "Location" end;
  >       .value
  >       | .targets[].status,
  >         (.implementations
  >          | sort_by([.file, .start.line])
  >          | .[]
  >          | if .file != ."check-site".file then
  >              error("check-site path mismatch: \(."check-site".file)")
  >            else "\(node_kind) \(.file | ltrimstr($root))"
  >            end)'
  > }
  $ index_source query contracts contracts.ml
  $ index_source left left shared.ml
  $ index_source right right shared.ml

Separate directory indexes with [SOURCE_ROOT].

  $ cat > path-resolution/query/.merlin <<'EOF'
  > INDEX stanza.ocaml-index
  > INDEX ../left/stanza.ocaml-index
  > INDEX ../right/stanza.ocaml-index
  > SOURCE_ROOT ..
  > B .
  > S .
  > EOF
  $ (cd path-resolution/query && path_impls contracts.ml)
  complete
  Uid left/shared.ml
  Location left/shared.ml
  Uid right/shared.ml
  Location right/shared.ml

Global and stanza indexes give the same results without duplicates.

  $ (cd path-resolution && \
  >  ocaml-index aggregate query/stanza.ocaml-index right/stanza.ocaml-index \
  >    left/stanza.ocaml-index --root . --rewrite-root -o global.ocaml-index)
  $ cat > path-resolution/query/.merlin <<'EOF'
  > INDEX ../global.ocaml-index
  > INDEX stanza.ocaml-index
  > SOURCE_ROOT ..
  > B .
  > S .
  > EOF
  $ (cd path-resolution && path_impls query/contracts.ml)
  complete
  Uid left/shared.ml
  Location left/shared.ml
  Uid right/shared.ml
  Location right/shared.ml

An index without a facts channel makes discovery partial without losing
known implementations.

  $ cat > channel.ml <<'EOF'
  > module type S = sig val x : int end
  > module Kept : S = struct let x = 1 end
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
  partial
  Kept 2:7 2:11 annotation

Aggregation keeps available facts but does not track inputs with missing
channels.

  $ for suffix in cmt ocaml-index; do
  >   ocaml-index aggregate channel.$suffix incomplete.$suffix \
  >     -o project.ocaml-index
  >   impls_of_module_type S channel.ml
  >   ocaml-index aggregate incomplete.$suffix channel.$suffix \
  >     -o project.ocaml-index
  >   impls_of_module_type S channel.ml
  > done
  complete
  Kept 2:7 2:11 annotation
  complete
  Kept 2:7 2:11 annotation
  complete
  Kept 2:7 2:11 annotation
  complete
  Kept 2:7 2:11 annotation

If every input lacks a facts channel, the aggregate has none.

  $ for suffix in cmt ocaml-index; do
  >   ocaml-index aggregate incomplete.$suffix incomplete.$suffix \
  >     -o project.ocaml-index
  >   impls_of_module_type S channel.ml
  > done
  unavailable
  unavailable

Aggregation unions facts across inputs with missing channels.

  $ cat > additional.ml <<'EOF'
  > module Also : Channel.S = struct let x = 2 end
  > EOF
  $ $OCAMLC -bin-annot -c additional.ml
  $ ocaml-index aggregate additional.cmt -o additional.ocaml-index
  $ for suffix in cmt ocaml-index; do
  >   ocaml-index aggregate channel.$suffix incomplete.$suffix \
  >     additional.$suffix -o project.ocaml-index
  >   impls_of_module_type S channel.ml
  > done
  complete
  Also 1:7 1:11 annotation
  Kept 2:7 2:11 annotation
  complete
  Also 1:7 1:11 annotation
  Kept 2:7 2:11 annotation

An unknown module-type name selects nothing.

  $ impls_of Nonexistent <<'EOF'
  > module type S = sig
  >   type t
  > end
  > EOF
