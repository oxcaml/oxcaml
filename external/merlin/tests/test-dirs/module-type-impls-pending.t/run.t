Pending behavior of the [module-type-impls] query.

Each case in this file documents intended behavior: the expected output below
is the contract, and some of it is not produced yet because of known gaps in
the fact collector (see the module-type-index branch) or in the query itself.
A failing diff here is the specification of the fix; once a gap is closed, its
case should pass unchanged and can move into [module-type-impls.t].

The query takes no module-type selector: it reports every module-type
declaration of the buffer, each identified by its declaration uid.  The
selection below happens on the display side, so the tests stay deterministic.
The helpers are the same as in [module-type-impls.t], except that rows equal
up to their witness instance are printed once, in line order:

  $ print_results () {
  >   local module_type="${1-}"
  >   jq -r --arg module_type "$module_type" '
  >     def position: "\(.line):\(.col)";
  >     (.value.targets[]
  >      | select(.target == $module_type)
  >      | .status),
  >     ([.value.implementations[]
  >       | select(($module_type == "") or (.target == $module_type))]
  >      | map([(.name // "<anon>"),
  >             (.start | position),
  >             (.end | position),
  >             (.check // .kind // "<none>")]
  >            | join(" "))
  >      | unique
  >      | .[])'
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
  <anon> 11:19 11:20 package
  <anon> 9:21 9:22 package

A packed unit is checked against its [.mli] like any other unit, and members
compiled with [-for-pack] root their facts at the packed name, so references
between members join.

  $ cat > pmember.ml <<'EOF'
  > module type S = sig
  >   type t
  > end
  > EOF
  $ cat > puser.ml <<'EOF'
  > module M : Pmember.S = struct
  >   type t = int
  > end
  > EOF
  $ cat > pck.mli <<'EOF'
  > module Pmember : sig
  >   module type S = sig
  >     type t
  >   end
  > end
  > module Puser : sig
  >   module M : Pmember.S
  > end
  > EOF
  $ $OCAMLC -bin-annot -c pck.mli
  $ $OCAMLC -bin-annot -for-pack Pck -c pmember.ml
  $ $OCAMLC -bin-annot -for-pack Pck -c puser.ml
  $ $OCAMLC -bin-annot -pack -o pck.cmo pmember.cmo puser.cmo
  $ ocaml-index aggregate pck.cmti pck.cmt pmember.cmt puser.cmt \
  >   -o pack.ocaml-index

The pack was checked against [pck.mli], so querying the interface buffer
reports the pack as the [(interface)] implementation.

  $ $MERLIN single module-type-impls \
  >   -index-file ./pack.ocaml-index \
  >   -filename ./pck.mli < ./pck.mli \
  >   | print_results "(interface)"
  Pck 0:-1 0:-1 interface

A member annotated against a sibling member's module type implements it.

  $ $MERLIN single module-type-impls \
  >   -index-file ./pack.ocaml-index \
  >   -filename ./pmember.ml < ./pmember.ml \
  >   | print_results S
  complete
  M 1:7 1:8 annotation

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
  $ $OCAMLC -bin-annot -c ifun.mli ifun.ml fclient.ml
  $ ocaml-index aggregate ifun.cmti ifun.cmt fclient.cmt \
  >   -o ifun.ocaml-index
  $ $MERLIN single module-type-impls \
  >   -index-file ./ifun.ocaml-index \
  >   -filename ./ifun.mli < ./ifun.mli \
  >   | print_results S
  complete
  A 1:7 1:8 argument
  F 4:7 4:8 interface
  Ifun 0:-1 0:-1 interface
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
  $ $OCAMLC -bin-annot -c cont.mli cont.ml
  $ ocaml-index aggregate cont.cmti cont.cmt -o cont.ocaml-index
  $ $MERLIN single module-type-impls \
  >   -index-file ./cont.ocaml-index \
  >   -filename ./cont.mli < ./cont.mli \
  >   | print_results Container.Local
  complete
  C 8:7 8:8 annotation
  C 8:7 8:8 interface
  Cont 0:-1 0:-1 interface
  Impl 14:7 14:11 annotation

A name that is not a module-type declaration of the buffer selects nothing:
the query only ever answers for the buffer's own declarations, identified by
their uids.

  $ impls_of Nonexistent <<'EOF'
  > module type S = sig
  >   type t
  > end
  > EOF
