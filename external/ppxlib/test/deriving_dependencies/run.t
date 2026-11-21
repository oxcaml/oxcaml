Dependencies between derivers listed separately.

[use_foo] without [foo] fails.

  $ cat > test.ml <<EOF
  > type t = int [@@deriving use_foo]
  > EOF
  $ ./driver.exe -impl test.ml
  type t = int[@@deriving use_foo]
  include
    struct
      let _ = fun (_ : t) -> ()
      [%%ocaml.error
        "Deriver foo is needed for use_foo, you need to add it before in the list"]
    end[@@ocaml.doc "@inline"][@@merlin.hide ]

[use_foo] before [foo] fails.

  $ cat > test.ml <<EOF
  > type t = int [@@deriving use_foo, foo]
  > EOF
  $ ./driver.exe -impl test.ml
  type t = int[@@deriving (use_foo, foo)]
  include
    struct
      [@@@ocaml.warning "-60"]
      let _ = fun (_ : t) -> ()
      [%%ocaml.error
        "Deriver foo is needed for use_foo, you need to add it before in the list"]
      let `Foo _ = let module Foo = struct  end in `Foo false
    end[@@ocaml.doc "@inline"][@@merlin.hide ]

[use_foo] after [foo] works.

  $ cat > test.ml <<EOF
  > type t = int [@@deriving foo, use_foo]
  > EOF
  $ ./driver.exe -impl test.ml
  type t = int[@@deriving (foo, use_foo)]
  include
    struct
      [@@@ocaml.warning "-60"]
      let _ = fun (_ : t) -> ()
      let `Foo _ = let module Foo = struct  end in `Foo false
    end[@@ocaml.doc "@inline"][@@merlin.hide ]

Dependencies between derivers within an alias.

[use_before_dep] works.

  $ cat > test.ml <<EOF
  > type t = int [@@deriving use_before_dep]
  > EOF
  $ ./driver.exe -impl test.ml
  type t = int[@@deriving use_before_dep]
  include
    struct
      [@@@ocaml.warning "-60"]
      let _ = fun (_ : t) -> ()
      let `Foo _ = let module Foo = struct  end in `Foo false
    end[@@ocaml.doc "@inline"][@@merlin.hide ]

[dep_before_use] fails.

  $ cat > test.ml <<EOF
  > type t = int [@@deriving dep_before_use]
  > EOF
  $ ./driver.exe -impl test.ml
  type t = int[@@deriving dep_before_use]
  include
    struct
      [@@@ocaml.warning "-60"]
      let _ = fun (_ : t) -> ()
      [%%ocaml.error
        "Deriver foo is needed for dep_before_use, you need to add it before in the list"]
      let `Foo _ = let module Foo = struct  end in `Foo false
    end[@@ocaml.doc "@inline"][@@merlin.hide ]

TODO: ppx_deriving should apply derivers in topsort order.

Duplicate derivers.

Just [foo] ppx

  $ cat > test.ml <<EOF
  > type t = int [@@deriving_inline foo]
  > [@@@end]
  > EOF
  $ ./driver.exe -diff-cmd - -impl test.ml > /dev/null
  $ cat test.ml.ppx-corrected
  type t = int [@@deriving_inline foo]
  include
    struct
      [@@@ocaml.warning "-60"]
      let _ = fun (_ : t) -> ()
      let `Foo _ = let module Foo = struct  end in `Foo false
    end[@@ocaml.doc "@inline"]
  [@@@end]

Listing [foo, bar] applies them in that order.

  $ cat > test.ml <<EOF
  > type t = int [@@deriving_inline foo, bar]
  > [@@@end]
  > EOF
  $ ./driver.exe -diff-cmd - -impl test.ml > /dev/null
  $ cat test.ml.ppx-corrected
  type t = int [@@deriving_inline foo, bar]
  include
    struct
      [@@@ocaml.warning "-60"]
      let _ = fun (_ : t) -> ()
      let `Foo _ = let module Foo = struct  end in `Foo false
      let `Bar _ = let module Bar = struct  end in `Bar false
    end[@@ocaml.doc "@inline"]
  [@@@end]

[Deriver.add_alias] applies derivers in reverse order (see the TODO above).

  $ cat > test.ml <<EOF
  > type t = int [@@deriving_inline foo_bar ~foo]
  > [@@@end]
  > EOF
  $ ./driver.exe -diff-cmd - -impl test.ml > /dev/null
  $ cat test.ml.ppx-corrected
  type t = int [@@deriving_inline foo_bar ~foo]
  include
    struct
      [@@@ocaml.warning "-60"]
      let _ = fun (_ : t) -> ()
      let `Bar _ = let module Bar = struct  end in `Bar false
      let `Foo _ = let module Foo = struct  end in `Foo true
    end[@@ocaml.doc "@inline"]
  [@@@end]

Direct duplication of [foo] ppx

  $ cat > test.ml <<EOF
  > type t = int [@@deriving_inline foo, foo ~foo]
  > [@@@end]
  > EOF
  $ ./driver.exe -diff-cmd - -impl test.ml > /dev/null
  $ cat test.ml.ppx-corrected
  type t = int [@@deriving_inline foo, foo ~foo]
  include
    struct
      [@@@ocaml.warning "-60"]
      let _ = fun (_ : t) -> ()
      let `Foo _ = let module Foo = struct  end in `Foo false
      let `Foo _ = let module Foo = struct  end in `Foo true
    end[@@ocaml.doc "@inline"]
  [@@@end]

Indirect duplication of [foo] ppx via [foo_bar] alias

  $ cat > test.ml <<EOF
  > type t = int [@@deriving_inline foo_bar ~bar, foo ~foo]
  > [@@@end]
  > EOF
  $ ./driver.exe -diff-cmd - -impl test.ml > /dev/null
  $ cat test.ml.ppx-corrected
  type t = int [@@deriving_inline foo_bar ~bar, foo ~foo]
  include
    struct
      [@@@ocaml.warning "-60"]
      let _ = fun (_ : t) -> ()
      let `Bar _ = let module Bar = struct  end in `Bar true
      let `Foo _ = let module Foo = struct  end in `Foo false
      let `Foo _ = let module Foo = struct  end in `Foo true
    end[@@ocaml.doc "@inline"]
  [@@@end]

A deriver can mark the attributes it reads as handled, so that the driver's checks
don't report them as unused: [baz] marks those on the type parameters.

  $ cat > test.ml <<EOF
  > type 'a[@phantom] t [@@deriving baz]
  > EOF
  $ ./driver.exe -check -impl test.ml
  type 'a[@phantom ] t[@@deriving baz]
  include struct let _ = fun (_ : 'a t) -> () end[@@ocaml.doc "@inline"]
  [@@merlin.hide ]
