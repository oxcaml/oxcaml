The build preprocesses with [-as-pp], which is [-dump-ast -embed-errors]. [as_pp]
prints the result as source instead. Merlin and the compiler's [-ppx] run the driver
with [-as-ppx], on a binary AST: [as_ppx] prints the result as source.

By default ([Options.continue_after_exception = false]), the first rewriter which
raises stops the driver (see the end). These first pass [-continue-after-exception
-no-check], upstream ppxlib's defaults: when a rewriter raises, the error takes the
place of the code the rewriter was to generate, and the other rewriters still run.

  $ as_pp() { ./raising_rewriters.exe -embed-errors $EXC -no-color -diff-cmd - "$@"; }
  $ as_ppx() {
  >   ./raising_rewriters.exe -apply '' -no-check -dump-ast -o input.ast "$1" "$2" &&
  >   ./raising_rewriters.exe -as-ppx $EXC input.ast output.ast &&
  >   ./raising_rewriters.exe -apply '' -no-check -no-color "$1" output.ast
  > }
  $ EXC='-continue-after-exception -no-check'

When a deriver raises, its error takes the place of the code it was to generate.

  $ cat > test.ml <<EOF
  > type t = int [@@deriving raise]
  > EOF
  $ as_pp test.ml
  type t = int[@@deriving raise]
  [%%ocaml.error "the deriver raised"]
  $ as_ppx -impl test.ml
  type t = int[@@deriving raise]
  [%%ocaml.error "the deriver raised"]

The same in a signature.

  $ cat > test.mli <<EOF
  > type t = int [@@deriving raise]
  > EOF
  $ as_pp test.mli
  type t = int[@@deriving raise]
  [%%ocaml.error "the deriver raised"]
  $ as_ppx -intf test.mli
  type t = int[@@deriving raise]
  [%%ocaml.error "the deriver raised"]

When the inline code is stale and the deriver doesn't raise, the driver proposes a
correction.

  $ cat > test.ml <<EOF
  > type t = int [@@deriving_inline nothing]
  > let stale = ()
  > [@@@end]
  > EOF
  $ as_pp test.ml
  type t = int[@@deriving_inline nothing]
  let stale = ()
  [@@@end ]
  $ cat test.ml.ppx-corrected
  type t = int [@@deriving_inline nothing]
  let _ = fun (_ : t) -> ()
  [@@@end]
  $ as_ppx -impl test.ml
  type t = int[@@deriving_inline nothing]
  let stale = ()
  [@@@end ]

When an inline deriver raises, the driver accepts the code which is there, and proposes
the error as a correction.

  $ cat > test.ml <<EOF
  > type t = int [@@deriving_inline raise]
  > let stale = ()
  > [@@@end]
  > EOF
  $ as_pp test.ml
  type t = int[@@deriving_inline raise]
  let stale = ()
  [@@@end ]
  $ cat test.ml.ppx-corrected
  type t = int [@@deriving_inline raise]
  [%%ocaml.error "the deriver raised"]
  [@@@end]
  $ as_ppx -impl test.ml
  type t = int[@@deriving_inline raise]
  let stale = ()
  [@@@end ]

The same in a signature.

  $ cat > test.mli <<EOF
  > type t = int [@@deriving_inline raise]
  > val stale : unit
  > [@@@end]
  > EOF
  $ as_pp test.mli
  type t = int[@@deriving_inline raise]
  val stale : unit
  [@@@end ]
  $ cat test.mli.ppx-corrected
  type t = int [@@deriving_inline raise]
  [%%ocaml.error "the deriver raised"]
  [@@@end]
  $ as_ppx -intf test.mli
  type t = int[@@deriving_inline raise]
  val stale : unit
  [@@@end ]

The [drop] pass runs after the extensions are expanded, and removes the bindings marked
[@@drop].

  $ cat > test.ml <<EOF
  > let x = () [@@drop]
  > let y = ()
  > EOF
  $ as_pp test.ml
  let y = ()

When an extension raises, its error is lost if a later pass removes the code the
extension was in.

  $ cat > test.ml <<EOF
  > let x = [%raise] [@@drop]
  > let y = ()
  > EOF
  $ as_pp test.ml
  let y = ()
  $ as_ppx -impl test.ml
  let y = ()

By default, the driver stops at the first rewriter which raises, and reports its error
with the code as it was before any rewriting.

  $ EXC=

  $ cat > test.ml <<EOF
  > type t = int [@@deriving raise]
  > EOF
  $ as_pp test.ml
  [%%ocaml.error "the deriver raised"]
  type t = int[@@deriving raise]
  $ cat > test.mli <<EOF
  > type t = int [@@deriving raise]
  > EOF
  $ as_pp test.mli
  [%%ocaml.error "the deriver raised"]
  type t = int[@@deriving raise]

It neither accepts the inline code which is there, nor proposes the error as a
correction.

  $ cat > test.ml <<EOF
  > type t = int [@@deriving_inline raise]
  > let stale = ()
  > [@@@end]
  > EOF
  $ rm -f test.ml.ppx-corrected
  $ as_pp test.ml
  [%%ocaml.error "the deriver raised"]
  type t = int[@@deriving_inline raise]
  let stale = ()
  [@@@end ]
  $ test -e test.ml.ppx-corrected
  [1]

And a later pass doesn't lose the error.

  $ cat > test.ml <<EOF
  > let x = [%raise] [@@drop]
  > let y = ()
  > EOF
  $ as_pp test.ml
  [%%ocaml.error "the extension raised"]
  let x = [%raise ][@@drop ]
  let y = ()
