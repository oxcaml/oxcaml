The build preprocesses with [-as-pp], which is [-dump-ast -embed-errors]. [as_pp]
prints the result as source instead. Merlin and the compiler's [-ppx] run the driver
with [-as-ppx], on a binary AST: [as_ppx] prints the result as source.

These show the default, [Options.continue_after_exception = true]: when a rewriter
raises, the error takes the place of the code the rewriter was to generate, and the
other rewriters still run. A distribution which sets it to [false] gets the error
reported with the code as it was before any rewriting instead.

  $ as_pp() { ./raising_rewriters.exe -embed-errors -no-color -diff-cmd - "$@"; }
  $ as_ppx() {
  >   ./raising_rewriters.exe -apply '' -no-check -dump-ast -o input.ast "$1" "$2" &&
  >   ./raising_rewriters.exe -as-ppx input.ast output.ast &&
  >   ./raising_rewriters.exe -apply '' -no-check -no-color "$1" output.ast
  > }

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
