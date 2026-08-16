The [intf-weaknesses] query strengthens the interface a buffer's declarations
live in. A module type declared in an [_intf.ml] is shared by every unit that
includes it, so a strengthening is sound only when every implementation
supports it: the query merges over the implementations the configured index
records, keeping the annotations they agree on.

[hof_once] applies its callback once and [hof_twice] applies it twice, so the
two implementations agree that the callback may be [local] and disagree about
whether it may be [once].

  $ cat hof_intf.ml
  module type S = sig
    val app : ('a -> 'b) -> 'a -> unit
  end

  $ cat hof_once.mli
  include Hof_intf.S

  $ cat hof_once.ml
  let app (type b) f x =
    let _ : b = f x in
    ()
  ;;

  $ cat hof_twice.ml
  let app (type b) f x =
    let _ : b = f x in
    let _ : b = f x in
    ()
  ;;

With no index configured, nothing discovers the units that include the module
type, and the buffer declaring it has no interface of its own to strengthen:

  $ $MERLIN single intf-weaknesses -filename ./hof_intf.ml < ./hof_intf.ml | jq -c .value
  []

Querying an implementation asks about that unit's own interface. The
declaration is not written in [hof_once.mli] — it comes from the shared module
type — so there is nothing to edit there:

  $ $OCAMLC -bin-annot-cms -c hof_intf.ml hof_once.mli hof_once.ml hof_twice.mli hof_twice.ml

  $ $MERLIN single intf-weaknesses -filename ./hof_once.ml < ./hof_once.ml | jq -c .value
  []

Querying the declaration merges over both implementations. The edits target the
file the module type is declared in:

  $ ocaml-index aggregate *.cms *.cmsi -o .merlin-index

  $ $MERLIN single intf-weaknesses -index-file .merlin-index \
  > -filename ./hof_intf.ml < ./hof_intf.ml | jq -r '.value[].intf_file' \
  > | sed "s|.*/||"
  hof_intf.ml

The atoms the implementations agree on are kept and the ones they do not are
dropped: the callback is strengthened to [local], and [once] — which only
[hof_once] supports — does not appear. How a suggestion is rendered in full is
pinned by the single-implementation tests; what this pins is which atoms
survive the merge.

  $ $MERLIN single intf-weaknesses -index-file .merlin-index \
  > -filename ./hof_intf.ml < ./hof_intf.ml | jq -r '.value[].edits[].new_text' \
  > | grep -o -E '\b(local|once)\b' | sort -u
  local

The two merged expectations above record the intended behaviour of the merge;
they cannot be produced until the analysis and the module-type-implementation
discovery this query builds on are in the tree.
