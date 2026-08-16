The [module-type-impls] query answers from the compiler facts recorded in the
configured indexes. With no index configured it reports [unavailable] rather
than a complete empty answer, so a client can tell "nothing implements this
module type" apart from "the facts were never loaded".

  $ cat main.ml
  module type S = sig
    type t
  end

  module M : S = struct
    type t = int
  end

  $ $MERLIN single module-type-impls -filename ./main.ml < ./main.ml
  {
    "class": "return",
    "value": {
      "status": "unavailable",
      "reasons": [
        {
          "kind": "no-index-files"
        }
      ],
      "implementations": []
    },
    "notifications": []
  }

An index that carries no usable module-facts channel is also [unavailable], not
[partial]: the query has nothing to answer from, so it must not look like a
degraded answer. Only the status and the reason kinds are checked here, since
the reader's messages mention absolute paths.

  $ cat > not-an-index << EOF
  > this is not an index file
  > EOF

  $ $MERLIN single module-type-impls -index-file ./not-an-index \
  >   -filename ./main.ml < ./main.ml | jq '.value.status'
  "unavailable"

  $ $MERLIN single module-type-impls -index-file ./not-an-index \
  >   -filename ./main.ml < ./main.ml | jq -c '[.value.reasons[].kind]'
  ["facts-channel-absent","reader-problem"]
