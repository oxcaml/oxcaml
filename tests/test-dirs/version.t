  $ $MERLIN single version | revert-newlines | jq .value.magicNumbers
  {
    "cmi_magic_number": "Caml1999I569",
    "ast_intf_magic_number": "Caml1999N569",
    "ast_impl_magic_number": "Caml1999M569",
    "cmt_magic_number": "Caml1999T569",
    "cms_magic_number": "Caml1999S569",
    "index_magic_number": "Merl2023I569"
  }

  $ ocaml-index magic-numbers | jq
  {
    "cmi_magic_number": "Caml1999I569",
    "ast_intf_magic_number": "Caml1999N569",
    "ast_impl_magic_number": "Caml1999M569",
    "cmt_magic_number": "Caml1999T569",
    "cms_magic_number": "Caml1999S569",
    "index_magic_number": "Merl2023I569"
  }

Verify there is no difference between Merlin and Ocaml-index
  $ $MERLIN single version | revert-newlines | jq --sort-keys .value.magicNumbers > merlin-magic-numbers.json
  $ ocaml-index magic-numbers | jq --sort-keys > ocaml-index-magic-numbers.json
  $ diff merlin-magic-numbers.json ocaml-index-magic-numbers.json
