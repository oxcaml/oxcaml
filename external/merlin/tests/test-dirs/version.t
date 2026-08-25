  $ $MERLIN single version | revert-newlines | jq .value.magicNumbers
  {
    "cmi_magic_number": "Caml1999I585",
    "ast_intf_magic_number": "Caml1999N585",
    "ast_impl_magic_number": "Caml1999M585",
    "cmt_magic_number": "Caml1999T585",
    "cms_magic_number": "Caml1999S585",
    "index_magic_number": "Merl2023I585"
  }

  $ ocaml-index magic-numbers | jq
  {
    "cmi_magic_number": "Caml1999I585",
    "ast_intf_magic_number": "Caml1999N585",
    "ast_impl_magic_number": "Caml1999M585",
    "cmt_magic_number": "Caml1999T585",
    "cms_magic_number": "Caml1999S585",
    "index_magic_number": "Merl2023I585"
  }

Verify there is no difference between Merlin and Ocaml-index
  $ $MERLIN single version | revert-newlines | jq --sort-keys .value.magicNumbers > merlin-magic-numbers.json
  $ ocaml-index magic-numbers | jq --sort-keys > ocaml-index-magic-numbers.json
  $ diff merlin-magic-numbers.json ocaml-index-magic-numbers.json
