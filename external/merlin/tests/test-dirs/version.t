  $ $MERLIN single version | revert-newlines | jq .value.magicNumbers
  {
    "cmi_magic_number": "Caml1999I590",
    "ast_intf_magic_number": "Caml1999N590",
    "ast_impl_magic_number": "Caml1999M590",
    "cmt_magic_number": "Caml1999T590",
    "cms_magic_number": "Caml1999S590",
    "index_magic_number": "Merl2023I590"
  }

  $ ocaml-index magic-numbers | jq
  {
    "cmi_magic_number": "Caml1999I590",
    "ast_intf_magic_number": "Caml1999N590",
    "ast_impl_magic_number": "Caml1999M590",
    "cmt_magic_number": "Caml1999T590",
    "cms_magic_number": "Caml1999S590",
    "index_magic_number": "Merl2023I590"
  }

Verify there is no difference between Merlin and Ocaml-index
  $ $MERLIN single version | revert-newlines | jq --sort-keys .value.magicNumbers > merlin-magic-numbers.json
  $ ocaml-index magic-numbers | jq --sort-keys > ocaml-index-magic-numbers.json
  $ diff merlin-magic-numbers.json ocaml-index-magic-numbers.json
