  $ $MERLIN single version | revert-newlines | jq .value.magicNumbers
  {
    "cmi_magic_number": "Caml1999I592",
    "ast_intf_magic_number": "Caml1999N592",
    "ast_impl_magic_number": "Caml1999M592",
    "cmt_magic_number": "Caml1999T592",
    "cms_magic_number": "Caml1999S592",
    "index_magic_number": "Merl2023I592"
  }

  $ ocaml-index magic-numbers | jq
  {
    "cmi_magic_number": "Caml1999I592",
    "ast_intf_magic_number": "Caml1999N592",
    "ast_impl_magic_number": "Caml1999M592",
    "cmt_magic_number": "Caml1999T592",
    "cms_magic_number": "Caml1999S592",
    "index_magic_number": "Merl2023I592"
  }

Verify there is no difference between Merlin and Ocaml-index
  $ $MERLIN single version | revert-newlines | jq --sort-keys .value.magicNumbers > merlin-magic-numbers.json
  $ ocaml-index magic-numbers | jq --sort-keys > ocaml-index-magic-numbers.json
  $ diff merlin-magic-numbers.json ocaml-index-magic-numbers.json
