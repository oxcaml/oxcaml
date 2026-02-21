  $ $MERLIN single version | revert-newlines | jq .value.magicNumbers
  {
    "cmi_magic_number": "Caml1999I573",
    "ast_intf_magic_number": "Caml1999N573",
    "ast_impl_magic_number": "Caml1999M573",
    "cmt_magic_number": "Caml1999T573",
    "cms_magic_number": "Caml1999S573",
    "index_magic_number": "Merl2023I573"
  }

  $ ocaml-index magic-numbers | jq
  {
    "cmi_magic_number": "Caml1999I573",
    "ast_intf_magic_number": "Caml1999N573",
    "ast_impl_magic_number": "Caml1999M573",
    "cmt_magic_number": "Caml1999T573",
    "cms_magic_number": "Caml1999S573",
    "index_magic_number": "Merl2023I573"
  }

Verify there is no difference between Merlin and Ocaml-index
  $ $MERLIN single version | revert-newlines | jq --sort-keys .value.magicNumbers > merlin-magic-numbers.json
  $ ocaml-index magic-numbers | jq --sort-keys > ocaml-index-magic-numbers.json
  $ diff merlin-magic-numbers.json ocaml-index-magic-numbers.json
