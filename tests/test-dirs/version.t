  $ $MERLIN single version | revert-newlines | jq .value.magicNumbers
  {
    "cmi_magic_number": "Caml1999I565",
    "ast_intf_magic_number": "Caml1999N565",
    "ast_impl_magic_number": "Caml1999M565",
    "cmt_magic_number": "Caml1999T565",
    "cms_magic_number": "Caml1999S565",
    "index_magic_number": "Merl2023I565"
  }

  $ ocaml-index magic-numbers | jq
  {
    "cmi_magic_number": "Caml1999I565",
    "ast_intf_magic_number": "Caml1999N565",
    "ast_impl_magic_number": "Caml1999M565",
    "cmt_magic_number": "Caml1999T565",
    "cms_magic_number": "Caml1999S565",
    "index_magic_number": "Merl2023I565"
  }

Verify there is no difference between Merlin and Ocaml-index
  $ $MERLIN single version | revert-newlines | jq --sort-keys .value.magicNumbers > merlin-magic-numbers.json
  $ ocaml-index magic-numbers | jq --sort-keys > ocaml-index-magic-numbers.json
  $ diff merlin-magic-numbers.json ocaml-index-magic-numbers.json
