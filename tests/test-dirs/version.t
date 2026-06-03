  $ $MERLIN single version | revert-newlines | jq .value.magicNumbers
  {
    "cmi_magic_number": "Caml1999I578",
    "ast_intf_magic_number": "Caml1999N578",
    "ast_impl_magic_number": "Caml1999M578",
    "cmt_magic_number": "Caml1999T578",
    "cms_magic_number": "Caml1999S578",
    "index_magic_number": "Merl2023I578"
  }

  $ ocaml-index magic-numbers | jq
  {
    "cmi_magic_number": "Caml1999I578",
    "ast_intf_magic_number": "Caml1999N578",
    "ast_impl_magic_number": "Caml1999M578",
    "cmt_magic_number": "Caml1999T578",
    "cms_magic_number": "Caml1999S578",
    "index_magic_number": "Merl2023I578"
  }

Verify there is no difference between Merlin and Ocaml-index
  $ $MERLIN single version | revert-newlines | jq --sort-keys .value.magicNumbers > merlin-magic-numbers.json
  $ ocaml-index magic-numbers | jq --sort-keys > ocaml-index-magic-numbers.json
  $ diff merlin-magic-numbers.json ocaml-index-magic-numbers.json
