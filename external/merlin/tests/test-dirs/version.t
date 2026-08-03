  $ $MERLIN single version | revert-newlines | jq .value.magicNumbers
  {
    "cmi_magic_number": "Caml1999I582",
    "ast_intf_magic_number": "Caml1999N582",
    "ast_impl_magic_number": "Caml1999M582",
    "cmt_magic_number": "Caml1999T582",
    "cms_magic_number": "Caml1999S582",
    "index_magic_number": "Merl2023I582"
  }

  $ ocaml-index magic-numbers | jq
  {
    "cmi_magic_number": "Caml1999I582",
    "ast_intf_magic_number": "Caml1999N582",
    "ast_impl_magic_number": "Caml1999M582",
    "cmt_magic_number": "Caml1999T582",
    "cms_magic_number": "Caml1999S582",
    "index_magic_number": "Merl2023I582"
  }

Verify there is no difference between Merlin and Ocaml-index
  $ $MERLIN single version | revert-newlines | jq --sort-keys .value.magicNumbers > merlin-magic-numbers.json
  $ ocaml-index magic-numbers | jq --sort-keys > ocaml-index-magic-numbers.json
  $ diff merlin-magic-numbers.json ocaml-index-magic-numbers.json
