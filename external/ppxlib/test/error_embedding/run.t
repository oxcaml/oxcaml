Most errors happening during ppxlib rewriting process are ultimately turned into
error extension nodes.

Undefined derivers are turned into error nodes

  $ echo "type t = int [@@deriving undefined]" >> undefined_deriver.ml
  $ ./deriver.exe undefined_deriver.ml
  type t = int[@@deriving undefined]
  include
    struct
      let _ = fun (_ : t) -> ()
      [%%ocaml.error
        "Ppxlib.Deriving: 'undefined' is not a supported type deriving generator"]
    end[@@ocaml.doc "@inline"][@@merlin.hide ]

Error nodes are generated when parsing of payload fails.

export_string expects only one argument, a string, and output it.
Anything else will embed an error extension node

  $ echo "let _ = [%export_string \"string\"]" > parsing_payload_extension.ml
  $ echo "let _ = [%export_string \"string\" \"other\"]" >> parsing_payload_extension.ml
  $ echo "let _ = [%export_string identifier]" >> parsing_payload_extension.ml
  $ ./extender.exe parsing_payload_extension.ml
  [%%ocaml.error "constant expected"]
  [%%ocaml.error "constant expected"]
  let _ = "string"
  let _ = [%export_string "string" "other"]
  let _ = [%export_string identifier]

  $ echo "type a = int [@@deriving a_string]" > parsing_payload_deriver.ml
  $ echo "type b = int [@@deriving a_string unexpected_args]" >> parsing_payload_deriver.ml
  $ ./deriver.exe parsing_payload_deriver.ml
  type a = int[@@deriving a_string]
  include struct let _ = fun (_ : a) -> ()
                 let _ = "derived_string" end[@@ocaml.doc "@inline"][@@merlin.hide
                                                                      ]
  type b = int[@@deriving a_string unexpected_args]
  include
    struct
      let _ = fun (_ : b) -> ()
      [%%ocaml.error
        "Ppxlib.Deriving: non-optional labelled argument or record expected"]
    end[@@ocaml.doc "@inline"][@@merlin.hide ]

Error nodes are generated when dependent derivers are not applied.

  $ echo "type a = int [@@deriving a_dependent_string]" > dependent_derivers.ml
  $ ./deriver.exe dependent_derivers.ml
  type a = int[@@deriving a_dependent_string]
  include
    struct
      let _ = fun (_ : a) -> ()
      [%%ocaml.error
        "Deriver a_string is needed for a_dependent_string, you need to add it before in the list"]
      let _ = "derived_string"
    end[@@ocaml.doc "@inline"][@@merlin.hide ]
  $ echo "type b = int [@@deriving a_dependent_string, a_string]" > dependent_derivers.ml
  $ ./deriver.exe dependent_derivers.ml
  type b = int[@@deriving (a_dependent_string, a_string)]
  include
    struct
      let _ = fun (_ : b) -> ()
      [%%ocaml.error
        "Deriver a_string is needed for a_dependent_string, you need to add it before in the list"]
      let _ = "derived_string"
      let _ = "derived_string"
    end[@@ocaml.doc "@inline"][@@merlin.hide ]
  $ echo "type b = int [@@deriving a_string, a_dependent_string]" > dependent_derivers.ml
  $ ./deriver.exe dependent_derivers.ml
  type b = int[@@deriving (a_string, a_dependent_string)]
  include
    struct
      let _ = fun (_ : b) -> ()
      let _ = "derived_string"
      let _ = "derived_string"
    end[@@ocaml.doc "@inline"][@@merlin.hide ]

Flag `-raise-embedded-errors` raises the first embedded error in the AST.

  $ echo "let () = ()" > embedded_error.ml
  $ echo "module T = struct [%%ocaml.error \"error 1\"] end" >> embedded_error.ml
  $ echo "[%%ocaml.error \"error 2\"]" >> embedded_error.ml
  $ ./extender.exe embedded_error.ml -raise-embedded-errors
  File "embedded_error.ml", line 2, characters 21-32:
  2 | module T = struct [%%ocaml.error "error 1"] end
                           ^^^^^^^^^^^
  Error: error 1
  [1]

Undeprecated `[@@@deriving.end]` runs just fine.

  $ echo "type t [@@deriving_inline a_string] [@@@deriving.end]" > embedded_error.ml
  $ ./deriver.exe embedded_error.ml -diff-cmd "diff -u --label source --label derived"
  type t[@@deriving_inline a_string]
  [@@@deriving.end ]
  --- source
  +++ derived
  @@ -1 +1,4 @@
  -type t [@@deriving_inline a_string] [@@@deriving.end]
  +type t [@@deriving_inline a_string] 
  +let _ = fun (_ : t) -> ()
  +let _ = "derived_string"
  +[@@@deriving.end]
  [1]

Deprecated `[@@@deriving.end]` produces an error.

  $ echo "type t [@@deriving_inline a_string] [@@@deriving.end]" > embedded_error.ml
  $ ./deriver.exe embedded_error.ml -allow-deriving-end false
  [%%ocaml.error
    "ppxlib: [@@@deriving.end] is deprecated, please use [@@@ppxlib.inline.end]. If you need the deprecated attribute temporarily, pass [-allow-deriving-end] to the ppx driver)."]
  type t[@@deriving_inline a_string]
  [@@@deriving.end ]
