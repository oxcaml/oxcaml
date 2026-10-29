(** Erasure of the erasable OxCaml syntax extensions, as a typed copy of an
    OxCaml parsetree into OCaml 5.4's parsetree.

    Any construct that the upstream compiler cannot express with the same
    meaning (comprehensions, [include functor], [let mutable], ...) raises a
    located [Location.Error]. *)

val structure : Parsetree.structure -> Ast_504.Parsetree.structure

val signature : Parsetree.signature -> Ast_504.Parsetree.signature
