(* TEST
 flags = "-dtypedtree -dno-locations";
 expect;
*)

module X = struct end
[%%expect{|
[
  structure_item
    Tstr_module (Present)
    X/289
      module_expr
        Tmod_structure
        []
]

module X : sig end
|}]

module X = struct end [@foo]
[%%expect{|
[
  structure_item
    Tstr_module (Present)
    X/290
      module_expr
        attribute "foo"
          []
        Tmod_structure
        []
]

module X : sig end
|}]

module Y = X
[%%expect{|
[
  structure_item
    Tstr_module (Absent)
    Y/291
      module_expr
        Tmod_ident "X/290"
]

module Y = X
|}]

module type T = sig module Y = X end
[%%expect{|
[
  structure_item
    Tstr_modtype "T/293"
      module_type
        Tmty_signature
        [
          signature_item
            Tsig_module (Absent)
            Y/292
              module_type
                Tmty_alias "X/290"
        ]
        join_const(unique,uncontended,read_write,static);meet_const(local,once,nonportable,unforkable,yielding,stateful)
        []
]

module type T = sig module Y = X end
|}]
