(* TEST
 flags = "-dtypedtree -dno-locations";
 expect;
*)

module X = struct end
[%%expect{|
[
  structure_item
    Tstr_module (Present)
    X/327
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
    X/328
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
    Y/329
      module_expr
        Tmod_ident "X/328"
]

module Y = X
|}]

module type T = sig module Y = X end
[%%expect{|
[
  structure_item
    Tstr_modtype "T/331"
      module_type
        Tmty_signature
        [
          signature_item
            Tsig_module (Absent)
            Y/330
              module_type
                Tmty_alias "X/328"
        ]
        join_const(unique,uncontended,physical,read_write,static);meet_const(local,once,nonportable,partial,unforkable,yielding,stateful,erased)
        []
]

module type T = sig module Y = X end
|}]
