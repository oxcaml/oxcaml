This test checks that primitive aliases are correctly encoded when migrated
down to our 5.2 AST.

  $ cat > primitive_alias.ml << EOF
  > external f : int -> int = Foo.f
  > external f' = Foo.f'
  > module type S = sig
  >   external g = Foo.g
  > end
  > EOF

  $ ./id_driver.exe primitive_alias.ml
  [%%ppxlib.migration.pstr_primitive_alias_5_6 external f : int -> int]
  [%%ppxlib.migration.pstr_primitive_alias_5_6
    external f' : [%ppxlib.migration.none_5_6 ]]
  module type S  =
    sig
      [%%ppxlib.migration.psig_primitive_alias_5_6
        external g : [%ppxlib.migration.none_5_6 ]]
    end

And that they are correctly decoded during the round trip:

  $ ./id_driver.exe primitive_alias.ml --use-compiler-pp
  external f : int -> int = Foo.f
  external f'  = Foo.f'
  module type S  = sig external g  = Foo.g end
