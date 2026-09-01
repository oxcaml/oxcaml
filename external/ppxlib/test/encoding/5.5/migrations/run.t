This test checks that external types are correctly encoded
when migrated down to our 5.2 AST

  $ cat > external_type.ml << EOF
  > type t = external "gmp"
  > module type S = sig
  >   type t = external "gmp"
  >   type t2 := external "gmp"
  > end
  > EOF

  $ ./id_driver.exe external_type.ml
  [%%ppxlib.migration.external_pstr_type_5_5 type t]
  module type S  =
    sig
      [%%ppxlib.migration.external_psig_5_5 : type t]
      [%%ppxlib.migration.external_psig_5_5 : type t2]
    end

And that they are correctly migrated back:

  $ ./id_driver.exe external_type.ml --use-compiler-pp
  type t = external "gmp"
  module type S  = sig type t = external "gmp" type t2 := external "gmp" end
