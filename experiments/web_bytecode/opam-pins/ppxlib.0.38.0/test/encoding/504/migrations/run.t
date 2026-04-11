This test checks that labeled tuple types are correctly encoded
when migrated down to our 5.2 AST

  $ cat > type.ml << EOF
  > type t = (a: int * b: int * string)
  > EOF

  $ ./id_driver.exe type.ml
  type t =
    [%ppxlib.migration.ptyp_labeled_tuple_5_4 :
      (('a * int) * ('b * int) * (_ * string))]

And that it is correctly decoded when migrated back up to 5.4+ ASTS:

  $ ./id_driver.exe type.ml --use-compiler-pp
  type t = (a:int * b:int * string)

Same for expressions:

  $ cat > expression.ml << EOF
  > let x = (~a:0, ~b:1, "abc")
  > EOF

  $ ./id_driver.exe expression.ml
  let x =
    [%ppxlib.migration.pexp_labeled_tuple_5_4
      (((`Some `a), 0), ((`Some `b), 1), (`None, "abc"))]

  $ ./id_driver.exe expression.ml --use-compiler-pp
  let x = (~a:0, ~b:1, "abc")

And same for patterns:

  $ cat > pattern.ml << EOF
  > let (~a, ~b:_, c, ..) = x
  > EOF

  $ ./id_driver.exe pattern.ml
  let [%ppxlib.migration.ppat_labeled_tuple_5_4 ?
        (((a, a), (b, _), (_, c)), open_)]
    = x

  $ ./id_driver.exe pattern.ml --use-compiler-pp
  let (~a, ~b:_, c, ..) = x

We also check that bivariant type parameters are correctly encoded and migrated:

  $ cat > bivariant.ml << EOF
  > type +-'a t = A
  > type +-'a extensible += B
  > class [+-'a] c = object end
  > class type [+-'a] d = object end
  > EOF

  $ ./id_driver.exe bivariant.ml
  [%%ppxlib.migration.bivariant_str_item_5_4 type 'a t =
                                               | A ]
  [%%ppxlib.migration.bivariant_str_item_5_4 type 'a extensible +=  
                                               | B ]
  [%%ppxlib.migration.bivariant_str_item_5_4 class ['a] c = object  end]
  [%%ppxlib.migration.bivariant_str_item_5_4 class type ['a] d = object  end]

  $ ./id_driver.exe bivariant.ml --use-compiler-pp
  type +-'a t =
    | A 
  type +-'a extensible +=  
    | B 
  class [+-'a] c = object  end
  class type [+-'a] d = object  end
Bivariant are also correctly handled in a signature context:

  $ cat > bivariant.mli << EOF
  > type +-'a t
  > type +-'a u := 'a v
  > type +-'a extensible += B
  > class [+-'a] c : object end
  > class type [+-'a] d = object end
  > module M : S with type +-'a t = 'a u
  > module M2 : S with type +-'a t := 'a u
  > EOF

  $ ./id_driver.exe bivariant.mli
  [%%ppxlib.migration.bivariant_sig_item_5_4 : type 'a t]
  [%%ppxlib.migration.bivariant_sig_item_5_4 : type 'a u := 'a v]
  [%%ppxlib.migration.bivariant_sig_item_5_4 : type 'a extensible +=  
                                                 | B ]
  [%%ppxlib.migration.bivariant_sig_item_5_4 : class ['a] c : object  end]
  [%%ppxlib.migration.bivariant_sig_item_5_4 : class type ['a] d = object  end]
  module M :
  [%ppxlib.migration.bivariant_pmty_with_5_4 :
    module _ : S with type 'a  t =  'a u]
  module M2 :
  [%ppxlib.migration.bivariant_pmty_with_5_4 :
    module _ : S with type 'a  t :=  'a u]

  $ ./id_driver.exe bivariant.mli --use-compiler-pp
  type +-'a t
  type +-'a u := 'a v
  type +-'a extensible +=  
    | B 
  class [+-'a] c : object  end
  class type [+-'a] d = object  end
  module M : S with type +-'a  t =  'a u
  module M2 : S with type +-'a  t :=  'a u
