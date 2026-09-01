Test that attribute replacement works in all the various contexts it can be applied.

We include extra alert attributes where possible as they should pass through unchanged and
in the same order.

Class expressions
  $ cat > test.ml << EOF
  > class class_ = c [@test.clx "suffix"]
  > EOF
  $ ./driver.exe test.ml
  class class_ = c__suffix

Class fields
  $ cat > test.ml << EOF
  > class class_field =
  >   object
  >     val foo = () [@@alert "-1"] [@@test.clf "suffix"] [@@alert "-2"]
  >   end
  > EOF
  $ ./driver.exe test.ml
  class class_field =
    object val foo__suffix = ()[@@alert "-1"][@@alert "-2"] end

Class types
  $ cat > test.ml << EOF
  > class type class_type = ct[@test.clt "suffix"]
  > EOF
  $ ./driver.exe test.ml
  class type class_type = ct__suffix

Class type fields
  $ cat > test.ml << EOF
  > class type class_type_field = object
  >   val x : int [@@alert "-1"] [@@test.ctf "suffix"] [@@alert "-2"]
  > end
  > EOF
  $ ./driver.exe test.ml
  class type class_type_field =
    object val  x__suffix : int[@@alert "-1"][@@alert "-2"] end

Types
  $ cat > test.ml << EOF
  > module type S = sig
  >   val _e : (t[@alert "-1"] [@test.typ "suffix"] [@alert "-2"])
  > end
  > EOF
  $ ./driver.exe test.ml
  module type S  = sig val _e : ((t__suffix)[@alert "-1"][@alert "-2"]) end

Expressions
  $ cat > test.ml << EOF
  > let _ = foo [@alert "-1"] [@test.exp "suffix"] [@alert "-2"]
  > EOF
  $ ./driver.exe test.ml
  let _ = ((foo__suffix)[@alert "-1"][@alert "-2"])

Explicit test for the ident in a function application because it acts differently due to
"special functions".
  $ cat > test.ml << EOF
  > let _ = (foo [@alert "-1"] [@test.exp "suffix"] [@alert "-2"]) ()
  > EOF
  $ ./driver.exe test.ml
  let _ = ((foo__suffix)[@alert "-1"][@alert "-2"]) ()

Module expressions
  $ cat > test.ml << EOF
  > include M [@alert "-1"] [@test.mod_exp "suffix"] [@alert "-2"]
  > EOF
  $ ./driver.exe test.ml
  include ((M__suffix)[@alert "-1"][@alert "-2"])

Module types
  $ cat > test.ml << EOF
  > module F : S [@alert "-1"] [@test.mod_typ "suffix"] [@alert "-2"] = struct end
  > EOF
  $ ./driver.exe test.ml
  module F : ((S__suffix)[@alert "-1"][@alert "-2"]) = struct  end 

Patterns
  $ cat > test.ml << EOF
  > let _ = match () with (a [@test.pat "suffix"]) -> ignore a__suffix
  > EOF
  $ ./driver.exe test.ml
  let _ = match () with | a__suffix -> ignore a__suffix

Extension signature item
  $ cat > test.ml << EOF
  > module type S = sig
  >   [%%foo] [@@test.sig.ext "suffix"]
  > end
  > EOF
  $ ./driver.exe test.ml
  module type S  = sig [%%foo__suffix ] end

Extension structure item
  $ cat > test.ml << EOF
  > module S = struct
  >   [%%foo] [@@test.str.ext "suffix"]
  > end
  > EOF
  $ ./driver.exe test.ml
  module S = struct [%%foo__suffix ] end

Eval structure item 
  $ cat > test.ml << EOF
  > module _ = struct
  >   ident [@@test.str.evl "suffix"]
  > end
  > EOF
  $ ./driver.exe test.ml
  module _ = struct ;;ident__suffix end


Test that the "attr_multiple_replace" infrastructure works.
  $ cat > test.ml << EOF
  > let _ =
  >   foo
  >   [@alert "-1"]
  >   [@suffix "_suffix"]
  >   [@alert "-2"]
  >   [@prefix "prefix_"]
  >   [@alert "-3"]
  > EOF
  $ ./driver.exe test.ml
  let _ = ((prefix_foo_suffix)[@alert "-1"][@alert "-2"][@alert "-3"])

Demonstrate error when multiple instances of one attribute are passed.
  $ cat > test.ml << EOF
  > let _ =
  >   foo
  >   [@prefix "prefix_"]
  >   [@suffix "_suffix"]
  >   [@suffix "_again"]
  > EOF
  $ ./driver.exe test.ml
  File "test.ml", line 5, characters 4-10:
  5 |   [@suffix "_again"]
          ^^^^^^
  Error: Duplicated attribute
  [1]

Demonstrate error when multiple instances of one attribute are passed and
no other attributes are available.
  $ cat > test.ml << EOF
  > let _ =
  >   foo
  >   [@suffix "_suffix"]
  >   [@suffix "_again"]
  > EOF
  $ ./driver.exe test.ml
  File "test.ml", line 4, characters 4-10:
  4 |   [@suffix "_again"]
          ^^^^^^
  Error: Duplicated attribute
  [1]
