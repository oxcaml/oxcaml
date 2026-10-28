Ppxlib applies migrations to the AST. In some cases, especially when applying
downward migrations, extra information must be encoded in the AST in order to
be able to properly re-encode the AST when migrating back up.

This means that sometimes the extra information is left in the AST especially
when printing the code as source of the current compiler (where the current
compiler is less than the internal AST version).

Therefore we must clean the produced AST to remove ppxlib.migration attributes.

  $ touch file.ml
  $ ./driver.exe file.ml
  let f a = fun b -> a + b
  module F = (F)(struct  end)

Now we ensure no attributes are left in the AST after we force a downard
migration using the --use-compiler-pp flag.

  $ ./driver.exe --use-compiler-pp file.ml
  let f a b = a + b
  module F = (F)(struct  end)

