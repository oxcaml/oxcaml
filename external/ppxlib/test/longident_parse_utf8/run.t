[Longident.parse] takes a string which starts like an identifier for the compiler, as
one with a non-ASCII letter does, as an identifier with a module path, not as an
operator.

  $ ./test.exe
  É.x: Ldot (Lident "\195\137", "x")
  x.É: Ldot (Lident "x", "\195\137")
  M.É: Ldot (Lident "M", "\195\137")
  +.: Lident "+."
  M.(+.): Ldot (Lident "M", "+.")
