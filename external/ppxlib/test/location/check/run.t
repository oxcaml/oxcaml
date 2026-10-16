Rebuilding a [let*] doesn't trip the location check, even though the binding
operator's location overlaps the body.

  $ cat > letop.ml << EOF
  > let _ = [%rebuild_letop let* x = e in x]
  > EOF
  $ ./driver.exe -locations-check letop.ml
  let _ = let* x = e
           in x

The location check rejects a jkind annotation whose location is outside its
parent's.

  $ cat > bad_jkind_loc.ml << EOF
  > [%%bad_jkind_loc]
  > EOF
  $ ./driver.exe -locations-check bad_jkind_loc.ml
  File "bad_jkind_loc.ml", line 1, characters 0-17:
  1 | [%%bad_jkind_loc]
      ^^^^^^^^^^^^^^^^^
  Error: invalid output from ppx:
         this type declaration is built from a jkind annotation whose location is outside of this node's.
  Child jkind annotation found at:
  File "bad_jkind_loc.ml", line 1, characters 0-22:
  [1]
