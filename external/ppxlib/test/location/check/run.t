Rebuilding a [let*] doesn't trip the location check, even though the binding
operator's location overlaps the body.

  $ cat > letop.ml << EOF
  > let _ = [%rebuild_letop let* x = e in x]
  > EOF
  $ ./driver.exe -locations-check letop.ml
  let _ = let* x = e
           in x
