This test checks that an extension point or attribute that expands to another extension
point or attribute will continue to be expanded recursively until it cannot be
expanded anymore.

It uses the extension and attribute defined in driver.ml which expands:
- [%ext x ;; ["%ext", "@attr", ...]] into [%ext xx ;; ["@attr", ...]]
- [%ext x ;; ["@attr", "%ext", ...]] into xx [@attr ["%ext", ...]]
- [%ext x ;; []] into xx
- x [@attr ["%ext", "@attr", ...]] into [%ext xx ;; ["@attr", ...]]
- x [@attr ["@attr", "%ext", ...]] into xx [@attr ["%ext", ...]]
- x [@attr []] into xx

It adds an "x" to the ident to make it clear how many times the attribute has been applied.

First check that an extension point is expanded recursively:
  $ cat > test.ml << EOF
  > let () = [%ext a ;; ["%ext"]];;
  > EOF
  $ ./driver.exe test.ml
  let () = axx

An attribute is also expanded recursively:
  $ cat > test.ml << EOF
  > let () = b [@attr ["@attr"]];;
  > EOF
  $ ./driver.exe test.ml
  let () = bxx

An extension that expands into an attribute and vice versa work:
  $ cat > test.ml << EOF
  > let () = [%ext c ;; ["@attr"]];;
  > let () = d [@attr ["%ext"]];;
  > EOF
  $ ./driver.exe test.ml
  let () = cxx
  let () = dxx

Something a bit silly to validate it carries on until it's done:
  $ cat > test.ml << EOF
  > let () = e [@attr ["%ext"; "@attr"; "@attr"; "%ext"; "%ext"]];;
  > EOF
  $ ./driver.exe test.ml
  let () = exxxxxx
