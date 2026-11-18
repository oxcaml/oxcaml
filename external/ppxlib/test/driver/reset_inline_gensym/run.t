Generated names are numbered across the whole file, so a change to one inline
expectation renumbers the names in all the later ones.

  $ cat > test.ml <<EOF
  > type a = int [@@deriving_inline gensym]
  > [@@@end]
  > type b = int [@@deriving_inline gensym]
  > [@@@end]
  > EOF
  $ ./driver.exe -diff-cmd - -impl test.ml > /dev/null
  $ cat test.ml.ppx-corrected
  type a = int [@@deriving_inline gensym]
  let _ = fun (_ : a) -> ()
  let x__001_ = ()
  let _ = x__001_
  [@@@end]
  type b = int [@@deriving_inline gensym]
  let _ = fun (_ : b) -> ()
  let x__002_ = ()
  let _ = x__002_
  [@@@end]

With [-reset-inline-gensym], the numbering restarts before each inline expectation.

  $ ./driver.exe -diff-cmd - -reset-inline-gensym -impl test.ml > /dev/null
  $ cat test.ml.ppx-corrected
  type a = int [@@deriving_inline gensym]
  let _ = fun (_ : a) -> ()
  let x__001_ = ()
  let _ = x__001_
  [@@@end]
  type b = int [@@deriving_inline gensym]
  let _ = fun (_ : b) -> ()
  let x__001_ = ()
  let _ = x__001_
  [@@@end]
