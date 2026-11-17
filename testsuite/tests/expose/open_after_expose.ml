(* Compiled with [-expose Lib__bar Foo -open Wrap]: the [Foo] opened after the
   exposed name shadows it. *)
let y = Foo.only_wrap
