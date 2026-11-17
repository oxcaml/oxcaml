(* Compiled with [-bind Foo Lib__bar -open Wrap]: the [Foo] opened after the
   bound name shadows it. *)
let y = Foo.only_wrap
