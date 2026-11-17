(* Compiled with [-open Wrap -bind Foo Lib__bar]: the bound name shadows the
   [Foo] opened before it, so [Foo.x] is [Lib__bar.x]. *)
let y = Foo.x
