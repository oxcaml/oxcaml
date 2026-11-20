(* Compiled with [-open Wrap -expose Lib__bar Foo]: the exposed name shadows the
   [Foo] opened before it, so [Foo.x] is [Lib__bar.x]. *)
let y = Foo.x
