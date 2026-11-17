(* Compiled with [-expose Lib__bar Foo -open Foo]: the [-open] refers to the
   name exposed before it, so [x] is [Lib__bar.x]. *)
let y = x
