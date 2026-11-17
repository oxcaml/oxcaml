(* Compiled with [-bind Foo Lib__bar -open Foo]: the [-open] refers to the
   name bound before it, so [x] is [Lib__bar.x]. *)
let y = x
