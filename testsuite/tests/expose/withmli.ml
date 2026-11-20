(* References [Foo], resolved via [-expose Lib__bar Foo]; checked against
   withmli.mli, which was also compiled with -expose. *)
let w = Foo.x
