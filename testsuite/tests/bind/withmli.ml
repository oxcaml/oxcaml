(* References [Foo], resolved via [-bind Foo Lib__bar]; checked against
   withmli.mli, which was also compiled with -bind. *)
let w = Foo.x
