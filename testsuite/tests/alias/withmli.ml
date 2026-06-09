(* References [Foo], resolved via [-alias Foo Bar]; checked against withmli.mli,
   which was also compiled with -alias. The aliases injected into the .mli and
   the .ml must resolve to the same module so the implementation matches. *)
let w = Foo.x
