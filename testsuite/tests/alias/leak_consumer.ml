(* No [-alias]: resolves the included value and [Leak.Foo.x] through the
   signature recorded in [leak.cmi]. *)
let _ = Leak.c
let _ : Bar.t = Leak.Foo.x
