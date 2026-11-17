(* Compiled WITHOUT [-bind]: [use_foo.cmi] does not mention [Foo], the type
   of [Use_foo.y] is recorded as [Lib__bar.t]. *)
let z : Lib__bar.t = Use_foo.y
