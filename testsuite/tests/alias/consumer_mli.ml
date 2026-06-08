(* No [-alias]: resolves [Withmli.w : Foo.t] to [Bar.t] through the hidden
   alias recorded in withmli.cmi. *)
let zz : Bar.t = Withmli.w
