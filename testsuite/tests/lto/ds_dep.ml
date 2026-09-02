(* A closure-allocating function that is always inlined into consumers. After
   pass 1, every caller has inlined [make], so nothing in the program uses this
   unit's copy: the whole-program Reaper deletes [make]'s code, and with it the
   only occurrence of [f]'s set of closures in this unit. Consumers' inlined
   copies of the set survive, and their slots belong to this unit, so this
   unit's reaped .cmx must still export live offsets for them (from the pass-1
   assignment recorded in the .cmr). *)
let[@inline always] make x =
  let[@inline never] [@local never] f () = x + 1 in
  f
