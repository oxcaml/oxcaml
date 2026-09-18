(* Intermediate unit for the transitive-inlining example in
   e2e_inline.ml. [relay] is [@inline always], so when [E2e_inline]
   inlines it, the closure pulled in from [E2e_inline_lib] is
   attributed directly to the lib: this [E2e_inline_mid] frame is
   dropped from the mangled name, because only the first debuginfo
   frame is kept (see #5099). *)

let[@inline always] relay a = E2e_inline_lib.relayed a
