(* Regression test for oxcaml PR #7484 ("Avoid useless copies of functions
   (Again)"): a call-site [@zero_alloc assume] must cover closure allocations
   inside the inlined callee.

   How a call-site [assume] is meant to survive inlining: the assume lives in
   the call site's [Debuginfo.t]. When Simplify inlines the callee it records
   that dbg in the environment ([DE.enter_inlined_apply]), and every dbg in the
   inlined body is rewritten by [Inlined_debuginfo.rewrite], which calls
   [Debuginfo.inline] and meets the call site's [assume_zero_alloc] into it. The
   zero_alloc checker then ignores allocations whose dbg carries the assume.

   #7484 added an early-out to [simplify_function0]: if the old code was already
   simplified, needs no resimplification, the set has a single function and
   simplifying it removed no operations, keep the old code id. The old code's
   dbg is the original definition site, with no inlining frames and hence no
   assume. The closure below ([fun y -> ...]) is exactly such a set: one
   function, nothing to simplify once [x] is unknown. So the dbg of the
   allocation of a set of closures must come from the set of closures itself
   (which is rewritten upon inlining like any other term), not from the code of
   one of its functions; otherwise the checker reports the allocation against
   [f], at the bare definition location with no "inlined from" frame.

   [@inlined never] on the call is a workaround; [@zero_alloc assume] on the
   definition of [matches] is not. *)

type 'a t =
  | Base of 'a
  | And of 'a t * 'a t
  | Not of 'a t

let rec eval t f =
  match t with
  | Base x -> f x
  | And (a, b) -> eval a f && eval b f
  | Not a -> not (eval a f)

let matches t ~x = eval t (fun y -> Int.equal y x)

let[@zero_alloc] f t x = (matches [@zero_alloc assume]) t ~x
