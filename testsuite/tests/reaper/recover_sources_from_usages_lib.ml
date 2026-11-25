external opaque : 'a -> 'a = "%opaque"

(* The set of closures allocated by [make_c] and [mk (opaque 20) 7] share the
   function slot of [mk], and the reaper changes the representation of both.
   Likewise, [make_g]'s set of closures and [mkg (mk (opaque 20) 7)] share the
   function slot of [mkg].

   [o] is always [make_g c1] or [make_g c2], but it may come from [id], which
   is exported, so it has [any_source] and both sets of closures of [mkg] are
   candidates when rewriting its type. The metadata of its value slot [v] is
   then the join of [c1] and [c2], which escape and have [any_usage], and of
   [mk (opaque 20) 7], which has known usages. Recovering the sources from
   these usages only found [mk (opaque 20) 7] and dropped [make_c]'s set of
   closures, so that [o]'s exported type gave [v] the wrong representation, and
   inlining [o] below reached [Invalid] at run time.

   The call [r] in the library is needed to trigger the bug: it is a direct
   call to the code of [o] with [o] as callee, so the [my_closure] of that code
   has [any_source]. Reading [v] from it may then read it from
   [mkg (mk (opaque 20) 7)], which prevents changing its representation.
   Otherwise, its [v] slot is deleted, the two candidate sets for [o] have
   incompatible representations, and [o]'s type is forgotten.
   *)

let[@inline] mk a b =
  let () = () in
  fun () -> a + b

let[@inline never] make_c x = mk 10 x

let[@inline] mkg v =
  let () = () in
  fun () -> v () + 1

let[@inline never] make_g v = mkg v

let[@opaque] escape _x = ()

let c1 = make_c 5
let c2 = make_c 6

let () = escape c1
let () = escape (mkg (mk (opaque 20) 7))

let[@inline never] id x = x
let o = if opaque true then make_g c1 else id (make_g c2)

let r = (o[@inlined never]) ()