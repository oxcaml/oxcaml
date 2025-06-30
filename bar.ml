
let tail_call _ = true

external opaque : 'a -> 'b = "%opaque"

let f x =
  let _unused_local = ref None in
  opaque false && tail_call x
