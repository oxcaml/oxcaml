external __dummy2__ : unit -> 'a = "%opaque"
let merge_fold cmp =
  let loop () () = (__dummy2__ ()) cmp
  [@@inline never ][@@local never ] in
  loop () ()
  [@@inline ][@@local never ]

let merge_iter () =
  merge_fold (__dummy2__ ()) [@@inline ][@@local never ]