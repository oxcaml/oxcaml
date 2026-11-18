exception Shared_exception

let exception_ = Shared_exception
let cell = ref 10
let read () = !cell
let b_identity : (int ref * (unit -> int) * exn) option ref = ref None
let () = Shared.record "both"
