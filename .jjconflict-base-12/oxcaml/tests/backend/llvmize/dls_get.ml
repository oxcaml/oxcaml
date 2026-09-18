type dls_state = Obj.t array

external get_dls_state : unit -> dls_state = "%dls_get"

let should_grow idx =
  let st = get_dls_state () in
  let sz = Array.length st in
  idx >= sz
