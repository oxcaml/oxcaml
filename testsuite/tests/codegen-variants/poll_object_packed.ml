external fully_polled : unit -> bool
  = "caml_sys_exit" "caml_code_is_fully_polled"
[@@noalloc]

let () =
  Printf.printf "sum %d, %s\n"
    (Poll_object_pack.Poll_object_unit.sum 0 4)
    (if fully_polled () then "fully polled" else "not fully polled")
