let[@inline never] [@local never] f_start () = ()

let _ = f_start ()

(* Test case 1: Simple top-level function with string parameter *)
let[@inline never] [@local never] f_simple_backtrace (message : string)
    (x : int) =
  let result = String.length message + x in
  result

let _ = f_simple_backtrace "hello world" 42

(* Test case 2: Recursive countdown with leaf function that takes a record *)
type debug_info =
  { name : string;
    value : int;
    timestamp : float
  }

let[@inline never] [@local never] f_leaf (info : debug_info) (extra : string) =
  let combined = info.name ^ " " ^ extra in
  let time_factor = int_of_float info.timestamp in
  String.length combined + info.value + time_factor

let[@inline never] [@local never] rec f_countdown (n : int) (msg : string) =
  if n <= 0
  then
    let debug_record = { name = msg; value = n * 10; timestamp = 3.14159 } in
    f_leaf debug_record "final" + 1
  else
    let next_msg = msg ^ ">" in
    f_countdown (n - 1) next_msg + 1

let _ = f_countdown 4 "start"
