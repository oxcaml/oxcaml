external print_and_add : int -> int -> int = "" "print_and_add"

external too_many :
  int ->
  int ->
  int ->
  int ->
  int ->
  int ->
  int ->
  int ->
  int ->
  int ->
  int ->
  int ->
  int = "" "too_many"

external int_and_float : int -> float -> int -> float -> unit = "int_and_float"

let[@inline never] [@local never] call_too_many () =
  too_many 1 2 3 4 5 6 7 8 9 10 11 12

let[@inline never] [@local never] call_print_and_add () = print_and_add 9 10

let[@inline never] [@local never] call_int_and_float () =
  int_and_float 1 2.0 3 4.0
