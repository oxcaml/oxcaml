let[@inline never] [@local never] f_start () = ()

let _ = f_start ()

(* Functions with multiple arguments *)
let[@inline never] [@local never] f_two_args (x : int) (y : string) = x, y

let _ = f_two_args 42 "hello"

let _ = f_two_args (-10) "world"

let _ = f_two_args 0 ""

let[@inline never] [@local never] f_three_args (x : int) (y : float) (z : bool)
    =
  x, y, z

let _ = f_three_args 42 3.14 true

let _ = f_three_args (-5) 0.0 false

let _ = f_three_args max_int infinity true

let[@inline never] [@local never] f_five_args (a : int) (b : string) (c : float)
    (d : bool) (e : char) =
  a, b, c, d, e

let _ = f_five_args 100 "test" 2.71 false 'x'

let _ = f_five_args (-42) "debug" (-1.5) true '\n'

(* Pattern matching on pairs and tuples *)
(* CR sspies: Functions with pattern matching parameters display as generic
   'param=[...] : ocaml_value' in the debugger rather than showing the
   destructured variable names and their individual types. For example,
   f_pair_match shows 'param=[42, "hello"] : ocaml_value' instead of showing the
   extracted 'x : int' and 'y : string' variables separately. *)
let[@inline never] [@local never] f_pair_match ((x, y) : int * string) =
  let result = x + String.length y in
  result

let _ = f_pair_match (42, "hello")

let _ = f_pair_match (0, "")

let _ = f_pair_match (-10, "test")

let[@inline never] [@local never] f_triple_match
    ((x, y, z) : int * float * bool) =
  if z then x + int_of_float y else x - int_of_float y

let _ = f_triple_match (100, 3.14, true)

let _ = f_triple_match (50, 2.5, false)

let _ = f_triple_match (-20, 0.5, true)

let[@inline never] [@local never] f_nested_tuple_match
    (((a, b), (c, d)) : (int * string) * (float * bool)) =
  a, b, c, d

let _ = f_nested_tuple_match ((42, "inner"), (3.14, true))

let _ = f_nested_tuple_match ((0, "empty"), (0.0, false))

(* Record pattern matching *)
type point =
  { x : int;
    y : int
  }

type person =
  { name : string;
    age : int;
    active : bool
  }

let[@inline never] [@local never] f_record_match { x; y } = (x * x) + (y * y)

let _ = f_record_match { x = 3; y = 4 }

let _ = f_record_match { x = 0; y = 0 }

let _ = f_record_match { x = -5; y = 12 }

let[@inline never] [@local never] f_person_match { name; age; active } =
  if active then name ^ " (" ^ string_of_int age ^ ")" else "inactive"

let _ = f_person_match { name = "Alice"; age = 30; active = true }
