let[@inline never] [@local never] call_with x =
  let x = Many_args_defn.huge_add x x 3 x x x x x x x x x (x + 30) in
  x + 1
