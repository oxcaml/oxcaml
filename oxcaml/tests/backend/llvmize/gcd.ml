module D = Gcd_data

let[@inline never] gcd () =
  while !D.y <> 0 do
    let new_x = !D.y in
    let new_y = !D.x mod !D.y in
    D.x := new_x;
    D.y := new_y
  done;
  D.res := !D.x
