let x : int ref = Gcd_data.x

let y : int ref = Gcd_data.y

let res : int ref = Gcd_data.res

let[@inline never] gcd () =
  while !y <> 0 do
    let new_x = !y in
    let new_y = !x mod !y in
    x := new_x;
    y := new_y
  done;
  res := !x
