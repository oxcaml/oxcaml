module D = Int_ops_data

let[@inline never] add () = D.x + D.y

let[@inline never] sub () = D.x - D.y

let[@inline never] mul () = D.x * D.y

let[@inline never] div () = if D.y = 0 then 0 else D.x / D.y

let[@inline never] mod_ () = D.x mod D.y

let[@inline never] land_ () = D.x land D.y

let[@inline never] lor_ () = D.x lor D.y

let[@inline never] lxor_ () = D.x lxor D.y

let[@inline never] lnot_ () = lnot D.x

let[@inline never] lsl_ () = D.x lsl D.y

let[@inline never] lsr_ () = D.x lsr D.y

let[@inline never] asr_ () = D.x asr D.y
