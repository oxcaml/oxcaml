let x = Sys.opaque_identity (-15)

let y = Sys.opaque_identity 4

let[@inline never] add () = x + y

let[@inline never] sub () = x - y

let[@inline never] mul () = x * y

let[@inline never] div () = if y = 0 then 0 else x / y

let[@inline never] mod_ () = x mod y

let[@inline never] land_ () = x land y

let[@inline never] lor_ () = x lor y

let[@inline never] lor_ () = x lor y

let[@inline never] lxor_ () = x lxor y

let[@inline never] lnot_ () = lnot x

let[@inline never] lsl_ () = x lsl y

let[@inline never] lsr_ () = x lsr y

let[@inline never] asr_ () = x asr y
