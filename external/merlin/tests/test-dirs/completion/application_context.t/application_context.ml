let foo ~i ~j ?k ~(l : [%call_pos]) = i + j
let bar = 10
let y = foo ~i:5
