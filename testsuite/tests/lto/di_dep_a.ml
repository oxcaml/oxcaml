let k = Sys.opaque_identity 3

let[@inline always] triple n = n * k
