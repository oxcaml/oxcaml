external get : bytes -> int -> int64x2# = "%caml_bytes_getu128u#"
external set : bytes -> int -> int64x2# -> unit = "%caml_bytes_setu128u#"

let[@inline never] copy0 src dst = set dst 0 (get src 0)
let[@inline never] copy16 src dst = set dst 16 (get src 16)
let[@inline never] copy1 src dst = set dst 1 (get src 1)
let[@inline never] copy65520 src dst = set dst 65520 (get src 65520)
let[@inline never] copy65536 src dst = set dst 65536 (get src 65536)
