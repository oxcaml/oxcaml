let apply5 f a b c d e = (Sys.opaque_identity f) a b c d e

let add5 a b c d e = a + b + c + d + e

let add6 a b c d e f = a + b + c + d + e + f

let partial = Sys.opaque_identity (apply5 add6 1 2 3 4 5)

let total () = apply5 add5 1 2 3 4 5 + partial 6
