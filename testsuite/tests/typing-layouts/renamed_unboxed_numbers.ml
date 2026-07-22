(* TEST
 expect;
*)

let nativeint (x : nativeint_u) : nativeint# = x
let int32 (x : int32_u) : int32# = x
let int64 (x : int64_u) : int64# = x
[%%expect{|
val nativeint : nativeint_u -> nativeint# = <fun>
val int32 : int32_u -> int32# = <fun>
val int64 : int64_u -> int64# = <fun>
|}]

type nativeint_u_kind : word = nativeint_u
type int32_u_kind : bits32 = int32_u
type int64_u_kind : bits64 = int64_u
[%%expect{|
type nativeint_u_kind = nativeint_u
type int32_u_kind = int32_u
type int64_u_kind = int64_u
|}]
