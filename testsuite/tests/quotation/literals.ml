(* TEST
 flags = "-extension runtime_metaprogramming -extension small_numbers";
 { expect; expect.opt; }
*)

#syntax quotations on

(* int *)
<[ 0 ]>
[%%expect {|
- : <[int]> expr = <[0]>
|}];;
<[ 1 ]>
[%%expect {|
- : <[int]> expr = <[1]>
|}];;
<[ -1 ]>
[%%expect {|
- : <[int]> expr = <[-1]>
|}];;
<[ 42 ]>
[%%expect {|
- : <[int]> expr = <[42]>
|}];;

(* char *)
<[ 'a' ]>
[%%expect {|
- : <[char]> expr = <['a']>
|}];;
<[ ' ' ]>
[%%expect {|
- : <[char]> expr = <[' ']>
|}];;
<[ '\n' ]>
[%%expect {|
- : <[char]> expr = <['\n']>
|}];;
<[ '\000' ]>
[%%expect {|
- : <[char]> expr = <['\000']>
|}];;

(* char# *)
<[ #'a' ]>
[%%expect {|
- : <[char#]> expr = <[#'a']>
|}];;
<[ #'\n' ]>
[%%expect {|
- : <[char#]> expr = <[#'\n']>
|}];;

(* string *)
<[ "" ]>
[%%expect {|
- : <[string]> expr = <[""]>
|}];;
<[ "foo" ]>
[%%expect {|
- : <[string]> expr = <["foo"]>
|}];;

(* float *)
<[ 0. ]>
[%%expect {|
- : <[float]> expr = <[0.]>
|}];;
<[ 1.5 ]>
[%%expect {|
- : <[float]> expr = <[1.5]>
|}];;
<[ -1.5 ]>
[%%expect {|
- : <[float]> expr = <[-1.5]>
|}];;

(* float32 *)
<[ 0.s ]>
[%%expect {|
- : <[float32]> expr = <[0.s]>
|}];;
<[ 1.5s ]>
[%%expect {|
- : <[float32]> expr = <[1.5s]>
|}];;
<[ -1.5s ]>
[%%expect {|
- : <[float32]> expr = <[-1.5s]>
|}];;

(* float# *)
<[ #0. ]>
[%%expect {|
- : <[float#]> expr = <[#0.]>
|}];;
<[ #1.5 ]>
[%%expect {|
- : <[float#]> expr = <[#1.5]>
|}];;
<[ -#1.5 ]>
[%%expect {|
- : <[float#]> expr = <[-#1.5]>
|}];;

(* float32# *)
<[ #0.s ]>
[%%expect {|
- : <[float32#]> expr = <[#0.s]>
|}];;
<[ #1.5s ]>
[%%expect {|
- : <[float32#]> expr = <[#1.5s]>
|}];;
<[ -#1.5s ]>
[%%expect {|
- : <[float32#]> expr = <[-#1.5s]>
|}];;

(* int8 *)
<[ 0s ]>
[%%expect {|
- : <[int8]> expr = <[0s]>
|}];;
<[ 127s ]>
[%%expect {|
- : <[int8]> expr = <[127s]>
|}];;
<[ -128s ]>
[%%expect {|
- : <[int8]> expr = <[-128s]>
|}];;

(* int16 *)
<[ 0S ]>
[%%expect {|
- : <[int16]> expr = <[0S]>
|}];;
<[ 32767S ]>
[%%expect {|
- : <[int16]> expr = <[32767S]>
|}];;
<[ -32768S ]>
[%%expect {|
- : <[int16]> expr = <[-32768S]>
|}];;

(* int32 *)
<[ 0l ]>
[%%expect {|
- : <[int32]> expr = <[0l]>
|}];;
<[ 2147483647l ]>
[%%expect {|
- : <[int32]> expr = <[2147483647l]>
|}];;
<[ -2147483648l ]>
[%%expect {|
- : <[int32]> expr = <[-2147483648l]>
|}];;

(* int64 *)
<[ 0L ]>
[%%expect {|
- : <[int64]> expr = <[0L]>
|}];;
<[ 9223372036854775807L ]>
[%%expect {|
- : <[int64]> expr = <[9223372036854775807L]>
|}];;
<[ -9223372036854775808L ]>
[%%expect {|
- : <[int64]> expr = <[-9223372036854775808L]>
|}];;

(* nativeint *)
<[ 0n ]>
[%%expect {|
- : <[nativeint]> expr = <[0n]>
|}];;
<[ 1n ]>
[%%expect {|
- : <[nativeint]> expr = <[1n]>
|}];;
<[ -1n ]>
[%%expect {|
- : <[nativeint]> expr = <[-1n]>
|}];;

(* int# *)
<[ #0m ]>
[%%expect {|
- : <[int#]> expr = <[#0m]>
|}];;
<[ #42m ]>
[%%expect {|
- : <[int#]> expr = <[#42m]>
|}];;
<[ -#1m ]>
[%%expect {|
- : <[int#]> expr = <[-#1m]>
|}];;

(* int8# *)
<[ #0s ]>
[%%expect {|
- : <[int8#]> expr = <[#0s]>
|}];;
<[ #127s ]>
[%%expect {|
- : <[int8#]> expr = <[#127s]>
|}];;
<[ -#128s ]>
[%%expect {|
- : <[int8#]> expr = <[-#128s]>
|}];;

(* int16# *)
<[ #0S ]>
[%%expect {|
- : <[int16#]> expr = <[#0S]>
|}];;
<[ #32767S ]>
[%%expect {|
- : <[int16#]> expr = <[#32767S]>
|}];;
<[ -#32768S ]>
[%%expect {|
- : <[int16#]> expr = <[-#32768S]>
|}];;

(* int32# *)
<[ #0l ]>
[%%expect {|
- : <[int32#]> expr = <[#0l]>
|}];;
<[ #2147483647l ]>
[%%expect {|
- : <[int32#]> expr = <[#2147483647l]>
|}];;
<[ -#2147483648l ]>
[%%expect {|
- : <[int32#]> expr = <[-#2147483648l]>
|}];;

(* int64# *)
<[ #0L ]>
[%%expect {|
- : <[int64#]> expr = <[#0L]>
|}];;
<[ #9223372036854775807L ]>
[%%expect {|
- : <[int64#]> expr = <[#9223372036854775807L]>
|}];;
<[ -#9223372036854775808L ]>
[%%expect {|
- : <[int64#]> expr = <[-#9223372036854775808L]>
|}];;

(* nativeint# *)
<[ #0n ]>
[%%expect {|
- : <[nativeint#]> expr = <[#0n]>
|}];;
<[ #1n ]>
[%%expect {|
- : <[nativeint#]> expr = <[#1n]>
|}];;
<[ -#1n ]>
[%%expect {|
- : <[nativeint#]> expr = <[-#1n]>
|}];;
