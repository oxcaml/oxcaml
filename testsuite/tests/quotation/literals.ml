(* TEST
 flags = "-extension runtime_metaprogramming -extension small_numbers";
 expect;
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
>> Fatal error: Translquote: cannot quote constant #'a'
Uncaught exception: Misc.Fatal_error

|}];;
<[ #'\n' ]>
[%%expect {|
>> Fatal error: Translquote: cannot quote constant #'\n'
Uncaught exception: Misc.Fatal_error

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
- : <[float#]> expr = <[#-1.5]>
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
- : <[float32#]> expr = <[#-1.5s]>
|}];;

(* int8 *)
<[ 0s ]>
[%%expect {|
>> Fatal error: Translquote: cannot quote constant 0s
Uncaught exception: Misc.Fatal_error

|}];;
<[ 127s ]>
[%%expect {|
>> Fatal error: Translquote: cannot quote constant 127s
Uncaught exception: Misc.Fatal_error

|}];;
<[ -128s ]>
[%%expect {|
>> Fatal error: Translquote: cannot quote constant -128s
Uncaught exception: Misc.Fatal_error

|}];;

(* int16 *)
<[ 0S ]>
[%%expect {|
>> Fatal error: Translquote: cannot quote constant 0S
Uncaught exception: Misc.Fatal_error

|}];;
<[ 32767S ]>
[%%expect {|
>> Fatal error: Translquote: cannot quote constant 32767S
Uncaught exception: Misc.Fatal_error

|}];;
<[ -32768S ]>
[%%expect {|
>> Fatal error: Translquote: cannot quote constant -32768S
Uncaught exception: Misc.Fatal_error

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
>> Fatal error: Translquote: cannot quote constant #0m
Uncaught exception: Misc.Fatal_error

|}];;
<[ #42m ]>
[%%expect {|
>> Fatal error: Translquote: cannot quote constant #42m
Uncaught exception: Misc.Fatal_error

|}];;
<[ -#1m ]>
[%%expect {|
>> Fatal error: Translquote: cannot quote constant -#1m
Uncaught exception: Misc.Fatal_error

|}];;

(* int8# *)
<[ #0s ]>
[%%expect {|
>> Fatal error: Translquote: cannot quote constant #0s
Uncaught exception: Misc.Fatal_error

|}];;
<[ #127s ]>
[%%expect {|
>> Fatal error: Translquote: cannot quote constant #127s
Uncaught exception: Misc.Fatal_error

|}];;
<[ -#128s ]>
[%%expect {|
>> Fatal error: Translquote: cannot quote constant -#128s
Uncaught exception: Misc.Fatal_error

|}];;

(* int16# *)
<[ #0S ]>
[%%expect {|
>> Fatal error: Translquote: cannot quote constant #0S
Uncaught exception: Misc.Fatal_error

|}];;
<[ #32767S ]>
[%%expect {|
>> Fatal error: Translquote: cannot quote constant #32767S
Uncaught exception: Misc.Fatal_error

|}];;
<[ -#32768S ]>
[%%expect {|
>> Fatal error: Translquote: cannot quote constant -#32768S
Uncaught exception: Misc.Fatal_error

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
- : <[int32#]> expr = <[#-2147483648l]>
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
- : <[int64#]> expr = <[#-9223372036854775808L]>
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
- : <[nativeint#]> expr = <[#-1n]>
|}];;
