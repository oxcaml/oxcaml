(* TEST
 flags = "-extension small_numbers";
 expect;
*)

(* Operations are tested in tests/small_numbers *)

(* Boxed float32 *)

type t = float32;;
[%%expect{|
type t = float32
|}];;

let _ : float32 = 1.0s;;
[%%expect{|
- : float32 = 1.s
|}];;

let _ : float32 = 1.s;;
[%%expect{|
- : float32 = 1.s
|}];;

let _ : float32 = 1e10s;;
[%%expect{|
- : float32 = 1e+10s
|}];;

let _ : float32 = 1e+1s;;
[%%expect{|
- : float32 = 10.s
|}];;

let _ : float32 = 1e-1s;;
[%%expect{|
- : float32 = 0.100000001s
|}];;

let _ : float32 = 0x111.000s;;
[%%expect{|
- : float32 = 273.s
|}];;

let _ : float32 = 0x1.4p+0s;;
[%%expect{|
- : float32 = 1.25s
|}];;

let _ : float32 = 0xf.ffffffffffff8p+1020s;;
[%%expect{|
- : float32 = infs
|}];;

let _ : float32 = 0x8p-972s;;
[%%expect{|
- : float32 = 0.s
|}];;

let _ : float32 = 0xc.d5e6fp+1_24s;;
[%%expect{|
- : float32 = 2.72982066e+38s
|}];;

(* A (trivial) match with no float32 cases is allowed. *)
let () =
  match 0.0s with
  | _ -> ()
;;
[%%expect{|
|}];;

let () =
  match 0.0s with
  | 0.0s -> ()
  | _ -> ()
;;
[%%expect{|
Line 1:
Error: float32 literal patterns are not supported.
|}];;

let () =
  match 0.0s with
  | 0.0s -> ()
  | 1.0s -> ()
  | _ -> ()
;;
[%%expect{|
Line 1:
Error: float32 literal patterns are not supported.
|}];;

let () =
  match 0.0s with
  | 0.0s -> ()
;;
[%%expect{|
Line 1:
Error: float32 literal patterns are not supported.
|}];;

(* Unboxed float32 *)

let ignore (f : float32#) = ()
[%%expect{|
val ignore : float32# -> unit = <fun>
|}];;

type t = float32#;;
[%%expect{|
type t = float32#
|}];;

let () = ignore #1.0s;;
[%%expect{|
|}];;

let () = ignore #1.s;;
[%%expect{|
|}];;

let () = ignore #1e10s;;
[%%expect{|
|}];;

let () = ignore #1e+1s;;
[%%expect{|
|}];;

let () = ignore #1e-1s;;
[%%expect{|
|}];;

let () = ignore #0x111.000s;;
[%%expect{|
|}];;

let () = ignore #0x1.4p+0s;;
[%%expect{|
|}];;

let () = ignore #0xf.ffffffffffff8p+1020s;;
[%%expect{|
|}];;

let () = ignore #0x8p-972s;;
[%%expect{|
|}];;

let () = ignore #0xc.d5e6fp+1_24s;;
[%%expect{|
|}];;

(* A (trivial) match with no float32 cases is allowed. *)
let () =
  match #0.0s with
  | _ -> ()
;;
[%%expect{|
|}];;

let () =
  match #0.0s with
  | #0.0s -> ()
  | _ -> ()
;;
[%%expect{|
Line 1:
Error: float32 literal patterns are not supported.
|}];;

let () =
  match #0.0s with
  | #0.0s -> ()
  | #1.0s -> ()
  | _ -> ()
;;
[%%expect{|
Line 1:
Error: float32 literal patterns are not supported.
|}];;

let () =
  match #0.0s with
  | #0.0s -> ()
;;
[%%expect{|
Line 1:
Error: float32 literal patterns are not supported.
|}];;

(* Tagged int8 *)

type t = int8
[%%expect{|
type t = int8
|}]

let _ : int8 = 1s
let _ : int8 = 0s
let _ : int8 = 127s
let _ : int8 = -128s
let _ : int8 = 128s
[%%expect{|
- : int8 = 1s
- : int8 = 0s
- : int8 = 127s
- : int8 = -128s
- : int8 = -128s
|}]

let _ : int8 = 129s
[%%expect{|
Line 1, characters 15-19:
1 | let _ : int8 = 129s
                   ^^^^
Error: Integer literal exceeds the range of representable integers of type "int8"
|}]

let _ : int8 = -129s
[%%expect{|
Line 1, characters 15-20:
1 | let _ : int8 = -129s
                   ^^^^^
Error: Integer literal exceeds the range of representable integers of type "int8"
|}]

let _ : int8 = 0x7fs
let _ : int8 = -0x80s
let _ : int8 = 0xffs
let _ : int8 = -0xffs
[%%expect{|
- : int8 = 127s
- : int8 = -128s
- : int8 = -1s
- : int8 = 1s
|}]

let _ : int8 = 0x1_00s
[%%expect{|
Line 1, characters 15-22:
1 | let _ : int8 = 0x1_00s
                   ^^^^^^^
Error: Integer literal exceeds the range of representable integers of type "int8"
|}]

let _ : int8 = -0x1_00s
[%%expect{|
Line 1, characters 15-23:
1 | let _ : int8 = -0x1_00s
                   ^^^^^^^^
Error: Integer literal exceeds the range of representable integers of type "int8"
|}]

let _ : int8 = 0o177s
let _ : int8 = -0o200s
let _ : int8 = 0o377s
let _ : int8 = -0o377s
[%%expect{|
- : int8 = 127s
- : int8 = -128s
- : int8 = -1s
- : int8 = 1s
|}]

let _ : int8 = 0o400s
[%%expect{|
Line 1, characters 15-21:
1 | let _ : int8 = 0o400s
                   ^^^^^^
Error: Integer literal exceeds the range of representable integers of type "int8"
|}]

let _ : int8 = -0o400s
[%%expect{|
Line 1, characters 15-22:
1 | let _ : int8 = -0o400s
                   ^^^^^^^
Error: Integer literal exceeds the range of representable integers of type "int8"
|}]

let _ : int8 = 0b0111_1111s
let _ : int8 = -0b1000_0000s
let _ : int8 = 0b1111_1111s
let _ : int8 = -0b1111_1111s
[%%expect{|
- : int8 = 127s
- : int8 = -128s
- : int8 = -1s
- : int8 = 1s
|}]

let _ : int8 = 0b1_0000_0000s
[%%expect{|
Line 1, characters 15-29:
1 | let _ : int8 = 0b1_0000_0000s
                   ^^^^^^^^^^^^^^
Error: Integer literal exceeds the range of representable integers of type "int8"
|}]

let _ : int8 = -0b1_0000_0000s
[%%expect{|
Line 1, characters 15-30:
1 | let _ : int8 = -0b1_0000_0000s
                   ^^^^^^^^^^^^^^^
Error: Integer literal exceeds the range of representable integers of type "int8"
|}]

let _ : int =
  match 0s with
  | _ -> 0
;;
[%%expect{|
- : int = 0
|}]

let _ : int =
  match 0s with
  | 0s -> 0
  | _ -> 1
;;
[%%expect{|
- : int = 0
|}]

let _ : int =
  match 1s with
  | 0s -> 0
  | 1s -> 1
  | _ -> 2
;;
[%%expect{|
- : int = 1
|}]

let _ : int =
  match -1s with
  | 0s -> 0
  | 1s -> 1
  | _ -> 2
;;
[%%expect{|
- : int = 2
|}]

(* Partial match *)
let _ : int =
  match 1s with
  | 0s -> 0
  | 1s -> 1
;;
[%%expect{|
Lines 2-4, characters 2-11:
2 | ..match 1s with
3 |   | 0s -> 0
4 |   | 1s -> 1
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
2s

- : int = 1
|}]

let _ : int =
  match 55s with
  | -128s | -127s | -126s | -125s | -124s | -123s | -122s | -121s | -120s
  | -119s | -118s | -117s | -116s | -115s | -114s | -113s | -112s | -111s
  | -110s | -109s | -108s | -107s | -106s | -105s | -104s | -103s | -102s
  | -101s | -100s | -99s | -98s | -97s | -96s | -95s | -94s | -93s | -92s | -91s
  | -90s | -89s | -88s | -87s | -86s | -85s | -84s | -83s | -82s | -81s | -80s
  | -79s | -78s | -77s | -76s | -75s | -74s | -73s | -72s | -71s | -70s | -69s
  | -68s | -67s | -66s | -65s | -64s | -63s | -62s | -61s | -60s | -59s | -58s
  | -57s | -56s | -55s | -54s | -53s | -52s | -51s | -50s | -49s | -48s | -47s
  | -46s | -45s | -44s | -43s | -42s | -41s | -40s | -39s | -38s | -37s | -36s
  | -35s | -34s | -33s | -32s | -31s | -30s | -29s | -28s | -27s | -26s | -25s
  | -24s | -23s | -22s | -21s | -20s | -19s | -18s | -17s | -16s | -15s | -14s
  | -13s | -12s | -11s | -10s | -9s | -8s | -7s | -6s | -5s | -4s | -3s | -2s
  | -1s | 0s | 1s | 2s | 3s | 4s | 5s | 6s | 7s | 8s | 9s | 10s | 11s | 12s
  | 13s | 14s | 15s | 16s | 17s | 18s | 19s | 20s | 21s | 22s | 23s | 24s | 25s
  | 26s | 27s | 28s | 29s | 30s | 31s | 32s | 33s | 34s | 35s | 36s | 37s | 38s
  | 39s | 40s | 41s | 42s | 43s | 44s | 45s | 46s | 47s | 48s | 49s | 50s | 51s
  | 52s | 53s | 54s | 55s | 56s | 57s | 58s | 59s | 60s | 61s | 62s | 63s | 64s
  | 65s | 66s | 67s | 68s | 69s | 70s | 71s | 72s | 73s | 74s | 75s | 76s | 77s
  | 78s | 79s | 80s | 81s | 82s | 83s | 84s | 85s | 86s | 87s | 88s | 89s | 90s
  | 91s | 92s | 93s | 94s | 95s | 96s | 97s | 98s | 99s | 100s | 101s | 102s
  | 103s | 104s | 105s | 106s | 107s | 108s | 109s | 110s | 111s | 112s | 113s
  | 114s | 115s | 116s | 117s | 118s | 119s | 120s | 121s | 122s | 123s | 124s
  | 125s | 126s | 127s -> 0
[%%expect{|
- : int = 0
|}]


(* Tagged int16 *)

type t = int16
[%%expect{|
type t = int16
|}]

let _ : int16 = 1S
let _ : int16 = 0S
let _ : int16 = 32767S
let _ : int16 = -32768S
let _ : int16 = 32768S
[%%expect{|
- : int16 = 1S
- : int16 = 0S
- : int16 = 32767S
- : int16 = -32768S
- : int16 = -32768S
|}]

let _ : int16 = 32769S
[%%expect{|
Line 1, characters 16-22:
1 | let _ : int16 = 32769S
                    ^^^^^^
Error: Integer literal exceeds the range of representable integers of type "int16"
|}]

let _ : int16 = -32769S
[%%expect{|
Line 1, characters 16-23:
1 | let _ : int16 = -32769S
                    ^^^^^^^
Error: Integer literal exceeds the range of representable integers of type "int16"
|}]

let _ : int16 = 0x7f_ffS
let _ : int16 = -0x80_00S
let _ : int16 = 0xff_ffS
let _ : int16 = -0xff_ffS
[%%expect{|
- : int16 = 32767S
- : int16 = -32768S
- : int16 = -1S
- : int16 = 1S
|}]

let _ : int16 = 0x1_00_00S
[%%expect{|
Line 1, characters 16-26:
1 | let _ : int16 = 0x1_00_00S
                    ^^^^^^^^^^
Error: Integer literal exceeds the range of representable integers of type "int16"
|}]

let _ : int16 = -0x1_00_00S
[%%expect{|
Line 1, characters 16-27:
1 | let _ : int16 = -0x1_00_00S
                    ^^^^^^^^^^^
Error: Integer literal exceeds the range of representable integers of type "int16"
|}]

let _ : int16 = 0o77777S
let _ : int16 = -0o100000S
let _ : int16 = 0o177777S
let _ : int16 = -0o177777S
[%%expect{|
- : int16 = 32767S
- : int16 = -32768S
- : int16 = -1S
- : int16 = 1S
|}]

let _ : int16 = 0o200000S
[%%expect{|
Line 1, characters 16-25:
1 | let _ : int16 = 0o200000S
                    ^^^^^^^^^
Error: Integer literal exceeds the range of representable integers of type "int16"
|}]

let _ : int16 = -0o200000S
[%%expect{|
Line 1, characters 16-26:
1 | let _ : int16 = -0o200000S
                    ^^^^^^^^^^
Error: Integer literal exceeds the range of representable integers of type "int16"
|}]

let _ : int16 = 0b0111_1111_1111_1111S
let _ : int16 = -0b1000_0000_0000_0000S
let _ : int16 = 0b1111_1111_1111_1111S
let _ : int16 = -0b1111_1111_1111_1111S
[%%expect{|
- : int16 = 32767S
- : int16 = -32768S
- : int16 = -1S
- : int16 = 1S
|}]

let _ : int16 = 0b1_0000_0000_0000_0000S
[%%expect{|
Line 1, characters 16-40:
1 | let _ : int16 = 0b1_0000_0000_0000_0000S
                    ^^^^^^^^^^^^^^^^^^^^^^^^
Error: Integer literal exceeds the range of representable integers of type "int16"
|}]

let _ : int16 = -0b1_0000_0000_0000_0000S
[%%expect{|
Line 1, characters 16-41:
1 | let _ : int16 = -0b1_0000_0000_0000_0000S
                    ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Integer literal exceeds the range of representable integers of type "int16"
|}]

let _ : int =
  match 0S with
  | _ -> 0
;;
[%%expect{|
- : int = 0
|}]

let _ : int =
  match 0S with
  | 0S -> 0
  | _ -> 1
;;
[%%expect{|
- : int = 0
|}]

let _ : int =
  match 1S with
  | 0S -> 0
  | 1S -> 1
  | _ -> 2
;;
[%%expect{|
- : int = 1
|}]

let _ : int =
  match -1S with
  | 0S -> 0
  | 1S -> 1
  | _ -> 2
;;
[%%expect{|
- : int = 2
|}]

(* Partial match *)
let _ : int =
  match 1S with
  | 0S -> 0
  | 1S -> 1
;;
[%%expect{|
Lines 2-4, characters 2-11:
2 | ..match 1S with
3 |   | 0S -> 0
4 |   | 1S -> 1
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
2S

- : int = 1
|}]

(* Untagged char *)
let ignore (_ : char#) = ()
[%%expect{|
val ignore : char# -> unit = <fun>
|}]

type t = char#
[%%expect{|
type t = char#
|}]

let () = ignore #'a'
[%%expect{|
|}]

let () = ignore #'\xFF'
[%%expect{|
|}]

let () = ignore #'\o000'
[%%expect{|
|}]

let () = ignore #'\123'
[%%expect{|
|}]

let _ : int =
  match #'a' with
  | _ -> 0
;;
[%%expect{|
- : int = 0
|}]

let _ : int =
  match #'a' with
  | #'a' -> 0
  | _ -> 1
;;
[%%expect{|
- : int = 0
|}]

let _ : int =
  match #'A' with
  | #'a' -> 0
  | #'A' -> 1
  | _ -> 2
;;
[%%expect{|
- : int = 1
|}]

let _ : int =
  match #'z' with
  | #'a' -> 0
  | #'A' -> 1
  | _ -> 2
;;
[%%expect{|
- : int = 2
|}]

let _ : int =
  match #'z' with
  | #'a'..#'z' -> 0
  | #'A' -> 1
  | _ -> 2
;;
[%%expect{|
- : int = 0
|}]

let _ : int =
  match #'m' with
  | #'a'..#'z' -> 0
  | #'A' -> 1
  | _ -> 2
;;
[%%expect{|
- : int = 0
|}]

let _ : int =
  match #'A' with
  | #'a'..#'z' -> 0
  | #'A' -> 1
  | _ -> 2
;;
[%%expect{|
- : int = 1
|}]

let _ : int =
  match #'B' with
  | #'a'..#'z' -> 0
  | #'A' -> 1
  | _ -> 2
;;
[%%expect{|
- : int = 2
|}]

(* Partial match *)
let _ : int =
  match #'b' with
  | #'a' -> 0
  | #'b' -> 1
;;
[%%expect{|
Lines 2-4, characters 2-13:
2 | ..match #'b' with
3 |   | #'a' -> 0
4 |   | #'b' -> 1
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
#'c'

- : int = 1
|}]


(* Untagged int8 *)

let ignore (_ : int8#) = ()
[%%expect{|
val ignore : int8# -> unit = <fun>
|}]

type t = int8#
[%%expect{|
type t = int8#
|}]

let () = ignore #1s
[%%expect{|
|}]

let () = ignore #0s
[%%expect{|
|}]

let () = ignore (-#128s)
[%%expect{|
|}]

let () = ignore (-#127s)
[%%expect{|
|}]

let () = ignore #0x7fs
[%%expect{|
|}]

let () = ignore (-#0x80s)
[%%expect{|
|}]

let () = ignore #0o177s
[%%expect{|
|}]

let () = ignore #0b01111111s
[%%expect{|
|}]

let _ : int =
  match #0s with
  | _ -> 0
;;
[%%expect{|
- : int = 0
|}]

let _ : int =
  match #0s with
  | #0s -> 0
  | _ -> 1
;;
[%%expect{|
- : int = 0
|}]

let _ : int =
  match #1s with
  | #0s -> 0
  | #1s -> 1
  | _ -> 2
;;
[%%expect{|
- : int = 1
|}]

let _ : int =
  match -#1s with
  | #0s -> 0
  | #1s -> 1
  | _ -> 2
;;
[%%expect{|
- : int = 2
|}]

(* Partial match *)
let _ : int =
  match #1s with
  | #0s -> 0
  | #1s -> 1
;;
[%%expect{|
Lines 2-4, characters 2-12:
2 | ..match #1s with
3 |   | #0s -> 0
4 |   | #1s -> 1
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
#2s

- : int = 1
|}]

let _ : int =
  match #55s with
  | -#128s | -#127s | -#126s | -#125s | -#124s | -#123s | -#122s | -#121s
  | -#120s | -#119s | -#118s | -#117s | -#116s | -#115s | -#114s | -#113s
  | -#112s | -#111s | -#110s | -#109s | -#108s | -#107s | -#106s | -#105s
  | -#104s | -#103s | -#102s | -#101s | -#100s | -#99s | -#98s | -#97s | -#96s
  | -#95s | -#94s | -#93s | -#92s | -#91s | -#90s | -#89s | -#88s | -#87s
  | -#86s | -#85s | -#84s | -#83s | -#82s | -#81s | -#80s | -#79s | -#78s
  | -#77s | -#76s | -#75s | -#74s | -#73s | -#72s | -#71s | -#70s | -#69s
  | -#68s | -#67s | -#66s | -#65s | -#64s | -#63s | -#62s | -#61s | -#60s
  | -#59s | -#58s | -#57s | -#56s | -#55s | -#54s | -#53s | -#52s | -#51s
  | -#50s | -#49s | -#48s | -#47s | -#46s | -#45s | -#44s | -#43s | -#42s
  | -#41s | -#40s | -#39s | -#38s | -#37s | -#36s | -#35s | -#34s | -#33s
  | -#32s | -#31s | -#30s | -#29s | -#28s | -#27s | -#26s | -#25s | -#24s
  | -#23s | -#22s | -#21s | -#20s | -#19s | -#18s | -#17s | -#16s | -#15s
  | -#14s | -#13s | -#12s | -#11s | -#10s | -#9s | -#8s | -#7s | -#6s | -#5s
  | -#4s | -#3s | -#2s | -#1s | #0s | #1s | #2s | #3s | #4s | #5s | #6s | #7s
  | #8s | #9s | #10s | #11s | #12s | #13s | #14s | #15s | #16s | #17s | #18s
  | #19s | #20s | #21s | #22s | #23s | #24s | #25s | #26s | #27s | #28s | #29s
  | #30s | #31s | #32s | #33s | #34s | #35s | #36s | #37s | #38s | #39s | #40s
  | #41s | #42s | #43s | #44s | #45s | #46s | #47s | #48s | #49s | #50s | #51s
  | #52s | #53s | #54s | #55s | #56s | #57s | #58s | #59s | #60s | #61s | #62s
  | #63s | #64s | #65s | #66s | #67s | #68s | #69s | #70s | #71s | #72s | #73s
  | #74s | #75s | #76s | #77s | #78s | #79s | #80s | #81s | #82s | #83s | #84s
  | #85s | #86s | #87s | #88s | #89s | #90s | #91s | #92s | #93s | #94s | #95s
  | #96s | #97s | #98s | #99s | #100s | #101s | #102s | #103s | #104s | #105s
  | #106s | #107s | #108s | #109s | #110s | #111s | #112s | #113s | #114s
  | #115s | #116s | #117s | #118s | #119s | #120s | #121s | #122s | #123s
  | #124s | #125s | #126s | #127s -> 0
[%%expect{|
- : int = 0
|}]

(* Untagged int16 *)

let ignore (_ : int16#) = ()
[%%expect{|
val ignore : int16# -> unit = <fun>
|}]

type t = int16#
[%%expect{|
type t = int16#
|}]

let () = ignore #1S
[%%expect{|
|}]

let () = ignore #0S
[%%expect{|
|}]

let () = ignore (-#32768S)
[%%expect{|
|}]

let () = ignore #32767S
[%%expect{|
|}]

let () = ignore #0x7fffS
[%%expect{|
|}]

let () = ignore (-#0x8000S)
[%%expect{|
|}]

let () = ignore #0o77777S
[%%expect{|
|}]

let () = ignore #0b0111111111111111S
[%%expect{|
|}]

let _ : int =
  match #0S with
  | _ -> 0
;;
[%%expect{|
- : int = 0
|}]

let _ : int =
  match #0S with
  | #0S -> 0
  | _ -> 1
;;
[%%expect{|
- : int = 0
|}]

let _ : int =
  match #1S with
  | #0S -> 0
  | #1S -> 1
  | _ -> 2
;;
[%%expect{|
- : int = 1
|}]

let _ : int =
  match -#1S with
  | #0S -> 0
  | #1S -> 1
  | _ -> 2
;;
[%%expect{|
- : int = 2
|}]

(* Partial match *)
let _ : int =
  match #1S with
  | #0S -> 0
  | #1S -> 1
;;
[%%expect{|
Lines 2-4, characters 2-12:
2 | ..match #1S with
3 |   | #0S -> 0
4 |   | #1S -> 1
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
#2S

- : int = 1
|}]
