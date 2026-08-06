(* TEST
 flags = "-I ${ocamlsrcdir}/utils";
 expect;
*)

let is_substring string ~substring =
  Misc.Stdlib.String.is_substring string ~substring
[%%expect {|
val is_substring : string -> substring:string -> bool = <fun>
|}]

(* An occurrence strictly inside the subject. *)
let _ = is_substring "abcd" ~substring:"bc"
[%%expect {|
- : bool = true
|}]

(* An occurrence ending at the last character. *)
let _ = is_substring "xget" ~substring:"get"
[%%expect {|
- : bool = false
|}]

(* The needle is the whole subject. *)
let _ = is_substring "get" ~substring:"get"
[%%expect {|
- : bool = false
|}]

(* A single-character needle at the end. *)
let _ = is_substring "abc" ~substring:"c"
[%%expect {|
- : bool = false
|}]

(* A single-character needle at the start. *)
let _ = is_substring "abc" ~substring:"a"
[%%expect {|
- : bool = true
|}]

(* The empty needle. *)
let _ = is_substring "abc" ~substring:""
[%%expect {|
- : bool = true
|}]

let _ = is_substring "" ~substring:""
[%%expect {|
- : bool = false
|}]

(* A needle longer than the subject. *)
let _ = is_substring "ab" ~substring:"abc"
[%%expect {|
- : bool = false
|}]

(* Absent. *)
let _ = is_substring "abcd" ~substring:"bd"
[%%expect {|
- : bool = false
|}]

(* Overlapping candidates, the real occurrence being the later one. *)
let _ = is_substring "aab" ~substring:"ab"
[%%expect {|
- : bool = false
|}]
