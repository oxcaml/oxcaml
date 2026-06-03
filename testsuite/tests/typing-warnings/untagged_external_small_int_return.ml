(* TEST
 expect;
*)

(* Tests of Warning 182 (untagged-external-small-int-return *)

external foo : int -> (int8 [@untagged]) = "" "foo"
[%%expect {|
Line 1, characters 23-27:
1 | external foo : int -> (int8 [@untagged]) = "" "foo"
                           ^^^^
Warning 182 [untagged-external-small-int-return]: Using "(int8[@untagged])" or "(int16[@untagged])" on C stub returns is not
  recommended since "[@untagged]" does not perform a sign-extension. Use
  "(int8[@unboxed])" or "(int16[@unboxed])" instead.

external foo : int -> (int8 [@untagged]) = "" "foo"
|}]

external foo : int -> (int16 [@untagged]) = "" "foo"
[%%expect {|
Line 1, characters 23-28:
1 | external foo : int -> (int16 [@untagged]) = "" "foo"
                           ^^^^^
Warning 182 [untagged-external-small-int-return]: Using "(int8[@untagged])" or "(int16[@untagged])" on C stub returns is not
  recommended since "[@untagged]" does not perform a sign-extension. Use
  "(int8[@unboxed])" or "(int16[@unboxed])" instead.

external foo : int -> (int16 [@untagged]) = "" "foo"
|}]

(* Warning is issued only for returns *)
external foo : (int8 [@untagged]) -> int = "" "foo"
external foo : (int16 [@untagged]) -> int = "" "foo"
[%%expect {|
external foo : (int8 [@untagged]) -> int = "" "foo"
external foo : (int16 [@untagged]) -> int = "" "foo"
|}]

(* Warning is not issued for [[@unboxed]] *)
external foo : int -> (int8 [@unboxed]) = "" "foo"
external foo : int -> (int16 [@unboxed]) = "" "foo"
[%%expect {|
external foo : int -> (int8 [@untagged]) = "" "foo"
external foo : int -> (int16 [@untagged]) = "" "foo"
|}]

(* Warning can be ignored *)
external foo : int -> (int8 [@untagged]) = "" "foo" [@@warning "-182"]
external foo : int -> (int16 [@untagged])
  = "" "foo" [@@warning "-untagged-external-small-int-return"]
[%%expect {|
external foo : int -> (int8 [@untagged]) = "" "foo"
external foo : int -> (int16 [@untagged]) = "" "foo"
|}]
