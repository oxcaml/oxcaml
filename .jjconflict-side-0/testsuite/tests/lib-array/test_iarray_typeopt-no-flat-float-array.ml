(* TEST
   include stdlib_stable;
   flags = "-dlambda";
   no-flat-float-array;
   expect;
*)

module Array = Stdlib.Array
open Stdlib_stable.IarrayLabels

[%%expect {|
0
module Array = Array
0
|}]

(* Regression test showing that an [i]array of iarrays
   has element kind [addr].

   In no-flat-float-array mode, an [: :] of polymorphic element type
   specializes to [addr] rather than [gen].
 *)

let _ = [: [: :] :];;

[%%expect {|
(makearray_imm[addr] (makearray_imm[addr]))
- : 'a iarray iarray = [:[::]:]
|}]

let _ = [| [: :] |];;

[%%expect {|
(makearray[addr] (makearray_imm[addr]))
- : '_weak1 iarray array = [|[::]|]
|}]
