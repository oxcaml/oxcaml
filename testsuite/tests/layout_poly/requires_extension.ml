(* TEST
   expect;
*)

let with_repr : (repr_ 'a). int = 1
;;
[%%expect {|
Line 1, characters 16-31:
1 | let with_repr : (repr_ 'a). int = 1
                    ^^^^^^^^^^^^^^^
Error: The extension "layout_poly" is disabled and cannot be used
|}];;
