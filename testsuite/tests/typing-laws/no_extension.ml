(* TEST
 flags = "-dsource";
 expect;
*)

(* Without [-extension laws], a law is parsed, but the type checker rejects
   it. *)

law_? trivial : 1 = 1
[%%expect {|

law_? trivial : 1 = 1;;
Line 1, characters 0-21:
1 | law_? trivial : 1 = 1
    ^^^^^^^^^^^^^^^^^^^^^
Error: The extension "laws" is disabled and cannot be used
|}]

module type S = sig
  law_? trivial : 1 = 1
end
[%%expect {|

module type S  = sig law_? trivial : 1 = 1 end;;
Line 2, characters 2-23:
2 |   law_? trivial : 1 = 1
      ^^^^^^^^^^^^^^^^^^^^^
Error: The extension "laws" is disabled and cannot be used
|}]
