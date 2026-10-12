(* TEST
 flags = "-dsource";
 expect;
*)

(* Without [-extension laws], a law is parsed, but the type checker rejects
   it. *)

law? trivial : true
[%%expect {|

law? trivial : true;;
Line 1, characters 0-19:
1 | law? trivial : true
    ^^^^^^^^^^^^^^^^^^^
Error: The extension "laws" is disabled and cannot be used
|}]

module type S = sig
  law? trivial : true
end
[%%expect {|

module type S  = sig law? trivial : true end;;
Line 2, characters 2-21:
2 |   law? trivial : true
      ^^^^^^^^^^^^^^^^^^^
Error: The extension "laws" is disabled and cannot be used
|}]
