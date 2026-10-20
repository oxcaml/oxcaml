(* TEST
 flags = "-dsource";
 expect;
*)

(* Without [-extension laws], a law is parsed, but the type checker rejects
   it. *)

law? trivial : 1 = 1
[%%expect {|

law? trivial : 1 = 1;;
Line 1, characters 0-20:
1 | law? trivial : 1 = 1
    ^^^^^^^^^^^^^^^^^^^^
Error: The extension "laws" is disabled and cannot be used
|}]

module type S = sig
  law? trivial : 1 = 1
end
[%%expect {|

module type S  = sig law? trivial : 1 = 1 end;;
Line 2, characters 2-22:
2 |   law? trivial : 1 = 1
      ^^^^^^^^^^^^^^^^^^^^
Error: The extension "laws" is disabled and cannot be used
|}]
