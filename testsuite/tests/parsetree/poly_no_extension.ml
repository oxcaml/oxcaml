(* TEST
   expect;
*)

(* Test that poly_ requires the layout_poly extension *)

let poly_ id : 'a. 'a -> 'a = fun x -> x
[%%expect{|
Line 1, characters 0-40:
1 | let poly_ id : 'a. 'a -> 'a = fun x -> x
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The extension "layout_poly" is disabled and cannot be used
|}]

module type S = sig
  val poly_ f : 'a. 'a -> 'a
end
[%%expect{|
Line 2, characters 2-28:
2 |   val poly_ f : 'a. 'a -> 'a
      ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The extension "layout_poly" is disabled and cannot be used
|}]
