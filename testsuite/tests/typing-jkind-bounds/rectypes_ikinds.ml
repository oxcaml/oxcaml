(* TEST
   flags = "-rectypes";
   expect;
*)

type 'a t constraint 'a = 'a * 'a
[%%expect{|
type 'a t constraint 'a = 'a * 'a
|}]

type 'a u = int constraint 'a = 'a * 'a
[%%expect{|
type 'a u = int constraint 'a = 'a * 'a
|}]

type bad : value
type other : value
type v : value mod contended = { x : (bad * other ref * 'a as 'a) }
[%%expect {|
type bad
type other
Line 3, characters 0-67:
3 | type v : value mod contended = { x : (bad * other ref * 'a as 'a) }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type definition does not satisfy its kind annotation
         value mod contended,
       because
       - bad is not mod contended
       - ref is not mod contended
|}]
