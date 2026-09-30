(* TEST
   expect;
*)

type 'a t1 : value_or_null & bits64 = 'a addr
type 'a t2 : value_or_null & bits64 = 'a addr_imm
[%%expect{|
type 'a t1 = 'a addr
type 'a t2 = 'a addr_imm
|}]
