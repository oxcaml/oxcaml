(* TEST
   include stdlib_alpha;
   expect;
*)

type 'a t1 : value & bits64 = 'a addr
type 'a t2 : value & bits64 = 'a addr_imm
[%%expect{|
type 'a t1 = 'a addr
type 'a t2 = 'a addr_imm
|}]
