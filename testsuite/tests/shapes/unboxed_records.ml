(* TEST
 flags = "-dshape";
 expect;
*)

type t = #{ a : int; b : string }
[%%expect{|
{
 "t"[type] -> Record_unboxed_product { a<.6>: int ; b<.7>: string  };
 }
type t = #{ a : int; b : string; }
|}]
