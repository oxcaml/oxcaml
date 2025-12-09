(* TEST
 flags = "-dshape";
 expect;
*)

type t = #{ a : int; b : string }
[%%expect{|
{
 "t"[type] -> Record_unboxed_product { a<.7>: int ; b<.8>: string  };
 }
type t = #{ a : int; b : string; }
|}]
