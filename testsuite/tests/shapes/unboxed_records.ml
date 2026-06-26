(* TEST
 flags = "-dshape";
 expect;
*)

type t = #{ a : int; b : string }
[%%expect{|
{
 "t"[type] -> Record_unboxed_product { a<.1>: int ; b<.2>: string  };
 }
type t = #{ a : int; b : string; }
|}]
