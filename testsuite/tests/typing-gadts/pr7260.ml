(* TEST
   expect;
*)

type bar = < bar : unit >

type _ ty = Int : int ty

type dyn = Dyn : 'a ty -> dyn

class foo =
  object (this)
    method foo (Dyn ty) = match ty with Int -> (this :> bar)
  end

(* fail, but not for scope *)

[%%expect
{|
type bar = < bar : unit >
type _ ty = Int : int ty
type dyn = Dyn : 'a ty -> dyn
Lines 8-10, characters 2-5:
 8 | ..object (this)
 9 |     method foo (Dyn ty) = match ty with Int -> (this :> bar)
10 |   end
Error: This non-virtual class has undeclared virtual methods.
       The following methods were not declared : "bar"
|}]
