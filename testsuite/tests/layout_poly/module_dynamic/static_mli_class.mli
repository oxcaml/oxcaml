@@ static

(* A class is dynamic, but a unit that defines one can still be static: in a
   structure, a class is under an implicit [@@ dynamic]. *)

class point : object
  val mutable x : int
end
