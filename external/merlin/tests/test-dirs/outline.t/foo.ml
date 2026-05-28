module Bar = struct
 type t = int
 module type S1 = sig
   type t

   val foo : t -> int
 end
end

class type class_type_a = object
  method a : int -> int
end

class class_b = object
  method b s = s ^ s
end

exception Ex of char

type ('a, 'b) eithery =
  | Lefty of 'a
  | Righty of 'b

type 'a point =
  { x : 'a
  ; y : 'a
  ; z : 'a
  }

include struct
  type t
  let x = 1
  include struct
    type u
    let y = "y"
  end
end

module type S = sig
  include sig
    type t
    val x : int
    include sig
      type u
      val y : string
    end
  end
end
