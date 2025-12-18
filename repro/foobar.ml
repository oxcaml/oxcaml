module Foo = struct
  type ('a, 'b, 'j) f1
  type ('a, 'b, 'c, 'j) f2
  type ('a, 'b, 'c, 'd, 'j) f3
  type ('a, 'b, 'c, 'd, 'e, 'j) f4
  type 'a wrap 

  module Gadt = struct
    type ('a, 'b) t =
      | A : { f : ('a, 'b, 'k) f1 }     -> (int, int) t
      | B : { f : ('a, 'b, 'c, 'k) f2 } -> (int, int) t
      | C :
          { f : ('a, 'b, 'c, 'k) f2
          ; x : 'a wrap
          }
          -> (int, int) t
      | D :
          { f : ('a, 'b, 'c, 'd, 'k) f3
          ; x : 'a wrap
          }
          -> (int, int) t
      | E :
          { f : ('a, 'b, 'c, 'd, 'k) f3
          ; x : 'a wrap
          ; y : 'b wrap
          }
          -> (int, int) t
      | F : { f : ('a, 'b, 'c, 'd, 'e, 'k) f4 } -> (int, int) t
      | G :
          { f : ('a, 'b, 'c, 'd, 'e, 'k) f4
          ; x : 'a wrap
          }
          -> (int, int) t
      | H :
          { f : ('a, 'b, 'c, 'd, 'e, 'k) f4
          ; x : 'a wrap
          ; y : 'b wrap
          }
          -> (int, int) t
      | I :
          { f : ('a, 'b, 'c, 'd, 'e, 'k) f4
          ; x : 'a wrap
          ; y : 'b wrap
          ; z : 'c wrap
          }
          -> (int, int) t
  end
end

type 'a list = Nil | Cons of 'a * 'a list

let rec diverge x = diverge x

let f : 'a. 'a -> 'a list = fun _ -> diverge "todo"

let _ : _ =
  f
    (diverge "todo" :
      ('a, ('b, 'c) Foo.Gadt.t) Foo.Gadt.t)
