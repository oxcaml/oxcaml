type 'a list = Nil | Cons of 'a * 'a list

let rec diverge x = diverge x

let f : 'a. 'a -> 'a list = fun _ -> diverge "todo"

let _ : _ =
  f
    (diverge "todo" :
      ('a, ('b, 'c) Foo.Gadt.t) Foo.Gadt.t)
