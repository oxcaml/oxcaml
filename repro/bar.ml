type 'a list = Nil | Cons of 'a * 'a list
let f : 'a. 'a -> 'a list = fun _ -> failwith "t o d o"
let _ : _ = f (failwith "t     o  do" : ('a, ('b, 'c) Foo.Gadt.t) Foo.Gadt.t)
