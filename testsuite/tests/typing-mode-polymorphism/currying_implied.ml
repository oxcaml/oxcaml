(* TEST
 flags = "-extension unique -extension mode_polymorphism_alpha -extension mode_polymorphism_printing";
 expect;
*)

module type Const2 = module type of struct
  let const2 x y = 0
end
[%%expect{|
module type Const2 =
  sig val const2 : 'a @ [< global] -> 'b @ 'n -> int @ 'm @@ stateless end
|}]

module type Fst2 = module type of struct
  let fst2 x y = x
end
[%%expect{|
module type Fst2 =
  sig
    val fst2 : 'a @ [< 'm & global] -> 'b @ 'n -> 'a @ [> 'm] @@ stateless
  end
|}]

module type Three = module type of struct
  let three x y z = (x, z)
end
[%%expect{|
module type Three =
  sig
    val three :
      'a @ [< 'n & global] ->
      'b @ [< global] -> 'c @ [< 'm & global] -> 'a * 'c @ [> 'm | 'n] @@
      stateless
  end
|}]

module type Pair = module type of struct
  let pair x y = (x, y)
end
[%%expect{|
module type Pair =
  sig
    val pair :
      'a @ [< 'n & global] -> 'b @ [< 'm & global] -> 'a * 'b @ [> 'm | 'n]
      @@ stateless
  end
|}]

module type Apply = module type of struct
  let apply f x = f x
end
[%%expect{|
module type Apply =
  sig
    val apply :
      ('a @ [> 'n] -> 'b @ [< 'm & global]) @ [< global] ->
      'a @ [< 'n] -> 'b @ [> 'm | dynamic] @@ stateless
  end
|}]

module type Compose = module type of struct
  let compose f g x = f (g x)
end
[%%expect{|
module type Compose =
  sig
    val compose :
      ('a @ [> 'n | dynamic] -> 'b @ [< 'm & global]) @ [< global] ->
      ('c @ [> 'o] -> 'a @ [< 'n & global]) @ [< global] ->
      'c @ [< 'o] -> 'b @ [> 'm | dynamic] @@ stateless
  end
|}]

module type Flip = module type of struct
  let flip f x y = f y x
end
[%%expect{|
module type Flip =
  sig
    val flip :
      ('a @ [< past('m) > 'q] ->
       ('b @ [> 'p] -> 'c @ [< 'o & global]) @ [> past('m) | past('n)]) @ [< past('n) & global] ->
      'b @ [< 'p & global] -> 'a @ [< 'q] -> 'c @ [> 'o | dynamic] @@
      stateless
  end
|}]

external ( + ) : int -> int -> int @@ stateless = "%addint"
[%%expect{|
external ( + ) : int -> int -> int = "%addint"
|}]

module type Add = module type of struct
  let add a b = a + b
end
[%%expect{|
module type Add =
  sig
    val add : int @ 'o -> (int @ 'n -> int @ [> dynamic]) @ 'm @@ stateless
  end
|}, Principal{|
module type Add =
  sig
    val add : int @ [< global] -> int @ 'm -> int @ [> dynamic] @@ stateless
  end
|}]

module type Once_closure = module type of struct
  let once_closure (x @ once) = fun y -> (x, y)
end
[%%expect{|
module type Once_closure =
  sig
    val once_closure :
      'a @ [< 'n & global > once] ->
      'b @ [< 'm & global] -> 'a * 'b @ [> 'm | 'n | once] @@ stateless
  end
|}]

module type Portable_closure = module type of struct
  let portable_closure (x @ portable contended) y = (x, y)
end
[%%expect{|
module type Portable_closure =
  sig
    val portable_closure :
      'a @ [< 'n & global portable > contended] ->
      'b @ [< 'm & global] -> 'a * 'b @ [> 'm | 'n | contended] @@ stateless
  end
|}]

type ('a, 'b) pair_record = { a : 'a; b : 'b }
[%%expect{|
type ('a, 'b) pair_record = { a : 'a; b : 'b; }
|}]

module type Mk_record = module type of struct
  let mk_record a b = { a; b }
end
[%%expect{|
module type Mk_record =
  sig
    val mk_record :
      'a @ [< 'n & global] ->
      'b @ [< 'm & global] -> ('a, 'b) pair_record @ [> 'm | 'n] @@ stateless
  end
|}]

module type Local_closure = module type of struct
  let local_closure x = exclave_ (fun y -> (x, y))
end
[%%expect{|
module type Local_closure =
  sig
    val local_closure :
      'a @ [< 'm & global] ->
      ('b @ [< 'n & global] -> 'a * 'b @ [> 'n | 'm]) @ [> close('m) | local]
      @@ stateless
  end
|}]

module type Use_and_return = module type of struct
  let use_and_return g x = ignore (g x); g
end
[%%expect{|
module type Use_and_return =
  sig
    val use_and_return :
      ('a @ [> 'm] ->
       'b @ [< global many uncontended forkable unyielding read_write]) @ [< 'n mod aliased contended immutable & global many] ->
      'a @ [< 'm] ->
      ('a @ [> 'm] ->
       'b @ [< global many uncontended forkable unyielding read_write]) @ [> 'n | aliased]
      @@ portable
  end
|}, Principal{|
module type Use_and_return =
  sig
    val use_and_return :
      ('a @ [> 'm] ->
       'b @ [< global many uncontended forkable unyielding read_write]) @ [< 'n & global many] ->
      'a @ [< 'm] ->
      ('a @ [> 'm] ->
       'b @ [< global many uncontended forkable unyielding read_write]) @ [> 'n | aliased]
      @@ portable
  end
|}]

module type Both_branches = module type of struct
  let both_branches g x = if x then g else (fun y -> y)
end
[%%expect{|
module type Both_branches =
  sig
    val both_branches :
      ('a @ [< 'm] -> 'a @ [> 'm]) @ [< 'n & global] ->
      bool @ 'o -> ('a @ [< 'm] -> 'a @ [> 'm]) @ [> 'n | dynamic] @@
      stateless
  end
|}]

type 'a cell = { mutable v : 'a }
[%%expect{|
type 'a cell = { mutable v : 'a; }
|}]

module type Store_and_call = module type of struct
  let store_and_call c g x = c.v <- g; c.v x
end
[%%expect{|
module type Store_and_call =
  sig
    val store_and_call :
      ('a @ [> 'n] -> 'b @ [< 'm & global]) cell @ [< past('p) & past('o) & global uncontended read_write] ->
      (('a @ [> 'n] -> 'b @ [< 'm & global]) @ [< global many uncontended forkable unyielding read_write] ->
       ('a @ [< 'n] -> 'b @ [> 'm | dynamic]) @ [> past('p) mod many forkable unyielding | nonportable stateful]) @ [> past('o) mod many forkable unyielding | nonportable stateful]
      @@ stateless
  end
|}, Principal{|
module type Store_and_call =
  sig
    val store_and_call :
      ('a @ [> 'n] -> 'b @ [< 'm & global]) cell @ [< global uncontended read_write] ->
      ('a @ [> 'n] -> 'b @ [< 'm & global]) @ [< global many uncontended forkable unyielding read_write] ->
      'a @ [< 'n] -> 'b @ [> 'm | dynamic] @@ stateless
  end
|}]

module type Unique_fst = module type of struct
  let use_unique (x @ unique) = ()
  let unique_fst (x @ unique) y = use_unique x
end
[%%expect{|
module type Unique_fst =
  sig
    val use_unique : 'a @ [< unique] -> unit @ 'm @@ stateless
    val unique_fst : 'a @ [< global unique] -> 'b @ 'm -> unit @ [> dynamic]
      @@ stateless
  end
|}]

module type Unique_closure = module type of struct
  let use_unique (x @ unique) = ()
  let unique_closure (x @ unique) = fun y -> use_unique x
end
[%%expect{|
module type Unique_closure =
  sig
    val use_unique : 'a @ [< unique] -> unit @ 'm @@ stateless
    val unique_closure :
      'a @ [< global unique] -> 'b @ 'm -> unit @ [> dynamic] @@ stateless
  end
|}]

module type Unique_cell = module type of struct
  let use_unique (x @ unique) = ()
  let unique_cell (c @ unique) x = c.v <- x; use_unique c
end
[%%expect{|
module type Unique_cell =
  sig
    val use_unique : 'a @ [< unique] -> unit @ 'm @@ stateless
    val unique_cell :
      'a cell @ [< past('m) & global unique corrupted write] ->
      ('a @ [< global many uncontended forkable unyielding read_write] ->
       unit @ [> dynamic]) @ [> past('m) mod many forkable unyielding | once corruptible writing]
      @@ stateless
  end
|}, Principal{|
module type Unique_cell =
  sig
    val use_unique : 'a @ [< unique] -> unit @ 'm @@ stateless
    val unique_cell :
      'a cell @ [< global unique corrupted write] ->
      'a @ [< global many uncontended forkable unyielding read_write] ->
      unit @ [> dynamic] @@ stateless
  end
|}]

module type Stack_args = module type of struct
  let stack_args g = g (stack_ (1, 2)) (stack_ (3, 4)); ()
end
[%%expect{|
module type Stack_args =
  sig
    val stack_args :
      (int * int @ [> local] -> int * int @ [> local] -> 'a @ 'm) @ 'o ->
      unit @ 'n @@ stateless
  end
|}]
