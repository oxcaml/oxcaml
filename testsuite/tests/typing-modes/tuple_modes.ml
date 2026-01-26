(* TEST
    expect;
*)

(* Test the tricky business of tuple_modes *)

let use_global : 'a @ global -> unit = fun _ -> ()
let use_global_product : ('a : value & value). 'a @ global -> unit = fun _ -> ()
let use_local : 'a @ local -> unit = fun _ -> ()
let use_local_product : ('a : value & value). 'a @ local -> unit = fun _ -> ()
[%%expect{|
val use_global : 'a -> unit = <fun>
val use_global_product : ('a : value & value). 'a -> unit = <fun>
val use_local : 'a @ local -> unit = <fun>
val use_local_product : ('a : value & value). 'a @ local -> unit = <fun>
|}]

let f x =
    let (_, x) : _ =
      42, local_ (Some x)
    in
    x
  ;;
[%%expect{|
Line 5, characters 4-5:
5 |     x
        ^
Error: This value is "local"
       because it is an element of the tuple at Line 3, characters 6-25
       which is "local"
       because it is a tuple that contains the expression at Line 3, characters 10-25
       which is "local".
       However, the highlighted expression is expected to be "local" to the parent region or "global"
       because it is a function return value.
       Hint: Use exclave_ to return a local value.
|}]

let f x =
    let #(_, x) : _ =
      #(42, local_ (Some x))
    in
    x
  ;;
[%%expect{|
Line 5, characters 4-5:
5 |     x
        ^
Error: This value is "local"
       because it is an element of the tuple at Line 3, characters 6-28
       which is "local"
       because it is a tuple that contains the expression at Line 3, characters 12-27
       which is "local".
       However, the highlighted expression is expected to be "local" to the parent region or "global"
       because it is a function return value.
       Hint: Use exclave_ to return a local value.
|}]

let f e0 (e1 @ local) =
    match e0, e1 with
    | x0, x1 -> use_global x0; use_local x1; ()
[%%expect{|
val f : 'a -> 'b @ local -> unit = <fun>
|}]

let f e0 (e1 @ local) =
    match #(e0, e1) with
    | #(x0, x1) -> use_global x0; use_local x1; ()
[%%expect{|
val f : 'a -> 'b @ local -> unit = <fun>
|}]

let f e0 (e1 @ local) =
    match e0, e1 with
    | x0, x1 -> use_global x0; use_global x1; ()
[%%expect{|
Line 3, characters 42-44:
3 |     | x0, x1 -> use_global x0; use_global x1; ()
                                              ^^
Error: This value is "local" to the parent region but is expected to be "global".
|}]

let f e0 (e1 @ local) =
    match #(e0, e1) with
    | #(x0, x1) -> use_global x0; use_global x1; ()
[%%expect{|
Line 3, characters 45-47:
3 |     | #(x0, x1) -> use_global x0; use_global x1; ()
                                                 ^^
Error: This value is "local" to the parent region but is expected to be "global".
|}]

let f e0 (e1 @ local) =
    match e0, e1 with
    | x0, x1 when x0 = x1 -> use_global x0; use_local x1; ()
    | x -> use_global x; ()
[%%expect{|
Line 4, characters 22-23:
4 |     | x -> use_global x; ()
                          ^
Error: This value is "local"
       because it is allocated at Line 2, characters 10-16 containing data
       which is "local" to the parent region
       because it is a tuple that contains the expression at Line 2, characters 14-16
       which is "local" to the parent region.
       However, the highlighted expression is expected to be "global".
|}]

let f e0 (e1 @ local) =
    match #(e0, e1) with
    | #(x0, x1) when x0 = x1 -> use_global x0; use_local x1; ()
    | x -> use_global_product x; ()
[%%expect{|
Line 4, characters 30-31:
4 |     | x -> use_global_product x; ()
                                  ^
Error: This value is "local" to the parent region
       because it is a tuple that contains the expression at Line 2, characters 16-18
       which is "local" to the parent region.
       However, the highlighted expression is expected to be "global".
|}]


let f e0 (e1 @ local) =
    match e0, e1 with
    | x0, x1 when x0 = x1 -> use_global x0; use_local x1; ()
    | x -> use_local x; ()
[%%expect{|
val f : 'a -> 'a @ local -> unit = <fun>
|}]

let f e0 (e1 @ local) =
    match #(e0, e1) with
    | #(x0, x1) when x0 = x1 -> use_global x0; use_local x1; ()
    | x -> use_local_product x; ()
[%%expect{|
val f : 'a -> 'a @ local -> unit = <fun>
|}]

(* we can return [e1], because it's regional. We can't return [x] (or its
   component) because [x] is allocated in the current region.  But when there is
   no allocation, as in the unboxed version just below, this is allowed.  *)
let f e0 (e1 @ local) =
    match e0, e1 with
    | x0, x1 when x0 = x1 -> use_global x0; use_local x1; e1
    | x -> use_local x; let (x0, x1) = x in x0
[%%expect{|
Line 4, characters 44-46:
4 |     | x -> use_local x; let (x0, x1) = x in x0
                                                ^^
Error: This value is "local"
       because it is an element of the tuple at Line 4, characters 39-40
       which is "local"
       because it is allocated at Line 2, characters 10-16 containing data
       which is "local" to the parent region
       because it is a tuple that contains the expression at Line 2, characters 14-16
       which is "local" to the parent region.
       However, the highlighted expression is expected to be "local" to the parent region or "global"
       because it is a function return value.
       Hint: Use exclave_ to return a local value.
|}]

let f e0 (e1 @ local) =
    match #(e0, e1) with
    | #(x0, x1) when x0 = x1 -> use_global x0; use_local x1; e1
    | x -> use_local_product x; let #(x0, x1) = x in x0
[%%expect{|
val f : 'a -> 'a @ local -> 'a @ local = <fun>
|}]

(* The value being matched upon is [local] in one branch, so the match result is
   [local]. *)
let f b e0 (e1 @ local) (e @ local)=
    match if b then e0, e1 else e with
    | x0, x1 -> use_global x0; use_local x1; ()
[%%expect{|
Line 3, characters 27-29:
3 |     | x0, x1 -> use_global x0; use_local x1; ()
                               ^^
Error: This value is "local" to the parent region
       because it is an element of the tuple at Line 2, characters 32-33
       which is "local" to the parent region.
       However, the highlighted expression is expected to be "global".
|}]

let f b e0 (e1 @ local) (e @ local)=
    match if b then #(e0, e1) else e with
    | #(x0, x1) -> use_global x0; use_local x1; ()
[%%expect{|
Line 3, characters 30-32:
3 |     | #(x0, x1) -> use_global x0; use_local x1; ()
                                  ^^
Error: This value is "local" to the parent region
       because it is an element of the tuple at Line 2, characters 35-36
       which is "local" to the parent region.
       However, the highlighted expression is expected to be "global".
|}]

let f b e0 (e1 @ local) e2 e3 =
    match if b then e0, e1 else e2, e3 with
    | x0, x1 -> use_global x0; use_local x1; ()
[%%expect{|
val f : bool -> 'a -> 'b @ local -> 'a -> 'b -> unit = <fun>
|}]

let f b e0 (e1 @ local) e2 e3 =
    match if b then #(e0, e1) else #(e2, e3) with
    | #(x0, x1) -> use_global x0; use_local x1; ()
[%%expect{|
val f : bool -> 'a -> 'b @ local -> 'a -> 'b -> unit = <fun>
|}]

let f b e0 (e1 @ local) e2 e3 =
    match if b then e0, e1 else e2, e3 with
    | x0, x1 -> use_global x0; use_global x1; ()
[%%expect{|
Line 3, characters 42-44:
3 |     | x0, x1 -> use_global x0; use_global x1; ()
                                              ^^
Error: This value is "local" to the parent region but is expected to be "global".
|}]

let f b e0 (e1 @ local) e2 e3 =
    match if b then #(e0, e1) else #(e2, e3) with
    | #(x0, x1) -> use_global x0; use_global x1; ()
[%%expect{|
Line 3, characters 45-47:
3 |     | #(x0, x1) -> use_global x0; use_global x1; ()
                                                 ^^
Error: This value is "local" to the parent region but is expected to be "global".
|}]

(* An unboxed tuple is not an allocation, but a regular tuple is *)
let f_unboxed_tuple (local_ a) (local_ b) =
  let t = #(a, b) in
  let #(a', _) = t in
  a'
[%%expect{|
val f_unboxed_tuple : 'a @ local -> 'b @ local -> 'a @ local = <fun>
|}]

let f_boxed_tuple (local_ a) (local_ b) =
  let t = (a, b) in
  let (a', _) = t in
  a'
[%%expect{|
Line 4, characters 2-4:
4 |   a'
      ^^
Error: This value is "local"
       because it is an element of the tuple at Line 3, characters 16-17
       which is "local"
       because it is allocated at Line 2, characters 10-16 containing data
       which is "local" to the parent region
       because it is a tuple that contains the expression at Line 2, characters 11-12
       which is "local" to the parent region.
       However, the highlighted expression is expected to be "local" to the parent region or "global"
       because it is a function return value.
       Hint: Use exclave_ to return a local value.
|}]
