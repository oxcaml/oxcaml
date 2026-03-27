(* TEST
    expect;
*)

let use_portable : 'a @ portable -> unit = fun _ -> ()
[%%expect{|
val use_portable : 'a @ portable -> unit = <fun>
|}]

(* The thunk and the result are required to be global. This is only because we
don't support allocating lazy values on the stack. *)
let foo () =
    lazy (let x @ local = "hello" in x)
[%%expect{|
Line 2, characters 37-38:
2 |     lazy (let x @ local = "hello" in x)
                                         ^
Error: This value is "local"
       but is expected to be "global"
         because it is a lazy expression and thus needs to be allocated on the heap.
|}]

let foo (local_ x) =
    lazy (let _ = x in ())
[%%expect{|
Line 2, characters 18-19:
2 |     lazy (let _ = x in ())
                      ^
Error: The value "x" is "local" to the parent region
       but is expected to be "global"
         because it is used inside the lazy expression at line 2, characters 4-26
         which is expected to be "global"
         because lazy expressions always need to be allocated on the heap.
|}]

(* For simplicity, we also require them to be [unyielding]. *)
let foo (x @ yielding) =
    lazy (let _ = x in ())
[%%expect{|
Line 2, characters 18-19:
2 |     lazy (let _ = x in ())
                      ^
Error: The value "x" is "yielding"
       but is expected to be "unyielding"
         because it is used inside the lazy expression at line 2, characters 4-26
         which is expected to be "unyielding"
         because lazy expressions always need to be allocated on the heap.
|}]

(* lazy expression is constructed as global *)
let foo () =
    lazy ("hello")
[%%expect{|
val foo : unit -> string lazy_t = <fun>
|}]

(* result of lazy is available as global always *)
let foo (x @ local) =
    match x with
    | lazy y -> y
[%%expect{|
val foo : 'a lazy_t @ local -> 'a = <fun>
|}]

(* one can construct [portable] lazy only if the result is [portable] *)
let foo () =
    let l = lazy (let x @ nonportable = fun x -> x in x) in
    use_portable l
[%%expect{|
Line 3, characters 17-18:
3 |     use_portable l
                     ^
Error: This value is "nonportable" but is expected to be "portable".
|}]

let foo () =
    let l = lazy (let x @ shareable = fun x -> x in x) in
    use_portable l
[%%expect{|
Line 3, characters 17-18:
3 |     use_portable l
                     ^
Error: This value is "shareable" but is expected to be "portable".
|}]

let foo () =
    let l = lazy (let x @ corruptable = fun x -> x in x) in
    use_portable l
[%%expect{|
Line 3, characters 17-18:
3 |     use_portable l
                     ^
Error: This value is "corruptable" but is expected to be "portable".
|}]


(* thunk is evaluated only when [uncontended] lazy is forced, so the thunk can be
    [nonportable] even if the lazy is [portable]. *)
let foo (x @ nonportable) =
    let l = lazy (let _ = x in ()) in
    use_portable l
[%%expect{|
val foo : 'a -> unit = <fun>
|}]

(* thunk is evaluated only when [uncontended] lazy is forced, so the thunk can be
    [shareable] even if the lazy is [portable]. *)
let foo (x @ shareable) =
    let l = lazy (let _ = x in ()) in
    use_portable l
[%%expect{|
val foo : 'a @ shareable -> unit = <fun>
|}]

(* thunk is evaluated only when [uncontended] lazy is forced, so the thunk can be
    [corruptable] even if the lazy is [portable]. *)
let foo (x @ corruptable) =
    let l = lazy (let _ = x in ()) in
    use_portable l
[%%expect{|
val foo : 'a @ corruptable -> unit = <fun>
|}]

(* For the same reason, [portable] lazy can close over things at [uncontended]. *)
let foo (x @ uncontended) =
    let l @ portable = lazy ( let _x @ uncontended = x in ()) in
    use_portable l
[%%expect{|
val foo : 'a -> unit = <fun>
|}]

(* For the same reason, [portable] lazy can close over things at [shared]. *)
let foo (x @ shared) =
    let l @ portable = lazy ( let _x @ shared = x in ()) in
    use_portable l
[%%expect{|
val foo : 'a @ shared -> unit = <fun>
|}]

(* For the same reason, [portable] lazy can close over things at [corrupted]. *)
let foo (x @ corrupted) =
    let l @ portable = lazy ( let _x @ corrupted = x in ()) in
    use_portable l
[%%expect{|
val foo : 'a @ corrupted -> unit = <fun>
|}]

(* Portable lazy gives portable result *)
let foo (x @ portable) =
    match x with
    | lazy r -> use_portable x
[%%expect{|
val foo : 'a lazy_t @ portable -> unit = <fun>
|}]

(* Shareable lazy gives shareable result *)
let foo (x @ shareable) =
    match x with
    | lazy r -> use_portable x
[%%expect{|
Line 3, characters 29-30:
3 |     | lazy r -> use_portable x
                                 ^
Error: This value is "shareable" but is expected to be "portable".
|}]

(* Corruptable lazy gives corruptable result *)
let foo (x @ corruptable) =
    match x with
    | lazy r -> use_portable x
[%%expect{|
Line 3, characters 29-30:
3 |     | lazy r -> use_portable x
                                 ^
Error: This value is "corruptable" but is expected to be "portable".
|}]

(* Nonportable lazy gives nonportable result *)
let foo (x @ nonportable) =
    match x with
    | lazy r -> use_portable x
[%%expect{|
Line 3, characters 29-30:
3 |     | lazy r -> use_portable x
                                 ^
Error: This value is "nonportable" but is expected to be "portable".
|}]

(* forcing a lazy is not concurrency-safe; therefore, we require uncontended
   access *)
let foo (x @ contended) =
    match x with
    | lazy _ -> ()
[%%expect{|
Line 3, characters 6-12:
3 |     | lazy _ -> ()
          ^^^^^^
Error: This value is "contended"
       but is expected to be "uncontended"
         because it is a lazy value being forced.
|}]

(* as above, but with shared *)
let foo (x @ shared) =
    match x with
    | lazy _ -> ()
[%%expect{|
Line 3, characters 6-12:
3 |     | lazy _ -> ()
          ^^^^^^
Error: This value is "shared"
       but is expected to be "uncontended"
         because it is a lazy value being forced.
|}]

(* as above, but with corrupted *)
let foo (x @ corrupted) =
    match x with
    | lazy _ -> ()
[%%expect{|
Line 3, characters 6-12:
3 |     | lazy _ -> ()
          ^^^^^^
Error: This value is "corrupted"
       but is expected to be "uncontended"
         because it is a lazy value being forced.
|}]

(* stdlib's [Lazy.force] is a special case of lazy pattern *)

(* thunk can close over [unique] values and be [once], even if the lazy itself is [many]. *)
let use_unique (_ @ unique) = ()
let use_many (_ @ many) = ()
[%%expect{|
val use_unique : 'a @ unique -> unit = <fun>
val use_many : 'a -> unit = <fun>
|}]

let foo () =
    let x = "hello" in
    let t = lazy (use_unique x) in
    use_many t
[%%expect{|
val foo : unit -> unit = <fun>
|}]
