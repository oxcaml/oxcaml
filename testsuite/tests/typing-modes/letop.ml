(* TEST
    expect;
*)

let portable_use : _ @ portable -> unit = fun _ -> ()

let ( let* ) o f =
  match o with
  | None -> None
  | Some x -> f x

let ( and* ) a b =
  match a, b with
  | Some a, Some b -> Some (a, b)
  | _ -> None

[%%expect{|
val portable_use : 'a @ portable -> unit = <fun>
val ( let* ) : 'a option -> ('a -> 'b option) -> 'b option = <fun>
val ( and* ) : 'a option -> 'b option -> ('a * 'b) option = <fun>
|}]

(* bindings are required to be legacy *)
let foo () =
    let* a = local_ "hello" in
    ()
[%%expect{|
Line 2, characters 13-27:
2 |     let* a = local_ "hello" in
                 ^^^^^^^^^^^^^^
Error: This value is "local" but is expected to be "global".
|}]

let foo () =
    let* a = Some "hello"
    and* b = local_ "hello" in
    ()
[%%expect{|
Line 3, characters 13-27:
3 |     and* b = local_ "hello" in
                 ^^^^^^^^^^^^^^
Error: This value is "local" but is expected to be "global".
|}]

(* Bindings are avialable as legacy *)
let foo () =
    let* a = Some (fun x -> x)
    and* b = Some (fun x -> x) in
    portable_use a
[%%expect{|
Line 4, characters 17-18:
4 |     portable_use a
                     ^
Error: This value is "nonportable"
         because it is an element of the tuple at line 2, characters 4-8
         which is "nonportable".
       However, the highlighted expression is expected to be "portable".
|}]

let foo () =
    let* a = Some (fun x -> x)
    and* b = Some (fun x -> x) in
    portable_use b
[%%expect{|
Line 4, characters 17-18:
4 |     portable_use b
                     ^
Error: This value is "nonportable"
         because it is an element of the tuple at line 2, characters 4-8
         which is "nonportable".
       However, the highlighted expression is expected to be "portable".
|}]

(* Body required to be legacy *)
let foo () =
    let _ =
        let* a = Some (fun x -> x) in
        local_ "hello"
    in
    ()
[%%expect{|
Line 4, characters 8-22:
4 |         local_ "hello"
            ^^^^^^^^^^^^^^
Error: This value is "local" but is expected to be "global".
|}]

(* The whole letop is available as legacy *)
let foo () =
    portable_use (
        let* a = Some (fun x -> x) in
        fun x -> x
    )
[%%expect{|
Lines 2-5, characters 17-5:
2 | .................(
3 |         let* a = Some (fun x -> x) in
4 |         fun x -> x
5 |     )
Error: This value is "nonportable" but is expected to be "portable".
|}]

(* Mode annotation on let*: parses correctly, constrains expression *)
let ( let@ ) (with_ @ local once) (f : (_ @ local -> _) @ local) =
  with_ f [@nontail]
[%%expect{|
val ( let@ ) :
  ('a : any) ('b : any) 'c.
    (('a @ local -> 'b) @ local -> 'c) @ local once ->
    ('a @ local -> 'b) @ local -> 'c =
  <fun>
|}]

(* Basic mode annotation on let@ - expression is constrained to local *)
let foo () =
    let@ (a @ local) = fun (f @ local) -> f "hello" in
    String.length a
[%%expect{|
val foo : unit -> int = <fun>
|}]

(* Mode annotation allows passing a local expression *)
let foo () =
    let@ (a @ local) =
      local_ (fun (f @ local) -> f "hello")
    in
    String.length a
[%%expect{|
val foo : unit -> int = <fun>
|}]

(* Without mode annotation on legacy operator, local expression is rejected *)
let foo () =
    let* a = local_ (Some "hello") in
    Some (String.length a)
[%%expect{|
Line 2, characters 13-34:
2 |     let* a = local_ (Some "hello") in
                 ^^^^^^^^^^^^^^^^^^^^^
Error: This value is "local" but is expected to be "global".
|}]

(* Mode annotation with the standard option let* - legacy operators
   don't benefit from mode annotations *)
let foo () =
    let* (a @ local) = Some "hello" in
    Some (String.length a)
[%%expect{|
Line 2, characters 23-35:
2 |     let* (a @ local) = Some "hello" in
                           ^^^^^^^^^^^^
Error: This value is "local" but is expected to be "global".
|}]
