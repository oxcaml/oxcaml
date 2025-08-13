(* TEST
   expect;
*)

(* [exn] currently crosses portability and contention.

   To make it safe, exception constructors under a portable lock:
   - Require their fields cross contention and be at portable during creation
   - Require their fields cross portability and mark as contended during pattern-matching

   This can be modeled as each exception constructor being contained in
   the original capsule it was defined in. Creating/destroying instances
   of that constructor moves its arguments across capsule boundaries.
*)

exception Nonportable of (unit -> unit)
exception Portable of unit
exception Portable' of (unit -> unit) @@ portable

[%%expect{|
exception Nonportable of (unit -> unit)
exception Portable of unit
exception Portable' of (unit -> unit) @@ portable
|}]

let x : exn = Nonportable (fun x -> x)
[%%expect{|
val x : exn = Nonportable <fun>
|}]

let (foo @ portable) () =
    match x with
    | Nonportable g -> g ()
    | _ -> ()
[%%expect{|
Line 3, characters 6-17:
3 |     | Nonportable g -> g ()
          ^^^^^^^^^^^
Error: This value is "nonportable" but expected to be "portable".
  Hint: All arguments of the constructor "Nonportable"
  must cross this axis to use it in this position.
|}]

let (foo @ portable) () =
    match x with
    | Portable g -> ()
    | _ -> ()
[%%expect{|
val foo : unit -> unit = <fun>
|}]

let (foo @ portable) () =
    match x with
    | Portable' g -> ()
    | _ -> ()
[%%expect{|
val foo : unit -> unit = <fun>
|}]

let (foo @ portable) g =
    raise (Nonportable g)
[%%expect{|
val foo : (unit -> unit) @ portable -> 'a = <fun>
|}]

let (foo @ portable) (g @ nonportable) =
    raise (Nonportable g)
[%%expect{|
Line 2, characters 23-24:
2 |     raise (Nonportable g)
                           ^
Error: This value is "nonportable" but expected to be "portable".
|}]

let (foo @ portable) () =
    raise (Portable ())
[%%expect{|
val foo : unit -> 'a = <fun>
|}]

let (foo @ portable) () =
    raise (Portable' (fun () -> ()))
[%%expect{|
val foo : unit -> 'a = <fun>
|}]

(* rebinding counts as usage *)
let (foo @ portable) () =
    let module M = struct
        exception Nonportable' = Nonportable
    end in
    ()
[%%expect{|
Line 3, characters 33-44:
3 |         exception Nonportable' = Nonportable
                                     ^^^^^^^^^^^
Error: This constructor is at mode "nonportable", but expected to be at mode "portable".
       Hint: all argument types must mode-cross for rebinding to succeed.
|}, Principal{|
Line 3, characters 33-44:
3 |         exception Nonportable' = Nonportable
                                     ^^^^^^^^^^^
Error: This constructor is at mode "contended", but expected to be at mode "uncontended".
       Hint: all argument types must mode-cross for rebinding to succeed.
|}]

(* Rebinding with crossing types succeeds. *)

exception Crossing of int list
[%%expect{|
exception Crossing of int list
|}]

let (cross @ portable) () =
    let module M = struct
        exception Crossing' = Crossing
    end in
    raise (M.Crossing' [3; 4; 5])
[%%expect{|
val cross : unit -> 'a = <fun>
|}, Principal{|
Line 3, characters 30-38:
3 |         exception Crossing' = Crossing
                                  ^^^^^^^^
Error: This constructor is at mode "contended", but expected to be at mode "uncontended".
       Hint: all argument types must mode-cross for rebinding to succeed.
|}]

(* CR modes: accepting this requires [coportable]. *)
exception SemiPortable of string * (unit -> unit)

let (foo @ portable) () =
    try () with
    SemiPortable (s, _) -> print_endline s
[%%expect{|
exception SemiPortable of string * (unit -> unit)
Line 5, characters 4-16:
5 |     SemiPortable (s, _) -> print_endline s
        ^^^^^^^^^^^^
Error: This value is "nonportable" but expected to be "portable".
  Hint: All arguments of the constructor "SemiPortable"
  must cross this axis to use it in this position.
|}]

exception Uncontended of unit
exception Uncontended' of int ref @@ contended
exception Contended of int ref
[%%expect{|
exception Uncontended of unit
exception Uncontended' of int ref @@ contended
exception Contended of int ref
|}]

let (foo @ portable) () =
    match x with
    | Uncontended () -> ()
    | _ -> ()
[%%expect{|
val foo : unit -> unit = <fun>
|}]

let (foo @ portable) () =
    match x with
    | Uncontended' _ -> ()
    | _ -> ()
[%%expect{|
val foo : unit -> unit = <fun>
|}]

let (foo @ portable) () =
    match x with
    | Contended _ -> ()
    | _ -> ()
[%%expect{|
val foo : unit -> unit = <fun>
|}]

let (foo @ portable) () =
    match x with
    | Contended r -> r := 4
    | _ -> ()
[%%expect{|
Line 3, characters 21-22:
3 |     | Contended r -> r := 4
                         ^
Error: This value is "contended" but expected to be "uncontended".
|}]

let (foo @ portable) () =
    raise (Uncontended ())
[%%expect{|
val foo : unit -> 'a = <fun>
|}]

let (foo @ portable) () =
    raise (Uncontended' (ref 42))
[%%expect{|
val foo : unit -> 'a = <fun>
|}]

let (foo @ portable) () =
    raise (Contended (ref 42))
[%%expect{|
Line 2, characters 11-20:
2 |     raise (Contended (ref 42))
               ^^^^^^^^^
Error: This value is "contended" but expected to be "uncontended".
  Hint: All arguments of the constructor "Contended"
  must cross this axis to use it in this position.
|}]

(* rebinding counts as usage *)
let (foo @ portable) () =
    let module M = struct
        exception Contended' = Contended
    end in
    ()
[%%expect{|
Line 3, characters 31-40:
3 |         exception Contended' = Contended
                                   ^^^^^^^^^
Error: This constructor is at mode "contended", but expected to be at mode "uncontended".
       Hint: all argument types must mode-cross for rebinding to succeed.
|}]

let (bar @ portable) () =
    let module M = struct
        exception Uncontended'' = Uncontended
    end in
    raise (M.Uncontended'' ())
[%%expect{|
val bar : unit -> 'a = <fun>
|}, Principal{|
Line 3, characters 34-45:
3 |         exception Uncontended'' = Uncontended
                                      ^^^^^^^^^^^
Error: This constructor is at mode "contended", but expected to be at mode "uncontended".
       Hint: all argument types must mode-cross for rebinding to succeed.
|}]


(* other extensible types are not affected *)
type t = ..

type t += Foo of int

let x : t = Foo 42

let (foo @ portable) () =
    ignore (x : _ @ portable)

[%%expect{|
type t = ..
type t += Foo of int
val x : t = Foo 42
Line 8, characters 12-13:
8 |     ignore (x : _ @ portable)
                ^
Error: The value "x" is nonportable, so cannot be used inside a function that is portable.
|}]

module type S = sig
    exception Exn of string ref
end
[%%expect{|
module type S = sig exception Exn of string ref end
|}]

(* CR dkalinichenko: fix. *)

let make_s : (unit -> (module S)) Modes.Portable.t =
    let module M = struct
        exception Exn of string ref
    end
    in
    { portable = fun () -> (module M : S) }

[%%expect{|
val make_s : (unit -> (module S)) Modes.Portable.t =
  {Modes.Portable.portable = <fun>}
|}]

let (foo @ portable) () =
    let module M = (val make_s.portable ()) in
    raise (M.Exn (ref "foo"))

[%%expect{|
val foo : unit -> 'a = <fun>
|}]

let (bar @ portable) f =
    let module M = (val make_s.portable ()) in
    try f () with
    | M.Exn r -> !r
    | _ -> "other exception"

[%%expect{|
val bar : (unit -> string) -> string = <fun>
|}]

let _ = bar foo

[%%expect{|
- : string = "foo"
|}]
