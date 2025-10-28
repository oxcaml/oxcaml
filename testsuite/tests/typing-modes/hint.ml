(* TEST
    expect;
*)

let test () =
    let x = ref 42 in
    let foo () =
        x := 24
    in
    let bar () =
        let _  = foo in ()
    in
    let (baz @ portable) () =
        let _ = bar in ()
    in
    ()
[%%expect{|
Line 10, characters 16-19:
10 |         let _ = bar in ()
                     ^^^
Error: The value "bar" is "nonportable"
       because it closes over the value "foo" (at Line 7, characters 17-20)
       which is "nonportable"
       because it contains a usage (of the value "x" at Line 4, characters 8-9)
       which is expected to be "uncontended".
       However, the highlighted value "bar" is expected to be "portable"
       because it is used inside a function which is expected to be "portable".
|}]


module M = struct
    let x = ref 42

    let foo () = x := 24
    let bar () = let _  = foo in ()
    let (baz @ portable) () = let _ = bar in ()
end
[%%expect{|
Line 6, characters 38-41:
6 |     let (baz @ portable) () = let _ = bar in ()
                                          ^^^
Error: The value "bar" is "nonportable"
       because it closes over the value "foo" (at Line 5, characters 26-29)
       which is "nonportable"
       because it contains a usage (of the value "x" at Line 4, characters 17-18)
       which is expected to be "uncontended".
       However, the highlighted value "bar" is expected to be "portable"
       because it is used inside a function which is expected to be "portable".
|}]

module M : sig
    val baz : unit -> unit @@ portable
end = struct
    let x = ref 42

    let foo () = x := 24
    let bar () = let _  = foo in ()
    let baz () = let _ = bar in ()
end
[%%expect{|
Lines 3-9, characters 6-3:
3 | ......struct
4 |     let x = ref 42
5 |
6 |     let foo () = x := 24
7 |     let bar () = let _  = foo in ()
8 |     let baz () = let _ = bar in ()
9 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           val x : int ref
           val foo : unit -> unit
           val bar : unit -> unit
           val baz : unit -> unit
         end (* at nonportable *)
       is not included in
         sig val baz : unit -> unit @@ portable end (* at nonportable *)
       Values do not match:
         val baz : unit -> unit (* in a structure at nonportable *)
       is not included in
         val baz : unit -> unit @@ portable (* in a structure at nonportable *)
       The left-hand side is "nonportable"
       because it closes over the value "bar" (at Line 8, characters 25-28)
       which is "nonportable"
       because it closes over the value "foo" (at Line 7, characters 26-29)
       which is "nonportable"
       because it contains a usage (of the value "x" at Line 6, characters 17-18)
       which is expected to be "uncontended".
       However, the right-hand side is "portable".
|}]
