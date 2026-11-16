(* TEST
    expect;
*)

let test () =
    let x = ref 42 in
    let foo () =
        x := 24
    in
    let bar () =
        let () = foo () in ()
    in
    let (baz @ portable) () =
        let () = bar () in ()
    in
    ()
[%%expect{|
Line 10, characters 17-20:
10 |         let () = bar () in ()
                      ^^^
Error: The value "bar" is "nonportable"
         because it closes over the value "foo" at line 7, characters 17-20
         which is "nonportable".
       However, the value "bar" highlighted is expected to be "portable"
         because it is used inside the function at lines 9-10, characters 25-29
         which is expected to be "portable".
|}]


module M = struct
    let x = ref 42

    let foo () = x := 24
    let bar () = let () = foo () in ()
    let (baz @ portable) () = let () = bar () in ()
end
[%%expect{|
Line 6, characters 39-42:
6 |     let (baz @ portable) () = let () = bar () in ()
                                           ^^^
Error: The value "bar" is "nonportable"
         because it closes over the value "foo" at line 5, characters 26-29
         which is "nonportable".
       However, the value "bar" highlighted is expected to be "portable"
         because it is used inside the function at line 6, characters 25-51
         which is expected to be "portable".
|}]

module M : sig
    val baz : unit -> unit @@ portable
end = struct
    let x = ref 42

    let foo () = x := 24
    let bar () = let () = foo () in ()
    let baz () = let () = bar () in ()
end
[%%expect{|
Lines 3-9, characters 6-3:
3 | ......struct
4 |     let x = ref 42
5 |
6 |     let foo () = x := 24
7 |     let bar () = let () = foo () in ()
8 |     let baz () = let () = bar () in ()
9 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           val x : int ref
           val foo : unit -> unit
           val bar : unit -> unit
           val baz : unit -> unit
         end @ nonportable
       is not included in
         sig val baz : unit -> unit @@ portable end @ nonportable
       Values do not match:
         val baz : unit -> unit (* in a structure at nonportable *)
       is not included in
         val baz : unit -> unit @@ portable (* in a structure at nonportable *)
       The first is "nonportable"
         because it closes over the value "bar" at line 8, characters 26-29
         which is "nonportable".
       However, the second is "portable".
|}]
