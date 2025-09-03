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
       because it closes over the value "x" (at Line 4, characters 8-9)
       which is expected to be "uncontended".
       However, the highlighted expression is expected to be "portable"
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
       because it closes over the value "x" (at Line 4, characters 17-18)
       which is expected to be "uncontended".
       However, the highlighted expression is expected to be "portable"
       because it is used inside a function which is expected to be "portable".
|}]
