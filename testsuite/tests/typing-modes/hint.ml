(* TEST
    structured_diagnostics = "true";
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
         because it closes over the value "foo" at line 7, characters 17-20
         which is "nonportable"
         because it contains a usage (of the value "x" at line 4, characters 8-9)
         which is expected to be "uncontended".
       However, the value "bar" highlighted is expected to be "portable"
         because it is used inside the function at lines 9-10, characters 25-25
         which is expected to be "portable".
|}, Structured{|
- {0:`bar`} is expected to be `portable`#0.
  ~- {1:The function `baz`}[ is annotated as `portable`#0].
    ~- {1*:It} closes over {0:`bar`}.
      ~- Therefore, [`bar`] is also expected to be `portable`#0.
- But {0:`bar`} is `nonportable`#1.
  ~- {0:`bar`} uses {2:`foo`}.
    ~- [`foo`] is `nonportable`#1.
      ~- {2:`foo`} uses {3:`x`} as `uncontended`#2 data.
        ~- The signature of {4:`:=`} requires {3:`x`} to be `uncontended`#2.
  ~= A function that closes over `uncontended`#2 data is `nonportable`#1.

entity 0: File "hint.ml", line 10, characters 16-19
entity 1: File "hint.ml", lines 9-10, characters 25-25
entity 2: File "hint.ml", line 7, characters 17-20
entity 3: File "hint.ml", line 4, characters 8-9
entity 4: File "hint.ml", line 4, characters 10-12
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
         because it closes over the value "foo" at line 5, characters 26-29
         which is "nonportable"
         because it contains a usage (of the value "x" at line 4, characters 17-18)
         which is expected to be "uncontended".
       However, the value "bar" highlighted is expected to be "portable"
         because it is used inside the function at line 6, characters 25-47
         which is expected to be "portable".
|}, Structured{|
- {0:`bar`} is expected to be `portable`#0.
  ~- {1:The function `baz`}[ is annotated as `portable`#0].
    ~- {1*:It} closes over {0:`bar`}.
      ~- Therefore, [`bar`] is also expected to be `portable`#0.
- But {0:`bar`} is `nonportable`#1.
  ~- {0:`bar`} uses {2:`foo`}.
    ~- [`foo`] is `nonportable`#1.
      ~- {2:`foo`} uses {3:`x`} as `uncontended`#2 data.
        ~- The signature of {4:`:=`} requires {3:`x`} to be `uncontended`#2.
  ~= A function that closes over `uncontended`#2 data is `nonportable`#1.

entity 0: File "hint.ml", line 6, characters 38-41
entity 1: File "hint.ml", line 6, characters 25-47
entity 2: File "hint.ml", line 5, characters 26-29
entity 3: File "hint.ml", line 4, characters 17-18
entity 4: File "hint.ml", line 4, characters 19-21
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
         end @ nonportable
       is not included in
         sig val baz : unit -> unit @@ portable end @ nonportable
       Values do not match:
         val baz : unit -> unit (* in a structure at nonportable *)
       is not included in
         val baz : unit -> unit @@ portable (* in a structure at nonportable *)
       The first is "nonportable"
         because it closes over the value "bar" at line 8, characters 25-28
         which is "nonportable"
         because it closes over the value "foo" at line 7, characters 26-29
         which is "nonportable"
         because it contains a usage (of the value "x" at line 6, characters 17-18)
         which is expected to be "uncontended".
       However, the second is "portable".
|}, Structured{|
- {0:The module} does not match its signature.
  - {1:`baz`} is expected to be `portable`#0.
    ~- [The signature requires {1*:it} to be `portable`#0].
  - But {1*:it}'s `nonportable`#1.
    ~- {1:`baz`} uses {2:`bar`}.
      ~- [`bar`] is `nonportable`#1.
        ~- {2:`bar`} uses {3:`foo`}.
          ~- [`foo`] is `nonportable`#1.
            ~- {3:`foo`} uses {4:`x`} as `uncontended`#2 data.
              ~- The signature of {5:`:=`} requires {4:`x`} to be `uncontended`#2.
    ~= A function that closes over `uncontended`#2 data is `nonportable`#1.

entity 0: File "hint.ml", lines 3-9, characters 6-3
entity 1: File "hint.ml", line 8, characters 8-11
entity 2: File "hint.ml", line 8, characters 25-28
entity 3: File "hint.ml", line 7, characters 26-29
entity 4: File "hint.ml", line 6, characters 17-18
entity 5: File "hint.ml", line 6, characters 19-21
|}]
