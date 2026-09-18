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
- {:The value `bar`} is expected to be `portable`#.
  ~- Because {*:it}'s used inside [{:the anonymous function}].
    ~- And {:the anonymous function} is expected to be `portable`#.
      ~- Because {*:it}['s annotated as `portable`#].
- But {*:it}'s `nonportable`#.
  ~- Because {*:it} closes over [{:the value `foo`}].
    ~- And {:the function `foo`} is `nonportable`#.
      ~- Because {*:it} closes over [{:the value `x`}].
        ~- And {:the value `x`} is used as `uncontended`#.
          ~- Because {:`(:=)`} requires {*:its} 1st argument, {:`x`}, to be `uncontended`#.
      ~- [rule] A function that closes over `uncontended`# data is `nonportable`#.
|}, StructuredTxt{|
Line 10, characters 16-19:
10 |         let _ = bar in ()
                     ^^^
Error: The value "bar" is expected to be "portable".
       But it's "nonportable".
         Because the function "foo" closes over the value "x"
         at line 4, characters 8-9.
         And it's used as "uncontended".
         A function that closes over "uncontended" data is "nonportable".
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
- {:The value `bar`} is expected to be `portable`#.
  ~- Because {*:it}'s used inside [{:the anonymous function}].
    ~- And {:the anonymous function} is expected to be `portable`#.
      ~- Because {*:it}['s annotated as `portable`#].
- But {*:it}'s `nonportable`#.
  ~- Because {*:it} closes over [{:the value `foo`}].
    ~- And {:the function `foo`} is `nonportable`#.
      ~- Because {*:it} closes over [{:the value `x`}].
        ~- And {:the value `x`} is used as `uncontended`#.
          ~- Because {:`(:=)`} requires {*:its} 1st argument, {:`x`}, to be `uncontended`#.
      ~- [rule] A function that closes over `uncontended`# data is `nonportable`#.
|}, StructuredTxt{|
Line 6, characters 38-41:
6 |     let (baz @ portable) () = let _ = bar in ()
                                          ^^^
Error: The value "bar" is expected to be "portable".
       But it's "nonportable".
         Because the function "foo" closes over the value "x"
         at line 4, characters 17-18.
         And it's used as "uncontended".
         A function that closes over "uncontended" data is "nonportable".
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
- {:The module} does not match its signature.
  - {:`baz`} is expected to be `portable`#.
    ~- Because {*:it}'s annotated [`@@ portable`#].
  - But {*:it}'s `nonportable`#.
    ~- Because {*:it} closes over [{:the value `bar`}].
      ~- And {:the function `bar`} is `nonportable`#.
        ~- Because {*:it} closes over [{:the value `foo`}].
          ~- And {:the function `foo`} is `nonportable`#.
            ~- Because {*:it} closes over [{:the value `x`}].
              ~- And {:the value `x`} is used as `uncontended`#.
                ~- Because {:`(:=)`} requires {*:its} 1st argument, {:`x`}, to be `uncontended`#.
            ~- [rule] A function that closes over `uncontended`# data is `nonportable`#.
|}, StructuredTxt{|
Lines 3-9, characters 6-3:
3 | ......struct
4 |     let x = ref 42
5 |
6 |     let foo () = x := 24
7 |     let bar () = let _  = foo in ()
8 |     let baz () = let _ = bar in ()
9 | end
Error: The module does not match its signature.
         "baz" is expected to be "portable".
         But it's "nonportable".
         Because the function "foo" closes over the value "x"
         at line 6, characters 17-18.
         And it's used as "uncontended".
         A function that closes over "uncontended" data is "nonportable".
|}]

type t = { mutable x : string }

let takes_unique (x : string @ unique) =
  ignore x;
  ()

let () =
  let t = { x = "hello" } in
  takes_unique t.x
[%%expect{|
type t = { mutable x : string; }
val takes_unique : string @ unique -> unit = <fun>
Line 9, characters 15-18:
9 |   takes_unique t.x
                   ^^^
Error: This value is "aliased"
         because it is the field "x" (with some modality) of the record at line 9, characters 15-16.
       However, the highlighted expression is expected to be "unique".
|}, Structured{|
- {:The expression} is expected to be `unique`#.
  ~- Because {:`takes_unique`} requires {*:its} 1st argument to be `unique`#.
    ~- Because {*:it}['s annotated as `unique`#].
- But {*:it}'s `aliased`#.
  ~- Because {:the field `x`} is declared [`mutable`].
  ~- [rule] Mutable fields imply the `@@ aliased`# modality by default.
|}, StructuredTxt{|
type t = { mutable x : string; }
val takes_unique : string @ unique -> unit = <fun>
Line 9, characters 15-18:
9 |   takes_unique t.x
                   ^^^
Error: The expression is expected to be "unique".
       But it's "aliased".
         Because the field "x" is declared "mutable" at line 1, characters 11-29.
         Mutable fields imply the "@@ aliased" modality by default.
|}]
