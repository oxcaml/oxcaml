Test Merlin's behavior in the presense of punned record fields

  $ file=test.ml; . helpers

Part 1: Expressions

Test that locating a variable in a punned record expression goes to the definition of the
variable rather than the declaration of the label. ie, in:
{ a; b }
locating the a goes to the variable a, not the label in the record's type declaration.

  $ locate 11:4
  Locating:
    { a; b }
      ^
  Found definition at 9:6:
    let a = "hello" in
        ^

  $ locate 11:7
  Locating:
    { a; b }
         ^
  Found definition at 10:6:
    let b = 42 in
        ^

Test that the occurrences query finds occurrences of the variable, not the label

  $ occurrences 11:4
  Occurrences of:
    { a; b }
      ^
  Occurrence at 9:6-7:
    let a = "hello" in
        ^
  Occurrence at 11:4-5:
    { a; b }
      ^

  $ occurrences 11:7
  Occurrences of:
    { a; b }
         ^
  Occurrence at 10:6-7:
    let b = 42 in
        ^
  Occurrence at 11:7-8:
    { a; b }
         ^

Part 2: Patterns

Test that locating a variable in a punned record pattern goes to the definition of the
label, rather than looking for the definition of the variable and deciding we are already
at the definition point. ie, in:
{ a; b }
locating the a goes to the label in the records type declaration.

  $ locate 15:8
  Locating:
  let f { a; b } =
          ^
  Found definition at 2:4:
    { a : string
      ^

  $ locate 15:11
  Locating:
  let f { a; b } =
             ^
  Found definition at 3:4:
    ; b : int
      ^

Test that the occurrences query finds occurrences of the variable, not the label

  $ occurrences 15:8
  Occurrences of:
  let f { a; b } =
          ^
  Occurrence at 15:8-9:
  let f { a; b } =
          ^
  Occurrence at 16:9-10:
    ignore a;
           ^

  $ occurrences 15:11
  Occurrences of:
  let f { a; b } =
             ^
  Occurrence at 15:11-12:
  let f { a; b } =
             ^
  Occurrence at 17:9-10:
    ignore b
           ^
