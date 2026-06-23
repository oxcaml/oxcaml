(* TEST
  expect;
*)

(* Regression test: capturing a local parameter and returning it causes hint
  reporting to fatal error, fixed by PR#6205 *)
let _bar (x @ local) =
  let f _ = x in
  f
[%%expect{|
Line 3, characters 2-3:
3 |   f
      ^
Error: This value is "local"
         because it is allocated at line 2, characters 8-13 containing data
         which is "local" to the parent region
         because it closes over the value "x" at line 2, characters 12-13
         which is "local" to the parent region.
       However, the highlighted expression is expected to be "local" to the parent region or "global"
         because it is a function return value.
         Hint: Use exclave_ to return a local value.
|}]
