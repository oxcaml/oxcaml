(* TEST
  expect;
*)

(* Regression test: capturing a local parameter and returning it causes hint
  reporting to fatal error, fixed by PR#6205 *)
let _bar (x @ local) =
  let f _ = x in
  f
[%%expect{|
>> Fatal error: Skip hint should not be printed
Uncaught exception: Typecore.Error(_, _, _)

|}]
