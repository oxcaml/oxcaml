(* TEST
 flags = "-extension layouts_alpha";
 expect;
*)

(* Regression test: the content type of an optional argument must be
   constrained to a value (or null) layout. When the content type is left
   unconstrained (here written as [_]), its layout must default to [value]
   rather than leaking [any] into [value_kind]. *)

let f (g : ?opt:_ -> _ -> _) x = g x
let _ = f
[%%expect{|
File "_none_", line 1:
Error: Non-value detected in [value_kind].
       Please report this error to the Jane Street compilers team.
       The layout of 'a is any
         because of the definition of f at line 1, characters 6-36.
       But the layout of 'a must be a value layout
         because it has to be value for the V1 safety check.
|}]

(* An explicitly non-value optional content type is rejected with a layout
   error, rather than panicking in [value_kind]. *)

let g (h : ?opt:(_ : float64) -> _ -> _) x = h x
[%%expect{|
File "_none_", line 1:
Error: Non-value detected in [value_kind].
       Please report this error to the Jane Street compilers team.
       The layout of 'a is float64
         because of the definition of g at line 1, characters 6-48.
       But the layout of 'a must be a value layout
         because it has to be value for the V1 safety check.
|}]
