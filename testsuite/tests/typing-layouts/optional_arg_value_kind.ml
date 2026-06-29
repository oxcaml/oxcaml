(* TEST
 flags = "-extension layouts_alpha";
 expect;
*)

(* Regression test: the content type of an optional argument must be
   constrained to [value_or_null]. When the content type is left
   unconstrained (here written as [_]), its layout must default
   rather than leaking [any] into [value_kind]. *)

let f (g : ?opt:_ -> _ -> _) x = g x
let _ = f
[%%expect{|
val f : (?opt:'a -> 'b -> 'c) -> 'b -> 'c = <fun>
- : (?opt:'a -> 'b -> 'c) -> 'b -> 'c = <fun>
|}]

(* An explicitly non-value optional content type is rejected with a layout
   error, rather than panicking in [value_kind]. *)

let g (h : ?opt:(_ : float64) -> _ -> _) x = h x
[%%expect{|
Line 1, characters 16-29:
1 | let g (h : ?opt:(_ : float64) -> _ -> _) x = h x
                    ^^^^^^^^^^^^^
Error: Optional argument types must have layout value.
       The layout of "'a" is float64
         because of the annotation on the wildcard _ at line 1, characters 16-29.
       But the layout of "'a" must be a value layout
         because the type argument of option has layout value_or_null.
|}]
