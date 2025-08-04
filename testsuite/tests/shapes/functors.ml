(* TEST
 flags = "-dshape";
 expect;
*)

module type S = sig
  type t
  val x : t
end
[%%expect{|
{
 "S"[module type] -> <.2>;
 }
module type S = sig type t val x : t end
|}]

module Falias (X : S) = X
[%%expect{|
{
 "Falias"[module] -> Abs<.4>(X, X<.3>);
 }
module Falias : functor (X : S) -> sig type t = X.t val x : t end
|}]

module Finclude (X : S) = struct
  include X
end
[%%expect{|
{
 "Finclude"[module] ->
   Abs<.6>
      (X, {
           "t"[type] -> X<.5> . "t"[type];
           "x"[value] -> X<.5> . "x"[value];
           });
 }
module Finclude : functor (X : S) -> sig type t = X.t val x : t end
|}]

module Fredef (X : S) = struct
  type t = X.t
  let x = X.x
end
[%%expect{|
{
 "Fredef"[module] -> Abs<.10>(X, {
                                  "t"[type] -> <.8>;
                                  "x"[value] -> <.9>;
                                  });
 }
module Fredef : functor (X : S) -> sig type t = X.t val x : X.t end
|}]

module Fignore (_ : S) = struct
  type t = Fresh
  let x = Fresh
end
[%%expect{|
{
 "Fignore"[module] ->
   Abs<.15>
      ((),
       {
        "t"[type] -> {<.12>
                      "Fresh"[constructor] -> {<.13>};
                      };
        "x"[value] -> <.14>;
        });
 }
module Fignore : S -> sig type t = Fresh val x : t end
|}]

module Arg : S = struct
  type t = T
  let x = T
end
[%%expect{|
{
 "Arg"[module] ->
   {<.20>
    "t"[type] -> {<.17>
                  "T"[constructor] -> {<.18>};
                  };
    "x"[value] -> <.19>;
    };
 }
module Arg : S
|}]

include Falias(Arg)
[%%expect{|
{
 "t"[type] -> {<.17>
               "T"[constructor] -> {<.18>};
               };
 "x"[value] -> <.19>;
 }
type t = Arg.t
val x : t = <abstr>
|}]

include Finclude(Arg)
[%%expect{|
{
 "t"[type] -> {<.17>
               "T"[constructor] -> {<.18>};
               };
 "x"[value] -> <.19>;
 }
type t = Arg.t
val x : t = <abstr>
|}]

include Fredef(Arg)
[%%expect{|
{
 "t"[type] -> <.8>;
 "x"[value] -> <.9>;
 }
type t = Arg.t
val x : Arg.t = <abstr>
|}]

include Fignore(Arg)
[%%expect{|
{
 "t"[type] -> {<.12>
               "Fresh"[constructor] -> {<.13>};
               };
 "x"[value] -> <.14>;
 }
type t = Fignore(Arg).t = Fresh
val x : t = Fresh
|}]

include Falias(struct type t = int let x = 0 end)
[%%expect{|
{
 "t"[type] -> <.22>;
 "x"[value] -> <.23>;
 }
type t = int
val x : t = 0
|}]

include Finclude(struct type t = int let x = 0 end)
[%%expect{|
{
 "t"[type] -> <.25>;
 "x"[value] -> <.26>;
 }
type t = int
val x : t = 0
|}]

include Fredef(struct type t = int let x = 0 end)
[%%expect{|
{
 "t"[type] -> <.8>;
 "x"[value] -> <.9>;
 }
type t = int
val x : int = 0
|}]

include Fignore(struct type t = int let x = 0 end)
[%%expect{|
{
 "t"[type] -> {<.12>
               "Fresh"[constructor] -> {<.13>};
               };
 "x"[value] -> <.14>;
 }
type t = Fresh
val x : t = Fresh
|}]

module Fgen () = struct
  type t = Fresher
  let x = Fresher
end
[%%expect{|
{
 "Fgen"[module] ->
   Abs<.37>
      ((),
       {
        "t"[type] -> {<.34>
                      "Fresher"[constructor] -> {<.35>};
                      };
        "x"[value] -> <.36>;
        });
 }
module Fgen : functor () -> sig type t = Fresher val x : t end
|}]

include Fgen ()
[%%expect{|
{
 "t"[type] -> {<.34>
               "Fresher"[constructor] -> {<.35>};
               };
 "x"[value] -> <.36>;
 }
type t = Fresher
val x : t = Fresher
|}]

(***************************************************************************)
(* Make sure we restrict shapes even when constraints imply [Tcoerce_none] *)
(***************************************************************************)

module type Small = sig
  type t
end
[%%expect{|
{
 "Small"[module type] -> <.40>;
 }
module type Small = sig type t end
|}]

module type Big = sig
  type t
  type u
end
[%%expect{|
{
 "Big"[module type] -> <.43>;
 }
module type Big = sig type t type u end
|}]

module type B2S = functor (X : Big) -> Small with type t = X.t
[%%expect{|
{
 "B2S"[module type] -> <.46>;
 }
module type B2S = functor (X : Big) -> sig type t = X.t end
|}]

module Big_to_small1 : B2S = functor (X : Big) -> X
[%%expect{|
{
 "Big_to_small1"[module] ->
   Abs<.48>(X, {<.47>
                "t"[type] -> X<.47> . "t"[type];
                });
 }
module Big_to_small1 : B2S
|}]

module Big_to_small2 : B2S = functor (X : Big) -> struct include X end
[%%expect{|
{
 "Big_to_small2"[module] -> Abs<.50>(X, {
                                         "t"[type] -> X<.49> . "t"[type];
                                         });
 }
module Big_to_small2 : B2S
|}]
