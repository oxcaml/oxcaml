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
 "S"[module type] -> <.7>;
 }
module type S = sig type t val x : t end
|}]

module Falias (X : S) = X
[%%expect{|
{
 "Falias"[module] -> Abs<.9>(X, X<.8>);
 }
module Falias : functor (X : S) -> sig type t = X.t val x : t end
|}]

module Finclude (X : S) = struct
  include X
end
[%%expect{|
{
 "Finclude"[module] ->
   Abs<.11>
      (X,
       {
        "t"[type] -> X<.10> . "t"[type];
        "x"[value] -> X<.10> . "x"[value];
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
 "Fredef"[module] ->
   Abs<.16>(X, {
                "t"[type] -> (X<.12> . "t"[type])<.13>;
                "x"[value] -> <.15>;
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
   Abs<.21>((), {
                 "t"[type] -> Variant Fresh<.19>;
                 "x"[value] -> <.20>;
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
 "Arg"[module] -> {<.26>
                   "t"[type] -> Variant T<.24>;
                   "x"[value] -> <.25>;
                   };
 }
module Arg : S
|}]

include Falias(Arg)
[%%expect{|
{
 "t"[type] -> Variant T<.24>;
 "x"[value] -> <.25>;
 }
type t = Arg.t
val x : t = <abstr>
|}]

include Finclude(Arg)
[%%expect{|
{
 "t"[type] -> Variant T<.24>;
 "x"[value] -> <.25>;
 }
type t = Arg.t
val x : t = <abstr>
|}]

include Fredef(Arg)
[%%expect{|
{
 "t"[type] -> Variant T<.24>;
 "x"[value] -> <.15>;
 }
type t = Arg.t
val x : Arg.t = <abstr>
|}]

include Fignore(Arg)
[%%expect{|
{
 "t"[type] -> Variant Fresh<.19>;
 "x"[value] -> <.20>;
 }
type t = Fignore(Arg).t = Fresh
val x : t = Fresh
|}]

include Falias(struct type t = int let x = 0 end)
[%%expect{|
{
 "t"[type] -> Predef int ();
 "x"[value] -> <.30>;
 }
type t = int
val x : t = 0
|}]

include Finclude(struct type t = int let x = 0 end)
[%%expect{|
{
 "t"[type] -> Predef int ();
 "x"[value] -> <.34>;
 }
type t = int
val x : t = 0
|}]

include Fredef(struct type t = int let x = 0 end)
[%%expect{|
{
 "t"[type] -> Predef int ();
 "x"[value] -> <.15>;
 }
type t = int
val x : int = 0
|}]

include Fignore(struct type t = int let x = 0 end)
[%%expect{|
{
 "t"[type] -> Variant Fresh<.19>;
 "x"[value] -> <.20>;
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
   Abs<.47>((), {
                 "t"[type] -> Variant Fresher<.45>;
                 "x"[value] -> <.46>;
                 });
 }
module Fgen : functor () -> sig type t = Fresher val x : t end
|}]

include Fgen ()
[%%expect{|
{
 "t"[type] -> Variant Fresher<.45>;
 "x"[value] -> <.46>;
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
 "Small"[module type] -> <.50>;
 }
module type Small = sig type t end
|}]

module type Big = sig
  type t
  type u
end
[%%expect{|
{
 "Big"[module type] -> <.53>;
 }
module type Big = sig type t type u end
|}]

module type B2S = functor (X : Big) -> Small with type t = X.t
[%%expect{|
{
 "B2S"[module type] -> <.56>;
 }
module type B2S = functor (X : Big) -> sig type t = X.t end
|}]

module Big_to_small1 : B2S = functor (X : Big) -> X
[%%expect{|
{
 "Big_to_small1"[module] ->
   Abs<.58>(X, {<.57>
                "t"[type] -> X<.57> . "t"[type];
                });
 }
module Big_to_small1 : B2S
|}]

module Big_to_small2 : B2S = functor (X : Big) -> struct include X end
[%%expect{|
{
 "Big_to_small2"[module] -> Abs<.60>(X, {
                                         "t"[type] -> X<.59> . "t"[type];
                                         });
 }
module Big_to_small2 : B2S
|}]
