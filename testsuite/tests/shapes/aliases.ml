(* TEST
 flags = "-dshape";
 expect;
*)
module A = struct type t end
module B = A
[%%expect{|
{
 "A"[module] -> {<.1>
                 "t"[type] -> <.0>;
                 };
 }
module A : sig type t end
{
 "B"[module] -> Alias(<.2>
                      {<.1>
                       "t"[type] -> <.0>;
                       });
 }
module B = A
|}]

type u = B.t

[%%expect{|
{
 "u"[type] -> <.3>;
 }
type u = B.t
|}]

module F (X : sig type t end) = X
module F' = F
[%%expect{|
{
 "F"[module] -> Abs<.7>(X, X<.6>);
 }
module F : functor (X : sig type t end) -> sig type t = X.t end
{
 "F'"[module] -> Alias(<.8>
                       Abs<.7>(X, X<.6>));
 }
module F' = F
|}]

module C = F'(A)
[%%expect{|
{
 "C"[module] -> {<.9>
                 "t"[type] -> <.0>;
                 };
 }
module C : sig type t = A.t end
|}]


module C = F(B)

[%%expect{|
{
 "C"[module] -> Alias(<.10>
                      {<.1>
                       "t"[type] -> <.0>;
                       });
 }
module C : sig type t = B.t end
|}]

module D = C

[%%expect{|
{
 "D"[module] -> Alias(<.11>
                      Alias(<.10>
                            {<.1>
                             "t"[type] -> <.0>;
                             }));
 }
module D = C
|}]

module G (X : sig type t end) = struct include X end
[%%expect{|
{
 "G"[module] -> Abs<.15>(X, {
                             "t"[type] -> X<.14> . "t"[type];
                             });
 }
module G : functor (X : sig type t end) -> sig type t = X.t end
|}]

module E = G(B)
[%%expect{|
{
 "E"[module] -> {<.16>
                 "t"[type] -> <.0>;
                 };
 }
module E : sig type t = B.t end
|}]

module M = struct type t let x = 1 end
module N : sig type t end = M
module O = N
[%%expect{|
{
 "M"[module] -> {<.19>
                 "t"[type] -> <.17>;
                 "x"[value] -> <.18>;
                 };
 }
module M : sig type t val x : int end
{
 "N"[module] -> {<.22>
                 "t"[type] -> <.17>;
                 };
 }
module N : sig type t end
{
 "O"[module] -> Alias(<.23>
                      {<.22>
                       "t"[type] -> <.17>;
                       });
 }
module O = N
|}]
