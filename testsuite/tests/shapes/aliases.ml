(* TEST
 flags = "-dshape";
 expect;
*)
module A = struct type t end
module B = A
[%%expect{|
{
 "A"[module] -> {<.6>
                 "t"[type] -> <.5>;
                 };
 }
module A : sig type t end
{
 "B"[module] -> Alias(<.7>
                      {<.6>
                       "t"[type] -> <.5>;
                       });
 }
module B = A
|}]

type u = B.t

[%%expect{|
{
 "u"[type] -> <.8>;
 }
type u = B.t
|}]

module F (X : sig type t end) = X
module F' = F
[%%expect{|
{
 "F"[module] -> Abs<.12>(X, X<.11>);
 }
module F : functor (X : sig type t end) -> sig type t = X.t end
{
 "F'"[module] -> Alias(<.13>
                       Abs<.12>(X, X<.11>));
 }
module F' = F
|}]

module C = F'(A)
[%%expect{|
{
 "C"[module] -> {<.14>
                 "t"[type] -> <.5>;
                 };
 }
module C : sig type t = A.t end
|}]


module C = F(B)

[%%expect{|
{
 "C"[module] -> Alias(<.15>
                      {<.6>
                       "t"[type] -> <.5>;
                       });
 }
module C : sig type t = B.t end
|}]

module D = C

[%%expect{|
{
 "D"[module] -> Alias(<.16>
                      Alias(<.15>
                            {<.6>
                             "t"[type] -> <.5>;
                             }));
 }
module D = C
|}]

module G (X : sig type t end) = struct include X end
[%%expect{|
{
 "G"[module] -> Abs<.19>(X, {
                             "t"[type] -> X<.18> . "t"[type];
                             });
 }
module G : functor (X : sig type t end) -> sig type t = X.t end
|}]

module E = G(B)
[%%expect{|
{
 "E"[module] -> {<.20>
                 "t"[type] -> <.5>;
                 };
 }
module E : sig type t = B.t end
|}]

module M = struct type t let x = 1 end
module N : sig type t end = M
module O = N
[%%expect{|
{
 "M"[module] -> {<.23>
                 "t"[type] -> <.21>;
                 "x"[value] -> <.22>;
                 };
 }
module M : sig type t val x : int end
{
 "N"[module] -> {<.26>
                 "t"[type] -> <.21>;
                 };
 }
module N : sig type t end
{
 "O"[module] -> Alias(<.27>
                      {<.26>
                       "t"[type] -> <.21>;
                       });
 }
module O = N
|}]
