(* TEST
 flags = "-dshape";
 expect;
*)
module M = struct end (* uid 0 *)
module F(X : sig end) = M
module App = F(List)
[%%expect{|
{
 "M"[module] -> {<.5>};
 }
module M : sig end
{
 "F"[module] -> Abs<.7>(X, {<.5>});
 }
module F : functor (X : sig end) -> sig end
{
 "App"[module] -> {<.8>};
 }
module App : sig end
|}]


module M = struct end (* uid 4 *)
module F(X : sig end) = struct include M type t end
module App = F(List)
[%%expect{|
{
 "M"[module] -> {<.9>};
 }
module M : sig end
{
 "F"[module] -> Abs<.12>(X, {
                             "t"[type] -> <.11>;
                             });
 }
module F : functor (X : sig end) -> sig type t end
{
 "App"[module] -> {<.13>
                   "t"[type] -> <.11>;
                   };
 }
module App : sig type t = F(List).t end
|}]

module M = struct end (* uid 9 *)
module F(X : sig end) = X
module App = F(M)
[%%expect{|
{
 "M"[module] -> {<.14>};
 }
module M : sig end
{
 "F"[module] -> Abs<.16>(X, X<.15>);
 }
module F : functor (X : sig end) -> sig end
{
 "App"[module] -> {<.17>};
 }
module App : sig end
|}]

module Id(X : sig end) = X
module Struct = struct
  module L = List
end
[%%expect{|
{
 "Id"[module] -> Abs<.19>(X, X<.18>);
 }
module Id : functor (X : sig end) -> sig end
{
 "Struct"[module] ->
   {<.21>
    "L"[module] -> Alias(<.20>
                         CU Stdlib . "List"[module]);
    };
 }
module Struct : sig module L = List end
|}]

module App = Id(List) (* this should have the App uid *)
module Proj = Struct.L
  (* this should have the Proj uid and be an alias to Struct.L *)
[%%expect{|
{
 "App"[module] -> (CU Stdlib . "List"[module])<.22>;
 }
module App : sig end
{
 "Proj"[module] -> Alias(<.23>
                         Alias(<.20>
                               CU Stdlib . "List"[module]));
 }
module Proj = Struct.L
|}]

module F (X :sig end ) = struct module M = X end
module N = F(struct end)
module O = N.M
[%%expect{|
{
 "F"[module] -> Abs<.26>(X, {
                             "M"[module] -> X<.24>;
                             });
 }
module F : functor (X : sig end) -> sig module M : sig end end
{
 "N"[module] -> {<.27>
                 "M"[module] -> {<.24>};
                 };
 }
module N : sig module M : sig end end
{
 "O"[module] -> Alias(<.28>
                      {<.24>});
 }
module O = N.M
|}]
