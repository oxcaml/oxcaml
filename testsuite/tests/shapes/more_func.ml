(* TEST
 flags = "-dshape";
 expect;
*)
module M = struct end (* uid 0 *)
module F(X : sig end) = M
module App = F(List)
[%%expect{|
{
 "M"[module] -> {<.6>};
 }
module M : sig end
{
 "F"[module] -> Abs<.8>(X, {<.6>});
 }
module F : functor (X : sig end) -> sig end
{
 "App"[module] -> {<.9>};
 }
module App : sig end
|}]


module M = struct end (* uid 4 *)
module F(X : sig end) = struct include M type t end
module App = F(List)
[%%expect{|
{
 "M"[module] -> {<.10>};
 }
module M : sig end
{
 "F"[module] -> Abs<.13>(X, {
                             "t"[type] -> ((? ) : value);
                             });
 }
module F : functor (X : sig end) -> sig type t end
{
 "App"[module] -> {<.14>
                   "t"[type] -> ((? ) : value);
                   };
 }
module App : sig type t = F(List).t end
|}]

module M = struct end (* uid 9 *)
module F(X : sig end) = X
module App = F(M)
[%%expect{|
{
 "M"[module] -> {<.15>};
 }
module M : sig end
{
 "F"[module] -> Abs<.17>(X, X<.16>);
 }
module F : functor (X : sig end) -> sig end
{
 "App"[module] -> {<.18>};
 }
module App : sig end
|}]

module Id(X : sig end) = X
module Struct = struct
  module L = List
end
[%%expect{|
{
 "Id"[module] -> Abs<.20>(X, X<.19>);
 }
module Id : functor (X : sig end) -> sig end
{
 "Struct"[module] ->
   {<.22>
    "L"[module] -> Alias(<.21>
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
 "App"[module] -> (CU Stdlib . "List"[module])<.23>;
 }
module App : sig end
{
 "Proj"[module] -> Alias(<.24>
                         Alias(<.21>
                               CU Stdlib . "List"[module]));
 }
module Proj = Struct.L
|}]

module F (X :sig end ) = struct module M = X end
module N = F(struct end)
module O = N.M
[%%expect{|
{
 "F"[module] -> Abs<.27>(X, {
                             "M"[module] -> X<.25>;
                             });
 }
module F : functor (X : sig end) -> sig module M : sig end end
{
 "N"[module] -> {<.28>
                 "M"[module] -> {<.25>};
                 };
 }
module N : sig module M : sig end end
{
 "O"[module] -> Alias(<.29>
                      {<.25>});
 }
module O = N.M
|}]
