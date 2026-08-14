(* TEST
 flags = "-dshape";
 expect;
*)
module M = struct end (* uid 0 *)
module F(X : sig end) = M
module App = F(List)
[%%expect{|
{
 "M"[module] -> {<.0>};
 }
module M : sig end
{
 "F"[module] -> Abs<.3>(X, {<.0>});
 }
module F : functor (X : sig end) -> sig end
{
 "App"[module] -> {<.4>};
 }
module App : sig end
|}]


module M = struct end (* uid 5 *)
module F(X : sig end) = struct include M type t end
module App = F(List)
[%%expect{|
{
 "M"[module] -> {<.5>};
 }
module M : sig end
{
 "F"[module] -> Abs<.9>(X, {
                            "t"[type] -> <.8>;
                            });
 }
module F : functor (X : sig end) -> sig type t end
{
 "App"[module] -> {<.10>
                   "t"[type] -> <.8>;
                   };
 }
module App : sig type t = F(List).t end
|}]

module M = struct end (* uid 11 *)
module F(X : sig end) = X
module App = F(M)
[%%expect{|
{
 "M"[module] -> {<.11>};
 }
module M : sig end
{
 "F"[module] -> Abs<.14>(X, X<.13>);
 }
module F : functor (X : sig end) -> sig end
{
 "App"[module] -> {<.15>};
 }
module App : sig end
|}]

module Id(X : sig end) = X
module Struct = struct
  module L = List
end
[%%expect{|
{
 "Id"[module] -> Abs<.18>(X, X<.17>);
 }
module Id : functor (X : sig end) -> sig end
{
 "Struct"[module] ->
   {<.20>
    "L"[module] -> Alias(<.19>
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
 "App"[module] -> (CU Stdlib . "List"[module])<.21>;
 }
module App : sig end
{
 "Proj"[module] -> Alias(<.22>
                         Alias(<.19>
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
