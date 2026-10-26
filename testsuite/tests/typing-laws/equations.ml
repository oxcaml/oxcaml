(* TEST
 flags = "-extension laws -extension module_strengthening";
 expect;
*)

(* When the compiler strengthens the type of a module with its path, it
   records that the values of the module are those of the path, as it
   records that its types are. The clauses of laws are compared up to these
   equations: the values of a structure including [Fact], of an alias of
   [Fact], of a functor parameter applied to [Fact] and of the result of a
   functor returning its parameter are those of [Fact]. *)

module Fact = struct let x = 0 end
module type Fact_sig = sig val x : int end
module Including = struct include Fact end
module Fact_alias = Fact
module Fact_param (X : Fact_sig) = struct law? applied : X.x = 0 end
module Fact_result (X : Fact_sig) = X
module Facts : sig
  law? included (y : int) : Fact.x = y
  law? aliased (y : int) : Fact.x = y
  law? applied : Fact.x = 0
  law? returned (y : int) : Fact.x = y
end = struct
  law? included (y : int) : Including.x = y
  law? aliased (y : int) : Fact_alias.x = y
  include Fact_param (Fact)
  module Returned = Fact_result (Fact)
  law? returned (y : int) : Returned.x = y
end
[%%expect {|
module Fact : sig val x : int end
module type Fact_sig = sig val x : int end
module Including : sig val x : int end
module Fact_alias = Fact
module Fact_param :
  functor (X : Fact_sig) -> sig law? applied : X.x = (0) end
module Fact_result : functor (X : Fact_sig) -> sig val x : int end
module Facts :
  sig
    law? included (y : int) : Fact.x = y
    law? aliased (y : int) : Fact.x = y
    law? applied : Fact.x = (0)
    law? returned (y : int) : Fact.x = y
  end
|}]

(* The module types a user writes are obligations, not facts: a module
   ascribed to [module type of Fact] is not [Fact], *)

module Forged : module type of struct include Fact end = struct let x = 1 end
module type Fact_law = sig law? l : Fact.x = 0 end
module Not_forged : Fact_law = struct law? l : Forged.x = 0 end
[%%expect {|
module Forged : sig val x : int @@ stateless end
module type Fact_law = sig law? l : Fact.x = (0) end
Line 3, characters 31-63:
3 | module Not_forged : Fact_law = struct law? l : Forged.x = 0 end
                                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig law? l : Forged.x = (0) end
       is not included in
         Fact_law
       Laws do not match:
         law? l : Forged.x = (0)
       is not included in
         law? l : Fact.x = (0)
       The clauses of the two laws differ.
|}]

(* nor is a module ascribed to [Fact_sig with Fact], *)

module Strengthened : Fact_sig with Fact = struct let x = 1 end
module Not_strengthened : Fact_law = struct law? l : Strengthened.x = 0 end
[%%expect {|
module Strengthened : sig val x : int end
Line 2, characters 37-75:
2 | module Not_strengthened : Fact_law = struct law? l : Strengthened.x = 0 end
                                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig law? l : Strengthened.x = (0) end
       is not included in
         Fact_law
       Laws do not match:
         law? l : Strengthened.x = (0)
       is not included in
         law? l : Fact.x = (0)
       The clauses of the two laws differ.
|}]

(* nor the submodule of a module ascribed to
   [With_module with module M = Fact], *)

module type With_module = sig module M : Fact_sig end
module With_fact : With_module with module M = Fact = struct
  module M = struct let x = 1 end
end
module Not_with_module : Fact_law = struct law? l : With_fact.M.x = 0 end
[%%expect {|
module type With_module = sig module M : Fact_sig end
module With_fact : sig module M : sig val x : int end end
Line 5, characters 36-73:
5 | module Not_with_module : Fact_law = struct law? l : With_fact.M.x = 0 end
                                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig law? l : With_fact.M.x = (0) end
       is not included in
         Fact_law
       Laws do not match:
         law? l : With_fact.M.x = (0)
       is not included in
         law? l : Fact.x = (0)
       The clauses of the two laws differ.
|}]

(* nor a functor parameter of type [module type of Fact]. *)

module Not_parameter (X : module type of struct include Fact end) :
  sig law? l : X.x = 0 end = struct
  law? l : Fact.x = 0
end
[%%expect {|
Lines 2-4, characters 29-3:
2 | .............................struct
3 |   law? l : Fact.x = 0
4 | end
Error: Signature mismatch:
       Modules do not match:
         sig law? l : Fact.x = (0) end
       is not included in
         sig law? l : X.x = (0) end
       Laws do not match:
         law? l : Fact.x = (0)
       is not included in
         law? l : X.x = (0)
       The clauses of the two laws differ.
|}]
