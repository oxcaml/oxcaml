(* TEST
 flags = "-extension laws -extension module_strengthening";
 expect;
*)

(* The identification of a module's values with those of a path is a fact
   that only the compiler records, when it strengthens the type of a
   module with its path. The module types a user writes are obligations:
   a module ascribed to [module type of Fact] is not [Fact]. *)

module Fact = struct let x = 0 exception E end
module Forged : module type of struct include Fact end = struct
  let x = 1
  exception E
end
module type Fact_law = sig law? l : Fact.x = 0 end
module Not_forged : Fact_law = struct law? l : Forged.x = 0 end
[%%expect {|
module Fact : sig val x : int exception E end
module Forged : sig val x : int @@ stateless exception E end
module type Fact_law = sig law? l : Fact.x = 0 end
Line 7, characters 31-63:
7 | module Not_forged : Fact_law = struct law? l : Forged.x = 0 end
                                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig law? l : Forged.x = 0 end
       is not included in
         Fact_law
       Laws do not match:
         law? l : Forged.x = 0
       is not included in
         law? l : Fact.x = 0
       The clauses of the laws differ.
|}]

(* Nor are its exceptions those of [Fact]. *)

module type Fact_exception = sig law? l : Fact.E = Fact.E end
module Not_forged_exception : Fact_exception = struct
  law? l : Fact.E = Forged.E
end
[%%expect {|
module type Fact_exception = sig law? l : Fact.E = Fact.E end
Lines 2-4, characters 47-3:
2 | ...............................................struct
3 |   law? l : Fact.E = Forged.E
4 | end
Error: Signature mismatch:
       Modules do not match:
         sig law? l : Fact.E = Forged.E end
       is not included in
         Fact_exception
       Laws do not match:
         law? l : Fact.E = Forged.E
       is not included in
         law? l : Fact.E = Fact.E
       The clauses of the laws differ.
|}]

(* Nor is a module ascribed to [Fact_sig with Fact], the module type of
   [Fact] as a user writes it. *)

module Mock : module type of struct include Fact end = struct
  let x = 42
  exception E
end
module type Fact_sig = sig val x : int exception E end
module Strengthened_mock : Fact_sig with Fact = struct
  let x = 1
  exception E
end
module Not_forged_strengthened : Fact_law = struct
  law? l : Strengthened_mock.x = 0
end
[%%expect {|
module Mock : sig val x : int @@ stateless exception E end
module type Fact_sig = sig val x : int exception E end
module Strengthened_mock : sig val x : int exception E end
Lines 10-12, characters 44-3:
10 | ............................................struct
11 |   law? l : Strengthened_mock.x = 0
12 | end
Error: Signature mismatch:
       Modules do not match:
         sig law? l : Strengthened_mock.x = 0 end
       is not included in
         Fact_law
       Laws do not match:
         law? l : Strengthened_mock.x = 0
       is not included in
         law? l : Fact.x = 0
       The clauses of the laws differ.
|}]

(* Nor is the submodule of a module ascribed to
   [With_module with module M = Fact]. *)

module Strengthened_fact : Fact_sig with Fact = Fact
module type With_module = sig module M : Fact_sig end
module With_fact : With_module with module M = Fact = struct
  module M = struct let x = 1 exception E end
end
module Not_forged_with_module : Fact_law = struct
  law? l : With_fact.M.x = 0
end
[%%expect {|
module Strengthened_fact : sig val x : int exception E end
module type With_module = sig module M : Fact_sig end
module With_fact : sig module M : sig val x : int exception E end end
Lines 6-8, characters 43-3:
6 | ...........................................struct
7 |   law? l : With_fact.M.x = 0
8 | end
Error: Signature mismatch:
       Modules do not match:
         sig law? l : With_fact.M.x = 0 end
       is not included in
         Fact_law
       Laws do not match:
         law? l : With_fact.M.x = 0
       is not included in
         law? l : Fact.x = 0
       The clauses of the laws differ.
|}]

(* Inside a functor, a parameter of type [module type of Fact] is not
   [Fact]. *)

module Not_forged_parameter (X : module type of struct include Fact end) :
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
         sig law? l : Fact.x = 0 end
       is not included in
         sig law? l : X.x = 0 end
       Laws do not match:
         law? l : Fact.x = 0
       is not included in
         law? l : X.x = 0
       The clauses of the laws differ.
|}]

(* The facts the compiler records: the values of a structure including
   [Fact], of an alias of [Fact], of a functor parameter applied to [Fact]
   and of the result of a functor returning its parameter are those of
   [Fact]. *)

module Including = struct include Fact end
module Fact_alias = Fact
module Fact_param (X : Fact_sig) = struct law? applied : X.x = 0 end
module Fact_result (X : Fact_sig) = X
module Facts : sig
  law? included (y : int) : Fact.x = y
  law? aliased (y : int) : Fact.x = y
  law? applied : Fact.x = 0
  law? returned (y : int) : Fact.x = y
  law? included_exception : Fact.E = Fact.E
end = struct
  law? included (y : int) : Including.x = y
  law? aliased (y : int) : Fact_alias.x = y
  include Fact_param (Fact)
  module Returned = Fact_result (Fact)
  law? returned (y : int) : Returned.x = y
  law? included_exception : Including.E = Fact.E
end
[%%expect {|
module Including : sig val x : int exception E end
module Fact_alias = Fact
module Fact_param : functor (X : Fact_sig) -> sig law? applied : X.x = 0 end
module Fact_result :
  functor (X : Fact_sig) -> sig val x : int exception E end
module Facts :
  sig
    law? included (y : int) : Fact.x = y
    law? aliased (y : int) : Fact.x = y
    law? applied : Fact.x = 0
    law? returned (y : int) : Fact.x = y
    law? included_exception : Fact.E = Fact.E
  end
|}]
