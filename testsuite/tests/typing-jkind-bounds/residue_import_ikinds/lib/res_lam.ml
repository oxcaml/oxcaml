

(* Basic interfaces *)

(* The types involved in our recursion *)
module type ET = sig type exp end
(* The recursive operations on our our types *)
module type E =
  sig
    include ET
    val eval : (string * exp) list -> exp -> exp
  end
(* Utility functor to extract useful types in its argument *)
module Types(X : sig type exp type a end) =
  struct type exp = X.exp type a = X.a end


(* Variables are common to lambda and expr *)

module VarT = struct
  type exp = [`Var of string]
end
module type VarS = sig
  type exp0 = private [> VarT.exp]
  include E with type exp = exp0
end
module Var(E : VarS) =
  struct
    type exp0 = VarT.exp
    type exp = E.exp
    let eval sub (`Var s as v : exp0) : exp =
      try List.assoc s sub with Not_found -> v
  end


(* The lambda language: free variables, substitutions, and evaluation *)

let gensym = let n = ref 0 in fun () -> incr n; "_" ^ string_of_int !n

(* Concrete definition of types in this language, with parameters to
   allow extension *)
module LamT = struct
  (* Define types using only one parameter for recursion; *)
  (* actual types are then extracted through the constraint *)
  type 'a exp = [VarT.exp | `Abs of string * 'e | `App of 'e * 'e]
  constraint 'a = <exp:'e;..>
end
(* Signature for parameters to the language construction functor *)
module type LamS = sig
  (* Close the recursion creating row-abstract types *)
  type exp0 = private [> a LamT.exp]
  and a = <exp:exp0>
  include E with type exp = exp0
end
module Lam(E : LamS) =
  struct
    type exp0 = E.a LamT.exp
    include Types(E)
    module LVar = Var(E)

    let eval subst : exp0 -> exp = function
        #LVar.exp0 as v -> LVar.eval subst v
      | `App(l1, l2) ->
          let l2' = E.eval subst l2 in
          let l1' = E.eval subst l1 in
          begin match l1' with
            `Abs (s, body) ->
              E.eval [s,l2'] body
          | _ ->
              `App (l1', l2')
          end
      | `Abs(s, l1) ->
          let s' = gensym () in
          `Abs(s', E.eval ((s,`Var s')::subst) l1)
  end


(* Signature for an actual module *)
module type LamF = sig
  (* Close the recursion creating concrete types *)
  type exp0 = a LamT.exp
  and a = <exp:exp0>
  include E with type exp = exp0
end
(* The actual language is a fix-point of the construction functor *)
module rec LamF : LamF = Lam(LamF)
let e1 = LamF.eval [] (`App(`Abs("x",`Var"x"), `Var"y"));;


(* The expr language of arithmetic expressions *)

(* Define the concrete types in the language *)
