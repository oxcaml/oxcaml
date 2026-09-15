(******************************************************************************
 *                                  OxCaml                                    *
 *                        Basile Clément, OCamlPro                            *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2024 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

open Datalog_imports

type 'a variable =
  { name : string;
    tid : 'a Type.Id.t
  }

module Variable = struct
  type 'a t = 'a variable

  module Id = struct
    type t = int

    let equal = Int.equal

    let hash : int -> int = Hashtbl.hash

    module Tree = Patricia_tree.Make (Numbers.Int)
    module Set = Tree.Set
    module Map = Tree.Map
    module Tbl = Numbers.Int.Tbl
  end

  include Heterogenous_list.Make (struct
    type nonrec 'a t = 'a t
  end)

  let print ppf { name; tid = _ } = Format.fprintf ppf "%s" name

  let create name = { name; tid = Type.Id.make () }

  let name { name; _ } = name

  let uid { tid; _ } = Type.Id.uid tid

  let provably_equal { tid = tid1; _ } { tid = tid2; _ } =
    Type.Id.provably_equal tid1 tid2

  let must_be_equal (type a b) (var1 : a t) (var2 : b t) : (a, b) Type.eq =
    match provably_equal var1 var2 with
    | Some Equal -> Equal
    | None ->
      Misc.fatal_errorf "Variables must be equal (%a vs. %a)" print var1 print
        var2
end

type 'a term =
  | Variable of 'a variable
  | Literal of 'a

let print_term print_lit ppf term =
  match term with
  | Variable var -> Variable.print ppf var
  | Literal lit -> print_lit ppf lit

let var var = Variable var

let lit lit = Literal lit

module Term = struct
  type 'a t = 'a term

  include Heterogenous_list.Make (struct
    type nonrec 'a t = 'a t
  end)

  let print_hlist ~pp_sep ppf terms =
    let rec loop : type t. first:_ -> _ -> t hlist -> unit =
     fun ~first ppf terms ->
      match terms with
      | [] -> ()
      | term :: terms ->
        if not first then pp_sep ppf ();
        print_term (fun ppf _ -> Format.pp_print_string ppf "<cst>") ppf term;
        loop ~first:false ppf terms
    in
    loop ~first:true ppf terms
end

type ('k, 'v) relation =
  | Table : (_, 'k, 'v) Table.Id.t -> ('k, 'v) relation
  | Unless : (_, 'k, 'v) Table.Id.t -> ('k, unit) relation
  | Distinct : 'k Value.repr -> ('k -> 'k -> nil, unit) relation
  | Filter : ('k Constant.hlist -> bool) * string -> ('k, unit) relation
  | Callback_with_bindings :
      (Bytecode.bindings_ref -> 'k Constant.hlist -> unit) * string
      -> ('k, unit) relation

module Relation = struct
  type ('k, 'v) t = ('k, 'v) relation

  let print (type k v) ppf (t : (k, v) t) =
    match t with
    | Table tid -> Table.Id.print ppf tid
    | Unless tid -> Format.fprintf ppf "~%a" Table.Id.print tid
    | Distinct _repr -> Format.pp_print_string ppf "distinct"
    | Filter (_fn, name) -> Format.pp_print_string ppf name
    | Callback_with_bindings (_fn, name) -> Format.pp_print_string ppf name

  let print_neg (type k v) ppf (t : (k, v) t) =
    match t with
    | Table tid -> Format.fprintf ppf "not %a" Table.Id.print tid
    | Unless tid -> Format.fprintf ppf "%a" Table.Id.print tid
    | Distinct _repr -> Format.pp_print_string ppf "equal"
    | Filter (_fn, name) -> Format.fprintf ppf "not %s" name
    | Callback_with_bindings (_fn, name) -> Format.fprintf ppf "not %s" name
end

type atom = Atom : ('k, 'v) relation * 'k Term.hlist -> atom

let print_atom ppf (Atom (relation, terms)) =
  Format.fprintf ppf "@[<1>@[%a@](@,@[%a@])@]" Relation.print relation
    (Term.print_hlist ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ "))
    terms

let print_neg_atom ppf (Atom (relation, terms)) =
  Format.fprintf ppf "@[<1>@[%a@](@,@[%a@])@]" Relation.print_neg relation
    (Term.print_hlist ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ "))
    terms

let atom relation terms = Atom (relation, terms)

let table tid args = atom (Table tid) args

let unless tid args = atom (Unless tid) args

let distinct repr k1 k2 = atom (Distinct repr) [k1; k2]

let filter ?(name = "<filter>") fn args = atom (Filter (fn, name)) args

let callback_with_bindings ~name fn args =
  atom (Callback_with_bindings (fn, name)) args

type rule =
  { head : atom iarray;
    body : atom iarray
  }

let print_rule ppf { head; body } =
  (* Note: we need to eta-expand `Iarray.iter` because of `local` mode
     restrictions. *)
  Format.fprintf ppf "@[<2>@[%a@]@ :-@ @[<hv>%a.@]@]"
    (Format.pp_print_iter
       ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ ")
       (fun f arr -> Iarray.iter f arr)
       print_atom)
    head
    (Format.pp_print_iter
       ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ ")
       (fun f arr -> Iarray.iter f arr)
       print_atom)
    body

let rule ~head ~body =
  { head = Iarray.of_list head; body = Iarray.of_list body }
