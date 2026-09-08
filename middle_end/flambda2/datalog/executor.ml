(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Basile Clément, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2024--2025 OCamlPro SAS                                    *)
(*   Copyright 2024--2025 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Datalog_imports

(* Note: we don't use [with_name] here to avoid the extra indirection during
   execution. *)
type vm_action =
  | Unless :
      ('t, 'k, 'v) Trie.is_trie
      * 't Channel.or_null_receiver
      * 'k Or_null_receiver.hlist
      * string
      * string list
      -> vm_action
  | Unless_eq :
      'k Or_null_receiver.t
      * 'k Or_null_receiver.t
      * string
      * string
      * 'k Value.repr
      -> vm_action
  | Filter :
      ('k Constant.hlist -> bool) * 'k Or_null_receiver.hlist * string list
      -> vm_action

let print_vm_action ff = function
  | Unless (_, _t, _l, t_name, l_names) ->
    Format.fprintf ff "if %s(%a):@ continue" t_name
      (Format.pp_print_list
         ~pp_sep:(fun ff () -> Format.fprintf ff ", ")
         Format.pp_print_string)
      l_names
  | Unless_eq (_x1, _x2, x1_name, x2_name, _repr) ->
    Format.fprintf ff "if %s == %s:@ continue" x1_name x2_name
  | Filter (_f, _args, args_names) ->
    Format.fprintf ff "<filter>(%a)"
      (Format.pp_print_list
         ~pp_sep:(fun ff () -> Format.fprintf ff ", ")
         Format.pp_print_string)
      args_names

let evaluate = function
  | Unless (is_trie, cell, args, _cell_name, _args_names) ->
    let value =
      match Channel.recv_or_null cell with
      | Null -> Misc.fatal_error "null"
      | This value -> value
    in
    if
      Or_null.is_this
        (Trie.find_or_null is_trie (Or_null_receiver.recv_hlist args) value)
    then Virtual_machine.Skip
    else Virtual_machine.Accept
  | Unless_eq (cell1, cell2, _cell1_name, _cell2_name, repr) ->
    if
      Value.equal_repr repr
        (Or_null_receiver.recv cell1)
        (Or_null_receiver.recv cell2)
    then Virtual_machine.Skip
    else Virtual_machine.Accept
  | Filter (f, args, _args_names) ->
    if f (Or_null_receiver.recv_hlist args)
    then Virtual_machine.Accept
    else Virtual_machine.Skip

module Join_iterator = struct
  module T0 = Leapfrog.Join (Trie.Iterator)
  include T0
  include Heterogenous_list.Make (T0)
end

module VM = Virtual_machine.Make (Join_iterator)

type bindings_ref =
  | Bindings_ref_innermost_first :
      ('a Value.hlist * 'a Or_null_receiver.hlist with_names)
      -> bindings_ref
[@@unboxed]

type 's builder =
  | Builder of
      ('s Value.hlist * 's Or_null_receiver.hlist with_names ->
      (vm_action, 's) VM.instruction)
[@@unboxed]

let build (Builder fn) = fn

let builder fn = Builder fn

let for_in { value = repr; name } iterators (body : _ -> _ builder) : _ builder
    =
  builder (fun (rev_reprs, rev_receivers) ->
      let sender, receiver = Channel.create_or_null Or_null.null in
      let body =
        let { values; names } = rev_receivers in
        build (body receiver)
          ( repr :: rev_reprs,
            { values = receiver :: values; names = name :: names } )
      in
      let iterator =
        let { values; names } = iterators in
        { value = Join_iterator.create values;
          name = String.concat " ⨝ " names
        }
      in
      VM.open_ iterator { value = sender; name } body VM.dispatch)

let map_instruction body fn = Builder (fun bindings -> fn (build body bindings))

let if_in key { values; names } body =
  let iterator =
    { value = Join_iterator.create values; name = String.concat " ⨝ " names }
  in
  map_instruction body (VM.seek key iterator)

let unless is_trie table args body =
  VM.action (Unless (is_trie, table.value, args.values, table.name, args.names))
  |> map_instruction body

let unless_eq repr recv1 recv2 body =
  VM.action (Unless_eq (recv1.value, recv2.value, recv1.name, recv2.name, repr))
  |> map_instruction body

let filter fn receivers body =
  VM.action (Filter (fn, receivers.values, receivers.names))
  |> map_instruction body

let call { value; name } args (body : _ builder) : _ builder =
  builder (fun bindings ->
      VM.call value ~name ~context:(Bindings_ref_innermost_first bindings) args
        (build body bindings))

(* NB: the variables must be passed in reverse order, i.e. innermost variable
   first. *)
let rec vm_break : type s.
    int -> s Or_null_receiver.hlist -> (vm_action, s) VM.instruction =
 fun n -> function
  | _ :: vars when n > 0 -> VM.up (vm_break (n - 1) vars)
  | [] | _ :: _ -> VM.advance

let break n =
  builder (fun (_, rev_receivers) -> vm_break n rev_receivers.values)

type t =
  { instruction : (vm_action, nil) VM.instruction;
    bytecode : VM.t
  }

let print ppf { instruction; _ } =
  VM.pp_instruction print_vm_action ppf instruction

let build builder =
  let instruction = build builder ([], { values = []; names = [] }) in
  { instruction; bytecode = VM.create ~evaluate instruction }

let run { bytecode; _ } = VM.run bytecode

(* API for the [Cursor]-based [Scheduler] *)

type bindings =
  | Bindings_innermost_first :
      'a Value.hlist * 'a Constant.hlist with_names
      -> bindings

let get_bindings (Bindings_ref_innermost_first (reprs, receivers)) =
  let values = Or_null_receiver.recv_hlist receivers.values in
  Bindings_innermost_first (reprs, { receivers with values })

let print_bindings ppf (Bindings_innermost_first (reprs, { values; names })) =
  let rec loop : type a.
      Format.formatter ->
      a Value.hlist ->
      a Constant.hlist ->
      string list ->
      bool =
   fun ppf reprs values names ->
    match reprs, values, names with
    | [], [], _ :: _ | _ :: _, _ :: _, [] ->
      Misc.fatal_error "Wrong number of names"
    | [], [], [] -> true
    | repr :: reprs, value :: values, name :: names ->
      let first = loop ppf reprs values names in
      if not first then Format.fprintf ppf ";@,";
      Format.fprintf ppf "@[<1>%s =@ %a@]" name (Value.print_repr repr) value;
      false
  in
  Format.fprintf ppf "@[<2>{ @[<v>";
  ignore (loop ppf reprs values names);
  Format.fprintf ppf "@] }@]"
