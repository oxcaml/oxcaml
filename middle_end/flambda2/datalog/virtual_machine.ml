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

type outcome =
  | Accept
  | Skip

module Make (Iterator : sig
  include Leapfrog.Iterator

  include Heterogenous_list.S with type 'a t := 'a t
end) =
struct
  type 's stack =
    | Stack_nil : nil stack
    | Stack_cons :
        'a Iterator.t
        * 'a option Channel.sender
        * ('a -> 's) continuation
        * 's stack
        -> ('a -> 's) stack

  and 's continuation = 's stack -> unit

  type ('a, 's) instruction =
    | Advance : ('a, 's) instruction
    | Up : ('x, 's) instruction -> ('x, 'a -> 's) instruction
    | Dispatch : ('a, 'b -> 's) instruction
    | Seek :
        'b option Channel.receiver
        * 'b Iterator.t
        * ('a, 's) instruction
        * string
        * string
        -> ('a, 's) instruction
    | Open :
        'b Iterator.t
        * 'b option Channel.sender
        * ('a, 'b -> 's) instruction
        * ('a, 'b -> 's) instruction
        * string
        * string
        -> ('a, 's) instruction
    | Action : 'a * ('a, 's) instruction -> ('a, 's) instruction
    | Call :
        ('c -> 'b Constant.hlist -> unit)
        * 'c
        * 'b Option_receiver.hlist
        * ('a, 's) instruction
        * string
        * string list
        -> ('a, 's) instruction

  let pp_instruction pp_act ff instr =
    let pp_initiator ppf depth =
      if depth > 0 then Format.pp_print_space ppf ()
    in
    let pp_terminator ppf = Format.fprintf ppf "@]" in
    let rec pp_terminators ppf depth =
      if depth > 0
      then (
        pp_terminator ppf;
        pp_terminators ppf (depth - 1))
    in
    let rec pp_instruction : type s. _ -> (_, s) instruction * int -> unit =
     fun ff (instr, depth) ->
      match instr with
      | Advance ->
        (* Default terminator *)
        pp_terminators ff depth
      | Up instr ->
        let rec print_breaks : type s. _ -> (_, s) instruction -> unit =
         fun n instr ->
          match instr with
          | Up instr -> print_breaks (n + 1) instr
          | Advance | Open _ | Seek _ | Dispatch | Action _ | Call _ ->
            Format.fprintf ff "%a" pp_initiator depth;
            if n > 1
            then Format.fprintf ff "break %d" n
            else Format.fprintf ff "break";
            for _ = 0 to n - 1 do
              pp_terminator ff
            done;
            pp_instruction ff (instr, depth - n)
        in
        print_breaks 1 instr
      | Open (_iterator, _var, instr1, Dispatch, iterator_name, var_name) ->
        Format.fprintf ff "%a@[<v 2>@[<hov 2>for %s in %s:@]%a" pp_initiator
          depth var_name iterator_name pp_instruction
          (instr1, depth + 1)
      | Seek (_var, _iterator, instr, var_name, iterator_name) ->
        Format.fprintf ff "%a@[<v 2>@[<hov 2>for _ in {%s} ⨝ %s:@]%a"
          pp_initiator depth var_name iterator_name pp_instruction
          (instr, depth + 1)
      | Dispatch ->
        Format.fprintf ff "%adispatch%a" pp_initiator depth pp_terminators depth
      | Open (_iterator, _var, instr1, instr2, iterator_name, var_name) ->
        Format.fprintf ff
          "%a@[<v 2>@[<hov 2>open (%s : %s) [@;<1 0>%a@;<1 -2>]@] {%a"
          pp_initiator depth var_name iterator_name pp_instruction (instr2, 0)
          pp_instruction
          (instr1, depth + 1)
      | Action (a, instr) ->
        Format.fprintf ff "%a@[<v 2>%a@]%a" pp_initiator depth pp_act a
          pp_instruction (instr, depth)
      | Call (_f, _c, _l, instr, name, names) ->
        Format.fprintf ff "%a%s (%a)%a" pp_initiator depth name
          (Format.pp_print_list
             ~pp_sep:(fun ff () -> Format.fprintf ff ", ")
             Format.pp_print_string)
          names pp_instruction (instr, depth)
    in
    pp_instruction ff (instr, 0)

  let[@inline always] dispatch ~advance (stack : (_ -> _) stack) =
    let (Stack_cons (iterator, cell, level, next_stack)) = stack in
    match Iterator.current iterator with
    | Some current_key ->
      Iterator.accept iterator;
      Channel.send cell (Some current_key);
      level stack
    | None -> advance next_stack

  let[@loop] rec advance : type s. s continuation =
   fun stack ->
    match stack with
    | Stack_nil -> ()
    | Stack_cons (iterator, _, _, _) as stack ->
      Iterator.advance iterator;
      dispatch ~advance stack

  type t = nil continuation

  let[@inline] execute (type a) ~(evaluate : a -> outcome) instruction =
    let rec execute : type s. (a, s) instruction -> s continuation =
     fun instruction stack ->
      match instruction with
      | Advance -> advance stack
      | Up k ->
        let (Stack_cons (_, _, _, stack)) = stack in
        execute k stack
      | Open (iterator, cell, for_each, k, _iterator_name, _cell_name) ->
        Iterator.init iterator;
        execute k (Stack_cons (iterator, cell, execute for_each, stack))
      | Seek (key_ref, iterator, k, _key_ref_name, _iterator_name) -> (
        let key = Option.get (Channel.recv key_ref) in
        Iterator.init iterator;
        Iterator.seek iterator key;
        match Iterator.current iterator with
        | Some current_key when Iterator.equal_key iterator current_key key ->
          Iterator.accept iterator;
          execute k stack
        | None | Some _ -> advance stack)
      | Dispatch -> dispatch ~advance stack
      | Action (op, k) -> (
        match (evaluate [@inlined hint]) op with
        | Accept -> execute k stack
        | Skip -> advance stack)
      | Call (f, ctx, rs, k, _name, _names) ->
        f ctx (Option_receiver.recv rs);
        execute k stack
    in
    execute instruction

  let create ~evaluate (instruction : (_, _) instruction) =
    execute ~evaluate instruction

  let run continuation = continuation Stack_nil

  let advance = Advance

  let up i = Up i

  let dispatch = Dispatch

  let seek r it k = Seek (r.value, it.value, k, r.name, it.name)

  let open_ i cell a dispatch =
    Open (i.value, cell.value, a, dispatch, i.name, cell.name)

  let action a k = Action (a, k)

  let call f ~name ~context y k = Call (f, context, y.values, k, name, y.names)
end
