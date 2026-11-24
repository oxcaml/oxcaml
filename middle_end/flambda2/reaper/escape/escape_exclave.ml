(******************************************************************************
 *                                  OxCaml                                    *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2026 Jane Street Group LLC                                   *
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

(*= OxCaml-Written-By: ai:fable-5.2
    OxCaml-Review-Status: ai:fable-5.2:xhigh *)

(* A function [f] made to return at mode local allocates the values it returns
   in the region of its caller, so it cannot simply be given a region spanning
   its body like the other functions. But the local stack is a stack: once [f]
   has closed a region of its own, what it allocates goes to its caller's
   region. So [f] gets a region for its temporaries if we can find where to
   close it: after the last use of anything living in it, and before the first
   allocation of anything returned. This is what [exclave_] expresses in the
   source language.

   We work at the granularity of the blocks of the body: the start of the body
   up to its first terminator, and the handlers of the continuations defined in
   it. The zone is a set of handlers, in which the region is closed, together
   with some applications ending a block outside of the zone, before which the
   region is closed (the exclave applications). It is the smallest set such
   that:

   - a block allocating a local value that [f] may return (see
   [Escape_solution.local_returned]) is in the zone;

   - an application that may deliver such a value, or a local block containing
   one, is in the zone or is an exclave application (the callee, which returns
   at mode local, allocates in the region current at the call);

   - the zone is closed under control flow: the targets of the jumps of a block
   of the zone, the continuation of an application of the zone, and the handlers
   defined in a block of the zone, are in the zone.

   The local values living in the region of [f] (the temporaries) are the ones
   allocated in a block outside of the zone, or delivered by an application that
   is neither in the zone nor an exclave application. The plan is valid if no
   variable occurring in the zone (in a block of the zone, as an argument of an
   exclave application, or as an argument of a jump from outside of the zone to
   the zone or to the return continuation) may hold a temporary. A temporary
   cannot be reached in any other way: it can only be stored in a local block by
   the construction of that block (a mutation makes the stored value escape),
   which is an occurrence.

   The region is closed where control enters the zone. For a handler of the zone
   that is only jumped to from outside of the zone, this is at the start of the
   handler. Otherwise it is before the jumps coming from outside of the zone,
   which must then all be [Apply_cont] expressions: nothing can be inserted
   before the jump of a [Switch] arm or of the return of an application, and the
   continuations [Escape_rebuild] uses for this purpose elsewhere are bound
   where the handler is not in scope.

   We give up (the function then gets no region of its own, and its temporaries
   live until its caller's region is closed) when:

   - the body contains an exception handler or a region of its own: closing our
   region while a try-region or another region opened after it is still open
   would let the latter restore the stack pointer above ours;

   - for the same reason, control would enter the zone inside the handler of a
   recursive continuation that is not in the zone (it may have a region);

   - the zone contains the start of the body;

   - nothing would be allocated in the region. *)

type mode =
  | Interior
  | Close_on_entry
  | Close_before_jump

type t =
  { modes : mode Continuation.Map.t;
    exclave_applies : Continuation.Set.t;
    (* A fresh continuation standing for the start of the body. *)
    entry : Continuation.t
  }

let in_zone t k = Continuation.Map.mem k t.modes

let has_mode t k mode =
  match Continuation.Map.find_opt k t.modes with
  | None -> false
  | Some mode' -> mode == mode'

let closed_before_jump t k = has_mode t k Close_before_jump

let closed_on_entry t k = has_mode t k Close_on_entry

let closed_before_apply t block =
  Continuation.Set.mem (Option.value block ~default:t.entry) t.exclave_applies

type block =
  { lets : (Bound_pattern.t * Rev_expr.rev_named) list;
    terminator : Rev_expr.tail_expr;
    (* The block in which the handler is defined. *)
    parent : Continuation.t option;
    recursive : bool
  }

exception Give_up

let rec collect_blocks blocks ~id ~parent ~recursive
    ({ expr; holed_expr } : Rev_expr.t) =
  let collect_handler blocks cont ~recursive (handler : Rev_expr.cont_handler) =
    if handler.is_exn_handler
    then raise Give_up
    else
      collect_blocks blocks ~id:cont ~parent:(Some id) ~recursive handler.expr
  in
  let rec collect blocks lets (holed : Rev_expr.rev_expr_holed) =
    match holed with
    | Hole -> blocks, lets
    | Let { bound_pattern; defining_expr; parent } ->
      (match[@ocaml.warning "-fragile-match"] defining_expr with
      | Named
          (Prim
             ( ( Variadic ((Begin_region _ | Begin_try_region _), _)
               | Unary ((End_region _ | End_try_region _), _) ),
               _ ))
      | Static_consts _ ->
        raise Give_up
      | Named _ | Set_of_closures _ -> ());
      collect blocks ((bound_pattern, defining_expr) :: lets) parent
    | Let_cont { cont; handler; parent } ->
      collect (collect_handler blocks cont ~recursive:false handler) lets parent
    | Let_cont_rec { invariant_params = _; handlers; parent } ->
      let blocks =
        Continuation.Lmap.fold
          (fun cont handler blocks ->
            collect_handler blocks cont ~recursive:true handler)
          handlers blocks
      in
      collect blocks lets parent
  in
  let blocks, lets = collect blocks [] holed_expr in
  Continuation.Map.add id { lets; terminator = expr; parent; recursive } blocks

type edge_kind =
  | Jump
  | Switch_arm
  | Return_of_apply

let edges (terminator : Rev_expr.tail_expr) =
  let jump kind apply_cont =
    ( Apply_cont_expr.continuation apply_cont,
      kind,
      Apply_cont_expr.args apply_cont )
  in
  match terminator with
  | Invalid _ -> []
  | Apply_cont apply_cont -> [jump Jump apply_cont]
  | Switch switch ->
    Target_ocaml_int.Map.fold
      (fun _ arm edges -> jump Switch_arm (Switch_arm.action arm) :: edges)
      (Switch_expr.arms switch) []
  | Apply apply -> (
    match Flambda.Apply.continuation apply with
    | Never_returns -> []
    | Return k -> [k, Return_of_apply, []])

let variables_of_names names =
  Name_occurrences.fold_variables names ~init:[] ~f:(fun vars var ->
      var :: vars)

let variables_of_simples simples =
  List.filter_map
    (fun simple ->
      Simple.pattern_match' simple
        ~const:(fun _ -> None)
        ~symbol:(fun _ ~coercion:_ -> None)
        ~var:(fun var ~coercion:_ -> Some var))
    simples

let variables_of_named (named : Rev_expr.rev_named) =
  match named with
  | Named named -> variables_of_names (Flambda.Named.free_names named)
  | Set_of_closures ({ value_slots; function_decls = _; dbg = _ }, _) ->
    variables_of_simples (List.map snd (Value_slot.Map.bindings value_slots))
  | Static_consts _ -> raise Give_up

let variables_of_terminator (terminator : Rev_expr.tail_expr) =
  match terminator with
  | Invalid _ -> []
  | Apply_cont apply_cont ->
    variables_of_names (Apply_cont_expr.free_names apply_cont)
  | Switch switch -> variables_of_names (Switch_expr.free_names switch)
  | Apply apply -> variables_of_names (Flambda.Apply.free_names apply)

let compute_exn solution ~continuation_info ~code_id ~return_continuation body =
  let entry = Continuation.create () in
  let blocks =
    collect_blocks Continuation.Map.empty ~id:entry ~parent:None
      ~recursive:false body
  in
  let params k =
    match Continuation.Map.find_opt k continuation_info with
    | Some { Traverse_acc.Continuation_info.params; _ } -> params
    | None -> raise Give_up
  in
  let returned = Escape_solution.local_returned solution code_id in
  (* The local values that may be passed to a continuation, with the ones they
     may contain. *)
  let delivered =
    let cache = ref Continuation.Map.empty in
    let rec add allocs alloc =
      if Code_id_or_name.Set.mem alloc allocs
      then allocs
      else
        Code_id_or_name.Set.fold
          (fun alloc allocs -> add allocs alloc)
          (Escape_solution.local_contents solution alloc)
          (Code_id_or_name.Set.add alloc allocs)
    in
    fun k ->
      match Continuation.Map.find_opt k !cache with
      | Some allocs -> allocs
      | None ->
        let allocs =
          List.fold_left
            (fun allocs param ->
              List.fold_left add allocs
                (Escape_solution.local_sources solution param))
            Code_id_or_name.Set.empty (params k)
        in
        cache := Continuation.Map.add k allocs !cache;
        allocs
  in
  let bound_vars (bound_pattern : Bound_pattern.t) =
    match bound_pattern with
    | Singleton bv -> [Bound_var.var bv]
    | Set_of_closures bvs -> List.map Bound_var.var bvs
    | Static _ -> raise Give_up
  in
  let children =
    Continuation.Map.fold
      (fun id block children ->
        match block.parent with
        | None -> children
        | Some parent ->
          Continuation.Map.update parent
            (fun ids -> Some (id :: Option.value ids ~default:[]))
            children)
      blocks Continuation.Map.empty
  in
  (* The zone. *)
  let zone = ref Continuation.Set.empty in
  let exclave_applies = ref Continuation.Set.empty in
  let rec add_to_zone k =
    match Continuation.Map.find_opt k blocks with
    | None -> ()
    | Some block ->
      if not (Continuation.Set.mem k !zone)
      then (
        zone := Continuation.Set.add k !zone;
        List.iter (fun (k, _, _) -> add_to_zone k) (edges block.terminator);
        List.iter add_to_zone
          (Option.value (Continuation.Map.find_opt k children) ~default:[]))
  in
  Continuation.Map.iter
    (fun id block ->
      if
        List.exists
          (fun (bound_pattern, _) ->
            List.exists
              (fun var ->
                Code_id_or_name.Set.mem (Code_id_or_name.var var) returned)
              (bound_vars bound_pattern))
          block.lets
      then add_to_zone id;
      match block.terminator with
      | Apply apply -> (
        match Flambda.Apply.continuation apply with
        | Never_returns -> ()
        | Return k ->
          if
            not
              (Code_id_or_name.Set.is_empty
                 (Code_id_or_name.Set.inter (delivered k) returned))
          then (
            exclave_applies := Continuation.Set.add id !exclave_applies;
            add_to_zone k))
      | Invalid _ | Apply_cont _ | Switch _ -> ())
    blocks;
  let zone = !zone in
  let exclave_applies = Continuation.Set.diff !exclave_applies zone in
  let in_zone k = Continuation.Set.mem k zone in
  if in_zone entry then raise Give_up;
  (* Control must not enter the zone inside a recursive handler that is not in
     the zone. *)
  let rec check_not_in_loop id =
    match Continuation.Map.find_opt id blocks with
    | None -> ()
    | Some block ->
      if block.recursive && not (in_zone id) then raise Give_up;
      Option.iter check_not_in_loop block.parent
  in
  Continuation.Set.iter
    (fun id ->
      Option.iter check_not_in_loop (Continuation.Map.find id blocks).parent)
    zone;
  Continuation.Set.iter check_not_in_loop exclave_applies;
  (* The temporaries. *)
  let temporaries =
    Continuation.Map.fold
      (fun id block temporaries ->
        if in_zone id
        then temporaries
        else
          let temporaries =
            List.fold_left
              (fun temporaries (bound_pattern, _) ->
                List.fold_left
                  (fun temporaries var ->
                    if Escape_solution.is_local_allocation solution var
                    then
                      Code_id_or_name.Set.add (Code_id_or_name.var var)
                        temporaries
                    else temporaries)
                  temporaries (bound_vars bound_pattern))
              temporaries block.lets
          in
          if Continuation.Set.mem id exclave_applies
          then temporaries
          else
            List.fold_left
              (fun temporaries (k, (kind : edge_kind), _) ->
                match kind with
                | Return_of_apply ->
                  Code_id_or_name.Set.union (delivered k) temporaries
                | Jump | Switch_arm -> temporaries)
              temporaries (edges block.terminator))
      blocks Code_id_or_name.Set.empty
  in
  if
    Code_id_or_name.Set.for_all
      (Escape_solution.is_unboxed solution)
      temporaries
  then raise Give_up;
  let check vars =
    List.iter
      (fun var ->
        if
          List.exists
            (fun alloc -> Code_id_or_name.Set.mem alloc temporaries)
            (Escape_solution.local_sources solution var)
        then raise Give_up)
      vars
  in
  Continuation.Map.iter
    (fun id block ->
      if in_zone id
      then (
        check (params id);
        List.iter
          (fun (_, named) -> check (variables_of_named named))
          block.lets;
        check (variables_of_terminator block.terminator))
      else if Continuation.Set.mem id exclave_applies
      then check (variables_of_terminator block.terminator)
      else
        List.iter
          (fun (k, _, args) ->
            if in_zone k || Continuation.equal k return_continuation
            then check (variables_of_simples args))
          (edges block.terminator))
    blocks;
  (* Where the region is closed. *)
  let entries =
    Continuation.Map.fold
      (fun id block entries ->
        List.fold_left
          (fun entries (k, (kind : edge_kind), _) ->
            if not (in_zone k)
            then entries
            else
              let from_zone =
                in_zone id
                ||
                match kind with
                | Return_of_apply -> Continuation.Set.mem id exclave_applies
                | Jump | Switch_arm -> false
              in
              Continuation.Map.update k
                (fun kinds ->
                  Some ((from_zone, kind) :: Option.value kinds ~default:[]))
                entries)
          entries (edges block.terminator))
      blocks Continuation.Map.empty
  in
  let modes =
    Continuation.Set.fold
      (fun k modes ->
        let entries =
          Option.value (Continuation.Map.find_opt k entries) ~default:[]
        in
        let from_zone, from_outside =
          List.partition (fun (from_zone, _) -> from_zone) entries
        in
        let mode =
          match from_zone, from_outside with
          | _, [] -> Interior
          | [], _ :: _ -> Close_on_entry
          | _ :: _, _ :: _ ->
            if
              List.for_all
                (fun (_, (kind : edge_kind)) ->
                  match kind with
                  | Jump -> true
                  | Switch_arm | Return_of_apply -> false)
                from_outside
            then Close_before_jump
            else raise Give_up
        in
        Continuation.Map.add k mode modes)
      zone Continuation.Map.empty
  in
  { modes; exclave_applies; entry }

let compute solution ~continuation_info ~code_id ~return_continuation body =
  match
    compute_exn solution ~continuation_info ~code_id ~return_continuation body
  with
  | plan -> Some plan
  | exception Give_up -> None
