(******************************************************************************
 *                                  OxCaml                                    *
 *                       Mark Shinwell, Jane Street                           *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2014--2025 Jane Street Group LLC                             *
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

module L = Linear
module V = Backend_var

module Phantom_vars = struct
  module Key = struct
    type t = V.t

    type key = t

    module Raw_set = V.Set

    module Set = struct
      type t =
        | Ok of Raw_set.t
        | Unreachable

      let of_list keys = Ok (Raw_set.of_list keys)

      let union s1 s2 =
        match s1, s2 with
        | Unreachable, x | x, Unreachable -> x
        | Ok s1, Ok s2 -> Ok (Raw_set.union s1 s2)

      let inter s1 s2 =
        match s1, s2 with
        | Unreachable, _ | _, Unreachable -> Unreachable
        | Ok s1, Ok s2 -> Ok (Raw_set.inter s1 s2)

      let diff s1 s2 =
        match s1, s2 with
        | Unreachable, _ -> Unreachable
        | s1, Unreachable -> s1
        | Ok s1, Ok s2 -> Ok (Raw_set.diff s1 s2)

      let fold f t init =
        match t with Unreachable -> init | Ok s -> Raw_set.fold f s init

      let print ppf = function
        | Unreachable -> Format.fprintf ppf "unreachable"
        | Ok s -> V.Set.print ppf s
    end

    module Map = V.Map

    let print = V.print

    let all_parents _t = []
  end

  module Index = V

  module Subrange_state : Compute_ranges_intf.S_subrange_state = struct
    type t = unit

    let create () = ()

    let advance_over_instruction _ _ = ()
  end

  module Subrange_info :
    Compute_ranges_intf.S_subrange_info
      with type key := Key.t
      with type subrange_state := Subrange_state.t = struct
    type t = unit

    let create _var _subrange_state ~fun_contains_calls:_ ~fun_num_stack_slots:_
        =
      ()

    let print _ppf () = ()
  end

  module Range_info : sig
    include
      Compute_ranges_intf.S_range_info
        with type key := Key.t
        with type index := Index.t

    val provenance : t -> V.Provenance.t option

    val is_parameter : t -> Is_parameter.t

    val defining_expr : t -> L.phantom_defining_expr
  end = struct
    type t =
      { provenance : V.Provenance.t option;
        is_parameter : Is_parameter.t;
        defining_expr : L.phantom_defining_expr
      }

    let create (fundecl : L.fundecl) var ~start_insn:_ =
      match V.Map.find var fundecl.fun_phantom_lets with
      | exception Not_found ->
        Misc.fatal_errorf
          "Available_ranges_phantom_vars.Range_info.create: phantom variable \
           occurs in [phantom_available_before] but not in [fun_phantom_lets]: \
           %a"
          V.print var
      | provenance, defining_expr ->
        (* Static phantom variables are sent via a different path (see
           [Available_ranges_all_vars]) since they are always available. *)
        (* TODO: implement is_static check when Backend_var.Provenance supports
           it *)
        let is_parameter = Is_parameter.local in
        let t = { provenance; is_parameter; defining_expr } in
        Some (var, t)

    let provenance t = t.provenance

    let is_parameter t = t.is_parameter

    let defining_expr t = t.defining_expr

    let print ppf t =
      Format.fprintf ppf "phantom_var(is_param=%a)" Is_parameter.print
        t.is_parameter
  end

  let available_before (insn : L.instruction) =
    match insn.phantom_available_before with
    | None -> None
    | Some set -> Some (Key.Set.of_list (V.Set.elements set))

  let available_across insn =
    (* Phantom variable availability never changes during the execution of a
       [Linear] instruction. *)
    available_before insn
end

module Impl = Compute_ranges.Make (Phantom_vars)
include Impl

let create fundecl =
  Format.eprintf "ARPV.create called for function %s@." fundecl.L.fun_name;
  Format.eprintf "ARPV: fun_phantom_lets has %d entries@."
    (V.Map.cardinal fundecl.L.fun_phantom_lets);
  V.Map.iter
    (fun v (prov, def_expr) ->
      Format.eprintf "  - var: %a, has_provenance: %b@." V.print v
        (Option.is_some prov))
    fundecl.L.fun_phantom_lets;
  let result = Impl.create fundecl in
  Format.eprintf "ARPV.create done@.";
  result

module Key = Phantom_vars.Key
module Subrange_state = Phantom_vars.Subrange_state
module Subrange_info = Phantom_vars.Subrange_info
module Range_info = Phantom_vars.Range_info
