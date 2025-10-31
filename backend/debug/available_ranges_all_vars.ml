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

module ARV = Available_ranges_vars
module ARPV = Available_ranges_phantom_vars
module L = Linear
module V = Backend_var

module Subrange_info = struct
  type t =
    | Non_phantom of
        { reg : Reg.t;
          offset : Stack_reg_offset.t option
        }
    | Phantom of L.phantom_defining_expr
end

module Subrange = struct
  type t =
    | Non_phantom of ARV.Subrange.t
    | Phantom of L.phantom_defining_expr * ARPV.Subrange.t

  let info = function
    | Non_phantom subrange ->
      let reg = ARV.Subrange_info.reg (ARV.Subrange.info subrange) in
      let offset = ARV.Subrange_info.offset (ARV.Subrange.info subrange) in
      Subrange_info.Non_phantom { reg; offset }
    | Phantom (defining_expr, _subrange) -> Subrange_info.Phantom defining_expr

  let start_pos = function
    | Non_phantom subrange -> ARV.Subrange.start_pos subrange
    | Phantom (_defining_expr, subrange) -> ARPV.Subrange.start_pos subrange

  let start_pos_offset = function
    | Non_phantom subrange -> ARV.Subrange.start_pos_offset subrange
    | Phantom (_defining_expr, subrange) ->
      ARPV.Subrange.start_pos_offset subrange

  let end_pos = function
    | Non_phantom subrange -> ARV.Subrange.end_pos subrange
    | Phantom (_defining_expr, subrange) -> ARPV.Subrange.end_pos subrange

  let end_pos_offset = function
    | Non_phantom subrange -> ARV.Subrange.end_pos_offset subrange
    | Phantom (_defining_expr, subrange) ->
      ARPV.Subrange.end_pos_offset subrange
end

module Range_info = struct
  type phantom_defining_expr =
    | Non_phantom
    | Phantom of L.phantom_defining_expr

  type t =
    { provenance : V.Provenance.t option;
      debuginfo : Debuginfo.t;
      is_parameter : Is_parameter.t;
      phantom_defining_expr : phantom_defining_expr
    }

  let provenance t = t.provenance

  let debuginfo t = t.debuginfo

  let is_parameter t = t.is_parameter

  let phantom_defining_expr t = t.phantom_defining_expr
end

module Range = struct
  type t =
    | Non_phantom of ARV.Range.t
    | Phantom of ARPV.Range.t

  let info = function
    | Non_phantom range ->
      let range_info = ARV.Range.info range in
      { Range_info.provenance = ARV.Range_info.provenance range_info;
        debuginfo = Debuginfo.none;
        (* TODO: Get from fundecl *)
        is_parameter = ARV.Range_info.is_parameter range_info;
        phantom_defining_expr = Non_phantom
      }
    | Phantom range ->
      let range_info = ARPV.Range.info range in
      { Range_info.provenance = ARPV.Range_info.provenance range_info;
        debuginfo = Debuginfo.none;
        (* TODO: Get from fundecl *)
        is_parameter = ARPV.Range_info.is_parameter range_info;
        phantom_defining_expr =
          Phantom (ARPV.Range_info.defining_expr range_info)
      }

  let extremities = function
    | Non_phantom range -> (
      match ARV.Range.estimate_lowest_address range with
      | None -> None
      | Some (label, _offset) ->
        (* Find the last label by folding through subranges *)
        let last_label =
          ARV.Range.fold range ~init:label ~f:(fun _acc subrange ->
              ARV.Subrange.end_pos subrange)
        in
        Some (label, last_label))
    | Phantom range -> (
      match ARPV.Range.estimate_lowest_address range with
      | None -> None
      | Some (label, _offset) ->
        let last_label =
          ARPV.Range.fold range ~init:label ~f:(fun _acc subrange ->
              ARPV.Subrange.end_pos subrange)
        in
        Some (label, last_label))

  let fold t ~init ~f =
    match t with
    | Non_phantom range ->
      ARV.Range.fold range ~init ~f:(fun acc subrange ->
          f acc (Subrange.Non_phantom subrange))
    | Phantom range ->
      let defining_expr =
        ARPV.Range_info.defining_expr (ARPV.Range.info range)
      in
      ARPV.Range.fold range ~init ~f:(fun acc subrange ->
          f acc (Subrange.Phantom (defining_expr, subrange)))
end

type t =
  { non_phantom : ARV.t;
    phantom : ARPV.t (* TODO: Add static phantom variables *)
  }

let empty = { non_phantom = ARV.empty; phantom = ARPV.empty }

let create ~available_ranges_vars ~available_ranges_phantom_vars
    (_fundecl : L.fundecl) =
  { non_phantom = available_ranges_vars;
    phantom = available_ranges_phantom_vars
  }

let iter t ~f =
  ARV.iter t.non_phantom ~f:(fun var range -> f var (Range.Non_phantom range));
  ARPV.iter t.phantom ~f:(fun var range -> f var (Range.Phantom range))

let fold t ~init ~f =
  let acc =
    ARV.fold t.non_phantom ~init ~f:(fun acc var range ->
        f acc var (Range.Non_phantom range))
  in
  ARPV.fold t.phantom ~init:acc ~f:(fun acc var range ->
      f acc var (Range.Phantom range))
