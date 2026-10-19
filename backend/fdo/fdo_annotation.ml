(******************************************************************************
 *                                  OxCaml                                    *
 *                               Jane Street                                  *
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

type t = Fdo_counter.t list

let empty = []

let is_empty t = List.is_empty t

let of_list counters = Fdo_counter.add_all [] counters

let to_list t = t

let combine t1 t2 = Fdo_counter.add_all t1 t2

let equal t1 t2 = List.equal Fdo_counter.equal t1 t2

let print_raw =
  Format.pp_print_list ~pp_sep:Format.pp_print_space (fun ppf counter ->
      Format.pp_print_string ppf (Fdo_counter.to_string counter))

let print ppf t = Format.fprintf ppf "@[<hov 1>(%a)@]" print_raw t

module With_body_hash = struct
  type annotation = t

  type t =
    { annotation : annotation;
      body_hash : Fdo_counter.Function_body_hash.t option
    }

  let create ~annotation ~body_hash = { annotation; body_hash }

  let annotation { annotation; _ } = annotation

  let body_hash { body_hash; _ } = body_hash

  let with_annotation t annotation = { t with annotation }

  let with_body_hash t body_hash = { t with body_hash = Some body_hash }

  let empty = { annotation = empty; body_hash = None }

  let is_empty { annotation; body_hash } =
    is_empty annotation && Option.is_none body_hash

  let print ppf { annotation; body_hash } =
    Format.fprintf ppf "@[<hov 1>(%a%a)@]" print_raw annotation
      (Format.pp_print_option
         ~none:(fun _ () -> ())
         (fun ppf body_hash ->
           Format.fprintf ppf " body_hash:%a"
             Fdo_counter.Function_body_hash.print body_hash))
      body_hash
end
