(******************************************************************************
 *                                  OxCaml                                    *
 *                        Xavier Clerc, Jane Street                           *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2025 Jane Street Group LLC                                   *
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

open Iarray_shim

type path = int list

module Singleton_mixed_block_element = struct
  type 'a t =
    | Value of Lambda.value_kind
    | Float_boxed of 'a
    | Float64
    | Float32
    | Bits8
    | Bits16
    | Bits32
    | Bits64
    | Vec128
    | Vec256
    | Vec512
    | Word
    | Untagged_immediate

  let print print_locality ppf t =
    match t with
    | Value vk ->
      Format.fprintf ppf "@[<hov 1>(Value %a)@]" Printlambda.value_kind vk
    | Float_boxed locality ->
      Format.fprintf ppf "@[<hov 1>(Float_boxed %a)@]" print_locality locality
    | Float64 -> Format.fprintf ppf "Float64"
    | Float32 -> Format.fprintf ppf "Float32"
    | Bits8 -> Format.fprintf ppf "Bits8"
    | Bits16 -> Format.fprintf ppf "Bits16"
    | Bits32 -> Format.fprintf ppf "Bits32"
    | Bits64 -> Format.fprintf ppf "Bits64"
    | Vec128 -> Format.fprintf ppf "Vec128"
    | Vec256 -> Format.fprintf ppf "Vec256"
    | Vec512 -> Format.fprintf ppf "Vec512"
    | Word -> Format.fprintf ppf "Word"
    | Untagged_immediate -> Format.fprintf ppf "Untagged_immediate"
end

type 'a shape = 'a Singleton_mixed_block_element.t iarray

type 'a shape_with_paths = ('a Singleton_mixed_block_element.t * path) iarray

type 'a tree =
  | Leaf of
      { element : 'a Singleton_mixed_block_element.t;
        new_index : int
      }
  | Node of { children : 'a tree iarray }

type 'a t =
  { prefix : 'a shape;
    suffix : 'a shape;
    flattened_reordered_shape : 'a shape_with_paths;
    forest : 'a tree iarray;
    print_locality : Format.formatter -> 'a -> unit
  }

let value_prefix t = t.prefix

let flat_suffix t = t.suffix

let value_prefix_len t = Iarray.length t.prefix

let flat_suffix_len t = Iarray.length t.suffix

let flattened_reordered_shape t = Iarray.map fst t.flattened_reordered_shape

let print_indentation ppf k =
  for _ = 1 to k do
    Format.fprintf ppf "  "
  done

let print_element print_locality ppf element new_index =
  Format.fprintf ppf "%a (new_index=%d)\n%!"
    (Singleton_mixed_block_element.print print_locality)
    element new_index

let rec print_tree print_locality ~indent ~index ppf tree =
  match tree with
  | Leaf { element; new_index } ->
    print_indentation ppf indent;
    Format.fprintf ppf "[%d] " index;
    print_element print_locality ppf element new_index
  | Node { children } ->
    print_indentation ppf indent;
    Format.fprintf ppf "[%d]\n%!" index;
    print_trees print_locality ~indent:(succ indent) ppf children

and print_trees print_locality ~indent ppf trees =
  Iarray.iteri
    (fun index tree -> print_tree print_locality ~indent ~index ppf tree)
    trees

let print ppf { forest; print_locality; _ } =
  Format.fprintf ppf "forest:\n%!";
  print_trees print_locality ~indent:0 ppf forest

let rec flatten_tree_array arr =
  Iarray.to_list arr
  |> List.concat_map (fun tree ->
         match tree with
         | Leaf { new_index; _ } -> [new_index]
         | Node { children } -> flatten_tree_array children)

let new_indexes_to_old_indexes t =
  let old_indexes_to_new_indexes =
    flatten_tree_array t.forest |> Iarray.of_list
  in
  let result = Array.make (Iarray.length old_indexes_to_new_indexes) (-1) in
  Iarray.iteri
    (fun old_index new_index -> result.(new_index) <- old_index)
    old_indexes_to_new_indexes;
  result |> Iarray.of_array

let new_index_to_old_path t new_index =
  snd t.flattened_reordered_shape.:(new_index)

let new_block_length t = Iarray.length t.flattened_reordered_shape

let lookup_path_producing_new_indexes ({ forest; _ } as t) path =
  let original_path = path in
  match path with
  | [] -> Misc.fatal_errorf "No path provided:@ %a" print t
  | index :: path ->
    let tree = forest.:(index) in
    let rec lookup_path' path tree =
      match path, tree with
      | [], Leaf { new_index; _ } -> [new_index]
      | index :: path, Node { children; _ } ->
        lookup_path' path children.:(index)
      | [], Node { children } -> flatten_tree_array children
      | _ :: _, Leaf _ ->
        Misc.fatal_errorf "Invalid path:@ %a@ shape: %a"
          (Format.pp_print_list Format.pp_print_int)
          original_path print t
    in
    lookup_path' path tree

type ('a, 'b) singleton_or_product =
  | Singleton of 'a
  | Product of 'b

let singleton_or_product_of_mixed_block_element
    (elt : _ Lambda.mixed_block_element) :
    (_ Singleton_mixed_block_element.t, _) singleton_or_product =
  match elt with
  | Value vk -> Singleton (Value vk)
  | Float_boxed locality -> Singleton (Float_boxed locality)
  | Float64 -> Singleton Float64
  | Float32 -> Singleton Float32
  | Bits8 -> Singleton Bits8
  | Bits16 -> Singleton Bits16
  | Bits32 -> Singleton Bits32
  | Bits64 -> Singleton Bits64
  | Vec128 -> Singleton Vec128
  | Vec256 -> Singleton Vec256
  | Vec512 -> Singleton Vec512
  | Word -> Singleton Word
  | Untagged_immediate -> Singleton Untagged_immediate
  | Product sub_elements -> Product sub_elements

(* CR-soon xclerc for xclerc: it is probably quite inefficient to map/concat
   repeatedly. *)
let rec flatten_one :
    int -> 'a Lambda.mixed_block_element -> 'a shape_with_paths =
 fun index element ->
  match singleton_or_product_of_mixed_block_element element with
  | Singleton element -> Iarray.make 1 (element, [index])
  | Product sub_elements ->
    flatten_list sub_elements
    |> Iarray.map (fun (sub_element, path) -> sub_element, index :: path)

and flatten_list : 'a Lambda.mixed_block_element iarray -> 'a shape_with_paths =
 fun sub_elements ->
  Iarray.mapi flatten_one sub_elements |> Misc.Stdlib.Iarray.concat_iarrays

let rec build_tree_one :
    (path, int) Hashtbl.t ->
    path ->
    int ->
    'a Lambda.mixed_block_element ->
    'a tree =
 fun old_path_to_new_index path index element ->
  match singleton_or_product_of_mixed_block_element element with
  | Singleton element -> (
    let path = List.rev (index :: path) in
    match Hashtbl.find_opt old_path_to_new_index path with
    | None ->
      Misc.fatal_errorf "build_tree_one: path=%s\n%!"
        (String.concat ", " (List.map string_of_int path))
    | Some new_index -> Leaf { element; new_index })
  | Product sub_elements ->
    let children =
      build_tree_list old_path_to_new_index (index :: path) sub_elements
    in
    Node { children }

and build_tree_list :
    (path, int) Hashtbl.t ->
    path ->
    'a Lambda.mixed_block_element iarray ->
    'a tree iarray =
 fun old_path_to_new_index path sub_elements ->
  Iarray.mapi
    (fun i sub_element ->
      build_tree_one old_path_to_new_index path i sub_element)
    sub_elements

let of_mixed_block_elements ~print_locality
    (original_shape : 'a Lambda.mixed_block_element iarray) : 'a t =
  let flattened_shape_with_paths = flatten_list original_shape in
  let prefix = ref [] in
  let suffix = ref [] in
  for idx = Iarray.length flattened_shape_with_paths - 1 downto 0 do
    let elem, path = flattened_shape_with_paths.:(idx) in
    let is_value =
      match elem with
      | Value _ -> true
      | Float_boxed _ | Float64 | Float32 | Bits8 | Bits16 | Bits32 | Bits64
      | Vec128 | Vec256 | Vec512 | Word | Untagged_immediate ->
        false
    in
    if is_value
    then prefix := (elem, path) :: !prefix
    else suffix := (elem, path) :: !suffix
  done;
  let prefix = Iarray.of_list !prefix in
  let suffix = Iarray.of_list !suffix in
  let flattened_reordered_shape = Iarray.append prefix suffix in
  let new_index_to_old_path =
    Array.make (Iarray.length flattened_reordered_shape) []
  in
  let old_path_to_new_index = Hashtbl.create (Iarray.length original_shape) in
  Iarray.iteri
    (fun new_index (_elem, old_path) ->
      new_index_to_old_path.(new_index) <- old_path;
      Hashtbl.replace old_path_to_new_index old_path new_index)
    flattened_reordered_shape;
  let forest = build_tree_list old_path_to_new_index [] original_shape in
  { prefix = Iarray.map fst prefix;
    suffix = Iarray.map fst suffix;
    flattened_reordered_shape;
    forest;
    print_locality
  }
