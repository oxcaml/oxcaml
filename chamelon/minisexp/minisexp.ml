(******************************************************************************
 *                                 Chamelon                                   *
 *                         Milla Valnet, OCamlPro                             *
 *                         Basile Clément, OCamlPro                           *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2026 OCamlPro                                                *
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

include Sexp

type sexp = t

(* [extract ?default name fields] finds the first S-expression of the form [List
   [Atom name; value]] and returns the corresponding [value], as well as the
   updated list without that entry.

   If no such entry exists, it returns [default] (or [List []] if not provided).

   This is intended to parse record S-expressions. *)
let extract ?(default = List []) field xs =
  let rec aux acc = function
    | [] -> default, List.rev acc
    | List [Atom name; value] :: rest when String.equal name field ->
      value, List.rev_append acc rest
    | other :: rest -> aux (other :: acc) rest
  in
  aux [] xs

module Fields = struct
  type _ t =
    | [] : unit t
    | ( :: ) : (string * (Sexp.t -> 'a)) * 'b t -> ('a * 'b) t

  let rec parse_list : type a. a t -> Sexp.t list -> a =
   fun fields sexps ->
    match fields with
    | [] -> (
      match sexps with
      | [] -> ()
      | _ :: _ ->
        failwith (Format.asprintf "extra fields: %a" Sexp.print (List sexps)))
    | (field_name, parse_field) :: fields ->
      let field_sexp, sexps = extract field_name sexps in
      let field = parse_field field_sexp in
      let rest = parse_list fields sexps in
      field, rest

  let parse fields = function
    | List xs -> parse_list fields xs
    | sexp ->
      invalid_arg (Format.asprintf "parse_named_tuple: %a@." Sexp.print sexp)
end

let variant_of_sexp ?default fields = function
  | List (Atom name :: args) as sexp -> (
    match List.assoc name fields with
    | field_of_sexp -> field_of_sexp args
    | exception Not_found -> (
      match default with
      | None ->
        invalid_arg (Format.asprintf "variant_of_sexp: %a" Sexp.print sexp)
      | Some default -> default name args))
  | _sexp -> invalid_arg "variant_of_sexp: not a variant"

let one_arg of_sexp = function
  | [sexp] -> of_sexp sexp
  | _ -> invalid_arg "one_arg"

let pair of_sexp1 of_sexp2 = function
  | [sexp1; sexp2] -> of_sexp1 sexp1, of_sexp2 sexp2
  | _ -> invalid_arg "pair"

let ( let+ ) m f sexp = f (m sexp)

let string_of_sexp = function
  | Atom s -> s
  | sexp -> invalid_arg (Format.asprintf "string_of_sexp: %a" Sexp.print sexp)

let bool_of_sexp = function
  | Atom "true" -> true
  | Atom "false" -> false
  | sexp -> invalid_arg (Format.asprintf "bool_of_sexp: %a" Sexp.print sexp)

let list_of_sexp of_sexp = function
  | List xs -> List.map of_sexp xs
  | sexp -> invalid_arg (Format.asprintf "list_of_sexp: %a" Sexp.print sexp)

let from_string str =
  let lexbuf = Lexing.from_string str in
  let buf = Buffer.create 1024 in
  Sexp_parser.sexps (Sexp_lexer.main ~buf) lexbuf
