(******************************************************************************
 *                                 Sexplib0                                   *
 *                                                                            *
 * Copyright (c) 2005--2026 Jane Street Group, LLC                            *
 *   <opensource-contacts@janestreet.com>                                     *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2023 OCamlPro                                                *
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

(* The pretty-printing primitives in this file have been adapted from
   https://github.com/janestreet/sexplib0/blob/638f99b9ffba6519d6a47f91507e15ce79cc0099/src/sexp.ml *)

type t =
  | Atom of string
  | List of t list

open Format

let must_escape str =
  let len = String.length str in
  len = 0
  ||
  let rec loop str ix =
    match str.[ix] with
    | '"' | '(' | ')' | ';' | '\\' -> true
    | '|' ->
      ix > 0
      &&
      let next = ix - 1 in
      Char.equal str.[next] '#' || loop str next
    | '#' ->
      ix > 0
      &&
      let next = ix - 1 in
      Char.equal str.[next] '|' || loop str next
    | '\000' .. '\032' | '\127' .. '\255' -> true
    | _ -> ix > 0 && loop str (ix - 1)
  in
  loop str (len - 1)

let length_of_escaped_string s =
  let n = ref 0 in
  for i = 0 to String.length s - 1 do
    n
      := !n
         +
         match String.unsafe_get s i with
         | '\"' | '\\' | '\n' | '\t' | '\r' | '\b' -> 2
         | ' ' .. '~' -> 1
         | _ -> 4
  done;
  !n

let escaped_bytes s bytes =
  let n = ref 0 in
  n := 0;
  for i = 0 to String.length s - 1 do
    (match String.unsafe_get s i with
    | ('\"' | '\\') as c ->
      Bytes.unsafe_set bytes !n '\\';
      incr n;
      Bytes.unsafe_set bytes !n c
    | '\n' ->
      Bytes.unsafe_set bytes !n '\\';
      incr n;
      Bytes.unsafe_set bytes !n 'n'
    | '\t' ->
      Bytes.unsafe_set bytes !n '\\';
      incr n;
      Bytes.unsafe_set bytes !n 't'
    | '\r' ->
      Bytes.unsafe_set bytes !n '\\';
      incr n;
      Bytes.unsafe_set bytes !n 'r'
    | '\b' ->
      Bytes.unsafe_set bytes !n '\\';
      incr n;
      Bytes.unsafe_set bytes !n 'b'
    | ' ' .. '~' as c -> Bytes.unsafe_set bytes !n c
    | c ->
      let a = Char.code c in
      Bytes.unsafe_set bytes !n '\\';
      incr n;
      Bytes.unsafe_set bytes !n (Char.chr (48 + (a / 100)));
      incr n;
      Bytes.unsafe_set bytes !n (Char.chr (48 + (a / 10 mod 10)));
      incr n;
      Bytes.unsafe_set bytes !n (Char.chr (48 + (a mod 10))));
    incr n
  done

let index_of_newline str start = String.index_from_opt str start '\n'

let is_one_line str =
  match index_of_newline str 0 with
  | None -> true
  | Some index -> index + 1 = String.length str

let escaped s =
  let length_of_escaped_string = length_of_escaped_string s in
  if length_of_escaped_string = String.length s
  then s
  else
    let bytes = Bytes.create length_of_escaped_string in
    escaped_bytes s bytes;
    Bytes.unsafe_to_string bytes

let esc_str str =
  let estr = escaped str in
  let elen = String.length estr in
  let res = Bytes.create (elen + 2) in
  Bytes.unsafe_blit_string estr 0 res 1 elen;
  Bytes.unsafe_set res 0 '"';
  Bytes.unsafe_set res (elen + 1) '"';
  Bytes.unsafe_to_string res

let get_substring str index end_pos_opt =
  let end_pos =
    match end_pos_opt with None -> String.length str | Some end_pos -> end_pos
  in
  String.sub str index (end_pos - index)

let rec print ppf = function
  | Atom str ->
    if not (must_escape str)
    then pp_print_string ppf str
    else if is_one_line str
    then pp_print_string ppf (esc_str str)
    else
      let rec loop index =
        let next_newline = index_of_newline str index in
        let next_line = get_substring str index next_newline in
        pp_print_string ppf (escaped next_line);
        match next_newline with
        | None -> ()
        | Some newline_index ->
          pp_print_string ppf "\\";
          pp_force_newline ppf ();
          pp_print_string ppf "\\n";
          loop (newline_index + 1)
      in
      pp_open_box ppf 0;
      (* the leading space is to line up the lines *)
      pp_print_string ppf " \"";
      loop 0;
      pp_print_string ppf "\"";
      pp_close_box ppf ()
  | List [] -> pp_print_string ppf "()"
  | List (h :: t) ->
    pp_open_box ppf 1;
    pp_print_string ppf "(";
    print ppf h;
    print_rest ppf t

and print_rest ppf = function
  | [] ->
    pp_print_string ppf ")";
    pp_close_box ppf ()
  | sexp :: sexps ->
    pp_print_space ppf ();
    print ppf sexp;
    print_rest ppf sexps
