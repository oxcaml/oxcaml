(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Liam Stevenson, Jane Street, New York                 *)
(*                                                                        *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t =
  | Null
  | False
  | True
  | String of string
  | Number of string
  | Object of (string * t) list
  | Array of t list

let int i = Number (Int.to_string i)

let float f =
  if Float.is_finite f
  then
    (* Use %.17g instead of string_of_float to avoid invalid JSON output.
       [string_of_float] can produce "1." for whole numbers, which is not valid
       JSON. %.17g provides full double precision and produces valid JSON
       numbers. *)
    Number (Format.asprintf "%.17g" f)
  else
    (* JSON can't represent nan or infinity, so use null like JavaScript's
       [JSON.stringify] does. *)
    Null

let option = function None -> Null | Some t -> t

module Print_mode = struct
  type t =
    | Pretty
    | Compact
end

(* [escape_unicode] is based on [Bytes.unsafe_escape], which is used for the
   format specifier [%S]. It is adjusted to output unicode escape chacarcters
   \u00HH instead of the usual OCaml character literals \DDD. Adds quotes around
   the input string. *)
let escape_unicode str =
  let s = Bytes.of_string str in
  let n = ref 2 in
  (* for the quotes *)
  for i = 0 to Bytes.length s - 1 do
    n
      := !n
         +
         match Bytes.unsafe_get s i with
         | '\"' | '\\' | '\n' | '\t' | '\r' | '\b' -> 2
         | ' ' .. '~' -> 1
         | _ -> 6
  done;
  begin
    let s' = Bytes.create !n in
    Bytes.unsafe_set s' 0 '\"';
    n := 1;
    for i = 0 to Bytes.length s - 1 do
      begin match Bytes.unsafe_get s i with
      | ('\"' | '\\') as c ->
        Bytes.unsafe_set s' !n '\\';
        incr n;
        Bytes.unsafe_set s' !n c
      | '\n' ->
        Bytes.unsafe_set s' !n '\\';
        incr n;
        Bytes.unsafe_set s' !n 'n'
      | '\t' ->
        Bytes.unsafe_set s' !n '\\';
        incr n;
        Bytes.unsafe_set s' !n 't'
      | '\r' ->
        Bytes.unsafe_set s' !n '\\';
        incr n;
        Bytes.unsafe_set s' !n 'r'
      | '\b' ->
        Bytes.unsafe_set s' !n '\\';
        incr n;
        Bytes.unsafe_set s' !n 'b'
      | ' ' .. '~' as c -> Bytes.unsafe_set s' !n c
      | c ->
        let a = Char.code c in
        let hex_char n =
          if n < 10 then Char.chr (48 + n) (* '0'-'9' *) else Char.chr (55 + n)
          (* 'A'-'F' *)
        in
        Bytes.unsafe_set s' !n '\\';
        incr n;
        Bytes.unsafe_set s' !n 'u';
        incr n;
        Bytes.unsafe_set s' !n '0';
        incr n;
        Bytes.unsafe_set s' !n '0';
        incr n;
        Bytes.unsafe_set s' !n (hex_char (a / 16));
        incr n;
        Bytes.unsafe_set s' !n (hex_char (a mod 16))
      end;
      incr n
    done;
    Bytes.unsafe_set s' !n '\"';
    String.of_bytes s'
  end

let print_quoted_string ppf s = Format.pp_print_string ppf (escape_unicode s)

let indent_size = 2

let print_break_if_pretty ppf ~(mode : Print_mode.t) ~spaces ~offset =
  match mode with
  | Compact -> ()
  | Pretty -> Format.pp_print_break ppf spaces offset

let print_in_box_if_pretty ~(mode : Print_mode.t) print ppf x =
  match mode with
  | Compact -> print ppf x
  | Pretty ->
    Format.pp_open_hvbox ppf 0;
    print ppf x;
    Format.pp_close_box ppf ()

let print_container ~mode ~opening ~closing ~print_item ppf items =
  Format.pp_print_char ppf opening;
  if not (List.is_empty items)
  then (
    let pp_sep ppf () =
      Format.pp_print_char ppf ',';
      print_break_if_pretty ppf ~mode ~spaces:1 ~offset:indent_size
    in
    print_break_if_pretty ppf ~mode ~spaces:0 ~offset:indent_size;
    Format.pp_print_list ~pp_sep
      (fun ppf item ->
        (* We put a box around items so that if the item contains a container,
           the container's contents are aligned with the start of the item. *)
        print_in_box_if_pretty ~mode print_item ppf item)
      ppf items;
    print_break_if_pretty ppf ~mode ~spaces:0 ~offset:0);
  Format.pp_print_char ppf closing

let rec print_value ppf ~mode (t : t) =
  match t with
  | Null -> Format.pp_print_string ppf "null"
  | False -> Format.pp_print_string ppf "false"
  | True -> Format.pp_print_string ppf "true"
  | String s -> print_quoted_string ppf s
  | Number n -> Format.pp_print_string ppf n
  | Array items ->
    print_container ~mode ~opening:'[' ~closing:']'
      ~print_item:(print_value ~mode) ppf items
  | Object fields ->
    print_container ~mode ~opening:'{' ~closing:'}'
      ~print_item:(print_field ~mode) ppf fields

and print_field ppf ~(mode : Print_mode.t) (name, value) =
  print_quoted_string ppf name;
  Format.pp_print_char ppf ':';
  (match mode with Pretty -> Format.pp_print_char ppf ' ' | Compact -> ());
  print_value ppf ~mode value

let print ~mode ppf t = print_in_box_if_pretty ~mode (print_value ~mode) ppf t

let to_string_pretty t = Format.asprintf "%a" (print ~mode:Pretty) t
