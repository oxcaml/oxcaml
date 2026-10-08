let version = "%%VERSION%%"

exception Json_error of string

let json_error s = raise (Json_error s)

exception End_of_array
exception End_of_object
exception End_of_input

type lexer_state = {
  buf : Buffer.t; (* Buffer used to accumulate substrings *)
  mutable lnum : int; (* Current line number (starting from 1) *)
  mutable bol : int;
      (* Absolute position of the first character of the current line
         (starting from 0) *)
  mutable fname : string option; (* Name describing the input file *)
}

module Lexer_state = struct
  type t = lexer_state = {
    buf : Buffer.t;
    mutable lnum : int;
    mutable bol : int;
    mutable fname : string option;
  }
end

let init_lexer ?buf ?fname ?(lnum = 1) () =
  let buf = match buf with None -> Buffer.create 256 | Some buf -> buf in
  { buf; lnum; bol = 0; fname }
