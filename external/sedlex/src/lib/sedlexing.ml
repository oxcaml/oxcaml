(* The package sedlex is released under the terms of an MIT-like license. *)
(* See the attached LICENSE file.                                         *)
(* Copyright 2005, 2013 by Alain Frisch and LexiFi.                       *)

exception InvalidCodepoint of int
exception MalFormed

module Uchar = struct
  (* This for compatibility with ocaml < 4.14.0 *)
  let utf_8_byte_length u =
    match Uchar.to_int u with
      | u when u < 0 -> assert false
      | u when u <= 0x007F -> 1
      | u when u <= 0x07FF -> 2
      | u when u <= 0xFFFF -> 3
      | u when u <= 0x10FFFF -> 4
      | _ -> assert false

  let utf_16_byte_length u =
    match Uchar.to_int u with
      | u when u < 0 -> assert false
      | u when u <= 0xFFFF -> 2
      | u when u <= 0x10FFFF -> 4
      | _ -> assert false

  let () =
    ignore utf_8_byte_length;
    ignore utf_16_byte_length

  include Uchar

  let of_int x =
    if Uchar.is_valid x then Uchar.unsafe_of_int x else raise MalFormed
end

(* shadow polymorphic equal *)
let ( = ) (a : int) b = a = b
let ( >>| ) o f = match o with Some x -> Some (f x) | None -> None

(* Absolute position from the beginning of the stream *)
type apos = int

type lexbuf = {
  refill : Uchar.t array -> int -> int -> int;
  bytes_per_char : Uchar.t -> int;
  mutable buf : Uchar.t array;
  mutable len : int;
  (* Number of meaningful uchar in buffer *)
  mutable offset : apos;
  (* Number of meaningful bytes in buffer *)
  mutable bytes_offset : apos;
  (* Position of the first uchar in buffer
      in the input stream *)
  mutable pos : int;
  (* Position of the first byte in buffer
      in the input stream *)
  mutable bytes_pos : int;
  (* Position of the beginning of the line in the buffer, in uchar *)
  mutable curr_bol : int;
  (* Position of the beginning of the line in the buffer, in bytes *)
  mutable curr_bytes_bol : int;
  (* Index of the current line in the input stream. *)
  mutable curr_line : int;
  (* starting position, in uchar. *)
  mutable start_pos : int;
  (* starting position, in bytes. *)
  mutable start_bytes_pos : int;
  (* First uchar we need to keep visible *)
  mutable start_bol : int;
  (* First byte we need to keep visible *)
  mutable start_bytes_bol : int;
  (* start from 1 *)
  mutable start_line : int;
  mutable marked_pos : int;
  mutable marked_bytes_pos : int;
  mutable marked_bol : int;
  mutable marked_bytes_bol : int;
  mutable marked_line : int;
  mutable marked_val : int;
  mutable filename : string;
  mutable finished : bool;
}

let chunk_size = 512

let empty_lexbuf bytes_per_char =
  {
    refill = (fun _ _ _ -> assert false);
    bytes_per_char;
    buf = [||];
    len = 0;
    offset = 0;
    bytes_offset = 0;
    pos = 0;
    bytes_pos = 0;
    curr_bol = 0;
    curr_bytes_bol = 0;
    curr_line = 1;
    start_pos = 0;
    start_bytes_pos = 0;
    start_bol = 0;
    start_bytes_bol = 0;
    start_line = 0;
    marked_pos = 0;
    marked_bytes_pos = 0;
    marked_bol = 0;
    marked_bytes_bol = 0;
    marked_line = 0;
    marked_val = 0;
    filename = "";
    finished = false;
  }

let dummy_uchar = Uchar.of_int 0
let nl_uchar = Uchar.of_int 10

let create ?(bytes_per_char = fun _ -> 1) refill =
  {
    (empty_lexbuf bytes_per_char) with
    refill;
    buf = Array.make chunk_size dummy_uchar;
  }

let set_position ?bytes_position lexbuf position =
  lexbuf.offset <- position.Lexing.pos_cnum - lexbuf.pos;
  lexbuf.curr_bol <- position.Lexing.pos_bol;
  lexbuf.curr_line <- position.Lexing.pos_lnum;
  let bytes_position = Option.value ~default:position bytes_position in
  lexbuf.bytes_offset <- bytes_position.Lexing.pos_cnum - lexbuf.bytes_pos;
  lexbuf.curr_bytes_bol <- bytes_position.Lexing.pos_bol

let set_filename lexbuf fname = lexbuf.filename <- fname

let from_gen ?bytes_per_char gen =
  let malformed = ref false in
  let refill buf pos len =
    let rec loop i =
      if !malformed then raise MalFormed;
      if i >= len then len
      else (
        match gen () with
          | Some c ->
              buf.(pos + i) <- c;
              loop (i + 1)
          | None -> i
          | exception MalFormed when i <> 0 ->
              malformed := true;
              i)
    in
    loop 0
  in
  create ?bytes_per_char refill

let from_int_array ?bytes_per_char a =
  from_gen ?bytes_per_char
    (Gen.init ~limit:(Array.length a) (fun i -> Uchar.of_int a.(i)))

let from_uchar_array ?(bytes_per_char = fun _ -> 1) a =
  let len = Array.length a in
  {
    (empty_lexbuf bytes_per_char) with
    buf = Array.init len (fun i -> a.(i));
    len;
    finished = true;
  }

let refill lexbuf =
  if lexbuf.len + chunk_size > Array.length lexbuf.buf then begin
    let s = lexbuf.start_pos in
    let s_bytes = lexbuf.start_bytes_pos in
    let ls = lexbuf.len - s in
    if ls + chunk_size <= Array.length lexbuf.buf then
      Array.blit lexbuf.buf s lexbuf.buf 0 ls
    else begin
      let newlen = (Array.length lexbuf.buf + chunk_size) * 2 in
      let newbuf = Array.make newlen dummy_uchar in
      Array.blit lexbuf.buf s newbuf 0 ls;
      lexbuf.buf <- newbuf
    end;
    lexbuf.len <- ls;
    lexbuf.offset <- lexbuf.offset + s;
    lexbuf.bytes_offset <- lexbuf.bytes_offset + s_bytes;
    lexbuf.pos <- lexbuf.pos - s;
    lexbuf.bytes_pos <- lexbuf.bytes_pos - s_bytes;
    lexbuf.marked_pos <- lexbuf.marked_pos - s;
    lexbuf.marked_bytes_pos <- lexbuf.marked_bytes_pos - s_bytes;
    lexbuf.start_pos <- 0;
    lexbuf.start_bytes_pos <- 0
  end;
  let n = lexbuf.refill lexbuf.buf lexbuf.pos chunk_size in
  if n = 0 then lexbuf.finished <- true else lexbuf.len <- lexbuf.len + n

let new_line lexbuf =
  lexbuf.curr_line <- lexbuf.curr_line + 1;
  lexbuf.curr_bol <- lexbuf.pos + lexbuf.offset;
  lexbuf.curr_bytes_bol <- lexbuf.bytes_pos + lexbuf.bytes_offset

let[@inline always] next_aux some none lexbuf =
  if (not lexbuf.finished) && lexbuf.pos = lexbuf.len then refill lexbuf;
  if lexbuf.finished && lexbuf.pos = lexbuf.len then none
  else begin
    let ret = lexbuf.buf.(lexbuf.pos) in
    lexbuf.pos <- lexbuf.pos + 1;
    lexbuf.bytes_pos <- lexbuf.bytes_pos + lexbuf.bytes_per_char ret;
    if Uchar.equal ret nl_uchar then new_line lexbuf;
    some ret
  end

let next lexbuf = (next_aux [@inlined]) (fun x -> Some x) None lexbuf
let __private__next_int lexbuf = (next_aux [@inlined]) Uchar.to_int (-1) lexbuf

let mark lexbuf i =
  lexbuf.marked_pos <- lexbuf.pos;
  lexbuf.marked_bytes_pos <- lexbuf.bytes_pos;
  lexbuf.marked_bol <- lexbuf.curr_bol;
  lexbuf.marked_bytes_bol <- lexbuf.curr_bytes_bol;
  lexbuf.marked_line <- lexbuf.curr_line;
  lexbuf.marked_val <- i

let start lexbuf =
  lexbuf.start_pos <- lexbuf.pos;
  lexbuf.start_bytes_pos <- lexbuf.bytes_pos;
  lexbuf.start_bol <- lexbuf.curr_bol;
  lexbuf.start_bytes_bol <- lexbuf.curr_bytes_bol;
  lexbuf.start_line <- lexbuf.curr_line;
  mark lexbuf (-1)

let backtrack lexbuf =
  lexbuf.pos <- lexbuf.marked_pos;
  lexbuf.bytes_pos <- lexbuf.marked_bytes_pos;
  lexbuf.curr_bol <- lexbuf.marked_bol;
  lexbuf.curr_bytes_bol <- lexbuf.marked_bytes_bol;
  lexbuf.curr_line <- lexbuf.marked_line;
  lexbuf.marked_val

let rollback lexbuf =
  lexbuf.pos <- lexbuf.start_pos;
  lexbuf.bytes_pos <- lexbuf.start_bytes_pos;
  lexbuf.curr_bol <- lexbuf.start_bol;
  lexbuf.curr_bytes_bol <- lexbuf.start_bytes_bol;
  lexbuf.curr_line <- lexbuf.start_line

let lexeme_start lexbuf = lexbuf.start_pos + lexbuf.offset
let lexeme_bytes_start lexbuf = lexbuf.start_bytes_pos + lexbuf.bytes_offset
let lexeme_end lexbuf = lexbuf.pos + lexbuf.offset
let lexeme_bytes_end lexbuf = lexbuf.bytes_pos + lexbuf.bytes_offset
let loc lexbuf = (lexbuf.start_pos + lexbuf.offset, lexbuf.pos + lexbuf.offset)

let bytes_loc lexbuf =
  ( lexbuf.start_bytes_pos + lexbuf.bytes_offset,
    lexbuf.bytes_pos + lexbuf.bytes_offset )

let lexeme_length lexbuf = lexbuf.pos - lexbuf.start_pos
let lexeme_bytes_length lexbuf = lexbuf.bytes_pos - lexbuf.start_bytes_pos

let sub_lexeme lexbuf pos len =
  Array.sub lexbuf.buf (lexbuf.start_pos + pos) len

let lexeme lexbuf =
  Array.sub lexbuf.buf lexbuf.start_pos (lexbuf.pos - lexbuf.start_pos)

let lexeme_char lexbuf pos = lexbuf.buf.(lexbuf.start_pos + pos)

let lexing_position_start lexbuf =
  {
    Lexing.pos_fname = lexbuf.filename;
    pos_lnum = lexbuf.start_line;
    pos_cnum = lexbuf.start_pos + lexbuf.offset;
    pos_bol = lexbuf.start_bol;
  }

let lexing_position_curr lexbuf =
  {
    Lexing.pos_fname = lexbuf.filename;
    pos_lnum = lexbuf.curr_line;
    pos_cnum = lexbuf.pos + lexbuf.offset;
    pos_bol = lexbuf.curr_bol;
  }

let lexing_positions lexbuf =
  let start_p = lexing_position_start lexbuf
  and curr_p = lexing_position_curr lexbuf in
  (start_p, curr_p)

let lexing_bytes_position_start lexbuf =
  {
    Lexing.pos_fname = lexbuf.filename;
    pos_lnum = lexbuf.start_line;
    pos_cnum = lexbuf.start_bytes_pos + lexbuf.bytes_offset;
    pos_bol = lexbuf.start_bytes_bol;
  }

let lexing_bytes_position_curr lexbuf =
  {
    Lexing.pos_fname = lexbuf.filename;
    pos_lnum = lexbuf.curr_line;
    pos_cnum = lexbuf.bytes_pos + lexbuf.bytes_offset;
    pos_bol = lexbuf.curr_bytes_bol;
  }

let lexing_bytes_positions lexbuf =
  let start_p = lexing_bytes_position_start lexbuf
  and curr_p = lexing_bytes_position_curr lexbuf in
  (start_p, curr_p)

let with_tokenizer lexer' lexbuf =
  let lexer () =
    let token = lexer' lexbuf in
    let start_p, curr_p = lexing_positions lexbuf in
    (token, start_p, curr_p)
  in
  lexer

module Chan = struct
  exception Missing_input

  type t = {
    b : Bytes.t;
    ic : in_channel;
    mutable len : int;
    mutable pos : int;
  }

  let min_buffer_size = 64

  let create ic len : t =
    let len = max len min_buffer_size in
    { b = Bytes.create len; ic; len = 0; pos = 0 }

  let available (t : t) = t.len - t.pos

  let rec ensure_bytes_available (t : t) ~can_refill n =
    if available t >= n then ()
    else if can_refill then (
      let len = t.len - t.pos in
      if len > 0 then Bytes.blit t.b t.pos t.b 0 len;
      let read = input t.ic t.b len (Bytes.length t.b - len) in
      t.len <- len + read;
      t.pos <- 0;
      if read = 0 then raise Missing_input
      else ensure_bytes_available t ~can_refill n)
    else raise Missing_input

  let ensure_bytes_available t ~can_refill n =
    (* [n] should not exceed the size of the buffer. Here we are
       conservative and make sure it doesn't exceed the mininum size
       for the buffer. *)
    if n <= 0 || n > min_buffer_size then invalid_arg "Sedlexing.Chan.ensure";
    ensure_bytes_available t ~can_refill n

  let get (t : t) i = Bytes.get t.b (t.pos + i)

  let advance (t : t) n =
    if t.pos + n > t.len then invalid_arg "advance";
    t.pos <- t.pos + n

  let raw_buf (t : t) = t.b
  let raw_pos (t : t) = t.pos
end

let make_from_channel ?bytes_per_char ic ~max_bytes_per_uchar
    ~min_bytes_per_uchar ~read_uchar =
  let t = Chan.create ic (chunk_size * max_bytes_per_uchar) in
  let malformed = ref false in
  let refill buf pos len =
    let rec loop i =
      if !malformed then raise MalFormed;
      if i = len then i
      else (
        match
          (* we refill our bytes buffer only if we haven't refilled any uchar yet. *)
          let can_refill = i = 0 in
          Chan.ensure_bytes_available t ~can_refill min_bytes_per_uchar;
          read_uchar ~can_refill t
        with
          | c ->
              buf.(pos + i) <- c;
              loop (i + 1)
          | exception MalFormed when i <> 0 ->
              malformed := true;
              i
          | exception Chan.Missing_input ->
              if i = 0 && Chan.available t > 0 then raise MalFormed;
              i)
    in
    loop 0
  in
  create ?bytes_per_char refill

module Latin1 = struct
  let from_gen s =
    from_gen ~bytes_per_char:(fun _ -> 1) (Gen.map Uchar.of_char s)

  let from_string s =
    let len = String.length s in
    {
      (empty_lexbuf (fun _ -> 1)) with
      buf = Array.init len (fun i -> Uchar.of_char s.[i]);
      len;
      finished = true;
    }

  let from_channel ic =
    make_from_channel ic
      ~bytes_per_char:(fun _ -> 1)
      ~min_bytes_per_uchar:1 ~max_bytes_per_uchar:1
      ~read_uchar:(fun ~can_refill:_ t ->
        let c = Chan.get t 0 in
        Chan.advance t 1;
        Uchar.of_char c)

  let to_latin1 c =
    if Uchar.is_char c then Uchar.to_char c
    else raise (InvalidCodepoint (Uchar.to_int c))

  let lexeme_char lexbuf pos = to_latin1 (lexeme_char lexbuf pos)

  let sub_lexeme lexbuf pos len =
    let s = Bytes.create len in
    for i = 0 to len - 1 do
      Bytes.set s i (to_latin1 lexbuf.buf.(lexbuf.start_pos + pos + i))
    done;
    Bytes.to_string s

  let lexeme lexbuf = sub_lexeme lexbuf 0 (lexbuf.pos - lexbuf.start_pos)
end

module Utf8 = struct
  module Helper = struct
    (* http://www.faqs.org/rfcs/rfc3629.html *)

    let width = function
      | '\000' .. '\127' -> 1
      | '\192' .. '\223' -> 2
      | '\224' .. '\239' -> 3
      | '\240' .. '\247' -> 4
      | _ -> raise MalFormed

    (* https://www.unicode.org/versions/corrigendum1.html *)
    let check_two n1 n2 =
      if n1 < 0xc2 || 0xdf < n1 then raise MalFormed;
      if n2 < 0x80 || 0xbf < n2 then raise MalFormed;
      if n2 lsr 6 != 0b10 then raise MalFormed;
      ((n1 land 0x1f) lsl 6) lor (n2 land 0x3f)

    let check_three n1 n2 n3 =
      if n1 = 0xe0 then (
        if n2 < 0xa0 || 0xbf < n2 then raise MalFormed;
        if n3 < 0x80 || 0xbf < n3 then raise MalFormed)
      else (
        if n1 < 0xe1 || 0xef < n1 then raise MalFormed;
        if n2 < 0x80 || 0xbf < n2 then raise MalFormed;
        if n3 < 0x80 || 0xbf < n3 then raise MalFormed);
      if n2 lsr 6 != 0b10 || n3 lsr 6 != 0b10 then raise MalFormed;
      let p =
        ((n1 land 0x0f) lsl 12) lor ((n2 land 0x3f) lsl 6) lor (n3 land 0x3f)
      in
      if p >= 0xd800 && p <= 0xdf00 then raise MalFormed;
      p

    let check_four n1 n2 n3 n4 =
      if n1 = 0xf0 then (
        if n2 < 0x90 || 0xbf < n2 then raise MalFormed;
        if n3 < 0x80 || 0xbf < n3 then raise MalFormed;
        if n4 < 0x80 || 0xbf < n4 then raise MalFormed)
      else if n1 = 0xf4 then (
        if n2 < 0x80 || 0x8f < n2 then raise MalFormed;
        if n3 < 0x80 || 0xbf < n3 then raise MalFormed;
        if n4 < 0x80 || 0xbf < n4 then raise MalFormed)
      else (
        if n1 < 0xf1 || 0xf3 < n1 then raise MalFormed;
        if n2 < 0x80 || 0xbf < n2 then raise MalFormed;
        if n3 < 0x80 || 0xbf < n3 then raise MalFormed;
        if n4 < 0x80 || 0xbf < n4 then raise MalFormed);
      if n2 lsr 6 != 0b10 || n3 lsr 6 != 0b10 || n4 lsr 6 != 0b10 then
        raise MalFormed;
      ((n1 land 0x07) lsl 18)
      lor ((n2 land 0x3f) lsl 12)
      lor ((n3 land 0x3f) lsl 6)
      lor (n4 land 0x3f)

    let next s i =
      let c1 = s.[i] in
      match width c1 with
        | 1 -> Char.code c1
        | 2 ->
            let n1 = Char.code c1 in
            let n2 = Char.code s.[i + 1] in
            check_two n1 n2
        | 3 ->
            let n1 = Char.code c1 in
            let n2 = Char.code s.[i + 1] in
            let n3 = Char.code s.[i + 2] in
            check_three n1 n2 n3
        | 4 ->
            let n1 = Char.code c1 in
            let n2 = Char.code s.[i + 1] in
            let n3 = Char.code s.[i + 2] in
            let n4 = Char.code s.[i + 3] in
            check_four n1 n2 n3 n4
        | _ -> assert false

    let gen_from_char_gen s =
      let next_or_fail () =
        match Gen.next s with None -> raise MalFormed | Some x -> Char.code x
      in
      fun () ->
        Gen.next s >>| fun c1 ->
        match width c1 with
          | 1 -> Uchar.of_char c1
          | 2 ->
              let n1 = Char.code c1 in
              let n2 = next_or_fail () in
              Uchar.of_int (check_two n1 n2)
          | 3 ->
              let n1 = Char.code c1 in
              let n2 = next_or_fail () in
              let n3 = next_or_fail () in
              Uchar.of_int (check_three n1 n2 n3)
          | 4 ->
              let n1 = Char.code c1 in
              let n2 = next_or_fail () in
              let n3 = next_or_fail () in
              let n4 = next_or_fail () in
              Uchar.of_int (check_four n1 n2 n3 n4)
          | _ -> raise MalFormed

    (**************************)

    let to_buffer a apos len b =
      for i = apos to apos + len - 1 do
        Buffer.add_utf_8_uchar b a.(i)
      done
  end

  let from_channel ic =
    make_from_channel ic ~bytes_per_char:Uchar.utf_8_byte_length
      ~min_bytes_per_uchar:1 ~max_bytes_per_uchar:4
      ~read_uchar:(fun ~can_refill t ->
        let w = Helper.width (Chan.get t 0) in
        Chan.ensure_bytes_available t ~can_refill w;
        let c =
          Helper.next (Bytes.unsafe_to_string (Chan.raw_buf t)) (Chan.raw_pos t)
        in
        Chan.advance t w;
        Uchar.of_int c)

  let from_gen s =
    from_gen ~bytes_per_char:Uchar.utf_8_byte_length
      (Helper.gen_from_char_gen s)

  let from_string s =
    from_gen (Gen.init ~limit:(String.length s) (fun i -> String.get s i))

  let sub_lexeme lexbuf pos len =
    let buf = Buffer.create (len * 4) in
    Helper.to_buffer lexbuf.buf (lexbuf.start_pos + pos) len buf;
    Buffer.contents buf

  let lexeme lexbuf = sub_lexeme lexbuf 0 (lexbuf.pos - lexbuf.start_pos)
end

module Utf16 = struct
  type byte_order = Little_endian | Big_endian

  module Helper = struct
    (* http://www.ietf.org/rfc/rfc2781.txt *)

    let number_of_pair bo c1 c2 =
      match bo with
        | Little_endian -> (c2 lsl 8) + c1
        | Big_endian -> (c1 lsl 8) + c2

    let get_bo bo c1 c2 =
      match !bo with
        | Some o -> o
        | None ->
            let o =
              match (c1, c2) with
                | 0xff, 0xfe -> Little_endian
                | _ -> Big_endian
            in
            bo := Some o;
            o

    let gen_from_char_gen opt_bo s =
      let next_or_fail () =
        match Gen.next s with None -> raise MalFormed | Some x -> Char.code x
      in
      let bo = ref opt_bo in
      fun () ->
        Gen.next s >>| fun c1 ->
        let n1 = Char.code c1 in
        let n2 = next_or_fail () in
        let o = get_bo bo n1 n2 in
        let w1 = number_of_pair o n1 n2 in
        if w1 = 0xfffe then raise (InvalidCodepoint w1);
        if w1 < 0xd800 || 0xdfff < w1 then Uchar.of_int w1
        else if w1 <= 0xdbff then (
          let n3 = next_or_fail () in
          let n4 = next_or_fail () in
          let w2 = number_of_pair o n3 n4 in
          if w2 < 0xdc00 || w2 > 0xdfff then raise MalFormed;
          let upper10 = (w1 land 0x3ff) lsl 10 and lower10 = w2 land 0x3ff in
          Uchar.of_int (0x10000 + upper10 + lower10))
        else raise MalFormed

    let to_buffer bo a apos len bom b =
      let store =
        match bo with
          | Big_endian -> Buffer.add_utf_16be_uchar b
          | Little_endian -> Buffer.add_utf_16le_uchar b
      in
      if bom then store (Uchar.of_int 0xfeff);
      (* first, store the BOM *)
      for i = apos to apos + len - 1 do
        store a.(i)
      done
  end

  let from_channel ic opt_bo =
    let bo = ref opt_bo in
    make_from_channel ic ~bytes_per_char:Uchar.utf_16_byte_length
      ~min_bytes_per_uchar:2 ~max_bytes_per_uchar:4
      ~read_uchar:(fun ~can_refill t ->
        let n1 = Char.code (Chan.get t 0) in
        let n2 = Char.code (Chan.get t 1) in
        let o = Helper.get_bo bo n1 n2 in
        let w1 = Helper.number_of_pair o n1 n2 in
        if w1 = 0xfffe then raise (InvalidCodepoint w1);
        if w1 < 0xd800 || 0xdfff < w1 then (
          Chan.advance t 2;
          Uchar.of_int w1)
        else if w1 <= 0xdbff then (
          Chan.ensure_bytes_available t ~can_refill 4;
          let n3 = Char.code (Chan.get t 2) in
          let n4 = Char.code (Chan.get t 3) in
          let w2 = Helper.number_of_pair o n3 n4 in
          if w2 < 0xdc00 || w2 > 0xdfff then raise MalFormed;
          let upper10 = (w1 land 0x3ff) lsl 10 and lower10 = w2 land 0x3ff in
          Chan.advance t 4;
          Uchar.of_int (0x10000 + upper10 + lower10))
        else raise MalFormed)

  let from_gen s opt_bo =
    from_gen ~bytes_per_char:Uchar.utf_16_byte_length
      (Helper.gen_from_char_gen opt_bo s)

  let from_string s =
    from_gen (Gen.init ~limit:(String.length s) (fun i -> String.get s i))

  let sub_lexeme lb pos len bo bom =
    let buf = Buffer.create ((len * 4) + 2) in
    (* +2 for the BOM *)
    Helper.to_buffer bo lb.buf (lb.start_pos + pos) len bom buf;
    Buffer.contents buf

  let lexeme lb bo bom = sub_lexeme lb 0 (lb.pos - lb.start_pos) bo bom
end
