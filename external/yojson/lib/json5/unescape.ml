open Let_syntax.Result

let ( % ) = Int.logor
let ( << ) = Int.shift_left
let ( >> ) = Int.shift_right
let ( & ) = Int.logand

let utf_8_string_of_unicode i =
  if i <= 0x007F then (
    let b = Bytes.create 1 in
    Bytes.set_int8 b 0 i;
    Ok (Bytes.to_string b))
  else if i <= 0x07FF then (
    let five_high_bits = i >> 6 & 0b11111 in
    let six_low_bits = i & 0b111111 in
    let high = 0b11000000 % five_high_bits << 8 in
    let low = 0b10000000 % six_low_bits in
    let n = high % low in
    let b = Bytes.create 2 in
    Bytes.set_int16_be b 0 n;
    Ok (Bytes.to_string b))
  else if i <= 0xFFFF then (
    let four_high_bits = i >> 12 & 0b1111 in
    let six_mid_bits = i >> 6 & 0b111111 in
    let six_low_bits = i & 0b111111 in
    let high = 0b11100000 % four_high_bits << 16 in
    let mid = 0b10000000 % six_mid_bits << 8 in
    let low = 0b10000000 % six_low_bits in
    let n = high % mid % low in
    let b = Bytes.create 3 in
    Bytes.set_int32_be b 0 (Int32.of_int n);
    Ok (Bytes.to_string b))
  else if i <= 0x10FFFF then (
    let three_hh_bits = i >> 18 & 0b111 in
    let six_hl_bits = i >> 12 & 0b111111 in
    let six_lh_bits = i >> 6 & 0b111111 in
    let six_ll_bits = i & 0b111111 in
    let hh = 0b11110000 % three_hh_bits << 24 in
    let hl = 0b10000000 % six_hl_bits << 16 in
    let lh = 0b10000000 % six_lh_bits << 8 in
    let ll = 0b10000000 % six_ll_bits in
    let n = hh % hl % lh % ll in
    let b = Bytes.create 4 in
    Bytes.set_int32_be b 0 (Int32.of_int n);
    Ok (Bytes.to_string b))
  else Error (Format.sprintf "invalid code point %X" i)

let unescape str =
  if String.length str < 2 then
    Error (Format.sprintf "too small escape sequence %s" str)
  else
    match str.[1] with
    | 'u' ->
        let escape_chars = String.sub str 2 4 in
        let* as_int =
          Format.sprintf "0x%s" escape_chars |> int_of_string_opt |> function
          | Some x -> Ok x
          | None -> Error (Format.sprintf "bad escape sequence %s" escape_chars)
        in
        utf_8_string_of_unicode as_int
    | 'x' ->
        let escape_chars = String.sub str 2 2 in
        let* as_int =
          Format.sprintf "0x%s" escape_chars |> int_of_string_opt |> function
          | Some x -> Ok x
          | None -> Error (Format.sprintf "bad escape sequence %s" escape_chars)
        in
        utf_8_string_of_unicode as_int
    (* https://spec.json5.org/#escapes table 1 *)
    | 'b' -> Ok "\b"
    | 'n' -> Ok "\n"
    | 'r' -> Ok "\r"
    | 't' -> Ok "\t"
    | 'f' -> Ok "\x0C"
    | 'v' -> Ok "\x0B"
    | '0' ->
        if String.length str = 2 then Ok "\x00"
        else if String.length str = 4 then
          let octal_str = String.(sub str 2 2) in
          let* as_int =
            Format.sprintf "0o%s" octal_str |> int_of_string_opt |> function
            | Some x -> Ok x
            | None -> Error (Format.sprintf "bad escape sequence %s" octal_str)
          in
          utf_8_string_of_unicode as_int
        else Error (Format.sprintf "invalid octal sequence %s" str)
    | '1' .. '9' -> Error (Format.sprintf "invalid escape sequence %c" str.[1])
    | c ->
        (* According to https://spec.json5.org/#escapes : "If any other
           character follows a reverse solidus, except for the decimal
           digits 1 through 9, that character will be included in the
           string, but the reverse solidus will not.".
           Alternatively, apostrophe, quotation mark and reverse solidus
           should also be included in the string without the leading
           reverse solidus. *)
        Ok (String.make 1 c)
