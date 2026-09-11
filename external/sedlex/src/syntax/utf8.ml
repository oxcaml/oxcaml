open Sedlexing

let unsafe_byte s j = Char.code (String.unsafe_get s j)
let malformed s j l = `Malformed (String.sub s j l)

let r_utf_8 s j l =
  (* assert (0 <= j && 0 <= l && j + l <= String.length s); *)
  let uchar c = `Uchar (Uchar.unsafe_of_int c) in
  match l with
    | 1 -> uchar (unsafe_byte s j)
    | 2 -> (
        let b0 = unsafe_byte s j in
        let b1 = unsafe_byte s (j + 1) in
        match Utf8.Helper.check_two b0 b1 with
          | i -> uchar i
          | exception MalFormed -> malformed s j l)
    | 3 -> (
        let b0 = unsafe_byte s j in
        let b1 = unsafe_byte s (j + 1) in
        let b2 = unsafe_byte s (j + 2) in
        match Utf8.Helper.check_three b0 b1 b2 with
          | i -> uchar i
          | exception MalFormed -> malformed s j l)
    | 4 -> (
        let b0 = unsafe_byte s j in
        let b1 = unsafe_byte s (j + 1) in
        let b2 = unsafe_byte s (j + 2) in
        let b3 = unsafe_byte s (j + 3) in
        match Utf8.Helper.check_four b0 b1 b2 b3 with
          | i -> uchar i
          | exception MalFormed -> malformed s j l)
    | _ -> assert false

let fold ~f acc s =
  let rec loop acc f s i last =
    if i > last then acc
    else (
      match Utf8.Helper.width (String.unsafe_get s i) with
        | exception MalFormed ->
            loop (f acc i (malformed s i 1)) f s (i + 1) last
        | need ->
            let rem = last - i + 1 in
            if rem < need then f acc i (malformed s i rem)
            else loop (f acc i (r_utf_8 s i need)) f s (i + need) last)
  in
  let pos = 0 in
  let len = String.length s in
  let last = pos + len - 1 in
  loop acc f s pos last
