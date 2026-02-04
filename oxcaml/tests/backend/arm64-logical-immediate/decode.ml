(** Decode ARM64 logical immediate encoding to 64-bit value *)

(** Rotate right a value of given width by rotation amount *)
let ror value width rotation =
  let open Nativeint in
  let rotation = rotation mod width in
  (* Handle width=64 specially to avoid shift overflow *)
  let mask = if width >= 64 then -1n else sub (shift_left 1n width) 1n in
  let value = logand value mask in
  if rotation = 0
  then value
  else begin
    let right_part = shift_right_logical value rotation in
    let left_part = shift_left value (width - rotation) in
    logand (logor right_part left_part) mask
  end

(** Replicate a pattern across 64 bits *)
let replicate pattern esize =
  let open Nativeint in
  let rec loop acc shift =
    if shift >= 64
    then acc
    else loop (logor acc (shift_left pattern shift)) (shift + esize)
  in
  loop pattern esize

(** Decode logical immediate from N, immr, imms fields *)
let decode_logical_immediate ~n ~immr ~imms =
  let open Nativeint in
  (* Validate N field *)
  if n <> 0 && n <> 1
  then
    invalid_arg
      (Printf.sprintf "decode_logical_immediate: invalid N=%d (must be 0 or 1)"
         n);
  (* Validate immr and imms ranges *)
  if immr < 0 || immr > 63
  then
    invalid_arg
      (Printf.sprintf "decode_logical_immediate: invalid immr=%d (must be 0-63)"
         immr);
  if imms < 0 || imms > 63
  then
    invalid_arg
      (Printf.sprintf "decode_logical_immediate: invalid imms=%d (must be 0-63)"
         imms);
  (* Determine element size and ones count from N and imms *)
  let len =
    if n = 1 && imms land 0x20 = 0
    then 6
    else if imms land 0x20 = 0
    then 5
    else if imms land 0x10 = 0
    then 4
    else if imms land 0x08 = 0
    then 3
    else if imms land 0x04 = 0
    then 2
    else if imms land 0x02 = 0
    then 1
    else 0
  in
  (* Validate element size *)
  if len = 0
  then
    invalid_arg
      (Printf.sprintf
         "decode_logical_immediate: invalid encoding n=%d imms=0x%02x \
          (reserved)"
         n imms);
  (* Element size is 2^len *)
  let esize = 1 lsl len in
  (* Extract S and R values *)
  let mask = (1 lsl len) - 1 in
  let s = imms land mask in
  let r = immr land mask in
  (* Number of consecutive ones *)
  let ones = s + 1 in
  (* Validate ones count doesn't exceed element size *)
  if ones > esize
  then
    invalid_arg
      (Printf.sprintf
         "decode_logical_immediate: invalid encoding (ones=%d > esize=%d)" ones
         esize);
  (* Check for all-ones pattern which is invalid *)
  if ones = esize
  then
    invalid_arg
      (Printf.sprintf
         "decode_logical_immediate: invalid encoding (produces all-ones \
          pattern)");
  (* Create element with 'ones' consecutive 1-bits *)
  let element = sub (shift_left 1n ones) 1n in
  (* Rotate element right by r positions *)
  let rotated = ror element esize r in
  (* Replicate across 64 bits *)
  replicate rotated esize
