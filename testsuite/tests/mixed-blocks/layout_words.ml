(* TEST
 include ocamlcommon;
 include ocamloptcomp;
 {
   native;
 }{
   bytecode;
 }
*)

(* Check the compiler's encoder for layout blocks,
   [Cmm_helpers.mixed_block_layout_words], against an independent
   decoder: the scan words must describe exactly the given words, and
   be in the canonical form which the runtime requires. *)

let mask_bits = 58
let repeat_max = 58

let bit word b = Nativeint.(logand (shift_right_logical word b) 1n) = 1n
let repeat word = Nativeint.(to_int (shift_right_logical word mask_bits))

(* Whether user word [i] is unscannable, according to [words]. *)
let decode words i =
  let k = Array.length words in
  if i < k * mask_bits
  then bit words.(i / mask_bits) (i mod mask_bits)
  else
    let last = words.(k - 1) in
    let n = repeat last in
    n > 0 && bit last (mask_bits - n + ((i - (k * mask_bits)) mod n))

let fail fmt =
  Printf.ksprintf (fun s -> print_endline ("FAIL: " ^ s); exit 1) fmt

(* [most_words], if given, is a number of scan words known to be
   enough for [unscannable]. *)
let check ?most_words name unscannable =
  let len = Array.length unscannable in
  let words =
    Array.of_list (Cmm_helpers.mixed_block_layout_words ~unscannable)
  in
  let k = Array.length words in
  if k = 0 then fail "%s: no scan words" name;
  Array.iteri
    (fun i u ->
      if decode words i <> u then fail "%s: word %d of %d" name i len)
    unscannable;
  Array.iteri
    (fun j w ->
      if j < k - 1 && repeat w <> 0
      then fail "%s: repeat in scan word %d of %d" name j k)
    words;
  let n = repeat words.(k - 1) in
  if n > repeat_max then fail "%s: repeat %d" name n;
  if len <= k * mask_bits <> (n = 0)
  then fail "%s: %d words, %d scan words, repeat %d" name len k n;
  if n = 0
  then begin
    if len <= (k - 1) * mask_bits
    then fail "%s: %d scan words for %d words" name k len;
    for i = len to (k * mask_bits) - 1 do
      if not (decode words i) then fail "%s: zero bit past the end" name
    done
  end;
  match most_words with
  | Some most when k > most ->
    fail "%s: %d scan words, but %d would do" name k most
  | Some _ | None -> ()

let () =
  Random.init 42;
  for _ = 1 to 2000 do
    let len = 1 + Random.int (if Random.bool () then 70 else 400) in
    (* arbitrary *)
    check "random" (Array.init len (fun _ -> Random.bool ()));
    (* a scannable prefix, then all unscannable: what the compiler
       emits while it still reorders fields *)
    let prefix = Random.int len in
    check "prefix" (Array.init len (fun i -> i >= prefix))
      ~most_words:((prefix / mask_bits) + 1);
    (* an arbitrary start, then a pattern repeated to the end: scan
       words which reach one period into the pattern are enough *)
    let period = 1 + Random.int repeat_max in
    let start = Random.int len in
    let pattern = Array.init period (fun _ -> Random.bool ()) in
    let head = Array.init start (fun _ -> Random.bool ()) in
    check "periodic"
      (Array.init len (fun i ->
           if i < start then head.(i) else pattern.((i - start) mod period)))
      ~most_words:((start + period + mask_bits - 1) / mask_bits)
  done;
  print_endline "OK"
