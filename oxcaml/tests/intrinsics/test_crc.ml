external int64_crc : initial:int64 -> data:int64 -> int64
  = "caml_int64_crc32_bytecode" "caml_int64_crc32"
  [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects] [@@unboxed]

external reference_int64_crc : initial:int64 -> data:int64 -> int64
  = "caml_int64_crc32_bytecode" "caml_int64_crc32"
  [@@noalloc] [@@no_effects] [@@no_coeffects] [@@unboxed]

let () =
  let check ~initial ~data =
    let expected = reference_int64_crc ~initial ~data in
    let actual = int64_crc ~initial ~data in
    if not (Int64.equal expected actual)
    then
      Printf.printf "crc32 ~initial:%Ld ~data:%Ld\n%Ld expected\n%Ld actual\n"
        initial data expected actual;
    actual
  in
  let data =
    Sys.opaque_identity
      [| 0L; 1L; 42L; 436297L; -1L; Int64.min_int; Int64.min_int |]
  in
  let rec loop i ~acc =
    if i <= 0
    then ()
    else
      let i = i - 1 in
      let data = data.(Sys.opaque_identity i) in
      let acc = check ~initial:acc ~data in
      loop i ~acc
  in
  let initial = Sys.opaque_identity 0L in
  let n = Sys.opaque_identity (Array.length data) in
  loop n ~acc:initial
