(* TEST *)

(* Test the rotation primitives at every width against reference
   implementations built from shifts.  Rotation counts outside [0, bits)
   have unspecified results and are not tested.

   The stdlib exposes rotations on int32, int64 and nativeint; the
   primitives also exist at the taggable widths, so declare those
   directly. *)

external int_rotl : int -> int -> int = "%int_rotl"
external int_rotr : int -> int -> int = "%int_rotr"
external int8_rotl : int8 -> int -> int8 = "%int8_rotl"
external int8_rotr : int8 -> int -> int8 = "%int8_rotr"
external int16_rotl : int16 -> int -> int16 = "%int16_rotl"
external int16_rotr : int16 -> int -> int16 = "%int16_rotr"
external int8_of_int : int -> int8 = "%int8_of_int"
external int_of_int8 : int8 -> int = "%int_of_int8"
external int16_of_int : int -> int16 = "%int16_of_int"
external int_of_int16 : int16 -> int = "%int_of_int16"

let opaque = Sys.opaque_identity

let check name x n expected actual =
  if expected <> actual then begin
    Printf.printf "%s: rotating %nx by %d: expected %nx, got %nx\n"
      name x n expected actual;
    exit 2
  end

(* Rotations at the width of [int] can be written directly: [lsl]
   truncates and [lsr] is a logical shift at that width. *)
let ref_int_rotl x n =
  if n = 0 then x else (x lsl n) lor (x lsr (Sys.int_size - n))

let ref_int_rotr x n =
  if n = 0 then x else (x lsr n) lor (x lsl (Sys.int_size - n))

let int_values =
  [ 0; 1; -1; 2; min_int; max_int; 0x0123456789ABCDE; -0x0123456789ABCDE ]

let test_int () =
  List.iter
    (fun x ->
      for n = 0 to Sys.int_size - 1 do
        let x = opaque x and n = opaque n in
        check "int_rotl" (Nativeint.of_int x) n
          (Nativeint.of_int (ref_int_rotl x n))
          (Nativeint.of_int (int_rotl x n));
        check "int_rotr" (Nativeint.of_int x) n
          (Nativeint.of_int (ref_int_rotr x n))
          (Nativeint.of_int (int_rotr x n))
      done)
    int_values

(* Rotations of the taggable widths below [int], via zero-extension to
   [int]. *)
let test_small ~bits ~of_int ~to_int ~rotl ~rotr =
  let mask = (1 lsl bits) - 1 in
  let ref_rot ~left ux n =
    let n = if left then n else bits - n in
    if n = 0 || n = bits then ux
    else ((ux lsl n) lor (ux lsr (bits - n))) land mask
  in
  let sign_extend ux = ((ux lxor (1 lsl (bits - 1))) - (1 lsl (bits - 1))) in
  List.iter
    (fun ux ->
      let x = of_int (sign_extend ux) in
      for n = 0 to bits - 1 do
        let x = opaque x and n = opaque n in
        check "small rotl" (Nativeint.of_int ux) n
          (Nativeint.of_int (sign_extend (ref_rot ~left:true ux n)))
          (Nativeint.of_int (to_int (rotl x n)));
        check "small rotr" (Nativeint.of_int ux) n
          (Nativeint.of_int (sign_extend (ref_rot ~left:false ux n)))
          (Nativeint.of_int (to_int (rotr x n)))
      done)
    [ 0; 1; mask; mask lsr 1; (mask lsr 1) + 1; 0x35 land mask; 0xA5 land mask ]

let int32_values =
  [ 0l; 1l; -1l; 2l; Int32.min_int; Int32.max_int; 0x01234567l; 0x89ABCDEFl ]

let ref_int32_rotl x n =
  if n = 0 then x
  else
    Int32.logor (Int32.shift_left x n) (Int32.shift_right_logical x (32 - n))

let test_int32 () =
  List.iter
    (fun x ->
      for n = 0 to 31 do
        let x = opaque x and n = opaque n in
        check "int32_rotl"
          (Nativeint.of_int32 x)
          n
          (Nativeint.of_int32 (ref_int32_rotl x n))
          (Nativeint.of_int32 (Int32.rotate_left x n));
        (* rotate_right is rotate_left by the complement *)
        check "int32_rotr"
          (Nativeint.of_int32 x)
          n
          (Nativeint.of_int32 (ref_int32_rotl x ((32 - n) land 31)))
          (Nativeint.of_int32 (Int32.rotate_right x n))
      done)
    int32_values

let int64_values =
  [ 0L; 1L; -1L; 2L; Int64.min_int; Int64.max_int; 0x0123456789ABCDEFL;
    0xF0F0F0F00F0F0F0FL ]

let ref_int64_rotl x n =
  if n = 0 then x
  else
    Int64.logor (Int64.shift_left x n) (Int64.shift_right_logical x (64 - n))

let test_int64 () =
  List.iter
    (fun x ->
      for n = 0 to 63 do
        let x = opaque x and n = opaque n in
        if ref_int64_rotl x n <> Int64.rotate_left x n then begin
          Printf.printf "int64_rotl: rotating %Lx by %d: expected %Lx, got %Lx\n"
            x n (ref_int64_rotl x n) (Int64.rotate_left x n);
          exit 2
        end;
        if ref_int64_rotl x ((64 - n) land 63) <> Int64.rotate_right x n
        then begin
          Printf.printf "int64_rotr: rotating %Lx by %d: got %Lx\n"
            x n (Int64.rotate_right x n);
          exit 2
        end
      done)
    int64_values

let nativeint_values =
  [ 0n; 1n; -1n; 2n; Nativeint.min_int; Nativeint.max_int; 0x01234567n ]

let ref_nativeint_rotl x n =
  if n = 0 then x
  else
    Nativeint.logor (Nativeint.shift_left x n)
      (Nativeint.shift_right_logical x (Nativeint.size - n))

let test_nativeint () =
  List.iter
    (fun x ->
      for n = 0 to Nativeint.size - 1 do
        let x = opaque x and n = opaque n in
        check "nativeint_rotl" x n (ref_nativeint_rotl x n)
          (Nativeint.rotate_left x n);
        check "nativeint_rotr" x n
          (ref_nativeint_rotl x ((Nativeint.size - n) land (Nativeint.size - 1)))
          (Nativeint.rotate_right x n)
      done)
    nativeint_values

(* Constant rotation counts (in the native compiler these select the
   immediate forms of the rotate instructions), and fully constant
   arguments (in the native compiler these are constant-folded). *)
let test_constant_counts () =
  assert (Int64.rotate_left (opaque 0x0123456789ABCDEFL) 8 = 0x23456789ABCDEF01L);
  assert (Int64.rotate_right (opaque 0x23456789ABCDEF01L) 8 = 0x0123456789ABCDEFL);
  assert (Int64.rotate_left (opaque 0x8000000000000001L) 1 = 3L);
  assert (Int64.rotate_left (opaque 1L) 0 = 1L);
  assert (Int64.rotate_right (opaque 1L) 0 = 1L);
  assert (Int32.rotate_left (opaque 0x01234567l) 8 = 0x23456701l);
  assert (Int32.rotate_right (opaque 0x23456701l) 8 = 0x01234567l);
  assert (Int32.rotate_left (opaque 0x80000001l) 1 = 3l);
  assert (Int32.rotate_left (opaque 1l) 0 = 1l);
  assert (Nativeint.rotate_left (opaque 1n) (Nativeint.size - 1)
          = Nativeint.min_int);
  assert (Nativeint.rotate_right (opaque Nativeint.min_int) (Nativeint.size - 1)
          = 1n)

let test_constant_folding () =
  assert (Int64.rotate_left 0x0123456789ABCDEFL 8 = 0x23456789ABCDEF01L);
  assert (Int64.rotate_right 0x23456789ABCDEF01L 8 = 0x0123456789ABCDEFL);
  assert (Int64.rotate_left 1L 63 = Int64.min_int);
  assert (Int64.rotate_right 1L 1 = Int64.min_int);
  assert (Int32.rotate_left 0x01234567l 8 = 0x23456701l);
  assert (Int32.rotate_right 0x23456701l 8 = 0x01234567l);
  assert (Int32.rotate_left 1l 31 = Int32.min_int);
  assert (Int32.rotate_right 1l 1 = Int32.min_int);
  assert (int_rotl 1 (Sys.int_size - 1) = min_int);
  assert (int_rotr min_int (Sys.int_size - 1) = 1);
  assert (int_of_int8 (int8_rotl (int8_of_int 1) 7) = -0x80);
  assert (int_of_int8 (int8_rotr (int8_of_int 1) 1) = -0x80);
  assert (int_of_int16 (int16_rotl (int16_of_int 1) 15) = -0x8000);
  assert (int_of_int16 (int16_rotr (int16_of_int 1) 1) = -0x8000)

let () =
  test_int ();
  test_small ~bits:8 ~of_int:int8_of_int ~to_int:int_of_int8 ~rotl:int8_rotl
    ~rotr:int8_rotr;
  test_small ~bits:16 ~of_int:int16_of_int ~to_int:int_of_int16
    ~rotl:int16_rotl ~rotr:int16_rotr;
  test_int32 ();
  test_int64 ();
  test_nativeint ();
  test_constant_counts ();
  test_constant_folding ()
