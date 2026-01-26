[@@@ocaml.warning "+a-40-41-42"]

module T = struct
  type reg_class =
    | Int64
    | Float128

  type[@ocamlformat "disable"] phys_reg =
    | X0  | X1  | X2  | X3  | X4  | X5  | X6  | X7
    | X8  | X9  | X10 | X11 | X12 | X13 | X14 | X15
                      | X19 | X20 | X21 | X22 | X23
    | X24 | X25 | X26 | X27 | X28
    | X16 | X17

    | D0  | D1  | D2  | D3  | D4  | D5  | D6  | D7
    | D8  | D9  | D10 | D11 | D12 | D13 | D14 | D15
    | D16 | D17 | D18 | D19 | D20 | D21 | D22 | D23
    | D24 | D25 | D26 | D27 | D28 | D29 | D30 | D31

  let[@ocamlformat "disable"] phys_gpr_regs_available =
    [| X0;  X1;  X2;  X3;  X4;  X5;  X6;  X7;
       X8;  X9;  X10; X11; X12; X13; X14; X15;
                      X19; X20; X21; X22; X23;
       X24; X25 |]

  let[@ocamlformat "disable"] phys_gpr_regs =
    Array.append phys_gpr_regs_available [| X26; X27; X28; X16; X17 |]

  let[@ocamlformat "disable"] phys_simd_regs =
    [| D0;  D1;  D2;  D3;  D4;  D5;  D6;  D7;
       D8;  D9;  D10; D11; D12; D13; D14; D15;
       D16; D17; D18; D19; D20; D21; D22; D23;
       D24; D25; D26; D27; D28; D29; D30; D31 |]

  module Reg_class = struct
    type t = reg_class

    let of_machtype typ =
      match (typ : Cmm.machtype_component) with
      | Val | Int | Addr -> Int64
      | Float | Float32 -> Float128
      | Vec128 | Valx2 -> Float128
      | Vec256 | Vec512 -> Misc.fatal_error "arm64: got 256/512 bit vector"

    let all = [Int64; Float128]

    let[@inline] to_int : t -> int = function Int64 -> 0 | Float128 -> 1

    let hash = to_int

    let equal l r = Int.equal (to_int l) (to_int r)

    let print : Format.formatter -> t -> unit =
     fun ppf reg_class ->
      Format.fprintf ppf "%s"
        (match reg_class with Int64 -> "int64" | Float128 -> "float128")
  end

  module Phys_reg = struct
    type t = phys_reg

    let[@ocamlformat "disable"] name = function
      | X0  -> "x0"  | X1  -> "x1"  | X2  -> "x2"  | X3  -> "x3"
      | X4  -> "x4"  | X5  -> "x5"  | X6  -> "x6"  | X7  -> "x7"
      | X8  -> "x8"  | X9  -> "x9"  | X10 -> "x10" | X11 -> "x11"
      | X12 -> "x12" | X13 -> "x13" | X14 -> "x14" | X15 -> "x15"
                                                   | X19 -> "x19"
      | X20 -> "x20" | X21 -> "x21" | X22 -> "x22" | X23 -> "x23"
      | X24 -> "x24" | X25 -> "x25" | X26 -> "x26" | X27 -> "x27"
      | X28 -> "x28"
      | X16 -> "x16" | X17 -> "x17"

      | D0  -> "d0"  | D1  -> "d1"  | D2  -> "d2"  | D3  -> "d3"
      | D4  -> "d4"  | D5  -> "d5"  | D6  -> "d6"  | D7  -> "d7"
      | D8  -> "d8"  | D9  -> "d9"  | D10 -> "d10" | D11 -> "d11"
      | D12 -> "d12" | D13 -> "d13" | D14 -> "d14" | D15 -> "d15"
      | D16 -> "d16" | D17 -> "d17" | D18 -> "d18" | D19 -> "d19"
      | D20 -> "d20" | D21 -> "d21" | D22 -> "d22" | D23 -> "d23"
      | D24 -> "d24" | D25 -> "d25" | D26 -> "d26" | D27 -> "d27"
      | D28 -> "d28" | D29 -> "d29" | D30 -> "d30" | D31 -> "d31"

    let[@inline][@ocamlformat "disable"] to_int = function
      | X0  -> 0  | X1  -> 1  | X2  -> 2  | X3  -> 3
      | X4  -> 4  | X5  -> 5  | X6  -> 6  | X7  -> 7
      | X8  -> 8  | X9  -> 9  | X10 -> 10 | X11 -> 11
      | X12 -> 12 | X13 -> 13 | X14 -> 14 | X15 -> 15
                                          | X19 -> 16
      | X20 -> 17 | X21 -> 18 | X22 -> 19 | X23 -> 20
      | X24 -> 21 | X25 -> 22 | X26 -> 23 | X27 -> 24
      | X28 -> 25
      | X16 -> 26 | X17 -> 27

      | D0  -> 28 | D1  -> 29 | D2  -> 30 | D3  -> 31
      | D4  -> 32 | D5  -> 33 | D6  -> 34 | D7  -> 35
      | D8  -> 36 | D9  -> 37 | D10 -> 38 | D11 -> 39
      | D12 -> 40 | D13 -> 41 | D14 -> 42 | D15 -> 43
      | D16 -> 44 | D17 -> 45 | D18 -> 46 | D19 -> 47
      | D20 -> 48 | D21 -> 49 | D22 -> 50 | D23 -> 51
      | D24 -> 52 | D25 -> 53 | D26 -> 54 | D27 -> 55
      | D28 -> 56 | D29 -> 57 | D30 -> 58 | D31 -> 59

    include Identifiable.Make (struct
      type t = phys_reg

      let hash = to_int

      let equal l r = Int.equal (to_int l) (to_int r)

      let compare l r = Int.compare (to_int l) (to_int r)

      let output out_channel t = Out_channel.output_string out_channel (name t)

      let print formatter t = Format.pp_print_string formatter (name t)
    end)

    let[@inline] reg_class = function
      | X0 | X1 | X2 | X3 | X4 | X5 | X6 | X7 | X8 | X9 | X10 | X11 | X12 | X13
      | X14 | X15 | X19 | X20 | X21 | X22 | X23 | X24 | X25 | X26 | X27 | X28
      | X16 | X17 ->
        Int64
      | D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9 | D10 | D11 | D12 | D13
      | D14 | D15 | D16 | D17 | D18 | D19 | D20 | D21 | D22 | D23 | D24 | D25
      | D26 | D27 | D28 | D29 | D30 | D31 ->
        Float128
  end

  let[@inline] registers = function
    | Int64 -> phys_gpr_regs
    | Float128 -> phys_simd_regs

  let[@inline] available_registers = function
    | Int64 -> phys_gpr_regs_available
    | Float128 -> phys_simd_regs

  let num_available_registers reg_class =
    Array.length (available_registers reg_class)

  let first_in_class reg_class = Phys_reg.to_int (registers reg_class).(0)

  let index_in_class phys_reg =
    let reg_class = Phys_reg.reg_class phys_reg in
    Phys_reg.to_int phys_reg - first_in_class reg_class

  (* See "DWARF for the ARM 64-bit architecture (AArch64)" available from
     developer.arm.com. *)
  let[@ocamlformat "disable"] dwarf_reg_number = function
    | X0  -> 0  | X1  -> 1  | X2  -> 2  | X3  -> 3
    | X4  -> 4  | X5  -> 5  | X6  -> 6  | X7  -> 7
    | X8  -> 8  | X9  -> 9  | X10 -> 10 | X11 -> 11
    | X12 -> 12 | X13 -> 13 | X14 -> 14 | X15 -> 15
    | X16 -> 16 | X17 -> 17             | X19 -> 19
    | X20 -> 20 | X21 -> 21 | X22 -> 22 | X23 -> 23
    | X24 -> 24 | X25 -> 25 | X26 -> 26 | X27 -> 27
    | X28 -> 28

    | D0  -> 64 | D1  -> 65 | D2  -> 66 | D3  -> 67
    | D4  -> 68 | D5  -> 69 | D6  -> 70 | D7  -> 71
    | D8  -> 72 | D9  -> 73 | D10 -> 74 | D11 -> 75
    | D12 -> 76 | D13 -> 77 | D14 -> 78 | D15 -> 79
    | D16 -> 80 | D17 -> 81 | D18 -> 82 | D19 -> 83
    | D20 -> 84 | D21 -> 85 | D22 -> 86 | D23 -> 87
    | D24 -> 88 | D25 -> 89 | D26 -> 90 | D27 -> 91
    | D28 -> 92 | D29 -> 93 | D30 -> 94 | D31 -> 95

  let dwarf_reg_number typ phys_reg =
    if
      not
        (Reg_class.equal
           (Reg_class.of_machtype typ)
           (Phys_reg.reg_class phys_reg))
    then Misc.fatal_error "Register class mismatch";
    dwarf_reg_number phys_reg

  let gpr_name = phys_gpr_regs |> Array.map Phys_reg.name

  let float_reg_name =
    Array.init (Array.length phys_simd_regs) (Printf.sprintf "d%d")

  let float32_reg_name =
    Array.init (Array.length phys_simd_regs) (Printf.sprintf "s%d")

  let vec128_reg_name =
    Array.init (Array.length phys_simd_regs) (Printf.sprintf "q%d")

  let register_name typ phys_reg =
    if
      not
        (Reg_class.equal
           (Reg_class.of_machtype typ)
           (Phys_reg.reg_class phys_reg))
    then Misc.fatal_error "Register class mismatch";
    let index_in_class = index_in_class phys_reg in
    let names =
      match (typ : Cmm.machtype_component) with
      | Int | Addr | Val -> gpr_name
      | Float -> float_reg_name
      | Float32 -> float32_reg_name
      | Vec128 | Valx2 -> vec128_reg_name
      | Vec256 | Vec512 -> Misc.fatal_error "arm64: got 256/512 bit vector"
    in
    names.(index_in_class)
end

include T
module Reg_class_tbl = Regs_utils.Make_reg_class_tbl (T)
