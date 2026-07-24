let bits ofs sz n = (n lsr ofs) land ((1 lsl sz) - 1)

let read_relocation_info value =
  (bits 0 24 value, bits 24 1 value = 1, 1 lsl (bits 25 2 value))