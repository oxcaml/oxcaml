(** Decode ARM64 logical immediate encoding to 64-bit value *)

(** [decode_logical_immediate ~n ~immr ~imms] decodes an ARM64 logical immediate
    encoding into the corresponding 64-bit value.

    The encoding uses three fields:
    - [n]: 1-bit field distinguishing 64-bit patterns (typically 1 for 64-bit, 0
      for 32-bit)
    - [immr]: 6-bit rotation amount
    - [imms]: 6-bit field encoding element size and run length

    The decoded value is constructed by: 1. Determining the element size (2, 4,
    8, 16, 32, or 64 bits) 2. Creating a pattern with consecutive 1-bits 3.
    Rotating the pattern within the element 4. Replicating the element across 64
    bits

    @raise Invalid_argument
      if the encoding is invalid (e.g., all zeros or all ones pattern) *)
val decode_logical_immediate : n:int -> immr:int -> imms:int -> Nativeint.t
