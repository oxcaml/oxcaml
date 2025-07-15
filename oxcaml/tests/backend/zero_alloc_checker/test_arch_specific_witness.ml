module Float64 = struct
  type t = float#

  external max : t -> t -> t
    = "caml_no_bytecode" "caml_simd_float64_max"
  [@@noalloc] [@@builtin] [@@unboxed]

end

let[@zero_alloc] max f1 f2 = Float64.max f1 f2

let[@zero_alloc strict] max_with_assume f1 f2 = (Float64.max[@zero_alloc assume]) f1 f2
