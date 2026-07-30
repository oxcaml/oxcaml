(* The handful of [Misc] helpers used by the copy of [Pprintast] linked into the
   standard library for runtime metaprogramming. The real [Misc] is compiler-only
   (it pulls in [Config] &c.), so these small, self-contained functions are
   reproduced here. *)

let fatal_error msg = failwith msg

let format_as_unboxed_literal s =
  if String.starts_with ~prefix:"-" s
  then "-#" ^ String.sub s 1 (String.length s - 1)
  else "#" ^ s

let pp_parens_if condition printer ppf arg =
  Format.fprintf ppf "%s%a%s"
    (if condition then "(" else "")
    printer arg
    (if condition then ")" else "")
