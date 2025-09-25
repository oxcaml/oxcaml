open Slambda0

open Format

let slambda0 pp_lam ppf slam =
  match slam with
  | SLquote lam -> fprintf ppf "⟪ %a ⟫" pp_lam lam
