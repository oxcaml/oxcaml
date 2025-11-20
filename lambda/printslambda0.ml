module SL = Slambda0

open Format

let slambda0 pp_lam ppf (slam : _ SL.t0) =
  match slam with
  | Quote lam -> fprintf ppf "⟪ %a ⟫" pp_lam lam
