(* The JKind solver specialized to Ikind.Ldd and Types.type_expr. *)

module Ldd = Ikind.Ldd

type ty = Types.type_expr

type constr = Ldd.constr

type lat = Ldd.lat

type kind = Ldd.node

type poly = Ldd.node

type ops =
  { const : lat -> kind;
    join : kind -> kind -> kind;
    meet : kind -> kind -> kind;
    modality : lat -> kind -> kind;
    constr : constr -> kind list -> kind;
    kind_of : ty -> kind;
    rigid : ty -> kind;
    pp_kind : kind -> string
  }

type ckind = ops -> kind

type constr_decl =
  | Ty of
      { args : ty list;
        kind : ckind;
        abstract : bool
      }
  | Poly of poly * poly list

type env =
  { kind_of : ty -> ckind;
    lookup : constr -> constr_decl
  }

type solver

val make_solver : env -> solver

val normalize : solver -> ckind -> poly

val constr_kind_poly : solver -> constr -> poly * poly list

val leq : solver -> ckind -> ckind -> bool

val leq_with_reason : solver -> ckind -> ckind -> int list option

val round_up : solver -> ckind -> lat

val clear_memos : unit -> unit

val pp : poly -> string

val pp_debug : poly -> string
