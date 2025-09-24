(* The JKind solver. *)

(** Functor inputs:
    - A lattice-valued decision diagram implementation [Ldd]
    - The type domain [Ty] with a comparison
    - The constructor domain [Constr] with a comparison

    Output:
    - A Church-encoded kind [ckind = ops -> kind], where [kind] is the backend
      semantic domain and [ops] provides the constructors. The solver interprets
      [ckind] under a lattice-polynomial backend. *)

module type LDD = sig
  type lat

  type node

  type var

  type constr

  module Name : sig
    type t =
      | Atom of { constr : constr; arg_index : int }
      | Param of int

    val param : int -> t

    val atomic : constr -> int -> t
  end

  val bot : node

  val const : lat -> node

  val rigid : Name.t -> var

  val new_var : unit -> var

  val var : var -> node

  val join : node -> node -> node

  val meet : node -> node -> node

  val sub_subsets : node -> node -> node

  val solve_lfp : var -> node -> unit

  val enqueue_lfp : var -> node -> unit

  val enqueue_gfp : var -> node -> unit

  val solve_pending : unit -> unit

  val decompose_linear : universe:var list -> node -> node * node list

  val leq : node -> node -> bool

  val leq_with_reason : node -> node -> int list option

  val round_up : node -> lat

  val map_rigid : (Name.t -> node) -> node -> node

  val clear_memos : unit -> unit

  val pp : node -> string

  val pp_debug : node -> string
end

module Make
    (Ldd : LDD) (Ty : sig
      type t

      val compare : t -> t -> int

      val unique_id : t -> int
    end) (Constr : sig
      type t = Ldd.constr

      val compare : t -> t -> int

      val to_string : t -> string
    end) : sig
  type ty = Ty.t

  type constr = Constr.t

  type lat = Ldd.lat

  type kind

  type solver

  type ops =
    { const : lat -> kind;
      join : kind list -> kind;
      meet : kind -> kind -> kind;
      modality : lat -> kind -> kind;
      constr : constr -> kind list -> kind;
      kind_of : ty -> kind;
      rigid : ty -> kind;
      pp_kind : kind -> string
    }

  type poly = Ldd.node

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

  val make_solver : env -> solver

  val normalize : solver -> ckind -> poly

  val constr_kind_poly : solver -> constr -> poly * poly list

  val leq : solver -> ckind -> ckind -> bool

  val leq_with_reason : solver -> ckind -> ckind -> int list option

  val round_up : solver -> ckind -> lat

  val clear_memos : unit -> unit

  val pp : poly -> string

  val pp_debug : poly -> string
end
