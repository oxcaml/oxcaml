open FourierMotzkin
open IntArith

type t =
  | Const of int
  | Atom of int
  | Add of t * t
  | Sub of t * t
  | Scale of int * t
  | Shr_atom of int * t * int
  | Lsr_atom of int * int
  | Or_atom of int * t * int

module Range = struct
  type t =
    { lo : int64;
      hi : int64
    }

  (** val lo : t -> int64 **)

  let lo t0 = t0.lo

  (** val hi : t -> int64 **)

  let hi t0 = t0.hi

  (** val full : t **)

  let full = { lo = Int64.min_int; hi = Int64.max_int }

  (** val const : int -> t **)

  let const c =
    let c0 = Int64.of_int c in
    { lo = c0; hi = c0 }

  (** val shr_signed : int -> t **)

  let shr_signed bits =
    { lo = Int64.shift_right Int64.min_int bits;
      hi = Int64.shift_right Int64.max_int bits
    }

  (** val shr_logical : int -> t **)

  let shr_logical bits =
    if bits <= 0
    then full
    else { lo = 0L; hi = Int64.shift_right_logical (-1L) bits }

  (** val add64 : int64 -> int64 -> int64 option **)

  let add64 = add_ovf64

  (** val mul64 : int64 -> int64 -> int64 option **)

  let mul64 = mul_ovf64

  (** val add : t -> t -> t option **)

  let add a b =
    match add64 a.lo b.lo with
    | Some lo0 -> (
      match add64 a.hi b.hi with
      | Some hi0 -> Some { lo = lo0; hi = hi0 }
      | None -> None)
    | None -> None

  (** val neg : t -> t option **)

  let neg r =
    if Int64.equal r.lo Int64.min_int
    then None
    else Some { lo = Int64.neg r.hi; hi = Int64.neg r.lo }

  (** val sub : t -> t -> t option **)

  let sub a b = match neg b with Some nb -> add a nb | None -> None

  (** val or_mask : int -> t -> t option **)

  let or_mask mask r =
    if mask < 0
    then None
    else
      match add64 r.hi (Int64.of_int mask) with
      | Some hi0 -> Some { lo = r.lo; hi = hi0 }
      | None -> None

  (** val scale : int -> t -> t option **)

  let scale k r =
    if Int.equal k 0
    then Some { lo = 0L; hi = 0L }
    else
      let k0 = Int64.of_int k in
      match mul64 k0 r.lo with
      | Some a -> (
        match mul64 k0 r.hi with
        | Some b ->
          if Int64.compare a b <= 0
          then Some { lo = a; hi = b }
          else Some { lo = b; hi = a }
        | None -> None)
      | None -> None
end

(** val shr_range_facts : bool -> int -> int -> Affine.t list **)

let shr_range_facts signed atom bits =
  if signed
  then
    if bits < 1
    then []
    else
      let hi0 = Int64.to_int (Int64.shift_right Int64.max_int bits) in
      let upper = Affine.add_const (Affine.scale (-1) (Affine.var atom)) hi0 in
      if 2 <= bits
      then
        let lo0 = Int64.to_int (Int64.shift_right Int64.min_int bits) in
        [upper; Affine.add_const (Affine.var atom) (Int.neg lo0)]
      else upper :: []
  else if bits < 1
  then []
  else
    let lower = Affine.var atom in
    if 2 <= bits
    then
      let hi0 = (1 lsl (64 - bits)) - 1 in
      [Affine.add_const (Affine.scale (-1) (Affine.var atom)) hi0; lower]
    else lower :: []

(** val to_affine : t -> (Affine.t * Affine.t list) option **)

let rec to_affine = function
  | Const c -> Some (Affine.constant c, [])
  | Atom id -> Some (Affine.var id, [])
  | Add (a, b) -> (
    match to_affine a with
    | Some p -> (
      let fa, sa = p in
      match to_affine b with
      | Some p0 -> (
        let fb, sb = p0 in
        match Affine.add_checked fa fb with
        | Some form -> Some (form, sa @ sb)
        | None -> None)
      | None -> None)
    | None -> None)
  | Sub (a, b) -> (
    match to_affine a with
    | Some p -> (
      let fa, sa = p in
      match to_affine b with
      | Some p0 -> (
        let fb, sb = p0 in
        match Affine.scale_checked (-1) fb with
        | Some nfb -> (
          match Affine.add_checked fa nfb with
          | Some form -> Some (form, sa @ sb)
          | None -> None)
        | None -> None)
      | None -> None)
    | None -> None)
  | Scale (k, a) -> (
    match to_affine a with
    | Some p -> (
      let fa, sa = p in
      match Affine.scale_checked k fa with
      | Some form -> Some (form, sa)
      | None -> None)
    | None -> None)
  | Shr_atom (atom, arg, bits) -> (
    match to_affine arg with
    | Some p -> (
      let fa, sa = p in
      let v = Affine.var atom in
      let pow = 1 lsl bits in
      match Affine.scale_checked (Int.neg pow) v with
      | Some m -> (
        match Affine.add_checked fa m with
        | Some s1 -> (
          match Affine.scale_checked pow v with
          | Some p0 -> (
            match Affine.add_const_checked p0 (pow - 1) with
            | Some pc -> (
              match Affine.scale_checked (-1) fa with
              | Some nfa -> (
                match Affine.add_checked pc nfa with
                | Some s2 ->
                  Some (v, s1 :: s2 :: (shr_range_facts true atom bits @ sa))
                | None -> None)
              | None -> None)
            | None -> None)
          | None -> None)
        | None -> None)
      | None -> None)
    | None -> None)
  | Lsr_atom (atom, bits) ->
    Some (Affine.var atom, shr_range_facts false atom bits)
  | Or_atom (atom, arg, mask) -> (
    match to_affine arg with
    | Some p -> (
      let fa, sa = p in
      let v = Affine.var atom in
      match Affine.scale_checked (-1) fa with
      | Some nfa -> (
        match Affine.add_checked v nfa with
        | Some s1 -> (
          match Affine.add_const_checked fa mask with
          | Some fm -> (
            match Affine.scale_checked (-1) v with
            | Some nv -> (
              match Affine.add_checked fm nv with
              | Some s2 -> Some (v, s1 :: s2 :: sa)
              | None -> None)
            | None -> None)
          | None -> None)
        | None -> None)
      | None -> None)
    | None -> None)
