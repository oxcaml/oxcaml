open Std
open Sexplib.Std
module Make(M:sig
                type t[@@deriving sexp_of]
                include
                  sig
                    [@@@ocaml.warning "-32"]
                    val sexp_of_t : t -> Sexplib0.Sexp.t
                  end[@@ocaml.doc "@inline"][@@merlin.hide ]
                val compare : t -> t -> int
                val byte_offset_within_file : t -> (int * int) option
              end) =
  struct
    type inner = {
      pos: int ;
      data: M.t }[@@deriving sexp_of]
    include
      struct
        let _ = fun (_ : inner) -> ()
        let sexp_of_inner =
          (fun { pos = pos__002_; data = data__004_ } ->
             let bnds__001_ = ([] : _ Stdlib.List.t) in
             let bnds__001_ =
               let arg__005_ = M.sexp_of_t data__004_ in
               ((Sexplib0.Sexp.List [Sexplib0.Sexp.Atom "data"; arg__005_])
                 :: bnds__001_ : _ Stdlib.List.t) in
             let bnds__001_ =
               let arg__003_ = sexp_of_int pos__002_ in
               ((Sexplib0.Sexp.List [Sexplib0.Sexp.Atom "pos"; arg__003_]) ::
                 bnds__001_ : _ Stdlib.List.t) in
             Sexplib0.Sexp.List bnds__001_ : inner -> Sexplib0.Sexp.t)
        let _ = sexp_of_inner
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
    let compare_inner (a : inner) (b : inner) =
      let c = Int.compare a.pos b.pos in
      if c <> 0 then c else M.compare a.data b.data
    type type_ =
      | Start 
      | End [@@deriving sexp_of]
    include
      struct
        let _ = fun (_ : type_) -> ()
        let sexp_of_type_ =
          (function
           | Start -> Sexplib0.Sexp.Atom "Start"
           | End -> Sexplib0.Sexp.Atom "End" : type_ -> Sexplib0.Sexp.t)
        let _ = sexp_of_type_
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
    type t = (type_ * inner)[@@deriving sexp_of]
    include
      struct
        let _ = fun (_ : t) -> ()
        let sexp_of_t =
          (fun (arg0__006_, arg1__007_) ->
             let res0__008_ = sexp_of_type_ arg0__006_
             and res1__009_ = sexp_of_inner arg1__007_ in
             Sexplib0.Sexp.List [res0__008_; res1__009_] : t ->
                                                             Sexplib0.Sexp.t)
        let _ = sexp_of_t
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
    let of_m m =
      match M.byte_offset_within_file m with
      | None -> []
      | Some (start, end_) ->
          [(Start, { pos = start; data = m });
          (End, { pos = end_; data = m })]
    let compare (type_1, inner_1) (type_2, inner_2) =
      let c = compare_inner inner_1 inner_2 in
      if c <> 0
      then c
      else
        (match (type_1, type_2) with
         | (Start, End) -> (-1)
         | (Start, Start) | (End, End) -> 0
         | (End, Start) -> 1)
    let sort ms =
      (ms |> (List.concat_map ~f:of_m)) |> (List.sort ~cmp:compare)
  end
