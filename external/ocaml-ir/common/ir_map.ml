open Sexplib.Std
open Std

module Label = struct
  include Ir_map_intf.Label
end

module Line_col = struct
  type t = int * int

  let compare (l1, c1) (l2, c2) =
    let c = Int.compare l1 l2 in
    if c = 0 then Int.compare c1 c2 else c
  ;;

  let equal q b = compare q b = 0

  let of_position { Location.Position.pos_lnum; pos_cnum; pos_bol; _ } =
    pos_lnum, pos_cnum - pos_bol
  ;;

  let get_better_interval (a_pos_start, a_pos_end) (b_pos_start, b_pos_end) =
    let comp =
      -(fun (a__001_ : t) (b__002_ : t) -> (compare a__001_ b__002_ [@merlin.hide]))
         a_pos_start
         b_pos_start
    in
    if comp <> 0
    then comp
    else
      (fun (a__003_ : t) (b__004_ : t) -> (compare a__003_ b__004_ [@merlin.hide]))
        a_pos_end
        b_pos_end
  ;;
end

module Tree = Ocaml_ir_interval_tree.Make (Line_col)

module Entry = struct
  type t = Label.t * Location.Simple.t * Location.Simple.t [@@deriving sexp]

  include struct
    let _ = fun (_ : t) -> ()

    let t_of_sexp =
      (let error_source__013_ = "ir_map.ml.Entry.t" in
       function
       | Sexplib0.Sexp.List [ arg0__006_; arg1__007_; arg2__008_ ] ->
         let res0__009_ = Label.t_of_sexp arg0__006_
         and res1__010_ = Location.Simple.t_of_sexp arg1__007_
         and res2__011_ = Location.Simple.t_of_sexp arg2__008_ in
         res0__009_, res1__010_, res2__011_
       | sexp__012_ ->
         Sexplib0.Sexp_conv_error.tuple_of_size_n_expected error_source__013_ 3 sexp__012_
        : Sexplib0.Sexp.t -> t)
    ;;

    let _ = t_of_sexp

    let sexp_of_t =
      (fun (arg0__014_, arg1__015_, arg2__016_) ->
         let res0__017_ = Label.sexp_of_t arg0__014_
         and res1__018_ = Location.Simple.sexp_of_t arg1__015_
         and res2__019_ = Location.Simple.sexp_of_t arg2__016_ in
         Sexplib0.Sexp.List [ res0__017_; res1__018_; res2__019_ ]
        : t -> Sexplib0.Sexp.t)
    ;;

    let _ = sexp_of_t
  end [@@ocaml.doc "@inline"] [@@merlin.hide]

  let create ~label ~source_location ~ir_location = label, source_location, ir_location
  let label (t, _, _) = t
  let source_location (_, t, _) = t
  let ir_location (_, _, t) = t

  let compare a b =
    (fun (a__020_ : Location.Simple.t * Location.Simple.t)
         (b__021_ : Location.Simple.t * Location.Simple.t) ->
      (let t__022_, t__023_ = a__020_ in
       let t__024_, t__025_ = b__021_ in
       match Location.Simple.compare t__022_ t__024_ with
       | 0 -> Location.Simple.compare t__023_ t__025_
       | n -> n) [@merlin.hide])
      (source_location a, ir_location a)
      (source_location b, ir_location b)
  ;;
end

type raw = Entry.t list [@@deriving sexp]

include struct
  let _ = fun (_ : raw) -> ()

  let raw_of_sexp =
    (fun x__027_ -> list_of_sexp Entry.t_of_sexp x__027_ : Sexplib0.Sexp.t -> raw)
  ;;

  let _ = raw_of_sexp

  let sexp_of_raw =
    (fun x__028_ -> sexp_of_list Entry.sexp_of_t x__028_ : raw -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_raw
end [@@ocaml.doc "@inline"] [@@merlin.hide]

type t = Entry.t Tree.t

let is_within
  (line, column)
  { Location.Simple.loc_start =
      { pos_lnum = start_lnum; pos_bol = start_bol; pos_cnum = start_cnum; _ }
  ; loc_end = { pos_lnum = end_lnum; pos_bol = end_bol; pos_cnum = end_cnum; _ }
  }
  =
  let after_start =
    (Int.equal line start_lnum && column >= start_cnum - start_bol) || line > start_lnum
  in
  let before_end =
    (Int.equal line end_lnum && column <= end_cnum - end_bol) || line < end_lnum
  in
  after_start && before_end
;;

let to_line_col ({ loc_start; loc_end; _ } : Location.Simple.t) =
  if loc_start.Location.Position.pos_cnum > loc_end.pos_cnum
  then
    let open Line_col in
    of_position loc_end, of_position loc_start
  else
    let open Line_col in
    of_position loc_start, of_position loc_end
;;

let create_source_to_ir (raw : raw) =
  List.map raw ~f:(fun entry -> to_line_col (Entry.source_location entry), entry)
  |> Tree.of_alist
;;

let create_ir_to_source (raw : raw) =
  List.map raw ~f:(fun entry -> to_line_col (Entry.ir_location entry), entry)
  |> Tree.of_alist
;;

let find_best_intervals tree line_col =
  Tree.fold_range tree line_col ~init:[] ~f:(fun ~interval ~data:entry -> function
    | [] -> [ interval, entry ]
    | (interval_best, _) :: _ as list ->
      let comp = Line_col.get_better_interval interval interval_best in
      if comp < 0
      then [ interval, entry ]
      else if comp > 0
      then list
      else (interval, entry) :: list)
  |> List.map ~f:snd
  |> List.sort ~cmp:Entry.compare
;;
