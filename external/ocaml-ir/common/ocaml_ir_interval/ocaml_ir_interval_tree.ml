module type Key  = sig type t val compare : t -> t -> int end
module Make(Key:Key) =
  struct
    module Interval =
      struct
        type t = {
          start: Key.t ;
          end_: Key.t }[@@deriving compare]
        include
          struct
            let _ = fun (_ : t) -> ()
            let compare =
              (fun a__001_ b__002_ ->
                 if Stdlib.(==) a__001_ b__002_
                 then 0
                 else
                   (match Key.compare a__001_.start b__002_.start with
                    | 0 -> Key.compare a__001_.end_ b__002_.end_
                    | n -> n) : t -> ((t)[@merlin.hide ]) -> int)
            let _ = compare
          end[@@ocaml.doc "@inline"][@@merlin.hide ]
        let create start end_ = { start; end_ }
        let relative_position_from_point t point =
          if (Key.compare t.end_ point) < 0
          then `Before
          else if (Key.compare t.start point) > 0 then `After else `Includes
      end
    module Elem =
      struct
        type 'a t = {
          interval: Interval.t ;
          data: 'a }
        let compare a b = Interval.compare a.interval b.interval
        let create ((r, l), data) =
          { interval = (Interval.create r l); data }
      end
    type 'a interval_tree =
      | Empty 
      | Node of
      {
      pivot: Key.t ;
      mid: 'a Elem.t list ;
      left: 'a interval_tree ;
      right: 'a interval_tree } 
    type 'a t = 'a interval_tree
    let median xs =
      let ar = Array.of_list xs in
      Array.sort Elem.compare ar;
      (let n = Array.length ar in ((ar.(n / 2)).interval).start)
    let partition (elements : 'a Elem.t list) pivot =
      List.fold_left
        (fun (before, mid, after) (element : 'a Elem.t) ->
           match Interval.relative_position_from_point element.interval pivot
           with
           | `Includes -> (before, (element :: mid), after)
           | `Before -> ((element :: before), mid, after)
           | `After -> (before, mid, (element :: after))) ([], [], [])
        elements
    let rec create =
      function
      | [] -> Empty
      | intervals ->
          let pivot = median intervals in
          let (left, mid, right) = partition intervals pivot in
          Node { pivot; mid; left = (create left); right = (create right) }
    let of_alist intervals = (List.map Elem.create intervals) |> create
    let fold_range root point ~f ~init =
      let rec fold_range_visitor node init =
        match node with
        | Empty -> init
        | Node { pivot; mid; left; right } ->
            let init =
              List.fold_left
                (fun accum Elem.{ interval; data }  ->
                   match Interval.relative_position_from_point interval point
                   with
                   | `Includes ->
                       f ~interval:(interval.start, interval.end_) ~data
                         accum
                   | `Before | `After -> accum) init mid in
            let cmp = Key.compare pivot point in
            if cmp = 0
            then init
            else
              if cmp < 0
              then fold_range_visitor right init
              else fold_range_visitor left init in
      fold_range_visitor root init
  end
