open StdLabels

module Blit = struct
  type ('src, 'dst) t =
    src:'src -> src_pos:int -> dst:'dst -> dst_pos:int -> len:int -> unit

  let bytes : (Bytes.t, Bytes.t) t = Bytes.blit
end

module Slice = struct
  type t =
    { pos : int
    ; len : int
    }
end

type 'a t =
  { mutable buf : 'a
  ; mutable buf_len : int
  ; mutable a_start : int
  ; mutable a_end : int
  ; mutable b_end : int
  ; mutable b_inuse : bool
  ; mutable reserving : bool
  }

let buffer t = t.buf
let _invariant t = if t.b_inuse then assert (t.b_end - t.a_start > t.buf_len - t.a_end)

let create buf ~len =
  { buf
  ; buf_len = len
  ; a_start = 0
  ; a_end = 0
  ; b_end = 0
  ; b_inuse = false
  ; reserving = false
  }
;;

let peek t =
  if t.a_start < t.a_end
  then Some { Slice.pos = t.a_start; len = t.a_end - t.a_start }
  else None
;;

let is_empty t = t.a_start = t.a_end
let length t = t.a_end - t.a_start + t.b_end

let junk t ~len:size =
  assert (length t >= size);
  let read_len = min size (t.a_end - t.a_start) in
  t.a_start <- t.a_start + read_len;
  if t.a_start = t.a_end
  then (
    t.a_start <- size - read_len;
    t.a_end <- t.b_end;
    t.b_end <- 0;
    t.b_inuse <- false)
;;

let space_left_for_a t = t.buf_len - t.a_end
let space_left_for_b t = t.a_start - t.b_end
let best_available t = if t.b_inuse then space_left_for_b t else space_left_for_a t

let max_available t =
  if t.b_inuse then space_left_for_b t else max (space_left_for_a t) (space_left_for_b t)
;;

let available_two_writes t =
  if t.b_inuse then space_left_for_b t else space_left_for_a t + space_left_for_b t
;;

let reserve t ~len:size =
  if t.reserving then failwith "previous reserve not committed";
  let space_left_for_a = space_left_for_a t in
  let space_left_for_b = space_left_for_b t in
  if t.b_inuse
  then
    if space_left_for_b >= size
    then (
      t.reserving <- true;
      Some t.b_end)
    else None
  else if space_left_for_a >= size
  then (
    t.reserving <- true;
    Some t.a_end)
  else if space_left_for_b >= size
  then (
    t.reserving <- true;
    t.b_inuse <- true;
    Some t.b_end)
  else None
;;

let commit t ~len =
  assert t.reserving;
  if t.b_inuse
  then (
    assert (t.b_end + len <= t.a_start);
    t.b_end <- t.b_end + len)
  else (
    assert (t.a_end + len <= t.buf_len);
    t.a_end <- t.a_end + len);
  t.reserving <- false
;;

let unused_space t = space_left_for_a t + space_left_for_b t

let compress t (blit : (_, _) Blit.t) =
  assert (not t.reserving);
  if t.a_start = 0
  then ()
  else (
    let src = t.buf in
    let dst = t.buf in
    if t.b_end = 0
    then (
      let len_a = t.a_end - t.a_start in
      blit ~src ~dst ~src_pos:t.a_start ~dst_pos:0 ~len:len_a;
      t.a_start <- 0;
      t.a_end <- len_a;
      t.b_end <- 0;
      t.b_inuse <- false)
    else if space_left_for_b t >= t.a_end - t.a_start
    then (
      let len_a = t.a_end - t.a_start in
      blit ~src ~dst ~src_pos:0 ~dst_pos:len_a ~len:t.b_end;
      blit ~src ~dst ~src_pos:t.a_start ~dst_pos:0 ~len:len_a;
      t.a_start <- 0;
      t.a_end <- len_a + t.b_end;
      t.b_end <- 0;
      t.b_inuse <- false))
;;

let resize t (blit : (_, _) Blit.t) dst ~len =
  assert (not t.reserving);
  let len_t = length t in
  assert (len >= len_t);
  let src = t.buf in
  let len_a = t.a_end - t.a_start in
  blit ~src ~dst ~src_pos:t.a_start ~dst_pos:0 ~len:len_a;
  blit ~src ~dst ~src_pos:0 ~dst_pos:len_a ~len:t.b_end;
  t.buf <- dst;
  t.buf_len <- len;
  t.a_start <- 0;
  t.b_end <- 0;
  t.a_end <- len_t;
  t.b_inuse <- false;
  t.a_start <- 0
;;

let pp pp_slice fmt t =
  let slice = { Slice.pos = t.a_start; len = t.a_end - t.a_start } in
  pp_slice fmt (t.buf, slice);
  if t.b_end > 0
  then (
    let slice = { Slice.pos = 0; len = t.b_end } in
    pp_slice fmt (t.buf, slice))
;;

module Bytes = struct
  type nonrec t = Bytes.t t

  let resize t ~len =
    let new_buf = Bytes.create len in
    resize t Blit.bytes new_buf ~len
  ;;

  let compress t = compress t Blit.bytes

  let pp_slice fmt (bytes, { Slice.pos; len }) =
    Format.fprintf fmt "%s" (Bytes.sub_string bytes ~pos ~len)
  ;;

  let pp fmt (t : t) = pp pp_slice fmt t

  module Writer = struct
    module Make_from_bytes (S : sig
      val add_subbytes : t -> Bytes.t -> pos:int -> len:int -> unit
    end) =
    struct
      include S

      let add_substring t src ~pos ~len =
        add_subbytes t (Bytes.unsafe_of_string src) ~len ~pos
      ;;

      let add_bytes t src = add_subbytes t src ~len:(Bytes.length src) ~pos:0
      let add_string t src = add_substring t src ~pos:0 ~len:(String.length src)
      let add_char t c = add_string t (String.make 1 c)
    end

    include Make_from_bytes (struct
      let add_subbytes (t : t) src ~pos:src_pos ~len =
        if unused_space t < len
        then (
          let len = max (t.buf_len + len) (t.buf_len * 2) in
          resize t ~len)
        else if len > available_two_writes t
        then compress t;
        let len_1 = min len (best_available t) in
        let dst_pos =
          match reserve t ~len:len_1 with
          | None -> failwith "Unexpected"
          | Some e -> e
        in
        let dst = buffer t in
        Blit.bytes ~src ~src_pos ~dst_pos ~dst ~len:len_1;
        commit t ~len:len_1;
        let len_2 = len - len_1 in
        if len_2 > 0
        then (
          let src_pos = src_pos + len_1 in
          let dst_pos =
            match reserve t ~len:len_2 with
            | None -> failwith "Unexpected"
            | Some e -> e
          in
          Blit.bytes ~src ~src_pos ~dst_pos ~dst ~len:len_2;
          commit t ~len:len_2)
      ;;
    end)
  end
end
