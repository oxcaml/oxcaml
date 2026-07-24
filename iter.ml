open! Stdlib

include (struct
    module I64 = struct
      external ( + ) : int64# -> int64# -> int64# = "%int64#_add"
      external ( * ) : int64# -> int64# -> int64# = "%int64#_mul"
      external ( < ) : int64# -> int64# -> bool = "%int64#_lessthan"
      external ( >= ) : int64# -> int64# -> bool = "%int64#_greaterequal"
      external of_int : int -> int64# = "%int64#_of_int"
    end

    module Idx = struct
      external get : 'block -> ('block, 'a) idx_mut -> 'a = "%get_idx"
      external to_raw : ('block, 'a) idx_mut -> int64# = "%identity"
      external of_raw : int64# -> ('block, 'a) idx_mut = "%identity"
      external array_element_size_in_bytes : 'a array -> int = "%array_element_size_in_bytes"

      let[@inline] advance (idx : (_, 'a) idx_mut) ~(by : int64#) : (_, 'a) idx_mut =
        let bytes =
          I64.(by * of_int (array_element_size_in_bytes ([||] : 'a array)))
        in
        of_raw I64.(to_raw idx + bytes)
      ;;

      let[@inline] raw_less (i : (_, 'a) idx_mut) (j : (_, 'a) idx_mut) =
        I64.(to_raw i < to_raw j)
      ;;
    end

    let[@inline] iter ~block ~idx ~len ~f =
      if not I64.(#0L < len)
      then ()
      else (
        let stop = Idx.advance idx ~by:len in
        let mutable idx = idx in
        while Idx.raw_less idx stop do
          f (Idx.get block idx);
          idx <- Idx.advance idx ~by:#1L
        done)
    ;;

    let[@inline] iter_naive ~block ~idx ~len ~f =
      assert (I64.(len >= #0L && len < #1_000_000L));
      let mutable i = #0L in
      while I64.(i < len) do
        f (Idx.get block (Idx.advance idx ~by:i));
        i <- I64.(i + #1L)
      done
    ;;
  end : sig
    val iter
      :  block:'block
      -> idx:('block, 'a) idx_mut
      -> len:int64#
      -> f:('a -> unit)
      -> unit

    val iter_naive
      :  block:'block
      -> idx:('block, 'a) idx_mut
      -> len:int64#
      -> f:('a -> unit)
      -> unit
  end)

