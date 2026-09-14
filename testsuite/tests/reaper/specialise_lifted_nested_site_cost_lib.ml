module Make (X : sig
  type t
  val of_int : int -> t
  val to_int : t -> int
end) : sig
  val fold : int list -> init:int -> f:(X.t -> int -> int) -> int
  val fold2 : int list -> init:int -> f:(X.t -> int -> int) -> int
end = struct
  let fold xs ~init ~f =
    let[@inline never] rec loop xs acc =
      match xs with
      | [] -> acc
      | x :: xs ->
        let a = f (X.of_int (x land 63)) acc in
        let b =
          if a land 1 = 0 then a * 3 + X.to_int (X.of_int (a lsr 1))
          else a - 7
        in
        let c =
          if b > 1000 then b mod 997 else b + X.to_int (X.of_int 5)
        in
        let d = if c < 0 then -c else c lxor 0xff in
        let e = if d land 2 = 0 then d + 11 else d * 13 in
        let g = if e land 4 = 0 then e - 17 else e / 3 in
        loop xs (g + X.to_int (X.of_int (acc land 7)))
    in
    loop xs init

  let fold2 xs ~init ~f =
    let[@inline never] rec loop xs acc =
      match xs with
      | [] -> acc
      | x :: xs ->
        let a = f (X.of_int (x land 63)) acc in
        let b =
          if a land 1 = 0 then a * 5 + X.to_int (X.of_int (a lsr 2))
          else a - 9
        in
        let c =
          if b > 2000 then b mod 991 else b + X.to_int (X.of_int 6)
        in
        let d = if c < 0 then -c else c lxor 0xf0 in
        let e = if d land 2 = 0 then d + 19 else d * 17 in
        let g = if e land 4 = 0 then e - 23 else e / 5 in
        loop xs (g + X.to_int (X.of_int (acc land 3)))
    in
    loop xs init
end
