(* TEST
 multicore;
 { bytecode; } { native; }
*)

type target =
  | Broadcast
  | Entity of int32_u * int32_u
  | Topic of int32_u * int32_u
[@@unboxed]

module Target_array = struct
  external length : target array @ contended -> int @@ portable
    = "%array_length"
  external get : target array -> int -> target @@ portable
    = "%array_safe_get"
  external set : target array @ write uncontended -> int -> target -> unit
    @@ portable = "%array_safe_set"
end

module Slice : sig @@ portable
  type t
  val of_array : target array -> t @ local
  val length : t @ local -> int
  val set : t @ local -> int -> target -> unit
  val fork_join2 :
    t @ local -> pivot:int ->
    (t @ local -> unit) @ portable ->
    (t @ local -> unit) @ portable -> unit
end = struct
  type t =
    { array : target array @@ contended global; start : int; stop : int }

  let of_array array = exclave_
    { array; start = 0; stop = Target_array.length array }

  let length slice = slice.stop - slice.start

  let set slice i value =
    if i < 0 || i >= length slice then invalid_arg "Slice.set";
    (* The bounds check keeps this store inside the borrowed range. *)
    Target_array.set (Obj.magic_uncontended slice.array) (slice.start + i) value

  let fork_join2 { array; start; stop } ~pivot left right =
    if pivot < 0 || pivot > stop - start then invalid_arg "Slice.fork_join2";
    let middle = start + pivot in
    let worker = Domain.Safe.spawn (fun () ->
      left { array; start; stop = middle })
    in
    Fun.protect ~finally:(fun () -> Domain.join worker) (fun () ->
      right { array; start = middle; stop })
end

let check_out_of_bounds (slice @ local) index =
  match Slice.set slice index Broadcast with
  | exception Invalid_argument _ -> ()
  | () -> failwith "wrote outside the borrowed range"

let () =
  let targets = [| Broadcast; Broadcast; Broadcast; Broadcast; Broadcast |] in
  let parent = Domain.self () in
  Slice.fork_join2 (Slice.of_array targets) ~pivot:2
    (fun left ->
       assert (Domain.self () <> parent);
       assert (Slice.length left = 2);
       check_out_of_bounds left 2;
       for i = 0 to Slice.length left - 1 do
         Slice.set left i (Entity (#13l, #21l))
       done)
    (fun right ->
       assert (Domain.self () = parent);
       assert (Slice.length right = 3);
       check_out_of_bounds right (-1);
       for i = 0 to Slice.length right - 1 do
         Slice.set right i (Topic (#13l, #21l))
       done);
  for i = 0 to 4 do
    match Target_array.get targets i with
    | Entity (#13l, #21l) -> assert (i < 2)
    | Topic (#13l, #21l) -> assert (i >= 2)
    | _ -> assert false
  done
