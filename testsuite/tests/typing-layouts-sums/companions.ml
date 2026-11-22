(* TEST
 { bytecode; } { native; }
*)

module Packet = struct
  type 'a t = Empty | Data of 'a * int
end

type 'a alias = 'a Packet.t

module Truth = struct type t = bool = false | true end
let keep_builtin (value : Truth.t#) : bool# = value

module Reexport = struct type 'a t = 'a Packet.t = Empty | Data of 'a * int end
let reexport x : _ Reexport.t# = Reexport.Data (x, 13)

module Again = struct type 'a t = 'a Packet.t = Empty | Data of 'a * int end
type 'a via_alias = 'a Again.t
let share_reexports (x : string Reexport.t#) : string via_alias# = x

let boxed payload = Packet.Data (payload, 31)
let inline payload : _ Packet.t# = Packet.Data (payload, 31)

let read_boxed (packet : string Packet.t# box) =
  match packet with Packet.Empty -> "" | Data (payload, _) -> payload

let read_inline (packet : string alias#) =
  match packet with Packet.Empty -> 0 | Data (_, count) -> count

let () =
  assert (read_boxed (boxed "message") = "message");
  assert (read_inline (inline "message") = 31)

type raw = Absent | Count of int [@@unboxed]
type wrapped = raw box

let wrap count : wrapped = Count count
let empty () : wrapped = Absent
let read (value : wrapped) =
  match value with Absent -> 0 | Count count -> count

let () = assert (read (wrap 42) = 42 && read (empty ()) = 0)

type record_arm = Stop | Record of { count : int }
type _ indexed = Number : int indexed | Text : string indexed
let old_boxed (x : record_arm) = match x with Stop -> 0 | Record r -> r.count
let old_gadt : type a. a indexed -> a = function Number -> 0 | Text -> "ok"
