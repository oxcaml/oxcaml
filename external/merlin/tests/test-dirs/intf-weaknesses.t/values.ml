type fn = int -> int

let identity (x : int) = x
let make_fn () = identity

type t =
  { id : int
  ; label : string
  }

let create id label = { id; label }
let id t = t.id
let label t = t.label
let relabel t label = { t with label }
let default = { id = 0; label = "default" }

type queue = { mutable items : float list }

type metrics =
  { mutable count : int
  ; mutable total : float
  ; samples : queue
  }

let create_metrics () = { count = 0; total = 0.; samples = { items = [] } }

let record m x =
  m.count <- m.count + 1;
  m.total <- m.total +. x;
  m.samples.items <- x :: m.samples.items
;;

let mean m = if m.count = 0 then 0. else m.total /. float_of_int m.count

let rec rev_map f acc = function
  | [] -> acc
  | x :: rest -> rev_map f (f x :: acc) rest
;;

let summarize m ~f = rev_map f [] m.samples.items

(* Values inside a nested signature strengthen like top-level ones. *)
module Nested = struct
  let double x = x * 2
end
