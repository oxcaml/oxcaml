type elt : immutable_data
type arrow = elt -> elt

type flag =
  | On
  | Off

type cell : mutable_data

val return_first : elt -> elt -> elt
val apply : (elt -> elt) -> elt -> elt
val id : 'a -> 'a
val choose : flag -> 'a -> 'a -> 'a
val unstaged : elt -> elt -> elt -> unit
val staged_1ary : elt -> elt -> unit
val staged_2ary : elt -> elt -> elt -> unit
val staged_via_fun_syntax : elt -> elt -> elt -> unit
val point_free : elt -> elt -> elt -> unit
val call_twice : (elt -> elt) -> elt -> elt
val store : cell -> elt -> unit
val retain : (elt -> elt) -> unit
val use : arrow -> elt -> elt
val apply_once : ('a -> 'b) -> 'a -> unit
val apply_twice : ('a -> 'b) -> 'a -> unit
val churn_local : (cell @ local -> cell) -> unit
val churn_nonlocal : (cell -> cell) -> unit
val nest3 : ((((elt -> elt) -> elt) -> elt) -> elt) -> elt
val relay : (cell -> cell) -> (cell -> unit) -> unit
val relay_pinned : (cell -> cell) -> (cell @ contended local -> unit) -> unit

val weave
  :  ((((cell -> cell) -> cell) -> cell) -> unit)
  -> ((((cell -> cell) -> cell) -> cell) -> unit)
  -> unit

val weave_pinned
  :  ((((cell @ local -> cell) -> cell) -> cell) -> unit)
  -> ((((cell -> cell) -> cell) -> cell) -> unit)
  -> unit

val app : ('a -> 'b) -> 'a -> 'b
val app_pinned : ('a @ local -> 'b) -> 'a -> 'b
