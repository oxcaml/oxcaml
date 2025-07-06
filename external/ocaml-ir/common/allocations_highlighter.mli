val clear : string
val highlight_red : string
val underline : string
val underline_red : string
val underline_green : string
val highlight_green : string
val italic : string
val dark : string
val highlight_yellow : string

val highlight
  :  ?clear_marker:string
  -> marker:(Allocations.Item.t -> string)
  -> start_newline:(int -> string)
  -> filename:string
  -> content:string
  -> Format.formatter
  -> Allocations.t
  -> unit
