val fold :
  f:('a -> int -> [> `Malformed of string | `Uchar of Uchar.t ] -> 'a) ->
  'a ->
  string ->
  'a
