type a = int
type b = int
type c = int
type d = int
type 'a e = int constraint 'a = int * int

let double x = x * 2

type f = int

let quad x = x * 4

type g = { left : int }
and h = { right : g option }

type t = int

let scale x = x * 3
let id_portable x = x
let id_writing x = x
