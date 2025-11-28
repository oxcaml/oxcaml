type vrt  = Foo | Bar
type vrt' = Foo | Bar

type rcd  = { foo: int; mutable bar: string }
type rcd' = { foo: int; mutable bar: string }

type urcd  = #{ foo: int; bar: int }
type urcd' = #{ foo: int; bar: int }

type ('a : immediate & immediate) box_imm_imm = { box: 'a }
