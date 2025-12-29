type vrt  = Foo | Bar
type vrt' = Foo | Bar

type _         rcd1 = { foo: int; mutable bar: string }
type (_, _)    rcd2 = { foo: int; mutable bar: string }
type (_, _, _) rcd3 = { foo: int; mutable bar: string }

type rcd  = { foo: int; mutable bar: string }
type rcd' = { foo: int; mutable bar: string }

type urcd  = #{ foo: int; bar: int }
type urcd' = #{ foo: int; bar: int }

type ('a : immediate & immediate) box_imm_imm = { box: 'a }
