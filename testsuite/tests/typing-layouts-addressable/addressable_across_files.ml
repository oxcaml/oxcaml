type t_base : bits8 addressable

type t_any : any addressable

type t_product : (bits8 & value) addressable

kind_ k

type t_abstract : k addressable

type ('a : any addressable) requires_addressable
