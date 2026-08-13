module Module = struct end

module Module_gen = struct end [@@merlin.ppx_template_generated]

let value = ()
let value_gen = () [@@merlin.ppx_template_generated]

let func () = ()
let func_gen () = () [@@merlin.ppx_template_generated]

type t_abstract
type t_abstract_gen [@@merlin.ppx_template_generated]

type t_concrete = int
type t_concrete_gen = int [@@merlin.ppx_template_generated]

module Foo = Foo
module Foo_gen = Foo
