(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                 Thomas Del Vecchio, Jane Street, New York              *)
(*                                                                        *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Global = struct
  type ('a : any) t = { global : 'a @@ global } [@@unboxed]
end

module Portable = struct
  type ('a : any) t = { portable : 'a @@ portable } [@@unboxed]
end

module Contended = struct
  type ('a : any) t = { contended : 'a @@ contended } [@@unboxed]
end

module Portended = struct
  type ('a : any) t = { portended : 'a @@ portable contended } [@@unboxed]
end

module Aliased = struct
  type ('a : any) t = { aliased : 'a @@ aliased } [@@unboxed]
end

module Shared = struct
  type ('a : any) t = { shared : 'a @@ shared } [@@unboxed]
end

module Many = struct
  type ('a : any) t = { many : 'a @@ many } [@@unboxed]
end

module Unyielding = struct
  type ('a : any) t = { unyielding : 'a @@ unyielding } [@@unboxed]
end
