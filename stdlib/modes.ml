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
  type ('a : value_or_null) t = { global : 'a @@ global } [@@unboxed]
end

module Portable = struct
  type ('a : value_or_null) t = { portable : 'a @@ portable } [@@unboxed]
end

module Contended = struct
  type ('a : value_or_null) t = { contended : 'a @@ contended } [@@unboxed]
end

module Portended = struct
  type ('a : value_or_null) t =
    { portended : 'a @@ portable contended }
  [@@unboxed]
end

module Aliased = struct
  type ('a : value_or_null) t = { aliased : 'a @@ aliased } [@@unboxed]
end

module Shared = struct
  type ('a : value_or_null) t = { shared : 'a @@ shared } [@@unboxed]
end

module Many = struct
  type ('a : value_or_null) t = { many : 'a @@ many } [@@unboxed]
end

module Unyielding = struct
  type ('a : value_or_null) t = { unyielding : 'a @@ unyielding } [@@unboxed]
end
