(* TEST *)

module Kind_with_alert = struct
  kind_ k [@@alert kind_alert "foo"]
end

(* Alerts work on uses of the kind. Alerts for these uses appear in the
   reference file. *)
module Alert_on_use_in_jkind_decl = struct
  kind_ k = Kind_with_alert.k
end

module Alert_on_use_in_type_decl = struct
  type t : Kind_with_alert.k
end

module Alert_on_use_in_type_param = struct
  type ('a : Kind_with_alert.k) t
end

(* Suppress with -alert on declarations. No alerts for these uses. *)
module Suppress_alert_in_jkind_decl_via_decl = struct
  kind_ k = Kind_with_alert.k [@@alert "-kind_alert"]
end

module Suppress_alert_in_type_decl_via_decl = struct
  type t : Kind_with_alert.k [@@alert "-kind_alert"]
end

module Suppress_alert_in_type_param_via_decl  = struct
  type ('a : Kind_with_alert.k) t [@@alert "-kind_alert"]
end

(* Suppress with -alert on module. No alerts for these uses. *)
module Suppress_alert_in_jkind_decl_via_module = struct[@alert "-kind_alert"]
  kind_ k = Kind_with_alert.k
end

module Suppress_alert_in_type_decl_via_module = struct[@alert "-kind_alert"]
  type t : Kind_with_alert.k
end

module Suppress_alert_in_type_param_via_module = struct[@alert "-kind_alert"]
  type ('a : Kind_with_alert.k) t
end
