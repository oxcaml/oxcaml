(* The [Language_extension.For_pprintast] hook used by the copy of [Pprintast]
   linked into the standard library for runtime metaprogramming. The real one
   temporarily enables all extensions so extension syntax prints; here (no
   extension machinery is available) it is a passthrough, which is fine for the
   core language. *)

module For_pprintast = struct
  type printer_exporter =
    { print_with_maximal_extensions :
        'a. (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a -> unit
    }

  let make_printer_exporter () =
    { print_with_maximal_extensions = (fun pp fmt x -> pp fmt x) }
end
