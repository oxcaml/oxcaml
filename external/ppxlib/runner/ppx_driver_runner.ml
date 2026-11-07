let () =
  (* Forces [Deriving] to be linked: it registers -unused-code-warnings. *)
  let (_ : Ppxlib.Deriving.t -> unit) = Ppxlib.Deriving.ignore in
  Ppxlib.Driver.enable_checks ();
  Ppxlib.Driver.parse_additional_flags ~prog:Stdlib.Sys.executable_name
    ~flags:[ "-unused-code-warnings"; "true" ]

let () = Ppxlib.Driver.standalone ()
