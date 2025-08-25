(* TEST
 flags = "-dshape";
 expect;
*)

module M : sig

  exception Exn of { lbl_exn : int }
  type l = { lbl : int }
  type ext = ..
  type ext += Ext of { lbl_ext : int }
  type t = C of { lbl_cstr : int }
end = struct
  exception Exn of { lbl_exn : int }
  type l = { lbl : int }
  type ext = ..
  type ext += Ext of { lbl_ext : int }
  type t = C of { lbl_cstr : int }
end
[%%expect{|
{
 "M"[module] ->
   {<.52>
    "Exn"[extension constructor] ->
      Record_boxed { lbl_exn<.5>: Predef int ()
       };
    "Ext"[extension constructor] ->
      Record_boxed { lbl_ext<.13>: Predef int ()
       };
    "ext"[type] -> <.12>;
    "l"[type] -> Record_boxed { lbl<.10>: Predef int ()  };
    "t"[type] -> Variant C<.19> of lbl_cstr<.18>=Predef int () ;
    };
 }
module M :
  sig
    exception Exn of { lbl_exn : int; }
    type l = { lbl : int; }
    type ext = ..
    type ext += Ext of { lbl_ext : int; }
    type t = C of { lbl_cstr : int; }
  end
|}]
