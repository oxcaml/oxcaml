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
   {<.39>
    "Exn"[extension constructor] -> Record_boxed { lbl_exn<.0>: int  };
    "Ext"[extension constructor] -> Record_boxed { lbl_ext<.6>: int  };
    "ext"[type] -> ((? ) : value);
    "l"[type] -> Record_boxed { lbl<.4>: int  };
    "t"[type] -> Variant C<.11> of lbl_cstr<.10>=int ;
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
