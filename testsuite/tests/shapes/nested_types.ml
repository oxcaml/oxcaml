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
<<<<<<< HEAD
   {<.39>
||||||| 23e84b8c4d
=======
   {<.37>
>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a
    "Exn"[extension constructor] -> {<.1>
                                     "lbl_exn"[label] -> <.0>;
                                     };
    "Ext"[extension constructor] -> {<.7>
                                     "lbl_ext"[label] -> <.6>;
                                     };
    "ext"[type] -> <.5>;
    "l"[type] -> {<.3>
                  "lbl"[label] -> <.4>;
                  };
    "t"[type] ->
      {<.9>
       "C"[constructor] -> {<.11>
                            "lbl_cstr"[label] -> <.10>;
                            };
       };
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
