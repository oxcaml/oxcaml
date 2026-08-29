(* TEST
 flags = "-dshape";
 expect;
*)

(* CR rtjoa: The record types below have unboxed versions, which should get
   their own shapes ("[unboxed version]" entries). *)

type t = { i : int; s : string }
[%%expect{|
{
 "t"[type] -> Record_boxed<.0> { i<.1>: int ; s<.2>: string  };
 }
type t = { i : int; s : string; }
|}]

type u = t
[%%expect{|
{
 "u"[type] -> Record_boxed<.3> { i<.1>: int ; s<.2>: string  };
 }
type u = t
|}]

type v = t = { i : int; s : string }
[%%expect{|
{
 "v"[type] -> Record_boxed<.4> { i<.1>: int ; s<.2>: string  };
 }
type v = t = { i : int; s : string; }
|}]

type 'a p = { a : 'a; b : int }
[%%expect{|
{
 "p"[type] -> Abs<.7>(a/16, Record_boxed { a<.8>: a/16 ; b<.9>: int  });
 }
type 'a p = { a : 'a; b : int; }
|}]

type mix = { flt : float#; i : int }
[%%expect{|
{
 "mix"[type] -> Record_mixed<.10> { flt<.11>: float# ; i<.12>: int  };
 }
type mix = { flt : float#; i : int; }
|}]

type r = { next : r option; x : int }
[%%expect{|
{
 "r"[type] ->
   (Mutrec r/0 :=
             Record_boxed<.13> { next<.14>: Variant None<<predef:None>>
                                            | Some<<predef:Some>> of (r/0  )
             ; x<.15>: int
              };
            ).r/0;
 }
type r = { next : r option; x : int; }
|}]

type m1 = { m2 : m2; i : int }
and m2 = { m1 : m1 option }
[%%expect{|
{
 "m1"[type] ->
   (Mutrec m1/0 := Record_boxed<.16> { m2<.18>: m2/0  ; i<.19>: int  };
           m2/0 :=
             Record_boxed<.17> { m1<.20>: Variant None<<predef:None>>
                                          | Some<<predef:Some>> of (m1/0  )
              };
            ).m1/0;
 "m2"[type] ->
   (Mutrec m1/0 := Record_boxed<.16> { m2<.18>: m2/0  ; i<.19>: int  };
           m2/0 :=
             Record_boxed<.17> { m1<.20>: Variant None<<predef:None>>
                                          | Some<<predef:Some>> of (m1/0  )
              };
            ).m2/0;
 }
type m1 = { m2 : m2; i : int; }
and m2 = { m1 : m1 option; }
|}]

(* Float records have no unboxed version. *)
type fr = { f1 : float; f2 : float }
[%%expect{|
{
 "fr"[type] -> Record_floats<.21> { f1<.22>: float# ; f2<.23>: float#  };
 }
type fr = { f1 : float; f2 : float; }
|}]

module M : sig
  type mt = { j : int }
end = struct
  type mt = { j : int }
end
[%%expect{|
{
 "M"[module] -> {<.28>
                 "mt"[type] -> Record_boxed<.24> { j<.25>: int  };
                 };
 }
module M : sig type mt = { j : int; } end
|}]
