(* TEST
   flags = "-ikinds -ikinds-debug";
   expect;
*)

type t : immediate with string
[%%expect {|
[ikinds-todo] transl_declaration initial unboxed
[ikinds-todo] transl_declaration initial
[ikinds-todo] update_decl_jkind initial
[ikinds-todo] transl_declaration initial unboxed
[ikinds-todo] transl_declaration initial
[ikind] t/285[1]: Ty
[ikind] string/19!: Poly(base=[2,0,1,0,0,0,0,0,0,1,2,0,0] ⊓ string/19!.0; coeffs=[])
[ikind] t/285[1]: stored=immediate with string, base=([0,0,0,0,0,0,0,0,0,1,0,0,0] ⊓ t/285[1].0) ⊔ ([2,0,1,0,0,0,0,0,0,0,2,0,0] ⊓ string/19!.0 ⊓ t/285[1].0), coeffs=[]
[ikind] string/19!: Poly(base=[2,0,1,0,0,0,0,0,0,1,2,0,0] ⊓ string/19!.0; coeffs=[])
[ikind-subjkind] call origin=typedecl:normalize t/285[1] (Line 1, characters 0-30) allow_any=false
sub=immutable_data super=immediate with string
sub_poly=[2,0,1,0,0,0,0,0,0,1,2,0,0]
super_poly=[0,0,0,0,0,0,0,0,0,1,0,0,0] ⊔ ([2,0,1,0,0,0,0,0,0,0,2,0,0] ⊓ string/19!.0)
[ikind-subjkind] failure on axes: locality, uniqueness, externality
sub=immutable_data
super=immediate with string
Line 1, characters 0-30:
1 | type t : immediate with string
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is immutable_data
         because of the annotation on the declaration of the type t.
       But the kind of type "t" must be a subkind of immutable_data
         because of the annotation on the declaration of the type t.
|}]
