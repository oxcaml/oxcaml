(* TEST
   flambda;
   native;
*)

let[@inline never] next msg () =
  (*
  let r = ref [] in
  for i = 1 to 100_000 do r:= List.init 10_000 (fun _ -> 13) done;
  ignore (Sys.opaque_identity r);
     *)
  ignore (Gc.major_slice 100);
  (print_endline[@inlined never]) msg;
  1
;;


(* once compiled, this code currently results
   in roughly the following:
   ```
   *alloc* = caml_alloc_shr_check_gc (closure_tag, ...)
   store (+a *alloc* 6160) (apply 'next' "a" ())
   ```
   and therefore the funciton `next` is called while the
   *alloc* value is a root and can be scanned, however at that
   point, it's a block with a closure tag, but its first
   arity+start-of-env field is not set, breaking the invariant
   mentionned in a comment written for `caml_alloc_shr_check_gc`;
   it's not trivial to reproduce as a segfault/runtime error because
   for that we would need the GC to partially scan the closure block
   during the call to `next` (but only partially), and then resume
   it later, which is surprisingly hard to do *)
let[@inline never] test_big () =
  let a = next "a" () in
  let b = 0 in
  let rec f0 x0 = f256 x0 + a + b
  and f1 x1 = f0 x1 + a + b
  and f2 x2 = f1 x2 + a + b
  and f3 x3 = f2 x3 + a + b
  and f4 x4 = f3 x4 + a + b
  and f5 x5 = f4 x5 + a + b
  and f6 x6 = f5 x6 + a + b
  and f7 x7 = f6 x7 + a + b
  and f8 x8 = f7 x8 + a + b
  and f9 x9 = f8 x9 + a + b
  and f10 x10 = f9 x10 + a + b
  and f11 x11 = f10 x11 + a + b
  and f12 x12 = f11 x12 + a + b
  and f13 x13 = f12 x13 + a + b
  and f14 x14 = f13 x14 + a + b
  and f15 x15 = f14 x15 + a + b
  and f16 x16 = f15 x16 + a + b
  and f17 x17 = f16 x17 + a + b
  and f18 x18 = f17 x18 + a + b
  and f19 x19 = f18 x19 + a + b
  and f20 x20 = f19 x20 + a + b
  and f21 x21 = f20 x21 + a + b
  and f22 x22 = f21 x22 + a + b
  and f23 x23 = f22 x23 + a + b
  and f24 x24 = f23 x24 + a + b
  and f25 x25 = f24 x25 + a + b
  and f26 x26 = f25 x26 + a + b
  and f27 x27 = f26 x27 + a + b
  and f28 x28 = f27 x28 + a + b
  and f29 x29 = f28 x29 + a + b
  and f30 x30 = f29 x30 + a + b
  and f31 x31 = f30 x31 + a + b
  and f32 x32 = f31 x32 + a + b
  and f33 x33 = f32 x33 + a + b
  and f34 x34 = f33 x34 + a + b
  and f35 x35 = f34 x35 + a + b
  and f36 x36 = f35 x36 + a + b
  and f37 x37 = f36 x37 + a + b
  and f38 x38 = f37 x38 + a + b
  and f39 x39 = f38 x39 + a + b
  and f40 x40 = f39 x40 + a + b
  and f41 x41 = f40 x41 + a + b
  and f42 x42 = f41 x42 + a + b
  and f43 x43 = f42 x43 + a + b
  and f44 x44 = f43 x44 + a + b
  and f45 x45 = f44 x45 + a + b
  and f46 x46 = f45 x46 + a + b
  and f47 x47 = f46 x47 + a + b
  and f48 x48 = f47 x48 + a + b
  and f49 x49 = f48 x49 + a + b
  and f50 x50 = f49 x50 + a + b
  and f51 x51 = f50 x51 + a + b
  and f52 x52 = f51 x52 + a + b
  and f53 x53 = f52 x53 + a + b
  and f54 x54 = f53 x54 + a + b
  and f55 x55 = f54 x55 + a + b
  and f56 x56 = f55 x56 + a + b
  and f57 x57 = f56 x57 + a + b
  and f58 x58 = f57 x58 + a + b
  and f59 x59 = f58 x59 + a + b
  and f60 x60 = f59 x60 + a + b
  and f61 x61 = f60 x61 + a + b
  and f62 x62 = f61 x62 + a + b
  and f63 x63 = f62 x63 + a + b
  and f64 x64 = f63 x64 + a + b
  and f65 x65 = f64 x65 + a + b
  and f66 x66 = f65 x66 + a + b
  and f67 x67 = f66 x67 + a + b
  and f68 x68 = f67 x68 + a + b
  and f69 x69 = f68 x69 + a + b
  and f70 x70 = f69 x70 + a + b
  and f71 x71 = f70 x71 + a + b
  and f72 x72 = f71 x72 + a + b
  and f73 x73 = f72 x73 + a + b
  and f74 x74 = f73 x74 + a + b
  and f75 x75 = f74 x75 + a + b
  and f76 x76 = f75 x76 + a + b
  and f77 x77 = f76 x77 + a + b
  and f78 x78 = f77 x78 + a + b
  and f79 x79 = f78 x79 + a + b
  and f80 x80 = f79 x80 + a + b
  and f81 x81 = f80 x81 + a + b
  and f82 x82 = f81 x82 + a + b
  and f83 x83 = f82 x83 + a + b
  and f84 x84 = f83 x84 + a + b
  and f85 x85 = f84 x85 + a + b
  and f86 x86 = f85 x86 + a + b
  and f87 x87 = f86 x87 + a + b
  and f88 x88 = f87 x88 + a + b
  and f89 x89 = f88 x89 + a + b
  and f90 x90 = f89 x90 + a + b
  and f91 x91 = f90 x91 + a + b
  and f92 x92 = f91 x92 + a + b
  and f93 x93 = f92 x93 + a + b
  and f94 x94 = f93 x94 + a + b
  and f95 x95 = f94 x95 + a + b
  and f96 x96 = f95 x96 + a + b
  and f97 x97 = f96 x97 + a + b
  and f98 x98 = f97 x98 + a + b
  and f99 x99 = f98 x99 + a + b
  and f100 x100 = f99 x100 + a + b
  and f101 x101 = f100 x101 + a + b
  and f102 x102 = f101 x102 + a + b
  and f103 x103 = f102 x103 + a + b
  and f104 x104 = f103 x104 + a + b
  and f105 x105 = f104 x105 + a + b
  and f106 x106 = f105 x106 + a + b
  and f107 x107 = f106 x107 + a + b
  and f108 x108 = f107 x108 + a + b
  and f109 x109 = f108 x109 + a + b
  and f110 x110 = f109 x110 + a + b
  and f111 x111 = f110 x111 + a + b
  and f112 x112 = f111 x112 + a + b
  and f113 x113 = f112 x113 + a + b
  and f114 x114 = f113 x114 + a + b
  and f115 x115 = f114 x115 + a + b
  and f116 x116 = f115 x116 + a + b
  and f117 x117 = f116 x117 + a + b
  and f118 x118 = f117 x118 + a + b
  and f119 x119 = f118 x119 + a + b
  and f120 x120 = f119 x120 + a + b
  and f121 x121 = f120 x121 + a + b
  and f122 x122 = f121 x122 + a + b
  and f123 x123 = f122 x123 + a + b
  and f124 x124 = f123 x124 + a + b
  and f125 x125 = f124 x125 + a + b
  and f126 x126 = f125 x126 + a + b
  and f127 x127 = f126 x127 + a + b
  and f128 x128 = f127 x128 + a + b
  and f129 x129 = f128 x129 + a + b
  and f130 x130 = f129 x130 + a + b
  and f131 x131 = f130 x131 + a + b
  and f132 x132 = f131 x132 + a + b
  and f133 x133 = f132 x133 + a + b
  and f134 x134 = f133 x134 + a + b
  and f135 x135 = f134 x135 + a + b
  and f136 x136 = f135 x136 + a + b
  and f137 x137 = f136 x137 + a + b
  and f138 x138 = f137 x138 + a + b
  and f139 x139 = f138 x139 + a + b
  and f140 x140 = f139 x140 + a + b
  and f141 x141 = f140 x141 + a + b
  and f142 x142 = f141 x142 + a + b
  and f143 x143 = f142 x143 + a + b
  and f144 x144 = f143 x144 + a + b
  and f145 x145 = f144 x145 + a + b
  and f146 x146 = f145 x146 + a + b
  and f147 x147 = f146 x147 + a + b
  and f148 x148 = f147 x148 + a + b
  and f149 x149 = f148 x149 + a + b
  and f150 x150 = f149 x150 + a + b
  and f151 x151 = f150 x151 + a + b
  and f152 x152 = f151 x152 + a + b
  and f153 x153 = f152 x153 + a + b
  and f154 x154 = f153 x154 + a + b
  and f155 x155 = f154 x155 + a + b
  and f156 x156 = f155 x156 + a + b
  and f157 x157 = f156 x157 + a + b
  and f158 x158 = f157 x158 + a + b
  and f159 x159 = f158 x159 + a + b
  and f160 x160 = f159 x160 + a + b
  and f161 x161 = f160 x161 + a + b
  and f162 x162 = f161 x162 + a + b
  and f163 x163 = f162 x163 + a + b
  and f164 x164 = f163 x164 + a + b
  and f165 x165 = f164 x165 + a + b
  and f166 x166 = f165 x166 + a + b
  and f167 x167 = f166 x167 + a + b
  and f168 x168 = f167 x168 + a + b
  and f169 x169 = f168 x169 + a + b
  and f170 x170 = f169 x170 + a + b
  and f171 x171 = f170 x171 + a + b
  and f172 x172 = f171 x172 + a + b
  and f173 x173 = f172 x173 + a + b
  and f174 x174 = f173 x174 + a + b
  and f175 x175 = f174 x175 + a + b
  and f176 x176 = f175 x176 + a + b
  and f177 x177 = f176 x177 + a + b
  and f178 x178 = f177 x178 + a + b
  and f179 x179 = f178 x179 + a + b
  and f180 x180 = f179 x180 + a + b
  and f181 x181 = f180 x181 + a + b
  and f182 x182 = f181 x182 + a + b
  and f183 x183 = f182 x183 + a + b
  and f184 x184 = f183 x184 + a + b
  and f185 x185 = f184 x185 + a + b
  and f186 x186 = f185 x186 + a + b
  and f187 x187 = f186 x187 + a + b
  and f188 x188 = f187 x188 + a + b
  and f189 x189 = f188 x189 + a + b
  and f190 x190 = f189 x190 + a + b
  and f191 x191 = f190 x191 + a + b
  and f192 x192 = f191 x192 + a + b
  and f193 x193 = f192 x193 + a + b
  and f194 x194 = f193 x194 + a + b
  and f195 x195 = f194 x195 + a + b
  and f196 x196 = f195 x196 + a + b
  and f197 x197 = f196 x197 + a + b
  and f198 x198 = f197 x198 + a + b
  and f199 x199 = f198 x199 + a + b
  and f200 x200 = f199 x200 + a + b
  and f201 x201 = f200 x201 + a + b
  and f202 x202 = f201 x202 + a + b
  and f203 x203 = f202 x203 + a + b
  and f204 x204 = f203 x204 + a + b
  and f205 x205 = f204 x205 + a + b
  and f206 x206 = f205 x206 + a + b
  and f207 x207 = f206 x207 + a + b
  and f208 x208 = f207 x208 + a + b
  and f209 x209 = f208 x209 + a + b
  and f210 x210 = f209 x210 + a + b
  and f211 x211 = f210 x211 + a + b
  and f212 x212 = f211 x212 + a + b
  and f213 x213 = f212 x213 + a + b
  and f214 x214 = f213 x214 + a + b
  and f215 x215 = f214 x215 + a + b
  and f216 x216 = f215 x216 + a + b
  and f217 x217 = f216 x217 + a + b
  and f218 x218 = f217 x218 + a + b
  and f219 x219 = f218 x219 + a + b
  and f220 x220 = f219 x220 + a + b
  and f221 x221 = f220 x221 + a + b
  and f222 x222 = f221 x222 + a + b
  and f223 x223 = f222 x223 + a + b
  and f224 x224 = f223 x224 + a + b
  and f225 x225 = f224 x225 + a + b
  and f226 x226 = f225 x226 + a + b
  and f227 x227 = f226 x227 + a + b
  and f228 x228 = f227 x228 + a + b
  and f229 x229 = f228 x229 + a + b
  and f230 x230 = f229 x230 + a + b
  and f231 x231 = f230 x231 + a + b
  and f232 x232 = f231 x232 + a + b
  and f233 x233 = f232 x233 + a + b
  and f234 x234 = f233 x234 + a + b
  and f235 x235 = f234 x235 + a + b
  and f236 x236 = f235 x236 + a + b
  and f237 x237 = f236 x237 + a + b
  and f238 x238 = f237 x238 + a + b
  and f239 x239 = f238 x239 + a + b
  and f240 x240 = f239 x240 + a + b
  and f241 x241 = f240 x241 + a + b
  and f242 x242 = f241 x242 + a + b
  and f243 x243 = f242 x243 + a + b
  and f244 x244 = f243 x244 + a + b
  and f245 x245 = f244 x245 + a + b
  and f246 x246 = f245 x246 + a + b
  and f247 x247 = f246 x247 + a + b
  and f248 x248 = f247 x248 + a + b
  and f249 x249 = f248 x249 + a + b
  and f250 x250 = f249 x250 + a + b
  and f251 x251 = f250 x251 + a + b
  and f252 x252 = f251 x252 + a + b
  and f253 x253 = f252 x253 + a + b
  and f254 x254 = f253 x254 + a + b
  and f255 x255 = f254 x255 + a + b
  and f256 x256 = f255 x256 + a + b
  in
  f0

let () =
  Gc.set ({ (Gc.get ()) with minor_heap_size = 900; });
  let r = ref [] in
  for i = 1 to 11 do
    r := List.init 10_000 (fun _ -> 13) :: !r;
    if i mod 5 = 0 then Gc.full_major ();
  done;
  ignore (test_big ());
  ignore (Sys.opaque_identity r);
  ()


