(* TEST
 flambda2;
 {
   bytecode;
 }{
   native;
 }{
   flags = "-O3";
   native;
 }{
   flags = "-Oclassic";
   native;
 }
*)

(* A functional update of a record with at least [Config.max_young_wosize]
   fields is compiled by copying the original record and then writing the
   updated fields into the copy, rather than by building a fresh block.

   Flambda 2 used to give the copy the type of the original. Records are
   immutable, so that type records the values of the original's fields, and
   the writes that perform the update did not correct it. A later read of an
   updated field was then folded to the value it had in the original record,
   and the update was silently lost. This only showed up once the simplifier
   could see both the original's fields and the read, so [update] below must
   stay inlinable for this to be a regression test. *)

type t = {
  f0 : int; f1 : int; f2 : int; f3 : int; f4 : int; f5 : int; f6 : int;
  f7 : int; f8 : int; f9 : int; f10 : int; f11 : int; f12 : int; f13 : int;
  f14 : int; f15 : int; f16 : int; f17 : int; f18 : int; f19 : int;
  f20 : int; f21 : int; f22 : int; f23 : int; f24 : int; f25 : int;
  f26 : int; f27 : int; f28 : int; f29 : int; f30 : int; f31 : int;
  f32 : int; f33 : int; f34 : int; f35 : int; f36 : int; f37 : int;
  f38 : int; f39 : int; f40 : int; f41 : int; f42 : int; f43 : int;
  f44 : int; f45 : int; f46 : int; f47 : int; f48 : int; f49 : int;
  f50 : int; f51 : int; f52 : int; f53 : int; f54 : int; f55 : int;
  f56 : int; f57 : int; f58 : int; f59 : int; f60 : int; f61 : int;
  f62 : int; f63 : int; f64 : int; f65 : int; f66 : int; f67 : int;
  f68 : int; f69 : int; f70 : int; f71 : int; f72 : int; f73 : int;
  f74 : int; f75 : int; f76 : int; f77 : int; f78 : int; f79 : int;
  f80 : int; f81 : int; f82 : int; f83 : int; f84 : int; f85 : int;
  f86 : int; f87 : int; f88 : int; f89 : int; f90 : int; f91 : int;
  f92 : int; f93 : int; f94 : int; f95 : int; f96 : int; f97 : int;
  f98 : int; f99 : int; f100 : int; f101 : int; f102 : int; f103 : int;
  f104 : int; f105 : int; f106 : int; f107 : int; f108 : int; f109 : int;
  f110 : int; f111 : int; f112 : int; f113 : int; f114 : int; f115 : int;
  f116 : int; f117 : int; f118 : int; f119 : int; f120 : int; f121 : int;
  f122 : int; f123 : int; f124 : int; f125 : int; f126 : int; f127 : int;
  f128 : int; f129 : int; f130 : int; f131 : int; f132 : int; f133 : int;
  f134 : int; f135 : int; f136 : int; f137 : int; f138 : int; f139 : int;
  f140 : int; f141 : int; f142 : int; f143 : int; f144 : int; f145 : int;
  f146 : int; f147 : int; f148 : int; f149 : int; f150 : int; f151 : int;
  f152 : int; f153 : int; f154 : int; f155 : int; f156 : int; f157 : int;
  f158 : int; f159 : int; f160 : int; f161 : int; f162 : int; f163 : int;
  f164 : int; f165 : int; f166 : int; f167 : int; f168 : int; f169 : int;
  f170 : int; f171 : int; f172 : int; f173 : int; f174 : int; f175 : int;
  f176 : int; f177 : int; f178 : int; f179 : int; f180 : int; f181 : int;
  f182 : int; f183 : int; f184 : int; f185 : int; f186 : int; f187 : int;
  f188 : int; f189 : int; f190 : int; f191 : int; f192 : int; f193 : int;
  f194 : int; f195 : int; f196 : int; f197 : int; f198 : int; f199 : int;
  f200 : int; f201 : int; f202 : int; f203 : int; f204 : int; f205 : int;
  f206 : int; f207 : int; f208 : int; f209 : int; f210 : int; f211 : int;
  f212 : int; f213 : int; f214 : int; f215 : int; f216 : int; f217 : int;
  f218 : int; f219 : int; f220 : int; f221 : int; f222 : int; f223 : int;
  f224 : int; f225 : int; f226 : int; f227 : int; f228 : int; f229 : int;
  f230 : int; f231 : int; f232 : int; f233 : int; f234 : int; f235 : int;
  f236 : int; f237 : int; f238 : int; f239 : int; f240 : int; f241 : int;
  f242 : int; f243 : int; f244 : int; f245 : int; f246 : int; f247 : int;
  f248 : int; f249 : int; f250 : int; f251 : int; f252 : int; f253 : int;
  f254 : int; f255 : int;
}

let mk () = {
  f0 = 0; f1 = 1; f2 = 2; f3 = 3; f4 = 4; f5 = 5; f6 = 6; f7 = 7; f8 = 8;
  f9 = 9; f10 = 10; f11 = 11; f12 = 12; f13 = 13; f14 = 14; f15 = 15;
  f16 = 16; f17 = 17; f18 = 18; f19 = 19; f20 = 20; f21 = 21; f22 = 22;
  f23 = 23; f24 = 24; f25 = 25; f26 = 26; f27 = 27; f28 = 28; f29 = 29;
  f30 = 30; f31 = 31; f32 = 32; f33 = 33; f34 = 34; f35 = 35; f36 = 36;
  f37 = 37; f38 = 38; f39 = 39; f40 = 40; f41 = 41; f42 = 42; f43 = 43;
  f44 = 44; f45 = 45; f46 = 46; f47 = 47; f48 = 48; f49 = 49; f50 = 50;
  f51 = 51; f52 = 52; f53 = 53; f54 = 54; f55 = 55; f56 = 56; f57 = 57;
  f58 = 58; f59 = 59; f60 = 60; f61 = 61; f62 = 62; f63 = 63; f64 = 64;
  f65 = 65; f66 = 66; f67 = 67; f68 = 68; f69 = 69; f70 = 70; f71 = 71;
  f72 = 72; f73 = 73; f74 = 74; f75 = 75; f76 = 76; f77 = 77; f78 = 78;
  f79 = 79; f80 = 80; f81 = 81; f82 = 82; f83 = 83; f84 = 84; f85 = 85;
  f86 = 86; f87 = 87; f88 = 88; f89 = 89; f90 = 90; f91 = 91; f92 = 92;
  f93 = 93; f94 = 94; f95 = 95; f96 = 96; f97 = 97; f98 = 98; f99 = 99;
  f100 = 100; f101 = 101; f102 = 102; f103 = 103; f104 = 104; f105 = 105;
  f106 = 106; f107 = 107; f108 = 108; f109 = 109; f110 = 110; f111 = 111;
  f112 = 112; f113 = 113; f114 = 114; f115 = 115; f116 = 116; f117 = 117;
  f118 = 118; f119 = 119; f120 = 120; f121 = 121; f122 = 122; f123 = 123;
  f124 = 124; f125 = 125; f126 = 126; f127 = 127; f128 = 128; f129 = 129;
  f130 = 130; f131 = 131; f132 = 132; f133 = 133; f134 = 134; f135 = 135;
  f136 = 136; f137 = 137; f138 = 138; f139 = 139; f140 = 140; f141 = 141;
  f142 = 142; f143 = 143; f144 = 144; f145 = 145; f146 = 146; f147 = 147;
  f148 = 148; f149 = 149; f150 = 150; f151 = 151; f152 = 152; f153 = 153;
  f154 = 154; f155 = 155; f156 = 156; f157 = 157; f158 = 158; f159 = 159;
  f160 = 160; f161 = 161; f162 = 162; f163 = 163; f164 = 164; f165 = 165;
  f166 = 166; f167 = 167; f168 = 168; f169 = 169; f170 = 170; f171 = 171;
  f172 = 172; f173 = 173; f174 = 174; f175 = 175; f176 = 176; f177 = 177;
  f178 = 178; f179 = 179; f180 = 180; f181 = 181; f182 = 182; f183 = 183;
  f184 = 184; f185 = 185; f186 = 186; f187 = 187; f188 = 188; f189 = 189;
  f190 = 190; f191 = 191; f192 = 192; f193 = 193; f194 = 194; f195 = 195;
  f196 = 196; f197 = 197; f198 = 198; f199 = 199; f200 = 200; f201 = 201;
  f202 = 202; f203 = 203; f204 = 204; f205 = 205; f206 = 206; f207 = 207;
  f208 = 208; f209 = 209; f210 = 210; f211 = 211; f212 = 212; f213 = 213;
  f214 = 214; f215 = 215; f216 = 216; f217 = 217; f218 = 218; f219 = 219;
  f220 = 220; f221 = 221; f222 = 222; f223 = 223; f224 = 224; f225 = 225;
  f226 = 226; f227 = 227; f228 = 228; f229 = 229; f230 = 230; f231 = 231;
  f232 = 232; f233 = 233; f234 = 234; f235 = 235; f236 = 236; f237 = 237;
  f238 = 238; f239 = 239; f240 = 240; f241 = 241; f242 = 242; f243 = 243;
  f244 = 244; f245 = 245; f246 = 246; f247 = 247; f248 = 248; f249 = 249;
  f250 = 250; f251 = 251; f252 = 252; f253 = 253; f254 = 254; f255 = 255;
}

let update r x = { r with f0 = x }

let[@inline never] update_no_inline r x = { r with f0 = x }

let () =
  let r = mk () in
  let a = update r 4242 in
  Printf.printf "inlined:     f0 = %d, f1 = %d\n" a.f0 a.f1;
  let b = update_no_inline r 4242 in
  Printf.printf "not inlined: f0 = %d, f1 = %d\n" b.f0 b.f1;
  Printf.printf "original:    f0 = %d, f1 = %d\n" r.f0 r.f1
