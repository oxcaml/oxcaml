(* TEST
 include stdlib_upstream_compatible;
 flags = "-extension layouts_alpha";
 {
   native;
 }{
   bytecode;
 }
*)

(* The inline-record counterpart of functional_update_any_record_big.ml *)

module Float_u = Stdlib_upstream_compatible.Float_u

type ('a : any) big = Big of {
  x : 'a; f0 : float#; f1 : float#; f2 : float#; f3 : float#; f4 : float#;
  f5 : float#; f6 : float#; f7 : float#; f8 : float#; f9 : float#; f10 : float#;
  f11 : float#; f12 : float#; f13 : float#; f14 : float#; f15 : float#;
  f16 : float#; f17 : float#; f18 : float#; f19 : float#; f20 : float#;
  f21 : float#; f22 : float#; f23 : float#; f24 : float#; f25 : float#;
  f26 : float#; f27 : float#; f28 : float#; f29 : float#; f30 : float#;
  f31 : float#; f32 : float#; f33 : float#; f34 : float#; f35 : float#;
  f36 : float#; f37 : float#; f38 : float#; f39 : float#; f40 : float#;
  f41 : float#; f42 : float#; f43 : float#; f44 : float#; f45 : float#;
  f46 : float#; f47 : float#; f48 : float#; f49 : float#; f50 : float#;
  f51 : float#; f52 : float#; f53 : float#; f54 : float#; f55 : float#;
  f56 : float#; f57 : float#; f58 : float#; f59 : float#; f60 : float#;
  f61 : float#; f62 : float#; f63 : float#; f64 : float#; f65 : float#;
  f66 : float#; f67 : float#; f68 : float#; f69 : float#; f70 : float#;
  f71 : float#; f72 : float#; f73 : float#; f74 : float#; f75 : float#;
  f76 : float#; f77 : float#; f78 : float#; f79 : float#; f80 : float#;
  f81 : float#; f82 : float#; f83 : float#; f84 : float#; f85 : float#;
  f86 : float#; f87 : float#; f88 : float#; f89 : float#; f90 : float#;
  f91 : float#; f92 : float#; f93 : float#; f94 : float#; f95 : float#;
  f96 : float#; f97 : float#; f98 : float#; f99 : float#; f100 : float#;
  f101 : float#; f102 : float#; f103 : float#; f104 : float#; f105 : float#;
  f106 : float#; f107 : float#; f108 : float#; f109 : float#; f110 : float#;
  f111 : float#; f112 : float#; f113 : float#; f114 : float#; f115 : float#;
  f116 : float#; f117 : float#; f118 : float#; f119 : float#; f120 : float#;
  f121 : float#; f122 : float#; f123 : float#; f124 : float#; f125 : float#;
  f126 : float#; f127 : float#; f128 : float#; f129 : float#; f130 : float#;
  f131 : float#; f132 : float#; f133 : float#; f134 : float#; f135 : float#;
  f136 : float#; f137 : float#; f138 : float#; f139 : float#; f140 : float#;
  f141 : float#; f142 : float#; f143 : float#; f144 : float#; f145 : float#;
  f146 : float#; f147 : float#; f148 : float#; f149 : float#; f150 : float#;
  f151 : float#; f152 : float#; f153 : float#; f154 : float#; f155 : float#;
  f156 : float#; f157 : float#; f158 : float#; f159 : float#; f160 : float#;
  f161 : float#; f162 : float#; f163 : float#; f164 : float#; f165 : float#;
  f166 : float#; f167 : float#; f168 : float#; f169 : float#; f170 : float#;
  f171 : float#; f172 : float#; f173 : float#; f174 : float#; f175 : float#;
  f176 : float#; f177 : float#; f178 : float#; f179 : float#; f180 : float#;
  f181 : float#; f182 : float#; f183 : float#; f184 : float#; f185 : float#;
  f186 : float#; f187 : float#; f188 : float#; f189 : float#; f190 : float#;
  f191 : float#; f192 : float#; f193 : float#; f194 : float#; f195 : float#;
  f196 : float#; f197 : float#; f198 : float#; f199 : float#; f200 : float#;
  f201 : float#; f202 : float#; f203 : float#; f204 : float#; f205 : float#;
  f206 : float#; f207 : float#; f208 : float#; f209 : float#; f210 : float#;
  f211 : float#; f212 : float#; f213 : float#; f214 : float#; f215 : float#;
  f216 : float#; f217 : float#; f218 : float#; f219 : float#; f220 : float#;
  f221 : float#; f222 : float#; f223 : float#; f224 : float#; f225 : float#;
  f226 : float#; f227 : float#; f228 : float#; f229 : float#; f230 : float#;
  f231 : float#; f232 : float#; f233 : float#; f234 : float#; f235 : float#;
  f236 : float#; f237 : float#; f238 : float#; f239 : float#; f240 : float#;
  f241 : float#; f242 : float#; f243 : float#; f244 : float#; f245 : float#;
  f246 : float#; f247 : float#; f248 : float#; f249 : float#; f250 : float#;
  f251 : float#; f252 : float#; f253 : float#; y : int;
}

let mk () : int big =
  Big {
    x = 5; f0 = #0.5; f1 = #1.5; f2 = #2.5; f3 = #3.5; f4 = #4.5; f5 = #5.5;
    f6 = #6.5; f7 = #7.5; f8 = #8.5; f9 = #9.5; f10 = #10.5; f11 = #11.5;
    f12 = #12.5; f13 = #13.5; f14 = #14.5; f15 = #15.5; f16 = #16.5;
    f17 = #17.5; f18 = #18.5; f19 = #19.5; f20 = #20.5; f21 = #21.5;
    f22 = #22.5; f23 = #23.5; f24 = #24.5; f25 = #25.5; f26 = #26.5;
    f27 = #27.5; f28 = #28.5; f29 = #29.5; f30 = #30.5; f31 = #31.5;
    f32 = #32.5; f33 = #33.5; f34 = #34.5; f35 = #35.5; f36 = #36.5;
    f37 = #37.5; f38 = #38.5; f39 = #39.5; f40 = #40.5; f41 = #41.5;
    f42 = #42.5; f43 = #43.5; f44 = #44.5; f45 = #45.5; f46 = #46.5;
    f47 = #47.5; f48 = #48.5; f49 = #49.5; f50 = #50.5; f51 = #51.5;
    f52 = #52.5; f53 = #53.5; f54 = #54.5; f55 = #55.5; f56 = #56.5;
    f57 = #57.5; f58 = #58.5; f59 = #59.5; f60 = #60.5; f61 = #61.5;
    f62 = #62.5; f63 = #63.5; f64 = #64.5; f65 = #65.5; f66 = #66.5;
    f67 = #67.5; f68 = #68.5; f69 = #69.5; f70 = #70.5; f71 = #71.5;
    f72 = #72.5; f73 = #73.5; f74 = #74.5; f75 = #75.5; f76 = #76.5;
    f77 = #77.5; f78 = #78.5; f79 = #79.5; f80 = #80.5; f81 = #81.5;
    f82 = #82.5; f83 = #83.5; f84 = #84.5; f85 = #85.5; f86 = #86.5;
    f87 = #87.5; f88 = #88.5; f89 = #89.5; f90 = #90.5; f91 = #91.5;
    f92 = #92.5; f93 = #93.5; f94 = #94.5; f95 = #95.5; f96 = #96.5;
    f97 = #97.5; f98 = #98.5; f99 = #99.5; f100 = #100.5; f101 = #101.5;
    f102 = #102.5; f103 = #103.5; f104 = #104.5; f105 = #105.5; f106 = #106.5;
    f107 = #107.5; f108 = #108.5; f109 = #109.5; f110 = #110.5; f111 = #111.5;
    f112 = #112.5; f113 = #113.5; f114 = #114.5; f115 = #115.5; f116 = #116.5;
    f117 = #117.5; f118 = #118.5; f119 = #119.5; f120 = #120.5; f121 = #121.5;
    f122 = #122.5; f123 = #123.5; f124 = #124.5; f125 = #125.5; f126 = #126.5;
    f127 = #127.5; f128 = #128.5; f129 = #129.5; f130 = #130.5; f131 = #131.5;
    f132 = #132.5; f133 = #133.5; f134 = #134.5; f135 = #135.5; f136 = #136.5;
    f137 = #137.5; f138 = #138.5; f139 = #139.5; f140 = #140.5; f141 = #141.5;
    f142 = #142.5; f143 = #143.5; f144 = #144.5; f145 = #145.5; f146 = #146.5;
    f147 = #147.5; f148 = #148.5; f149 = #149.5; f150 = #150.5; f151 = #151.5;
    f152 = #152.5; f153 = #153.5; f154 = #154.5; f155 = #155.5; f156 = #156.5;
    f157 = #157.5; f158 = #158.5; f159 = #159.5; f160 = #160.5; f161 = #161.5;
    f162 = #162.5; f163 = #163.5; f164 = #164.5; f165 = #165.5; f166 = #166.5;
    f167 = #167.5; f168 = #168.5; f169 = #169.5; f170 = #170.5; f171 = #171.5;
    f172 = #172.5; f173 = #173.5; f174 = #174.5; f175 = #175.5; f176 = #176.5;
    f177 = #177.5; f178 = #178.5; f179 = #179.5; f180 = #180.5; f181 = #181.5;
    f182 = #182.5; f183 = #183.5; f184 = #184.5; f185 = #185.5; f186 = #186.5;
    f187 = #187.5; f188 = #188.5; f189 = #189.5; f190 = #190.5; f191 = #191.5;
    f192 = #192.5; f193 = #193.5; f194 = #194.5; f195 = #195.5; f196 = #196.5;
    f197 = #197.5; f198 = #198.5; f199 = #199.5; f200 = #200.5; f201 = #201.5;
    f202 = #202.5; f203 = #203.5; f204 = #204.5; f205 = #205.5; f206 = #206.5;
    f207 = #207.5; f208 = #208.5; f209 = #209.5; f210 = #210.5; f211 = #211.5;
    f212 = #212.5; f213 = #213.5; f214 = #214.5; f215 = #215.5; f216 = #216.5;
    f217 = #217.5; f218 = #218.5; f219 = #219.5; f220 = #220.5; f221 = #221.5;
    f222 = #222.5; f223 = #223.5; f224 = #224.5; f225 = #225.5; f226 = #226.5;
    f227 = #227.5; f228 = #228.5; f229 = #229.5; f230 = #230.5; f231 = #231.5;
    f232 = #232.5; f233 = #233.5; f234 = #234.5; f235 = #235.5; f236 = #236.5;
    f237 = #237.5; f238 = #238.5; f239 = #239.5; f240 = #240.5; f241 = #241.5;
    f242 = #242.5; f243 = #243.5; f244 = #244.5; f245 = #245.5; f246 = #246.5;
    f247 = #247.5; f248 = #248.5; f249 = #249.5; f250 = #250.5; f251 = #251.5;
    f252 = #252.5; f253 = #253.5; y = 999;
  }

let check name (f0 : float#) (f100 : float#) (f253 : float#) (y : int) =
  Printf.printf "%s: f0=%.1f f100=%.1f f253=%.1f y=%d\n" name
    (Float_u.to_float f0) (Float_u.to_float f100) (Float_u.to_float f253) y

(* Changes the representation, so shouldn't be compiled via Pduprecord *)
let () =
  let update_x (r : int big) (x : float#) : float# big =
    match r with Big r -> Big { r with x }
  in
  match update_x (mk ()) #2.5 with
  | Big r ->
    Printf.printf "cross x=%.1f " (Float_u.to_float r.x);
    check "cross" r.f0 r.f100 r.f253 r.y

(* CR rtjoa: Also test a representation-preserving update, once big blocks
   copied via Pduprecord can be updated soundly. *)
