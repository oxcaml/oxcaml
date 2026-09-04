(* TEST *)

(* The 256 source fields force record duplication rather than reconstruction.
   The extension constructor adds one more word to the runtime block. *)
type t = ..
type t += Record of {
  next : t;
  f001 : int; f002 : int; f003 : int; f004 : int;
  f005 : int; f006 : int; f007 : int; f008 : int;
  f009 : int; f010 : int; f011 : int; f012 : int;
  f013 : int; f014 : int; f015 : int; f016 : int;
  f017 : int; f018 : int; f019 : int; f020 : int;
  f021 : int; f022 : int; f023 : int; f024 : int;
  f025 : int; f026 : int; f027 : int; f028 : int;
  f029 : int; f030 : int; f031 : int; f032 : int;
  f033 : int; f034 : int; f035 : int; f036 : int;
  f037 : int; f038 : int; f039 : int; f040 : int;
  f041 : int; f042 : int; f043 : int; f044 : int;
  f045 : int; f046 : int; f047 : int; f048 : int;
  f049 : int; f050 : int; f051 : int; f052 : int;
  f053 : int; f054 : int; f055 : int; f056 : int;
  f057 : int; f058 : int; f059 : int; f060 : int;
  f061 : int; f062 : int; f063 : int; f064 : int;
  f065 : int; f066 : int; f067 : int; f068 : int;
  f069 : int; f070 : int; f071 : int; f072 : int;
  f073 : int; f074 : int; f075 : int; f076 : int;
  f077 : int; f078 : int; f079 : int; f080 : int;
  f081 : int; f082 : int; f083 : int; f084 : int;
  f085 : int; f086 : int; f087 : int; f088 : int;
  f089 : int; f090 : int; f091 : int; f092 : int;
  f093 : int; f094 : int; f095 : int; f096 : int;
  f097 : int; f098 : int; f099 : int; f100 : int;
  f101 : int; f102 : int; f103 : int; f104 : int;
  f105 : int; f106 : int; f107 : int; f108 : int;
  f109 : int; f110 : int; f111 : int; f112 : int;
  f113 : int; f114 : int; f115 : int; f116 : int;
  f117 : int; f118 : int; f119 : int; f120 : int;
  f121 : int; f122 : int; f123 : int; f124 : int;
  f125 : int; f126 : int; f127 : int; f128 : int;
  f129 : int; f130 : int; f131 : int; f132 : int;
  f133 : int; f134 : int; f135 : int; f136 : int;
  f137 : int; f138 : int; f139 : int; f140 : int;
  f141 : int; f142 : int; f143 : int; f144 : int;
  f145 : int; f146 : int; f147 : int; f148 : int;
  f149 : int; f150 : int; f151 : int; f152 : int;
  f153 : int; f154 : int; f155 : int; f156 : int;
  f157 : int; f158 : int; f159 : int; f160 : int;
  f161 : int; f162 : int; f163 : int; f164 : int;
  f165 : int; f166 : int; f167 : int; f168 : int;
  f169 : int; f170 : int; f171 : int; f172 : int;
  f173 : int; f174 : int; f175 : int; f176 : int;
  f177 : int; f178 : int; f179 : int; f180 : int;
  f181 : int; f182 : int; f183 : int; f184 : int;
  f185 : int; f186 : int; f187 : int; f188 : int;
  f189 : int; f190 : int; f191 : int; f192 : int;
  f193 : int; f194 : int; f195 : int; f196 : int;
  f197 : int; f198 : int; f199 : int; f200 : int;
  f201 : int; f202 : int; f203 : int; f204 : int;
  f205 : int; f206 : int; f207 : int; f208 : int;
  f209 : int; f210 : int; f211 : int; f212 : int;
  f213 : int; f214 : int; f215 : int; f216 : int;
  f217 : int; f218 : int; f219 : int; f220 : int;
  f221 : int; f222 : int; f223 : int; f224 : int;
  f225 : int; f226 : int; f227 : int; f228 : int;
  f229 : int; f230 : int; f231 : int; f232 : int;
  f233 : int; f234 : int; f235 : int; f236 : int;
  f237 : int; f238 : int; f239 : int; f240 : int;
  f241 : int; f242 : int; f243 : int; f244 : int;
  f245 : int; f246 : int; f247 : int; f248 : int;
  f249 : int; f250 : int; f251 : int; f252 : int;
  f253 : int; f254 : int; f255 : int;
}

let rec original = Record {
  next = original;
  f001 = 0; f002 = 0; f003 = 0; f004 = 0;
  f005 = 0; f006 = 0; f007 = 0; f008 = 0;
  f009 = 0; f010 = 0; f011 = 0; f012 = 0;
  f013 = 0; f014 = 0; f015 = 0; f016 = 0;
  f017 = 0; f018 = 0; f019 = 0; f020 = 0;
  f021 = 0; f022 = 0; f023 = 0; f024 = 0;
  f025 = 0; f026 = 0; f027 = 0; f028 = 0;
  f029 = 0; f030 = 0; f031 = 0; f032 = 0;
  f033 = 0; f034 = 0; f035 = 0; f036 = 0;
  f037 = 0; f038 = 0; f039 = 0; f040 = 0;
  f041 = 0; f042 = 0; f043 = 0; f044 = 0;
  f045 = 0; f046 = 0; f047 = 0; f048 = 0;
  f049 = 0; f050 = 0; f051 = 0; f052 = 0;
  f053 = 0; f054 = 0; f055 = 0; f056 = 0;
  f057 = 0; f058 = 0; f059 = 0; f060 = 0;
  f061 = 0; f062 = 0; f063 = 0; f064 = 0;
  f065 = 0; f066 = 0; f067 = 0; f068 = 0;
  f069 = 0; f070 = 0; f071 = 0; f072 = 0;
  f073 = 0; f074 = 0; f075 = 0; f076 = 0;
  f077 = 0; f078 = 0; f079 = 0; f080 = 0;
  f081 = 0; f082 = 0; f083 = 0; f084 = 0;
  f085 = 0; f086 = 0; f087 = 0; f088 = 0;
  f089 = 0; f090 = 0; f091 = 0; f092 = 0;
  f093 = 0; f094 = 0; f095 = 0; f096 = 0;
  f097 = 0; f098 = 0; f099 = 0; f100 = 0;
  f101 = 0; f102 = 0; f103 = 0; f104 = 0;
  f105 = 0; f106 = 0; f107 = 0; f108 = 0;
  f109 = 0; f110 = 0; f111 = 0; f112 = 0;
  f113 = 0; f114 = 0; f115 = 0; f116 = 0;
  f117 = 0; f118 = 0; f119 = 0; f120 = 0;
  f121 = 0; f122 = 0; f123 = 0; f124 = 0;
  f125 = 0; f126 = 0; f127 = 0; f128 = 0;
  f129 = 0; f130 = 0; f131 = 0; f132 = 0;
  f133 = 0; f134 = 0; f135 = 0; f136 = 0;
  f137 = 0; f138 = 0; f139 = 0; f140 = 0;
  f141 = 0; f142 = 0; f143 = 0; f144 = 0;
  f145 = 0; f146 = 0; f147 = 0; f148 = 0;
  f149 = 0; f150 = 0; f151 = 0; f152 = 0;
  f153 = 0; f154 = 0; f155 = 0; f156 = 0;
  f157 = 0; f158 = 0; f159 = 0; f160 = 0;
  f161 = 0; f162 = 0; f163 = 0; f164 = 0;
  f165 = 0; f166 = 0; f167 = 0; f168 = 0;
  f169 = 0; f170 = 0; f171 = 0; f172 = 0;
  f173 = 0; f174 = 0; f175 = 0; f176 = 0;
  f177 = 0; f178 = 0; f179 = 0; f180 = 0;
  f181 = 0; f182 = 0; f183 = 0; f184 = 0;
  f185 = 0; f186 = 0; f187 = 0; f188 = 0;
  f189 = 0; f190 = 0; f191 = 0; f192 = 0;
  f193 = 0; f194 = 0; f195 = 0; f196 = 0;
  f197 = 0; f198 = 0; f199 = 0; f200 = 0;
  f201 = 0; f202 = 0; f203 = 0; f204 = 0;
  f205 = 0; f206 = 0; f207 = 0; f208 = 0;
  f209 = 0; f210 = 0; f211 = 0; f212 = 0;
  f213 = 0; f214 = 0; f215 = 0; f216 = 0;
  f217 = 0; f218 = 0; f219 = 0; f220 = 0;
  f221 = 0; f222 = 0; f223 = 0; f224 = 0;
  f225 = 0; f226 = 0; f227 = 0; f228 = 0;
  f229 = 0; f230 = 0; f231 = 0; f232 = 0;
  f233 = 0; f234 = 0; f235 = 0; f236 = 0;
  f237 = 0; f238 = 0; f239 = 0; f240 = 0;
  f241 = 0; f242 = 0; f243 = 0; f244 = 0;
  f245 = 0; f246 = 0; f247 = 0; f248 = 0;
  f249 = 0; f250 = 0; f251 = 0; f252 = 0;
  f253 = 0; f254 = 0; f255 = 0;
}

let () =
  let original = Sys.opaque_identity original in
  match original with
  | Record fields ->
    let rec copy = Record { fields with next = copy } in
    let copy = Sys.opaque_identity copy in
    assert (Obj.size (Obj.repr copy) = Obj.size (Obj.repr original))
  | _ -> failwith "expected Record"
