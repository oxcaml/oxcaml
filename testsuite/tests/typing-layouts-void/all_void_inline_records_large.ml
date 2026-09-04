(* TEST
 flambda2;
 flags = "-extension layouts_beta";
 { bytecode; native; }
 { flags += " -Oclassic"; native; }
 { flags += " -O3"; native; }
*)

(* A has a payload, so B has block tag 1. With 256 source fields, the
   partial update uses record duplication even though its native payload
   is empty. Recursive initialization must preserve B. *)
type t = A of int | B of {
  f000 : unit#; f001 : unit#; f002 : unit#; f003 : unit#;
  f004 : unit#; f005 : unit#; f006 : unit#; f007 : unit#;
  f008 : unit#; f009 : unit#; f010 : unit#; f011 : unit#;
  f012 : unit#; f013 : unit#; f014 : unit#; f015 : unit#;
  f016 : unit#; f017 : unit#; f018 : unit#; f019 : unit#;
  f020 : unit#; f021 : unit#; f022 : unit#; f023 : unit#;
  f024 : unit#; f025 : unit#; f026 : unit#; f027 : unit#;
  f028 : unit#; f029 : unit#; f030 : unit#; f031 : unit#;
  f032 : unit#; f033 : unit#; f034 : unit#; f035 : unit#;
  f036 : unit#; f037 : unit#; f038 : unit#; f039 : unit#;
  f040 : unit#; f041 : unit#; f042 : unit#; f043 : unit#;
  f044 : unit#; f045 : unit#; f046 : unit#; f047 : unit#;
  f048 : unit#; f049 : unit#; f050 : unit#; f051 : unit#;
  f052 : unit#; f053 : unit#; f054 : unit#; f055 : unit#;
  f056 : unit#; f057 : unit#; f058 : unit#; f059 : unit#;
  f060 : unit#; f061 : unit#; f062 : unit#; f063 : unit#;
  f064 : unit#; f065 : unit#; f066 : unit#; f067 : unit#;
  f068 : unit#; f069 : unit#; f070 : unit#; f071 : unit#;
  f072 : unit#; f073 : unit#; f074 : unit#; f075 : unit#;
  f076 : unit#; f077 : unit#; f078 : unit#; f079 : unit#;
  f080 : unit#; f081 : unit#; f082 : unit#; f083 : unit#;
  f084 : unit#; f085 : unit#; f086 : unit#; f087 : unit#;
  f088 : unit#; f089 : unit#; f090 : unit#; f091 : unit#;
  f092 : unit#; f093 : unit#; f094 : unit#; f095 : unit#;
  f096 : unit#; f097 : unit#; f098 : unit#; f099 : unit#;
  f100 : unit#; f101 : unit#; f102 : unit#; f103 : unit#;
  f104 : unit#; f105 : unit#; f106 : unit#; f107 : unit#;
  f108 : unit#; f109 : unit#; f110 : unit#; f111 : unit#;
  f112 : unit#; f113 : unit#; f114 : unit#; f115 : unit#;
  f116 : unit#; f117 : unit#; f118 : unit#; f119 : unit#;
  f120 : unit#; f121 : unit#; f122 : unit#; f123 : unit#;
  f124 : unit#; f125 : unit#; f126 : unit#; f127 : unit#;
  f128 : unit#; f129 : unit#; f130 : unit#; f131 : unit#;
  f132 : unit#; f133 : unit#; f134 : unit#; f135 : unit#;
  f136 : unit#; f137 : unit#; f138 : unit#; f139 : unit#;
  f140 : unit#; f141 : unit#; f142 : unit#; f143 : unit#;
  f144 : unit#; f145 : unit#; f146 : unit#; f147 : unit#;
  f148 : unit#; f149 : unit#; f150 : unit#; f151 : unit#;
  f152 : unit#; f153 : unit#; f154 : unit#; f155 : unit#;
  f156 : unit#; f157 : unit#; f158 : unit#; f159 : unit#;
  f160 : unit#; f161 : unit#; f162 : unit#; f163 : unit#;
  f164 : unit#; f165 : unit#; f166 : unit#; f167 : unit#;
  f168 : unit#; f169 : unit#; f170 : unit#; f171 : unit#;
  f172 : unit#; f173 : unit#; f174 : unit#; f175 : unit#;
  f176 : unit#; f177 : unit#; f178 : unit#; f179 : unit#;
  f180 : unit#; f181 : unit#; f182 : unit#; f183 : unit#;
  f184 : unit#; f185 : unit#; f186 : unit#; f187 : unit#;
  f188 : unit#; f189 : unit#; f190 : unit#; f191 : unit#;
  f192 : unit#; f193 : unit#; f194 : unit#; f195 : unit#;
  f196 : unit#; f197 : unit#; f198 : unit#; f199 : unit#;
  f200 : unit#; f201 : unit#; f202 : unit#; f203 : unit#;
  f204 : unit#; f205 : unit#; f206 : unit#; f207 : unit#;
  f208 : unit#; f209 : unit#; f210 : unit#; f211 : unit#;
  f212 : unit#; f213 : unit#; f214 : unit#; f215 : unit#;
  f216 : unit#; f217 : unit#; f218 : unit#; f219 : unit#;
  f220 : unit#; f221 : unit#; f222 : unit#; f223 : unit#;
  f224 : unit#; f225 : unit#; f226 : unit#; f227 : unit#;
  f228 : unit#; f229 : unit#; f230 : unit#; f231 : unit#;
  f232 : unit#; f233 : unit#; f234 : unit#; f235 : unit#;
  f236 : unit#; f237 : unit#; f238 : unit#; f239 : unit#;
  f240 : unit#; f241 : unit#; f242 : unit#; f243 : unit#;
  f244 : unit#; f245 : unit#; f246 : unit#; f247 : unit#;
  f248 : unit#; f249 : unit#; f250 : unit#; f251 : unit#;
  f252 : unit#; f253 : unit#; f254 : unit#; f255 : unit#;
}

let make () = B {
  f000 = #(); f001 = #(); f002 = #(); f003 = #();
  f004 = #(); f005 = #(); f006 = #(); f007 = #();
  f008 = #(); f009 = #(); f010 = #(); f011 = #();
  f012 = #(); f013 = #(); f014 = #(); f015 = #();
  f016 = #(); f017 = #(); f018 = #(); f019 = #();
  f020 = #(); f021 = #(); f022 = #(); f023 = #();
  f024 = #(); f025 = #(); f026 = #(); f027 = #();
  f028 = #(); f029 = #(); f030 = #(); f031 = #();
  f032 = #(); f033 = #(); f034 = #(); f035 = #();
  f036 = #(); f037 = #(); f038 = #(); f039 = #();
  f040 = #(); f041 = #(); f042 = #(); f043 = #();
  f044 = #(); f045 = #(); f046 = #(); f047 = #();
  f048 = #(); f049 = #(); f050 = #(); f051 = #();
  f052 = #(); f053 = #(); f054 = #(); f055 = #();
  f056 = #(); f057 = #(); f058 = #(); f059 = #();
  f060 = #(); f061 = #(); f062 = #(); f063 = #();
  f064 = #(); f065 = #(); f066 = #(); f067 = #();
  f068 = #(); f069 = #(); f070 = #(); f071 = #();
  f072 = #(); f073 = #(); f074 = #(); f075 = #();
  f076 = #(); f077 = #(); f078 = #(); f079 = #();
  f080 = #(); f081 = #(); f082 = #(); f083 = #();
  f084 = #(); f085 = #(); f086 = #(); f087 = #();
  f088 = #(); f089 = #(); f090 = #(); f091 = #();
  f092 = #(); f093 = #(); f094 = #(); f095 = #();
  f096 = #(); f097 = #(); f098 = #(); f099 = #();
  f100 = #(); f101 = #(); f102 = #(); f103 = #();
  f104 = #(); f105 = #(); f106 = #(); f107 = #();
  f108 = #(); f109 = #(); f110 = #(); f111 = #();
  f112 = #(); f113 = #(); f114 = #(); f115 = #();
  f116 = #(); f117 = #(); f118 = #(); f119 = #();
  f120 = #(); f121 = #(); f122 = #(); f123 = #();
  f124 = #(); f125 = #(); f126 = #(); f127 = #();
  f128 = #(); f129 = #(); f130 = #(); f131 = #();
  f132 = #(); f133 = #(); f134 = #(); f135 = #();
  f136 = #(); f137 = #(); f138 = #(); f139 = #();
  f140 = #(); f141 = #(); f142 = #(); f143 = #();
  f144 = #(); f145 = #(); f146 = #(); f147 = #();
  f148 = #(); f149 = #(); f150 = #(); f151 = #();
  f152 = #(); f153 = #(); f154 = #(); f155 = #();
  f156 = #(); f157 = #(); f158 = #(); f159 = #();
  f160 = #(); f161 = #(); f162 = #(); f163 = #();
  f164 = #(); f165 = #(); f166 = #(); f167 = #();
  f168 = #(); f169 = #(); f170 = #(); f171 = #();
  f172 = #(); f173 = #(); f174 = #(); f175 = #();
  f176 = #(); f177 = #(); f178 = #(); f179 = #();
  f180 = #(); f181 = #(); f182 = #(); f183 = #();
  f184 = #(); f185 = #(); f186 = #(); f187 = #();
  f188 = #(); f189 = #(); f190 = #(); f191 = #();
  f192 = #(); f193 = #(); f194 = #(); f195 = #();
  f196 = #(); f197 = #(); f198 = #(); f199 = #();
  f200 = #(); f201 = #(); f202 = #(); f203 = #();
  f204 = #(); f205 = #(); f206 = #(); f207 = #();
  f208 = #(); f209 = #(); f210 = #(); f211 = #();
  f212 = #(); f213 = #(); f214 = #(); f215 = #();
  f216 = #(); f217 = #(); f218 = #(); f219 = #();
  f220 = #(); f221 = #(); f222 = #(); f223 = #();
  f224 = #(); f225 = #(); f226 = #(); f227 = #();
  f228 = #(); f229 = #(); f230 = #(); f231 = #();
  f232 = #(); f233 = #(); f234 = #(); f235 = #();
  f236 = #(); f237 = #(); f238 = #(); f239 = #();
  f240 = #(); f241 = #(); f242 = #(); f243 = #();
  f244 = #(); f245 = #(); f246 = #(); f247 = #();
  f248 = #(); f249 = #(); f250 = #(); f251 = #();
  f252 = #(); f253 = #(); f254 = #(); f255 = #();
}

let () =
  match Sys.opaque_identity (make ()) with
  | A _ -> failwith "initializer must produce B"
  | B r ->
    let rec copied = B { r with f000 = #() }
    and get () = copied in
    match (Sys.opaque_identity get) () with
    | B _ -> ()
    | A _ -> failwith "recursive update must preserve B"
