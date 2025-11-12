(* TEST
 include stdlib_upstream_compatible;
 reference = "${test_source_directory}/open.reference";
 flambda2;
 {
   native;
 }{
   bytecode;
 }
*)

open Stdlib_upstream_compatible
external [@layout_poly] id : ('a : any). 'a -> 'a = "%opaque"


let _ = print_endline "Test: [let open] with module ident"

module M = struct let foo = #42.0 let bar = "hello" end

let _ =
  let open M in
  print_float (Float_u.to_float (id foo));
  print_string " ";
  print_endline (id bar)


let _ = print_endline "Test: [let open] with inline struct"

let _ =
  let open struct let foo = #42.0 let bar = "hello" end in
  print_float (Float_u.to_float (id foo));
  print_string " ";
  print_endline (id bar)


let _ = print_endline "Test: [let open] with functor"

module Functor (X : sig end) = struct let foo = #42.0 let bar = "hello" end

let _ =
  let open Functor(struct end) in
  print_float (Float_u.to_float (id foo));
  print_string " ";
  print_endline (id bar)


let _ = print_endline "Test: Tests 1-3 with [open] instead of [let open]"

module M_4_1 = struct
  open M
  let _ = print_float (Float_u.to_float (id foo))
  let _ = print_string " "
  let _ = print_endline (id bar)
end

module M_4_2 = struct
  open struct let foo = #42.0 let bar = "hello" end
  let _ = print_float (Float_u.to_float (id foo))
  let _ = print_string " "
  let _ = print_endline (id bar)
end

module M_4_3 = struct
  open Functor(struct end)
  let _ = print_float (Float_u.to_float (id foo))
  let _ = print_string " "
  let _ = print_endline (id bar)
end


let _ = print_endline "Test: open shadowing open"

module Base = struct
  let x = #10.0
  let y = "original"
  let z = 100
end

module Override = struct
  let x = #20.0
  let y = "overridden"
end

module M_5 = struct
  open Base
  open Override

  let _ =
    print_float (Float_u.to_float (id x));
    print_string " ";
    print_string (id y);
    print_string " ";
    print_int (id z);
    print_newline ()
end


let _ = print_endline "Test: open shadowing a val"

module M_6 = struct
  let a = #5.0
  let b = "before open"

  open struct
    let b = "from open"
    let c = #7.0
  end

  let _ =
    print_float (Float_u.to_float (id a));
    print_string " ";
    print_string (id b);
    print_string " ";
    print_float (Float_u.to_float (id c));
    print_newline ()
end


let _ = print_endline "Test: val shadowing an open"

module M_7 = struct
  open struct
    let p = #8.0
    let q = "from open"
  end

  let p = #9.0
  let r = "after open"

  let _ =
    print_float (Float_u.to_float (id p));
    print_string " ";
    print_string (id q);
    print_string " ";
    print_string (id r);
    print_newline ()
end

let _ =
  print_endline "Test: opens can't violate the scannable tag size restriction"

module M_8 = struct
  let f_0 = #42.0
  let a_0 = "a"
  let a_1 = "a"
  let a_2 = "a"
  let a_3 = "a"
  let a_4 = "a"
  let a_5 = "a"
  let a_6 = "a"
  let a_7 = "a"
  let a_8 = "a"
  let a_9 = "a"
  let a_10 = "a"
  let a_11 = "a"
  let a_12 = "a"
  let a_13 = "a"
  let a_14 = "a"
  let a_15 = "a"
  let a_16 = "a"
  let a_17 = "a"
  let a_18 = "a"
  let a_19 = "a"
  let a_20 = "a"
  let a_21 = "a"
  let a_22 = "a"
  let a_23 = "a"
  let a_24 = "a"
  let a_25 = "a"
  let a_26 = "a"
  let a_27 = "a"
  let a_28 = "a"
  let a_29 = "a"
  let a_30 = "a"
  let a_31 = "a"
  let a_32 = "a"
  let a_33 = "a"
  let a_34 = "a"
  let a_35 = "a"
  let a_36 = "a"
  let a_37 = "a"
  let a_38 = "a"
  let a_39 = "a"
  let a_40 = "a"
  let a_41 = "a"
  let a_42 = "a"
  let a_43 = "a"
  let a_44 = "a"
  let a_45 = "a"
  let a_46 = "a"
  let a_47 = "a"
  let a_48 = "a"
  let a_49 = "a"
  let a_50 = "a"
  let a_51 = "a"
  let a_52 = "a"
  let a_53 = "a"
  let a_54 = "a"
  let a_55 = "a"
  let a_56 = "a"
  let a_57 = "a"
  let a_58 = "a"
  let a_59 = "a"
  let a_60 = "a"
  let a_61 = "a"
  let a_62 = "a"
  let a_63 = "a"
  let a_64 = "a"
  let a_65 = "a"
  let a_66 = "a"
  let a_67 = "a"
  let a_68 = "a"
  let a_69 = "a"
  let a_70 = "a"
  let a_71 = "a"
  let a_72 = "a"
  let a_73 = "a"
  let a_74 = "a"
  let a_75 = "a"
  let a_76 = "a"
  let a_77 = "a"
  let a_78 = "a"
  let a_79 = "a"
  let a_80 = "a"
  let a_81 = "a"
  let a_82 = "a"
  let a_83 = "a"
  let a_84 = "a"
  let a_85 = "a"
  let a_86 = "a"
  let a_87 = "a"
  let a_88 = "a"
  let a_89 = "a"
  let a_90 = "a"
  let a_91 = "a"
  let a_92 = "a"
  let a_93 = "a"
  let a_94 = "a"
  let a_95 = "a"
  let a_96 = "a"
  let a_97 = "a"
  let a_98 = "a"
  let a_99 = "a"
  let a_100 = "a"
  let a_101 = "a"
  let a_102 = "a"
  let a_103 = "a"
  let a_104 = "a"
  let a_105 = "a"
  let a_106 = "a"
  let a_107 = "a"
  let a_108 = "a"
  let a_109 = "a"
  let a_110 = "a"
  let a_111 = "a"
  let a_112 = "a"
  let a_113 = "a"
  let a_114 = "a"
  let a_115 = "a"
  let a_116 = "a"
  let a_117 = "a"
  let a_118 = "a"
  let a_119 = "a"
  let a_120 = "a"
  let a_121 = "a"
  let a_122 = "a"
  let a_123 = "a"
  let a_124 = "a"
  let a_125 = "a"
  let a_126 = "a"
  let a_127 = "a"
  let a_128 = "a"
  let a_129 = "a"
  let a_130 = "a"
  let a_131 = "a"
  let a_132 = "a"
  let a_133 = "a"
  let a_134 = "a"
  let a_135 = "a"
  let a_136 = "a"
  let a_137 = "a"
  let a_138 = "a"
  let a_139 = "a"
  let a_140 = "a"
  let a_141 = "a"
  let a_142 = "a"
  let a_143 = "a"
  let a_144 = "a"
  let a_145 = "a"
  let a_146 = "a"
  let a_147 = "a"
  let a_148 = "a"
  let a_149 = "a"
  let a_150 = "a"
  let a_151 = "a"
  let a_152 = "a"
  let a_153 = "a"
  let a_154 = "a"
  let a_155 = "a"
  let a_156 = "a"
  let a_157 = "a"
  let a_158 = "a"
  let a_159 = "a"
  let a_160 = "a"
  let a_161 = "a"
  let a_162 = "a"
  let a_163 = "a"
  let a_164 = "a"
  let a_165 = "a"
  let a_166 = "a"
  let a_167 = "a"
  let a_168 = "a"
  let a_169 = "a"
  let a_170 = "a"
  let a_171 = "a"
  let a_172 = "a"
  let a_173 = "a"
  let a_174 = "a"
  let a_175 = "a"
  let a_176 = "a"
  let a_177 = "a"
  let a_178 = "a"
  let a_179 = "a"
  let a_180 = "a"
  let a_181 = "[a_181]"
  let a_182 = "a"
  let a_183 = "a"
  let a_184 = "a"
  let a_185 = "a"
  let a_186 = "a"
  let a_187 = "a"
  let a_188 = "a"
  let a_189 = "a"
  let a_190 = "a"
  let a_191 = "a"
  let a_192 = "a"
  let a_193 = "a"
  let a_194 = "a"
  let a_195 = "a"
  let a_196 = "a"
  let a_197 = "a"
  let a_198 = "a"
  let a_199 = "a"
  let a_200 = "a"
  let a_201 = "a"
  let a_202 = "a"
  let a_203 = "a"
  let a_204 = "a"
  let a_205 = "a"
  let a_206 = "a"
  let a_207 = "a"
  let a_208 = "a"
  let a_209 = "a"
  let a_210 = "a"
  let a_211 = "a"
  let a_212 = "a"
  let a_213 = "a"
  let a_214 = "a"
  let a_215 = "a"
  let a_216 = "a"
  let a_217 = "a"
  let a_218 = "a"
  let a_219 = "a"
  let a_220 = "a"
  let a_221 = "a"
  let a_222 = "a"
  let a_223 = "a"
  let a_224 = "a"
  let a_225 = "a"
  let a_226 = "a"
  let a_227 = "a"
  let a_228 = "a"
  let a_229 = "a"
  let a_230 = "a"
  let a_231 = "a"
  let a_232 = "a"
  let a_233 = "a"
  let a_234 = "a"
  let a_235 = "a"
  let a_236 = "a"
  let a_237 = "a"
  let a_238 = "a"
  let a_239 = "a"
  let a_240 = "a"
  let a_241 = "a"
  open struct
    let f_1 = #43.0
    let a_242 = "a"
    let a_243 = "a"
    let a_244 = "a"
    let a_245 = "a"
    let a_246 = "a"
    let a_247 = "a"
    let a_248 = "a"
    let a_249 = "a"
    let a_250 = "[a_250]"
    let a_251 = "a"
    let a_252 = "a"
    let a_253 = "a"
    let a_254 = "a"
    let a_255 = "a"
    let a_256 = "a"
    let a_257 = "a"
    let a_258 = "a"
    let a_259 = "a"
    let a_260 = "a"
    let a_261 = "a"
    let a_262 = "a"
    let a_263 = "a"
    let a_264 = "a"
    let a_265 = "a"
  end

  let _ = print_float (Float_u.to_float (id f_0))
  let _ = print_string " "
  let _ = print_float (Float_u.to_float (id f_1))
  let _ = print_string " "
  let _ = print_string (id a_181)
  let _ = print_string " "
  let _ = print_endline (id a_250)
end
