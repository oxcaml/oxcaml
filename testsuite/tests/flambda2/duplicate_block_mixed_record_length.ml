(* TEST
   compile_only = "true";
   flambda2;
   setup-ocamlopt.byte-build-env;
   unset OCAMLPARAM;
   ocamlopt.byte with dump-raw;
   check-fexpr-dump;
*)

(* Regression test: When lowering a Pduprecord for all-value mixed record
   representations, [Lambda_to_flambda_primitives.convert_lprim] must use the
   correct size for a [Duplicate_block_kind.t].

   To exercise this, we use functional record updates, which duplicate the block
   when there are at least [Config.max_young_wosize] fields. The reference
   should show each [%duplicate_block] with length
   [385 = (2 * 192 pairs) + (0 * 64 unit#s) + (1 * 1 int)]. *)

type pair = #(int * int)

type t = {
  p0 : pair; p1 : pair; p2 : pair; p3 : pair; p4 : pair; p5 : pair; p6 : pair;
  p7 : pair; p8 : pair; p9 : pair; p10 : pair; p11 : pair; p12 : pair;
  p13 : pair; p14 : pair; p15 : pair; p16 : pair; p17 : pair; p18 : pair;
  p19 : pair; p20 : pair; p21 : pair; p22 : pair; p23 : pair; p24 : pair;
  p25 : pair; p26 : pair; p27 : pair; p28 : pair; p29 : pair; p30 : pair;
  p31 : pair; p32 : pair; p33 : pair; p34 : pair; p35 : pair; p36 : pair;
  p37 : pair; p38 : pair; p39 : pair; p40 : pair; p41 : pair; p42 : pair;
  p43 : pair; p44 : pair; p45 : pair; p46 : pair; p47 : pair; p48 : pair;
  p49 : pair; p50 : pair; p51 : pair; p52 : pair; p53 : pair; p54 : pair;
  p55 : pair; p56 : pair; p57 : pair; p58 : pair; p59 : pair; p60 : pair;
  p61 : pair; p62 : pair; p63 : pair; p64 : pair; p65 : pair; p66 : pair;
  p67 : pair; p68 : pair; p69 : pair; p70 : pair; p71 : pair; p72 : pair;
  p73 : pair; p74 : pair; p75 : pair; p76 : pair; p77 : pair; p78 : pair;
  p79 : pair; p80 : pair; p81 : pair; p82 : pair; p83 : pair; p84 : pair;
  p85 : pair; p86 : pair; p87 : pair; p88 : pair; p89 : pair; p90 : pair;
  p91 : pair; p92 : pair; p93 : pair; p94 : pair; p95 : pair; p96 : pair;
  p97 : pair; p98 : pair; p99 : pair; p100 : pair; p101 : pair; p102 : pair;
  p103 : pair; p104 : pair; p105 : pair; p106 : pair; p107 : pair; p108 : pair;
  p109 : pair; p110 : pair; p111 : pair; p112 : pair; p113 : pair; p114 : pair;
  p115 : pair; p116 : pair; p117 : pair; p118 : pair; p119 : pair; p120 : pair;
  p121 : pair; p122 : pair; p123 : pair; p124 : pair; p125 : pair; p126 : pair;
  p127 : pair; p128 : pair; p129 : pair; p130 : pair; p131 : pair; p132 : pair;
  p133 : pair; p134 : pair; p135 : pair; p136 : pair; p137 : pair; p138 : pair;
  p139 : pair; p140 : pair; p141 : pair; p142 : pair; p143 : pair; p144 : pair;
  p145 : pair; p146 : pair; p147 : pair; p148 : pair; p149 : pair; p150 : pair;
  p151 : pair; p152 : pair; p153 : pair; p154 : pair; p155 : pair; p156 : pair;
  p157 : pair; p158 : pair; p159 : pair; p160 : pair; p161 : pair; p162 : pair;
  p163 : pair; p164 : pair; p165 : pair; p166 : pair; p167 : pair; p168 : pair;
  p169 : pair; p170 : pair; p171 : pair; p172 : pair; p173 : pair; p174 : pair;
  p175 : pair; p176 : pair; p177 : pair; p178 : pair; p179 : pair; p180 : pair;
  p181 : pair; p182 : pair; p183 : pair; p184 : pair; p185 : pair; p186 : pair;
  p187 : pair; p188 : pair; p189 : pair; p190 : pair; p191 : pair; v0 : unit#;
  v1 : unit#; v2 : unit#; v3 : unit#; v4 : unit#; v5 : unit#; v6 : unit#;
  v7 : unit#; v8 : unit#; v9 : unit#; v10 : unit#; v11 : unit#; v12 : unit#;
  v13 : unit#; v14 : unit#; v15 : unit#; v16 : unit#; v17 : unit#; v18 : unit#;
  v19 : unit#; v20 : unit#; v21 : unit#; v22 : unit#; v23 : unit#; v24 : unit#;
  v25 : unit#; v26 : unit#; v27 : unit#; v28 : unit#; v29 : unit#; v30 : unit#;
  v31 : unit#; v32 : unit#; v33 : unit#; v34 : unit#; v35 : unit#; v36 : unit#;
  v37 : unit#; v38 : unit#; v39 : unit#; v40 : unit#; v41 : unit#; v42 : unit#;
  v43 : unit#; v44 : unit#; v45 : unit#; v46 : unit#; v47 : unit#; v48 : unit#;
  v49 : unit#; v50 : unit#; v51 : unit#; v52 : unit#; v53 : unit#; v54 : unit#;
  v55 : unit#; v56 : unit#; v57 : unit#; v58 : unit#; v59 : unit#; v60 : unit#;
  v61 : unit#; v62 : unit#; v63 : unit#;
  tag : int;
}

type inlined =
  | Inlined of {
      p0 : pair; p1 : pair; p2 : pair; p3 : pair; p4 : pair; p5 : pair;
      p6 : pair; p7 : pair; p8 : pair; p9 : pair; p10 : pair; p11 : pair;
      p12 : pair; p13 : pair; p14 : pair; p15 : pair; p16 : pair; p17 : pair;
      p18 : pair; p19 : pair; p20 : pair; p21 : pair; p22 : pair; p23 : pair;
      p24 : pair; p25 : pair; p26 : pair; p27 : pair; p28 : pair; p29 : pair;
      p30 : pair; p31 : pair; p32 : pair; p33 : pair; p34 : pair; p35 : pair;
      p36 : pair; p37 : pair; p38 : pair; p39 : pair; p40 : pair; p41 : pair;
      p42 : pair; p43 : pair; p44 : pair; p45 : pair; p46 : pair; p47 : pair;
      p48 : pair; p49 : pair; p50 : pair; p51 : pair; p52 : pair; p53 : pair;
      p54 : pair; p55 : pair; p56 : pair; p57 : pair; p58 : pair; p59 : pair;
      p60 : pair; p61 : pair; p62 : pair; p63 : pair; p64 : pair; p65 : pair;
      p66 : pair; p67 : pair; p68 : pair; p69 : pair; p70 : pair; p71 : pair;
      p72 : pair; p73 : pair; p74 : pair; p75 : pair; p76 : pair; p77 : pair;
      p78 : pair; p79 : pair; p80 : pair; p81 : pair; p82 : pair; p83 : pair;
      p84 : pair; p85 : pair; p86 : pair; p87 : pair; p88 : pair; p89 : pair;
      p90 : pair; p91 : pair; p92 : pair; p93 : pair; p94 : pair; p95 : pair;
      p96 : pair; p97 : pair; p98 : pair; p99 : pair; p100 : pair; p101 : pair;
      p102 : pair; p103 : pair; p104 : pair; p105 : pair; p106 : pair;
      p107 : pair; p108 : pair; p109 : pair; p110 : pair; p111 : pair;
      p112 : pair; p113 : pair; p114 : pair; p115 : pair; p116 : pair;
      p117 : pair; p118 : pair; p119 : pair; p120 : pair; p121 : pair;
      p122 : pair; p123 : pair; p124 : pair; p125 : pair; p126 : pair;
      p127 : pair; p128 : pair; p129 : pair; p130 : pair; p131 : pair;
      p132 : pair; p133 : pair; p134 : pair; p135 : pair; p136 : pair;
      p137 : pair; p138 : pair; p139 : pair; p140 : pair; p141 : pair;
      p142 : pair; p143 : pair; p144 : pair; p145 : pair; p146 : pair;
      p147 : pair; p148 : pair; p149 : pair; p150 : pair; p151 : pair;
      p152 : pair; p153 : pair; p154 : pair; p155 : pair; p156 : pair;
      p157 : pair; p158 : pair; p159 : pair; p160 : pair; p161 : pair;
      p162 : pair; p163 : pair; p164 : pair; p165 : pair; p166 : pair;
      p167 : pair; p168 : pair; p169 : pair; p170 : pair; p171 : pair;
      p172 : pair; p173 : pair; p174 : pair; p175 : pair; p176 : pair;
      p177 : pair; p178 : pair; p179 : pair; p180 : pair; p181 : pair;
      p182 : pair; p183 : pair; p184 : pair; p185 : pair; p186 : pair;
      p187 : pair; p188 : pair; p189 : pair; p190 : pair; p191 : pair;
      v0 : unit#; v1 : unit#; v2 : unit#; v3 : unit#; v4 : unit#; v5 : unit#;
      v6 : unit#; v7 : unit#; v8 : unit#; v9 : unit#; v10 : unit#; v11 : unit#;
      v12 : unit#; v13 : unit#; v14 : unit#; v15 : unit#; v16 : unit#;
      v17 : unit#; v18 : unit#; v19 : unit#; v20 : unit#; v21 : unit#;
      v22 : unit#; v23 : unit#; v24 : unit#; v25 : unit#; v26 : unit#;
      v27 : unit#; v28 : unit#; v29 : unit#; v30 : unit#; v31 : unit#;
      v32 : unit#; v33 : unit#; v34 : unit#; v35 : unit#; v36 : unit#;
      v37 : unit#; v38 : unit#; v39 : unit#; v40 : unit#; v41 : unit#;
      v42 : unit#; v43 : unit#; v44 : unit#; v45 : unit#; v46 : unit#;
      v47 : unit#; v48 : unit#; v49 : unit#; v50 : unit#; v51 : unit#;
      v52 : unit#; v53 : unit#; v54 : unit#; v55 : unit#; v56 : unit#;
      v57 : unit#; v58 : unit#; v59 : unit#; v60 : unit#; v61 : unit#;
      v62 : unit#; v63 : unit#;
      tag : int;
    }
  | Other

(* The functions take and return [Obj.t] so that the dump does not spell out
   the block shape of every parameter and result. *)
let[@inline never] update x =
  let r : t = Obj.obj x in
  Obj.repr { r with tag = r.tag + 1 }

let[@inline never] update_inlined x =
  match (Obj.obj x : inlined) with
  | Inlined r -> Obj.repr (Inlined { r with tag = r.tag + 1 })
  | Other -> Obj.repr Other
