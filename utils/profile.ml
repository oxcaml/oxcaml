(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                      Pierre Chambart, OCamlPro                         *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-18-40-42-48"]

type file = string

module Int = Misc.Stdlib.Int
module String = Misc.Stdlib.String

let data = {|
[{"args":{"path":["ocamlopt"]},"counters":{"time":7174392000,"calls":4481,"alloc":8216963512,"top-heap":445421544,"absolute-top-heap":445421544},"tid":0,"pid":0,"name":"ocamlopt","cat":"compiler","ts":1792773234794868,"dur":7563138}
,{"args":{"path":["ocamlopt","load_hidden_dirs"]},"counters":{"time":2000,"calls":1,"alloc":464,"top-heap":0,"absolute-top-heap":4910040},"tid":0,"pid":0,"name":"load_hidden_dirs","cat":"compiler","ts":1792773234798938,"dur":4}
,{"args":{"path":["ocamlopt","load_visible_dirs"]},"counters":{"time":7653000,"calls":1,"alloc":5079968,"top-heap":0,"absolute-top-heap":4910040},"tid":0,"pid":0,"name":"load_visible_dirs","cat":"compiler","ts":1792773234798945,"dur":7748}
,{"args":{"path":["ocamlopt","load_visible_manifests"]},"counters":{"time":1999,"calls":1,"alloc":448,"top-heap":0,"absolute-top-heap":4910040},"tid":0,"pid":0,"name":"load_visible_manifests","cat":"compiler","ts":1792773234806704,"dur":2}
,{"args":{"path":["ocamlopt","enqueue_hidden_manifests"]},"counters":{"time":1999,"calls":1,"alloc":448,"top-heap":0,"absolute-top-heap":4910040},"tid":0,"pid":0,"name":"enqueue_hidden_manifests","cat":"compiler","ts":1792773234806710,"dur":1}
,{"args":{"path":["ocamlopt","file=ctype.ml"]},"counters":{"time":7114985000,"calls":4471,"alloc":8187323040,"top-heap":440511504,"absolute-top-heap":445421544},"tid":0,"pid":0,"name":"file=ctype.ml","cat":"compiler","ts":1792773234809741,"dur":7548244}
,{"args":{"path":["ocamlopt","file=ctype.ml","parsing"]},"counters":{"time":54236000,"calls":5,"alloc":74554208,"top-heap":10480024,"absolute-top-heap":15390064},"tid":0,"pid":0,"name":"parsing","cat":"compiler","ts":1792773234809745,"dur":54725}
,{"args":{"path":["ocamlopt","file=ctype.ml","parsing","parser"]},"counters":{"time":53730999,"calls":1,"alloc":74551432,"top-heap":10480024,"absolute-top-heap":15390064},"tid":0,"pid":0,"name":"parser","cat":"compiler","ts":1792773234810249,"dur":54209}
,{"args":{"path":["ocamlopt","file=ctype.ml","parsing","-ppx"]},"counters":{"time":1000,"calls":1,"alloc":472,"top-heap":0,"absolute-top-heap":15390064},"tid":0,"pid":0,"name":"-ppx","cat":"compiler","ts":1792773234864466,"dur":2}
,{"args":{"path":["ocamlopt","file=ctype.ml","parsing","other"]},"counters":{"time":504000,"calls":3,"alloc":2304,"top-heap":0,"absolute-top-heap":15390064},"tid":0,"pid":0,"name":"other","cat":"compiler","ts":1792773234809745,"dur":514}
,{"args":{"path":["ocamlopt","file=ctype.ml","typing"]},"counters":{"time":1450264000,"calls":9,"alloc":969788248,"top-heap":158272832,"absolute-top-heap":173662896,"comprehensions":0,"immutable_arrays":0,"include_functor":0,"module_strengthening":0},"tid":0,"pid":0,"name":"typing","cat":"compiler","ts":1792773234864476,"dur":1515679}
,{"args":{"path":["ocamlopt","file=ctype.ml","typing","infer"]},"counters":{"time":740603000,"calls":1,"alloc":815914760,"top-heap":123578528,"absolute-top-heap":138968592},"tid":0,"pid":0,"name":"infer","cat":"compiler","ts":1792773234864488,"dur":778232}
,{"args":{"path":["ocamlopt","file=ctype.ml","typing","check_sig"]},"counters":{"time":35355000,"calls":1,"alloc":30026136,"top-heap":4390912,"absolute-top-heap":143359504},"tid":0,"pid":0,"name":"check_sig","cat":"compiler","ts":1792773235643969,"dur":38404}
,{"args":{"path":["ocamlopt","file=ctype.ml","typing","save_cmi"]},"counters":{"time":24749999,"calls":1,"alloc":11101816,"top-heap":4145248,"absolute-top-heap":147504752},"tid":0,"pid":0,"name":"save_cmi","cat":"compiler","ts":1792773235753480,"dur":26051}
,{"args":{"path":["ocamlopt","file=ctype.ml","typing","save_cmt"]},"counters":{"time":571877000,"calls":1,"alloc":56453600,"top-heap":10716048,"absolute-top-heap":158220800},"tid":0,"pid":0,"name":"save_cmt","cat":"compiler","ts":1792773235779539,"dur":594509}
,{"args":{"path":["ocamlopt","file=ctype.ml","typing","other"]},"counters":{"time":77679000,"calls":5,"alloc":56291936,"top-heap":15442096,"absolute-top-heap":173662896},"tid":0,"pid":0,"name":"other","cat":"compiler","ts":1792773234864476,"dur":78480}
,{"args":{"path":["ocamlopt","file=ctype.ml","transl"]},"counters":{"time":381076000,"calls":1,"alloc":417851824,"top-heap":19762024,"absolute-top-heap":193424920},"tid":0,"pid":0,"name":"transl","cat":"compiler","ts":1792773236380166,"dur":386721}
,{"args":{"path":["ocamlopt","file=ctype.ml","generate"]},"counters":{"time":5229377999,"calls":4451,"alloc":6725124392,"top-heap":251996624,"absolute-top-heap":445421544},"tid":0,"pid":0,"name":"generate","cat":"compiler","ts":1792773236766896,"dur":5591079}
,{"args":{"path":["ocamlopt","file=ctype.ml","generate","slambda_eval"]},"counters":{"time":10124000,"calls":5,"alloc":6151544,"top-heap":8368,"absolute-top-heap":193433288},"tid":0,"pid":0,"name":"slambda_eval","cat":"compiler","ts":1792773236766900,"dur":10202}
,{"args":{"path":["ocamlopt","file=ctype.ml","generate","slambda_eval","slambda_fracture"]},"counters":{"time":2426999,"calls":1,"alloc":4034040,"top-heap":0,"absolute-top-heap":193424920},"tid":0,"pid":0,"name":"slambda_fracture","cat":"compiler","ts":1792773236766903,"dur":2450}
,{"args":{"path":["ocamlopt","file=ctype.ml","generate","slambda_eval","static_eval"]},"counters":{"time":7686000,"calls":1,"alloc":2115480,"top-heap":8368,"absolute-top-heap":193433288},"tid":0,"pid":0,"name":"static_eval","cat":"compiler","ts":1792773236769359,"dur":7736}
,{"args":{"path":["ocamlopt","file=ctype.ml","generate","slambda_eval","other"]},"counters":{"time":10999,"calls":3,"alloc":2024,"top-heap":0,"absolute-top-heap":193433288},"tid":0,"pid":0,"name":"other","cat":"compiler","ts":1792773236766900,"dur":14}
,{"args":{"path":["ocamlopt","file=ctype.ml","generate","cmx_from_raw"]},"counters":{"time":59899000,"calls":34,"alloc":16592,"top-heap":0,"absolute-top-heap":193433288},"tid":0,"pid":0,"name":"cmx_from_raw","cat":"compiler","ts":1792773236810704,"dur":90146}
,{"args":{"path":["ocamlopt","file=ctype.ml","generate","flambda2"]},"counters":{"time":764892000,"calls":4343,"alloc":890785872,"top-heap":52957264,"absolute-top-heap":246390552},"tid":0,"pid":0,"name":"flambda2","cat":"compiler","ts":1792773236939495,"dur":866615}
,{"args":{"path":["ocamlopt","file=ctype.ml","generate","flambda2","lambda_to_flambda"]},"counters":{"time":371838000,"calls":481,"alloc":361517280,"top-heap":50646552,"absolute-top-heap":244079840},"tid":0,"pid":0,"name":"lambda_to_flambda","cat":"compiler","ts":1792773236939513,"dur":430419}
,{"args":{"path":["ocamlopt","file=ctype.ml","generate","flambda2","lambda_to_flambda","load_cmx"]},"counters":{"time":83390000,"calls":440,"alloc":77463648,"top-heap":16486120,"absolute-top-heap":209919408},"tid":0,"pid":0,"name":"load_cmx","cat":"compiler","ts":1792773236940717,"dur":88498}
,{"args":{"path":["ocamlopt","file=ctype.ml","generate","flambda2","lambda_to_flambda","load_cmx","cmx_from_raw"]},"counters":{"time":4407000,"calls":40,"alloc":18976,"top-heap":0,"absolute-top-heap":193433288},"tid":0,"pid":0,"name":"cmx_from_raw","cat":"compiler","ts":1792773236940726,"dur":4921}
,{"args":{"path":["ocamlopt","file=ctype.ml","generate","flambda2","lambda_to_flambda","load_cmx","create_import_map"]},"counters":{"time":88999,"calls":40,"alloc":22720,"top-heap":0,"absolute-top-heap":193433288},"tid":0,"pid":0,"name":"create_import_map","cat":"compiler","ts":1792773236940732,"dur":90}
,{"args":{"path":["ocamlopt","file=ctype.ml","generate","flambda2","lambda_to_flambda","load_cmx","typing_env_apply_renaming"]},"counters":{"time":54096000,"calls":40,"alloc":62034416,"top-heap":14388968,"absolute-top-heap":207822256},"tid":0,"pid":0,"name":"typing_env_apply_renaming","cat":"compiler","ts":1792773236940737,"dur":55709}
,{"args":{"path":["ocamlopt","file=ctype.ml","generate","flambda2","lambda_to_flambda","load_cmx","exported_code_from_raw"]},"counters":{"time":1137999,"calls":40,"alloc":884360,"top-heap":0,"absolute-top-heap":207822256},"tid":0,"pid":0,"name":"exported_code_from_raw","cat":"compiler","ts":1792773236940833,"dur":1135}
,{"args":{"path":["ocamlopt","file=ctype.ml","generate","flambda2","lambda_to_flambda","load_cmx","exported_code_apply_renaming"]},"counters":{"time":8109000,"calls":40,"alloc":5984256,"top-heap":0,"absolute-top-heap":207822256},"tid":0,"pid":0,"name":"exported_code_apply_renaming","cat":"compiler","ts":1792773236940839,"dur":8753}
,{"args":{"path":["ocamlopt","file=ctype.ml","generate","flambda2","lambda_to_flambda","load_cmx","other"]},"counters":{"time":15551000,"calls":240,"alloc":8518920,"top-heap":2097152,"absolute-top-heap":209919408},"tid":0,"pid":0,"name":"other","cat":"compiler","ts":1792773236940717,"dur":17888}
,{"args":{"path":["ocamlopt","file=ctype.ml","generate","flambda2","lambda_to_flambda","other"]},"counters":{"time":288447999,"calls":41,"alloc":284053632,"top-heap":34160432,"absolute-top-heap":244079840},"tid":0,"pid":0,"name":"other","cat":"compiler","ts":1792773236939513,"dur":341921}
,{"args":{"path":["ocamlopt","file=ctype.ml","generate","flambda2","flambda_to_cmm"]},"counters":{"time":392898000,"calls":3859,"alloc":529262528,"top-heap":2310712,"absolute-top-heap":246390552},"tid":0,"pid":0,"name":"flambda_to_cmm","cat":"compiler","ts":1792773237369944,"dur":436035}
,{"args":{"path":["ocamlopt","file=ctype.ml","generate","flambda2","flambda_to_cmm","cmx_from_raw"]},"counters":{"time":9247999,"calls":1929,"alloc":910552,"top-heap":0,"absolute-top-heap":244079840},"tid":0,"pid":0,"name":"cmx_from_raw","cat":"compiler","ts":1792773237372823,"dur":18118}
,{"args":{"path":["ocamlopt","file=ctype.ml","generate","flambda2","flambda_to_cmm","other"]},"counters":{"time":383650000,"calls":1930,"alloc":528351976,"top-heap":2310712,"absolute-top-heap":246390552},"tid":0,"pid":0,"name":"other","cat":"compiler","ts":1792773237369944,"dur":417916}
,{"args":{"path":["ocamlopt","file=ctype.ml","generate","flambda2","other"]},"counters":{"time":155999,"calls":3,"alloc":6064,"top-heap":0,"absolute-top-heap":246390552},"tid":0,"pid":0,"name":"other","cat":"compiler","ts":1792773236939495,"dur":161}
,{"args":{"path":["ocamlopt","file=ctype.ml","generate","compile_phrases"]},"counters":{"time":2732332999,"calls":5,"alloc":5166674944,"top-heap":162677760,"absolute-top-heap":409068312},"tid":0,"pid":0,"name":"compile_phrases","cat":"compiler","ts":1792773237806181,"dur":2852915}
,{"args":{"path":["ocamlopt","file=ctype.ml","generate","compile_phrases","cmm_invariants"]},"counters":{"time":431676,"calls":1,"alloc":943256,"top-heap":0,"absolute-top-heap":246390552},"tid":0,"pid":0,"name":"cmm_invariants","cat":"compiler","ts":1792773237807360,"dur":434}
,{"args":{"path":["ocamlopt","file=ctype.ml","generate","compile_phrases","cfg_invariants"]},"counters":{"time":238511,"calls":1,"alloc":471744,"top-heap":0,"absolute-top-heap":246390552},"tid":0,"pid":0,"name":"cfg_invariants","cat":"compiler","ts":1792773237807412,"dur":245}
,{"args":{"path":["ocamlopt","file=ctype.ml","generate","compile_phrases","cfg"]},"counters":{"time":2624102592,"calls":0,"alloc":4751405784,"top-heap":151323904,"absolute-top-heap":397714456},"tid":0,"pid":0,"name":"cfg","cat":"compiler","ts":1792773237807416,"dur":2624104}
,{"args":{"path":["ocamlopt","file=ctype.ml","generate","compile_phrases","cfg","cfg_polling"]},"counters":{"time":25146007,"calls":0,"alloc":55353600,"top-heap":3183344,"absolute-top-heap":249573896,"block":25624,"block_duplicate_reload":0,"block_duplicate_spill":0,"instruction":164621,"move":64309,"reload":0,"spill":0,"stack_slot":0},"tid":0,"pid":0,"name":"cfg_polling","cat":"compiler","ts":1792773237807419,"dur":25127}
,{"args":{"path":["ocamlopt","file=ctype.ml","generate","compile_phrases","cfg","cfg_zero_alloc_checker"]},"counters":{"time":64012527,"calls":0,"alloc":120322880,"top-heap":1962728,"absolute-top-heap":251536624,"block":25624,"block_duplicate_reload":0,"block_duplicate_spill":0,"instruction":164621,"move":64309,"reload":0,"spill":0,"stack_slot":0},"tid":0,"pid":0,"name":"cfg_zero_alloc_checker","cat":"compiler","ts":1792773237807424,"dur":63999}
,{"args":{"path":["ocamlopt","file=ctype.ml","generate","compile_phrases","cfg","cfg_zero_alloc_checker","check zero_alloc"]},"counters":{"time":43329954,"calls":0,"alloc":60624488,"top-heap":1016024,"absolute-top-heap":250589920},"tid":0,"pid":0,"name":"check zero_alloc","cat":"compiler","ts":1792773237807425,"dur":43322}
,{"args":{"path":["ocamlopt","file=ctype.ml","generate","compile_phrases","cfg","cfg_zero_alloc_checker","other"]},"counters":{"time":20682573,"calls":0,"alloc":59698392,"top-heap":946704,"absolute-top-heap":251536624},"tid":0,"pid":0,"name":"other","cat":"compiler","ts":1792773237807424,"dur":20677}
,{"args":{"path":["ocamlopt","file=ctype.ml","generate","compile_phrases","cfg","cfg_comballoc"]},"counters":{"time":21861791,"calls":0,"alloc":66407584,"top-heap":429752,"absolute-top-heap":251966376,"block":25624,"block_duplicate_reload":0,"block_duplicate_spill":0,"instruction":165837,"move":64309,"reload":0,"spill":0,"stack_slot":0},"tid":0,"pid":0,"name":"cfg_comballoc","cat":"compiler","ts":1792773237807447,"dur":21855}
,{"args":{"path":["ocamlopt","file=ctype.ml","generate","compile_phrases","cfg","cfg_cse"]},"counters":{"time":50610780,"calls":0,"alloc":126458248,"top-heap":1925296,"absolute-top-heap":253891672,"block":25624,"block_duplicate_reload":0,"block_duplicate_spill":0,"instruction":165837,"move":66073,"reload":0,"spill":0,"stack_slot":0},"tid":0,"pid":0,"name":"cfg_cse","cat":"compiler","ts":1792773237807451,"dur":50592}
,{"args":{"path":["ocamlopt","file=ctype.ml","generate","compile_phrases","cfg","cfg_deadcode"]},"counters":{"time":68403959,"calls":0,"alloc":146788552,"top-heap":4005184,"absolute-top-heap":257896856,"block":25624,"block_duplicate_reload":0,"block_duplicate_spill":0,"instruction":156891,"move":60397,"reload":0,"spill":0,"stack_slot":0},"tid":0,"pid":0,"name":"cfg_deadcode","cat":"compiler","ts":1792773237807462,"dur":68402}
,{"args":{"path":["ocamlopt","file=ctype.ml","generate","compile_phrases","cfg","regalloc"]},"counters":{"time":1586803436,"calls":0,"alloc":2781706032,"top-heap":106850920,"absolute-top-heap":364747776},"tid":0,"pid":0,"name":"regalloc","cat":"compiler","ts":1792773237807473,"dur":1586818}
,{"args":{"path":["ocamlopt","file=ctype.ml","generate","compile_phrases","cfg","regalloc","cfg_ls"]},"counters":{"time":1172184228,"calls":0,"alloc":2146411936,"top-heap":85902984,"absolute-top-heap":343799840,"block":31475,"block_duplicate_reload":2146,"block_duplicate_spill":0,"instruction":186173,"move":60397,"reload":17188,"spill":6243,"stack_slot":3011},"tid":0,"pid":0,"name":"cfg_ls","cat":"compiler","ts":1792773237807483,"dur":1172180}
,{"args":{"path":["ocamlopt","file=ctype.ml","generate","compile_phrases","cfg","regalloc","cfg_ls","stack_slots_optimize"]},"counters":{"time":44789314,"calls":0,"alloc":45347120,"top-heap":69800,"absolute-top-heap":257966656},"tid":0,"pid":0,"name":"stack_slots_optimize","cat":"compiler","ts":1792773237807522,"dur":44840}
,{"args":{"path":["ocamlopt","file=ctype.ml","generate","compile_phrases","cfg","regalloc","cfg_ls","other"]},"counters":{"time":1127394914,"calls":0,"alloc":2101064816,"top-heap":85833184,"absolute-top-heap":343799840},"tid":0,"pid":0,"name":"other","cat":"compiler","ts":1792773237807483,"dur":1127340}
,{"args":{"path":["ocamlopt","file=ctype.ml","generate","compile_phrases","cfg","regalloc","cfg_validate_description"]},"counters":{"time":373911142,"calls":0,"alloc":574532536,"top-heap":18417720,"absolute-top-heap":362217560,"block":31475,"block_duplicate_reload":2146,"block_duplicate_spill":0,"instruction":186173,"move":60397,"reload":17188,"spill":6243,"stack_slot":3011},"tid":0,"pid":0,"name":"cfg_validate_description","cat":"compiler","ts":1792773237807527,"dur":373927}
,{"args":{"path":["ocamlopt","file=ctype.ml","generate","compile_phrases","cfg","regalloc","other"]},"counters":{"time":40708065,"calls":0,"alloc":60761560,"top-heap":2530216,"absolute-top-heap":364747776},"tid":0,"pid":0,"name":"other","cat":"compiler","ts":1792773237807473,"dur":40709}
,{"args":{"path":["ocamlopt","file=ctype.ml","generate","compile_phrases","cfg","cfg_prologue"]},"counters":{"time":101385593,"calls":0,"alloc":168214992,"top-heap":7857904,"absolute-top-heap":372605680,"block":31475,"block_duplicate_reload":2146,"block_duplicate_spill":0,"instruction":188601,"move":60397,"reload":17188,"spill":6243,"stack_slot":3011},"tid":0,"pid":0,"name":"cfg_prologue","cat":"compiler","ts":1792773237807575,"dur":101400}
,{"args":{"path":["ocamlopt","file=ctype.ml","generate","compile_phrases","cfg","cfg_prologue_validate"]},"counters":{"time":62747240,"calls":0,"alloc":125775544,"top-heap":4412792,"absolute-top-heap":377018472,"block":31475,"block_duplicate_reload":2146,"block_duplicate_spill":0,"instruction":188601,"move":60397,"reload":17188,"spill":6243,"stack_slot":3011},"tid":0,"pid":0,"name":"cfg_prologue_validate","cat":"compiler","ts":1792773237807589,"dur":62749}
,{"args":{"path":["ocamlopt","file=ctype.ml","generate","compile_phrases","cfg","cfg_invariants"]},"counters":{"time":737905,"calls":0,"alloc":2033456,"top-heap":0,"absolute-top-heap":377018472},"tid":0,"pid":0,"name":"cfg_invariants","cat":"compiler","ts":1792773237807595,"dur":753}
,{"args":{"path":["ocamlopt","file=ctype.ml","generate","compile_phrases","cfg","cfg_simplify"]},"counters":{"time":130585193,"calls":0,"alloc":287280512,"top-heap":5586064,"absolute-top-heap":382604536,"block":47131,"block_duplicate_reload":4216,"block_duplicate_spill":0,"instruction":260882,"move":23931,"reload":33690,"spill":12384,"stack_slot":6022},"tid":0,"pid":0,"name":"cfg_simplify","cat":"compiler","ts":1792773237807596,"dur":130592}
,{"args":{"path":["ocamlopt","file=ctype.ml","generate","compile_phrases","cfg","peephole_optimize_cfg"]},"counters":{"time":41099071,"calls":0,"alloc":134048416,"top-heap":2010888,"absolute-top-heap":384615424,"block":24454,"block_duplicate_reload":2074,"block_duplicate_spill":0,"instruction":132678,"move":12119,"reload":16573,"spill":6234,"stack_slot":3011},"tid":0,"pid":0,"name":"peephole_optimize_cfg","cat":"compiler","ts":1792773237807602,"dur":41103}
,{"args":{"path":["ocamlopt","file=ctype.ml","generate","compile_phrases","cfg","save_cfg"]},"counters":{"time":18907070,"calls":0,"alloc":54973464,"top-heap":1835656,"absolute-top-heap":386451080,"block":22677,"block_duplicate_reload":2070,"block_duplicate_spill":0,"instruction":127443,"move":11675,"reload":16502,"spill":6141,"stack_slot":3011},"tid":0,"pid":0,"name":"save_cfg","cat":"compiler","ts":1792773237807622,"dur":18895}
,{"args":{"path":["ocamlopt","file=ctype.ml","generate","compile_phrases","cfg","cfg_block_layout"]},"counters":{"time":17051935,"calls":0,"alloc":55194792,"top-heap":2528192,"absolute-top-heap":388979272,"block":22677,"block_duplicate_reload":2070,"block_duplicate_spill":0,"instruction":127443,"move":11675,"reload":16502,"spill":6141,"stack_slot":3011},"tid":0,"pid":0,"name":"cfg_block_layout","cat":"compiler","ts":1792773237807625,"dur":17050}
,{"args":{"path":["ocamlopt","file=ctype.ml","generate","compile_phrases","cfg","cfg_reorder_blocks"]},"counters":{"time":14332294,"calls":0,"alloc":50022936,"top-heap":1147168,"absolute-top-heap":390126440,"block":22677,"block_duplicate_reload":2070,"block_duplicate_spill":0,"instruction":127443,"move":11675,"reload":16502,"spill":6141,"stack_slot":3011},"tid":0,"pid":0,"name":"cfg_reorder_blocks","cat":"compiler","ts":1792773237808224,"dur":14317}
,{"args":{"path":["ocamlopt","file=ctype.ml","generate","compile_phrases","cfg","cfg_available_regs"]},"counters":{"time":482559,"calls":0,"alloc":471744,"top-heap":0,"absolute-top-heap":390126440},"tid":0,"pid":0,"name":"cfg_available_regs","cat":"compiler","ts":1792773237808228,"dur":483}
,{"args":{"path":["ocamlopt","file=ctype.ml","generate","compile_phrases","cfg","cfg_to_linear"]},"counters":{"time":9242534,"calls":0,"alloc":18525992,"top-heap":729448,"absolute-top-heap":390855888},"tid":0,"pid":0,"name":"cfg_to_linear","cat":"compiler","ts":1792773237808228,"dur":9220}
,{"args":{"path":["ocamlopt","file=ctype.ml","generate","compile_phrases","cfg","save_linear"]},"counters":{"time":302791,"calls":0,"alloc":471744,"top-heap":0,"absolute-top-heap":390855888},"tid":0,"pid":0,"name":"save_linear","cat":"compiler","ts":1792773237808300,"dur":314}
,{"args":{"path":["ocamlopt","file=ctype.ml","generate","compile_phrases","cfg","emit_fundecl"]},"counters":{"time":389227390,"calls":0,"alloc":522241432,"top-heap":6719136,"absolute-top-heap":397575024},"tid":0,"pid":0,"name":"emit_fundecl","cat":"compiler","ts":1792773237808301,"dur":389212}
,{"args":{"path":["ocamlopt","file=ctype.ml","generate","compile_phrases","cfg","emit_fundecl","emit"]},"counters":{"time":387700557,"calls":0,"alloc":521221872,"top-heap":6719136,"absolute-top-heap":397575024},"tid":0,"pid":0,"name":"emit","cat":"compiler","ts":1792773237808301,"dur":387707}
,{"args":{"path":["ocamlopt","file=ctype.ml","generate","compile_phrases","cfg","emit_fundecl","emit","debug_inlined_frame_ranges"]},"counters":{"time":124757289,"calls":0,"alloc":349510288,"top-heap":3996592,"absolute-top-heap":394852480},"tid":0,"pid":0,"name":"debug_inlined_frame_ranges","cat":"compiler","ts":1792773237808301,"dur":124741}
,{"args":{"path":["ocamlopt","file=ctype.ml","generate","compile_phrases","cfg","emit_fundecl","emit","dwarf_inlined_frames"]},"counters":{"time":138520240,"calls":0,"alloc":98609104,"top-heap":1931512,"absolute-top-heap":396783992},"tid":0,"pid":0,"name":"dwarf_inlined_frames","cat":"compiler","ts":1792773237808325,"dur":138498}
,{"args":{"path":["ocamlopt","file=ctype.ml","generate","compile_phrases","cfg","emit_fundecl","emit","other"]},"counters":{"time":124423027,"calls":0,"alloc":73102480,"top-heap":791032,"absolute-top-heap":397575024},"tid":0,"pid":0,"name":"other","cat":"compiler","ts":1792773237808301,"dur":124468}
,{"args":{"path":["ocamlopt","file=ctype.ml","generate","compile_phrases","cfg","emit_fundecl","other"]},"counters":{"time":1526832,"calls":0,"alloc":1019560,"top-heap":0,"absolute-top-heap":397575024},"tid":0,"pid":0,"name":"other","cat":"compiler","ts":1792773237808301,"dur":1504}
,{"args":{"path":["ocamlopt","file=ctype.ml","generate","compile_phrases","cfg","other"]},"counters":{"time":21162509,"calls":0,"alloc":35113864,"top-heap":139432,"absolute-top-heap":397714456},"tid":0,"pid":0,"name":"other","cat":"compiler","ts":1792773237807416,"dur":21216}
,{"args":{"path":["ocamlopt","file=ctype.ml","generate","compile_phrases","other"]},"counters":{"time":107560219,"calls":3,"alloc":413854160,"top-heap":11353856,"absolute-top-heap":409068312},"tid":0,"pid":0,"name":"other","cat":"compiler","ts":1792773237806181,"dur":228130}
,{"args":{"path":["ocamlopt","file=ctype.ml","generate","emit_dwarf"]},"counters":{"time":88810999,"calls":17,"alloc":178828584,"top-heap":24672,"absolute-top-heap":409092984},"tid":0,"pid":0,"name":"emit_dwarf","cat":"compiler","ts":1792773240758035,"dur":90626}
,{"args":{"path":["ocamlopt","file=ctype.ml","generate","emit_dwarf","assign_abbrevs"]},"counters":{"time":43654000,"calls":1,"alloc":142819200,"top-heap":24672,"absolute-top-heap":409092984},"tid":0,"pid":0,"name":"assign_abbrevs","cat":"compiler","ts":1792773240758048,"dur":44289}
,{"args":{"path":["ocamlopt","file=ctype.ml","generate","emit_dwarf","debug_info_section"]},"counters":{"time":1999,"calls":1,"alloc":480,"top-heap":0,"absolute-top-heap":409092984},"tid":0,"pid":0,"name":"debug_info_section","cat":"compiler","ts":1792773240802344,"dur":2}
,{"args":{"path":["ocamlopt","file=ctype.ml","generate","emit_dwarf","dwarf_world_emit"]},"counters":{"time":45134000,"calls":11,"alloc":36005080,"top-heap":0,"absolute-top-heap":409092984},"tid":0,"pid":0,"name":"dwarf_world_emit","cat":"compiler","ts":1792773240802350,"dur":46308}
,{"args":{"path":["ocamlopt","file=ctype.ml","generate","emit_dwarf","dwarf_world_emit","debug_info_section"]},"counters":{"time":31851999,"calls":3,"alloc":23640544,"top-heap":0,"absolute-top-heap":409092984},"tid":0,"pid":0,"name":"debug_info_section","cat":"compiler","ts":1792773240802355,"dur":32461}
,{"args":{"path":["ocamlopt","file=ctype.ml","generate","emit_dwarf","dwarf_world_emit","debug_info_section","die_emission"]},"counters":{"time":26709999,"calls":1,"alloc":18614840,"top-heap":0,"absolute-top-heap":409092984},"tid":0,"pid":0,"name":"die_emission","cat":"compiler","ts":1792773240807544,"dur":27263}
,{"args":{"path":["ocamlopt","file=ctype.ml","generate","emit_dwarf","dwarf_world_emit","debug_info_section","other"]},"counters":{"time":5142000,"calls":2,"alloc":5025704,"top-heap":0,"absolute-top-heap":409092984},"tid":0,"pid":0,"name":"other","cat":"compiler","ts":1792773240802355,"dur":5197}
,{"args":{"path":["ocamlopt","file=ctype.ml","generate","emit_dwarf","dwarf_world_emit","abbreviations_table"]},"counters":{"time":23000,"calls":1,"alloc":42144,"top-heap":0,"absolute-top-heap":409092984},"tid":0,"pid":0,"name":"abbreviations_table","cat":"compiler","ts":1792773240834821,"dur":23}
,{"args":{"path":["ocamlopt","file=ctype.ml","generate","emit_dwarf","dwarf_world_emit","debug_loc"]},"counters":{"time":3999,"calls":1,"alloc":448,"top-heap":0,"absolute-top-heap":409092984},"tid":0,"pid":0,"name":"debug_loc","cat":"compiler","ts":1792773240839903,"dur":4}
,{"args":{"path":["ocamlopt","file=ctype.ml","generate","emit_dwarf","dwarf_world_emit","debug_ranges"]},"counters":{"time":8471000,"calls":1,"alloc":11287800,"top-heap":0,"absolute-top-heap":409092984},"tid":0,"pid":0,"name":"debug_ranges","cat":"compiler","ts":1792773240839910,"dur":8740}
,{"args":{"path":["ocamlopt","file=ctype.ml","generate","emit_dwarf","dwarf_world_emit","other"]},"counters":{"time":4783999,"calls":5,"alloc":1034144,"top-heap":0,"absolute-top-heap":409092984},"tid":0,"pid":0,"name":"other","cat":"compiler","ts":1792773240802350,"dur":5079}
,{"args":{"path":["ocamlopt","file=ctype.ml","generate","emit_dwarf","other"]},"counters":{"time":21000,"calls":4,"alloc":3824,"top-heap":0,"absolute-top-heap":409092984},"tid":0,"pid":0,"name":"other","cat":"compiler","ts":1792773240758035,"dur":25}
,{"args":{"path":["ocamlopt","file=ctype.ml","generate","write_asm"]},"counters":{"time":189796000,"calls":1,"alloc":337834720,"top-heap":0,"absolute-top-heap":409092984},"tid":0,"pid":0,"name":"write_asm","cat":"compiler","ts":1792773240848666,"dur":195337}
,{"args":{"path":["ocamlopt","file=ctype.ml","generate","record_unit zero_alloc"]},"counters":{"time":1266000,"calls":1,"alloc":654752,"top-heap":0,"absolute-top-heap":409092984},"tid":0,"pid":0,"name":"record_unit zero_alloc","cat":"compiler","ts":1792773241044017,"dur":1284}
,{"args":{"path":["ocamlopt","file=ctype.ml","generate","assemble"]},"counters":{"time":1194707999,"calls":3,"alloc":17216,"top-heap":0,"absolute-top-heap":409092984},"tid":0,"pid":0,"name":"assemble","cat":"compiler","ts":1792773241045337,"dur":1274595}
,{"args":{"path":["ocamlopt","file=ctype.ml","generate","assemble","compact"]},"counters":{"time":563692999,"calls":1,"alloc":15048,"top-heap":0,"absolute-top-heap":409092984},"tid":0,"pid":0,"name":"compact","cat":"compiler","ts":1792773241045340,"dur":575225}
,{"args":{"path":["ocamlopt","file=ctype.ml","generate","assemble","other"]},"counters":{"time":631014999,"calls":2,"alloc":2168,"top-heap":0,"absolute-top-heap":409092984},"tid":0,"pid":0,"name":"other","cat":"compiler","ts":1792773241045337,"dur":699369}
,{"args":{"path":["ocamlopt","file=ctype.ml","generate","other"]},"counters":{"time":187548999,"calls":42,"alloc":144160168,"top-heap":36328560,"absolute-top-heap":445421544},"tid":0,"pid":0,"name":"other","cat":"compiler","ts":1792773236766896,"dur":209353}
,{"args":{"path":["ocamlopt","file=ctype.ml","other"]},"counters":{"time":31000,"calls":5,"alloc":4368,"top-heap":0,"absolute-top-heap":445421544},"tid":0,"pid":0,"name":"other","cat":"compiler","ts":1792773234809741,"dur":39}
,{"args":{"path":["ocamlopt","other"]},"counters":{"time":51747999,"calls":6,"alloc":24559144,"top-heap":4910040,"absolute-top-heap":445421544},"tid":0,"pid":0,"name":"other","cat":"compiler","ts":1792773234794868,"dur":7138}
]|}

let trace_dir = lazy (Sys.getenv_opt "DUNE_ACTION_TRACE_DIR")

let make_trace_dir =
  lazy
    (match Lazy.force trace_dir with
    | None -> None
    | Some dir as some -> (
      match Sys.mkdir dir 0o777 with
      | () -> some
      | exception Sys_error _ when Sys.is_directory dir -> some
      | exception _ -> None))

let () =
  match Lazy.force make_trace_dir with
  | Some trace_dir ->
    let data = String.trim data ^ "\n" in
    let _, chan = Filename.open_temp_file ~temp_dir:trace_dir "compiler" ".json" in
    Fun.protect ~finally:(fun () -> close_out chan) (fun () ->
      output_string chan data;
      flush chan)
  | None -> ()

module Counters = struct
  type t = int String.Map.t

  let create () = String.Map.empty
  let get name t = String.Map.find_opt name t |> Option.value ~default:0
  let set name count t = String.Map.add name count t
  let incr name t =
    String.Map.update name
      (fun count_opt ->
        let count = Option.value ~default:0 count_opt in
        Some (succ count))
      t
  let is_empty = String.Map.is_empty
  let union = String.Map.union (fun _ count1 count2 -> Some (count1 + count2))
  let to_string t =
    t
    |> String.Map.bindings
    |> List.map (fun (name, count) -> Printf.sprintf "%s = %d" name count)
    |> String.concat "; "
    |> Printf.sprintf "[%s]"
end

external time_include_children: bool -> float = "caml_sys_time_include_children"
let cpu_time () = time_include_children true


module Measure = struct
  type t = {
    time : float;
    allocated_words : float;
    top_heap_words : int;
    counters : Counters.t;
  }
  let create ?(counters = Counters.create ()) () =
    let stat = Gc.quick_stat () in
    {
      time = cpu_time ();
      allocated_words = stat.minor_words +. stat.major_words;
      top_heap_words = stat.top_heap_words;
      counters = counters;
    }
  let zero = { time = 0.; allocated_words = 0.; top_heap_words = 0; counters = Counters.create () }
end

module Measure_diff = struct
  let timestamp = let r = ref (-1) in fun () -> incr r; !r
  type t = {
    timestamp : int;
    duration : float;
    allocated_words : float;
    top_heap_words_increase : int;
    counters : Counters.t;
  }
  let zero () = {
    timestamp = timestamp ();
    duration = 0.;
    allocated_words = 0.;
    top_heap_words_increase = 0;
    counters = Counters.create ();
  }
  let accumulate t (m1 : Measure.t) (m2 : Measure.t) = {
    timestamp = t.timestamp;
    duration = t.duration +. (m2.time -. m1.time);
    allocated_words =
      t.allocated_words +. (m2.allocated_words -. m1.allocated_words);
    top_heap_words_increase =
      t.top_heap_words_increase + (m2.top_heap_words - m1.top_heap_words);
    counters = Counters.union t.counters m2.counters
  }
  let of_diff m1 m2 =
    accumulate (zero ()) m1 m2
end

type hierarchy =
  | E of (string, Measure_diff.t * hierarchy) Hashtbl.t
[@@unboxed]

let create () = E (Hashtbl.create 2)
let hierarchy = ref (create ())
let initial_measure = ref None
let reset () = hierarchy := create (); initial_measure := None

let record_call_internal ?(accumulate = false) ?counter_f name f =
  if !Clflags.profile_columns = [] then f () else
  let E prev_hierarchy = !hierarchy in
  let start_measure = Measure.create () in
  if !initial_measure = None then initial_measure := Some start_measure;
  let this_measure_diff, this_table =
    (* We allow the recording of multiple categories by the same name, for tools
       like ocamldoc that use the compiler libs but don't care about profile
       information, and so may record, say, "parsing" multiple times. *)
    if accumulate
    then
      match Hashtbl.find prev_hierarchy name with
      | exception Not_found -> Measure_diff.zero (), Hashtbl.create 2
      | measure_diff, E table ->
        Hashtbl.remove prev_hierarchy name;
        measure_diff, table
    else Measure_diff.zero (), Hashtbl.create 2
  in
  hierarchy := E this_table;
  let counters = ref (Counters.create ()) in
  Misc.try_finally (
    match counter_f with
    | Some counter_f ->
        fun () ->
          let result = f () in
          if List.mem `Counters !Clflags.profile_columns then
            counters := counter_f result;
          result
    | None -> f
    )
    ~always:(fun () ->
        hierarchy := E prev_hierarchy;
        let end_measure = Measure.create ~counters:(!counters) () in
        let measure_diff =
          Measure_diff.accumulate this_measure_diff start_measure end_measure in
        Hashtbl.add prev_hierarchy name (measure_diff, E this_table))

let record_call = record_call_internal ?counter_f:None

let record_call_with_counters ?accumulate ~counter_f =
  record_call_internal ?accumulate ~counter_f

let record ?accumulate pass f x = record_call ?accumulate pass (fun () -> f x)

let record_with_counters ?accumulate ~counter_f pass f x =
  record_call_internal ?accumulate ~counter_f pass (fun () -> f x)

let file_prefix = "file="

let annotate_file_name name =
  let file_path = Clflags.prepend_directory name in
  file_prefix ^ file_path

type display = {
  to_string : max:float -> width:int -> string;
  worth_displaying : max:float -> bool;
}

let time_display precision v : display =
  (* Because indentation is meaningful, and because the durations are
     the first element of each row, we can't pad them with spaces. *)
  let to_string_without_unit v ~width = Printf.sprintf "%0*.*f" width precision v in
  let to_string ~max:_ ~width =
    to_string_without_unit v ~width:(width - 1) ^ "s" in
  let worth_displaying ~max:_ =
    float_of_string (to_string_without_unit v ~width:0) <> 0. in
  { to_string; worth_displaying }

let memory_word_display =
  (* To make memory numbers easily comparable across rows, we choose a single
     scale for an entire column. To keep the display compact and not overly
     precise (no one cares about the exact number of bytes), we pick the largest
     scale we can and we only show 3 digits. Avoiding showing tiny numbers also
     allows us to avoid displaying passes that barely allocate compared to the
     rest of the compiler.  *)
  let bytes_of_words words = words *. float_of_int (Sys.word_size / 8) in
  let to_string_without_unit v ~width scale =
    let precision = 3 and precision_power = 1e3 in
    let v_rescaled = bytes_of_words v /. scale in
    let v_rounded =
      floor (v_rescaled *. precision_power +. 0.5) /. precision_power in
    let v_str = Printf.sprintf "%.*f" precision v_rounded in
    let index_of_dot = String.index v_str '.' in
    let v_str_truncated =
      String.sub v_str 0
        (if index_of_dot >= precision
         then index_of_dot
         else precision + 1)
    in
    Printf.sprintf "%*s" width v_str_truncated
  in
  let choose_memory_scale =
    let units = [|"B"; "kB"; "MB"; "GB"|] in
    fun words ->
      let bytes = bytes_of_words words in
      let scale = ref (Array.length units - 1) in
      while !scale > 0 && bytes < 1024. ** float_of_int !scale do
        decr scale
      done;
      1024. ** float_of_int !scale, units.(!scale)
  in
  fun ?previous v : display ->
    let to_string ~max ~width =
      let scale, scale_str = choose_memory_scale max in
      let width = width - String.length scale_str in
      to_string_without_unit v ~width scale ^ scale_str
    in
    let worth_displaying ~max =
      let scale, _ = choose_memory_scale max in
      float_of_string (to_string_without_unit v ~width:0 scale) <> 0.
      && match previous with
      | None -> true
      | Some p ->
         (* This branch is for numbers that represent absolute quantity, rather
            than differences. It allows us to skip displaying the same absolute
            quantity many times in a row. *)
         to_string_without_unit p ~width:0 scale
         <> to_string_without_unit v ~width:0 scale
    in
    { to_string; worth_displaying }

let counters_display counters  =
  let to_string ~max:_ ~width:_ = Counters.to_string counters in
  let worth_displaying ~max:_ = not (Counters.is_empty counters) in
  0., { to_string; worth_displaying }

let profile_list (E table) =
  let l = Hashtbl.fold (fun k d l -> (k, d) :: l) table [] in
  List.sort (fun (_, (p1, _)) (_, (p2, _)) ->
    compare p1.Measure_diff.timestamp p2.Measure_diff.timestamp) l

let compute_other_category (E table : hierarchy) (total : Measure_diff.t) =
  let r = ref total in
  Hashtbl.iter (fun _pass ((p2 : Measure_diff.t), _) ->
    let p1 = !r in
    r := {
      timestamp = p1.timestamp;
      duration = p1.duration -. p2.duration;
      allocated_words = p1.allocated_words -. p2.allocated_words;
      top_heap_words_increase =
        p1.top_heap_words_increase - p2.top_heap_words_increase;
      counters = Counters.create ();
    }
  ) table;
  !r

type row = R of string * (float * display) list * row list

let rec rows_of_hierarchy ~nesting make_row name measure_diff hierarchy env =
  let rows =
    rows_of_hierarchy_list
      ~nesting:(nesting + 1) make_row hierarchy measure_diff env in
  let values, env =
    make_row env measure_diff ~toplevel_other:(nesting = 0 && name = "other") in
  R (name, values, rows), env

and rows_of_hierarchy_list ~nesting make_row hierarchy total env =
  let list = profile_list hierarchy in
  let list =
    if list <> [] || nesting = 0
    then list @ [ "other", (compute_other_category hierarchy total, create ()) ]
    else []
  in
  let env = ref env in
  List.map (fun (name, (measure_diff, hierarchy)) ->
    let a, env' =
      rows_of_hierarchy ~nesting make_row name measure_diff hierarchy !env in
    env := env';
    a
  ) list

let rows_of_hierarchy hierarchy measure_diff initial_measure columns timings_precision =
  (* Computing top heap size is a bit complicated: if the compiler applies a
     list of passes n times (rather than applying pass1 n times, then pass2 n
     times etc), we only show one row for that pass but what does "top heap
     size at the end of that pass" even mean?
     It seems the only sensible answer is to pretend the compiler applied pass1
     n times, pass2 n times by accumulating all the heap size increases that
     happened during each pass, and then compute what the heap size would have
     been. So that's what we do.
     There's a bit of extra complication, which is that the heap can increase in
     between measurements. So the heap sizes can be a bit off until the "other"
     rows account for what's missing. We special case the toplevel "other" row
     so that any increases that happened before the start of the compilation is
     correctly reported, as a lot of code may run before the start of the
     compilation (eg functor applications). *)
    let make_row prev_top_heap_words (p : Measure_diff.t) ~toplevel_other =
      let top_heap_words =
        prev_top_heap_words
        + p.top_heap_words_increase
        - if toplevel_other
          then initial_measure.Measure.top_heap_words
          else 0
      in
      let make value ~f = value, f value in
      List.map (function
        | `Time ->
          make p.duration ~f:(time_display timings_precision)
        | `Alloc ->
          make p.allocated_words ~f:memory_word_display
        | `Top_heap ->
          make (float_of_int p.top_heap_words_increase) ~f:memory_word_display
        | `Abs_top_heap ->
          make (float_of_int top_heap_words)
           ~f:(memory_word_display ~previous:(float_of_int prev_top_heap_words))
        | `Counters -> counters_display p.counters
      ) columns,
      top_heap_words
  in
  rows_of_hierarchy_list ~nesting:0 make_row hierarchy measure_diff
    initial_measure.top_heap_words

let max_by_column ~n_columns rows =
  let a = Array.make n_columns 0. in
  let rec loop (R (_, values, rows)) =
    List.iteri (fun i (v, _) -> a.(i) <- Float.max a.(i) v) values;
    List.iter loop rows
  in
  List.iter loop rows;
  a

let width_by_column ~n_columns ~display_cell rows =
  let a = Array.make n_columns 1 in
  let rec loop (R (_, values, rows)) =
    List.iteri (fun i cell ->
      let _, str = display_cell i cell ~width:0 in
      a.(i) <- Int.max a.(i) (String.length str)
    ) values;
    List.iter loop rows;
  in
  List.iter loop rows;
  a

let output_rows
    ~(output_row : prefix:string -> cell_strings:string list -> name:string -> unit)
    ~(new_prefix : prev:string -> curr_name:string -> string)
    ~(always_output_ancestors : bool)
    ~(pad_empty : bool)
    rows
  =
  let n_columns =
    match rows with
    | [] -> 0
    | R (_, values, _) :: _ -> List.length values
  in
  let maxs = max_by_column ~n_columns rows in
  let display_cell i (_, c) ~width =
    let display_cell = c.worth_displaying ~max:maxs.(i) in
    display_cell, if display_cell
                  then c.to_string ~max:maxs.(i) ~width
                  else if pad_empty then String.make width '-' else ""
  in
  let widths = width_by_column ~n_columns ~display_cell rows in
  (* We track print row functions in a queue to ensure ancestors not worth displaying have
  print functions executed if a descendant is worth displaying (possible with counters) *)
  let rec loop (R (name, values, rows)) ~prefix ~output_stack =
    let worth_displaying, cell_strings =
      values
      |> List.mapi (fun i cell -> display_cell i cell ~width:widths.(i))
      |> List.split
    in
    let should_output_row = List.exists (fun b -> b) worth_displaying in
    let output_current () =
      let cell_strings = if should_output_row then cell_strings else [] in
      output_row ~prefix ~cell_strings ~name
    in
    output_stack := output_current :: (if always_output_ancestors then !output_stack else []);
    if should_output_row then
      (List.rev !output_stack |> List.iter (fun f -> f ()); output_stack := []);
    List.iter (loop ~prefix:(new_prefix ~prev:prefix ~curr_name:name) ~output_stack) rows;
    if !output_stack <> [] then output_stack := List.tl !output_stack
  in
  List.iter (loop ~prefix:"" ~output_stack:(ref [])) rows

let output_columns output_rows_f columns ~timings_precision =
  match columns with
  | [] -> ()
  | _ :: _ ->
     let initial_measure =
       match !initial_measure with
       | Some v -> v
       | None -> Measure.zero
     in
     let total = Measure_diff.of_diff Measure.zero (Measure.create ()) in
     output_rows_f (rows_of_hierarchy !hierarchy total initial_measure columns timings_precision)

let print ppf =
  output_rows
    ~output_row:(fun ~prefix ~cell_strings ~name ->
      Format.fprintf ppf "%s%s %s@\n" prefix (String.concat " " cell_strings) name)
    ~new_prefix:(fun ~prev ~curr_name:_ -> "  " ^ prev)
    ~always_output_ancestors:true
    ~pad_empty:true
  |> output_columns

let column_mapping = [
  `Time, "time";
  `Alloc, "alloc";
  `Top_heap, "top-heap";
  `Abs_top_heap, "absolute-top-heap";
  `Counters, "counters"
]

let output_to_csv ppf columns =
  let sanitise_for_csv =
    String.map (fun c -> if Char.equal c ',' then '_' else c) in
  let to_csv cell_strings =
    cell_strings |> List.map sanitise_for_csv |> String.concat ","
  in
  let string_columns = List.map (fun col -> List.assoc col column_mapping) columns in
  Format.fprintf ppf "%s@\n" (to_csv ("pass name" :: string_columns));
  let add_suffix pass =
    let suffix = if String.starts_with ~prefix:file_prefix pass then "/" else "" in
    pass ^ suffix
  in
  let output_row_f =
    output_rows
      ~output_row:(fun ~prefix ~cell_strings ~name ->
        Format.fprintf ppf "%s@\n" (to_csv ((prefix ^ add_suffix name) :: cell_strings)))
      ~new_prefix:(fun ~prev ~curr_name ->
        Format.sprintf "%s%s/" prev (add_suffix curr_name))
      ~always_output_ancestors:false ~pad_empty:false
  in
  output_columns output_row_f columns

let all_columns = List.map fst column_mapping
let column_names = List.map snd column_mapping

let options_doc =
  Printf.sprintf
    " Print performance information for each pass\
   \n    The columns are: %s."
    (String.concat " " column_names)

let generate = "generate"
let transl = "transl"
let typing = "typing"
