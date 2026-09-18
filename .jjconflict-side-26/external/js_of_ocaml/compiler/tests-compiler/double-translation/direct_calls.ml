(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

open Util

let%expect_test "direct calls with --effects=double-translation" =
  let code =
    compile_and_parse
      ~effects:`Double_translation
      {|
         let l = ref []

         (* Arity of the argument of a function / direct call *)
         let test1 () =
           let f g x =
             l := (fun () -> ()) :: !l; (* pervent inlining *)
             try g x with e -> raise e in
           ignore (f (fun x -> x + 1) 7);
           ignore (f (fun x -> x *. 2.) 4.)

         (* Arity of the argument of a function / CPS call *)
         let test2 () =
           let f g x =
             l := (fun () -> ()) :: !l; (* pervent inlining *)
             try g x with e -> raise e in
           ignore (f (fun x -> x + 1) 7);
           ignore (f (fun x -> x ^ "a") "a")

         (* Arity of functions in a functor / direct call *)
         let test3 x =
       let module F(_ : sig end) = struct
         let r = ref 0
         let () = for _ = 0 to 2 do incr r done (* pervent inlining *)
         let f x = x + 1
       end in
           let module M1 = F (struct end) in
           let module M2 = F (struct end) in
           (M1.f 1, M2.f 2)

         (* Arity of functions in a functor / CPS call *)
         let test4 x =
           let module F(_ : sig end) =
             struct
               let r = ref 0
               let () = for _ = 0 to 2 do incr r done (* pervent inlining *)
               let f x = Printf.printf "%d" x
             end in
           let module M1 = F (struct end) in
           let module M2 = F (struct end) in
           M1.f 1; M2.f 2

         (* Result of double-translating two mutually recursive functions *)
         let test5 () =
           let g x =
             l := (fun () -> ()) :: !l; (* pervent inlining *)
             let rec f y = if y = 0 then 1 else x + h (y - 1)
             and h z = if z = 0 then 1 else x + f (z - 1)
             in
             print_int (f 12 + h 100)
           in
           ignore (g 42);
           ignore (g (-5));
|}
  in
  print_program code;
  [%expect
    {|
    (function(globalThis){
       "use strict";
       var
        runtime = globalThis.jsoo_runtime,
        caml_get_global = runtime.caml_get_global,
        caml_maybe_attach_backtrace = runtime.caml_maybe_attach_backtrace,
        caml_string_of_jsbytes = runtime.caml_string_of_jsbytes,
        caml_wrap_exception = runtime.caml_wrap_exception;
       function caml_call1(f, a0){
        return (f.l >= 0 ? f.l : f.l = f.length) === 1
                ? f(a0)
                : runtime.caml_call_gen(f, [a0]);
       }
       function caml_call2(f, a0, a1){
        return (f.l >= 0 ? f.l : f.l = f.length) === 2
                ? f(a0, a1)
                : runtime.caml_call_gen(f, [a0, a1]);
       }
       var
        dummy = 0,
        Stdlib = caml_get_global("Stdlib"),
        Stdlib_Printf = caml_get_global("Stdlib__Printf"),
        cst_a = caml_string_of_jsbytes("a"),
        cst_a$0 = caml_string_of_jsbytes("a"),
        _a_ = [0, [4, 0, 0, 0, 0], caml_string_of_jsbytes("%d")],
        l = [0, 0];
       function test1(param){
        function f(g, x){
         l[1] = [0, function(param){return 0;}, l[1]];
         try{g(); return;}
         catch(e$0){
          var e = caml_wrap_exception(e$0);
          throw caml_maybe_attach_backtrace(e, 0);
         }
        }
        f(function(x){});
        f(function(x){});
        return 0;
       }
       function test2(param){
        function f(g, x){
         l[1] = [0, function(param){return 0;}, l[1]];
         try{g(x); return;}
         catch(e$0){
          var e = caml_wrap_exception(e$0);
          throw caml_maybe_attach_backtrace(e, 0);
         }
        }
        f(function(x){}, 7);
        f(function(x){return caml_call2(Stdlib[28], x, cst_a$0);}, cst_a);
        return 0;
       }
       function test3(x){
        function F(symbol){
         var for$ = 0, r = [0, 0];
         for(;;){
          r[1]++;
          var _b_ = for$ + 1 | 0;
          if(2 === for$) break;
          for$ = _b_;
         }
         function f(x){return x + 1 | 0;}
         return [0, , f];
        }
        var M1 = F(), M2 = F(), _b_ = M2[2].call(null, 2);
        return [0, M1[2].call(null, 1), _b_];
       }
       function test4(x){
        function F(symbol){
         var for$ = 0, r = [0, 0];
         for(;;){
          r[1]++;
          var _b_ = for$ + 1 | 0;
          if(2 === for$) break;
          for$ = _b_;
         }
         function f(x){return caml_call2(Stdlib_Printf[2], _a_, x);}
         return [0, , f];
        }
        var M1 = F(), M2 = F();
        M1[2].call(null, 1);
        return M2[2].call(null, 2);
       }
       function test5(param){
        function g(x){
         l[1] = [0, function(param){return 0;}, l[1]];
         function f(y){return 0 === y ? 1 : x + h(y - 1 | 0) | 0;}
         function h(z){return 0 === z ? 1 : x + f(z - 1 | 0) | 0;}
         var _a_ = h(100), _a_ = f(12) + _a_ | 0;
         return caml_call1(Stdlib[44], _a_);
        }
        g(42);
        g(- 5);
        return 0;
       }
       runtime.caml_register_global
        ([0, l, test1, test2, test3, test4, test5], "Test");
       return;
      }
      (globalThis));
    //end
    |}]
