This list is already trimmed down a bit to those files that are potentially relevant.

**Deleted during the merge:** Note the dynlink one:
```
    deleted:    otherlibs/unix/nanosecond_stat.h
    deleted:    stdlib/hashbang
    deleted:    otherlibs/dynlink/dynlink_compilerlibs/Makefile.copy-sources
```

**Deleted upstream, but modified by us:**
```
    deleted by them: otherlibs/dynlink/Makefile
```

**Deleted by us, but modified upsteam:**
```
    deleted by us:   driver/optcompile.ml
    deleted by us:   toplevel/native/topeval.ml
```

The relevant upstream commits are 23e84b8c4d (previous version) and
2a5ed46dd2 (new version). You can access the diff between both versions
with `git diff 23e84b8c4d 2a5ed46dd2 -- path/to/file`. The diff of the
upstream modifications is below:

```
diff --git a/driver/optcompile.ml b/driver/optcompile.ml
index 1638951fbb..5278e5bfe7 100644
--- a/driver/optcompile.ml
+++ b/driver/optcompile.ml
@@ -24,7 +24,8 @@ let with_info =
   Compile_common.with_info ~native:true ~tool_name

 let interface ~source_file ~output_prefix =
-  with_info ~source_file ~output_prefix ~dump_ext:"cmi" @@ fun info ->
+  let unit_info = Unit_info.make ~source_file Intf output_prefix in
+  with_info ~dump_ext:"cmi" unit_info @@ fun info ->
   Compile_common.interface info

 let (|>>) (x, y) f = (x, f y)
@@ -108,7 +109,8 @@ let implementation ~backend ~start_from ~source_file ~output_prefix =
     then flambda info backend typed
     else clambda info backend typed
   in
-  with_info ~source_file ~output_prefix ~dump_ext:"cmx" @@ fun info ->
+  let unit_info = Unit_info.make ~source_file Impl output_prefix in
+  with_info ~dump_ext:"cmx" unit_info @@ fun info ->
   match (start_from:Clflags.Compiler_pass.t) with
   | Parsing -> Compile_common.implementation info ~backend
   | Emit -> emit info
diff --git a/toplevel/native/topeval.ml b/toplevel/native/topeval.ml
index 241849bd60..194e8e06ca 100644
--- a/toplevel/native/topeval.ml
+++ b/toplevel/native/topeval.ml
@@ -106,7 +106,7 @@ let load_lambda ppf ~module_ident ~required_globals phrase_name lam size =
 (* Print the outcome of an evaluation *)

 let pr_item =
-  Printtyp.print_items
+  Out_type.print_items
     (fun env -> function
       | Sig_value(id, {val_kind = Val_reg; val_type}, _) ->
           Some (outval_of_value env (toplevel_value id) val_type)
@@ -129,7 +129,7 @@ let name_expression ~loc ~attrs exp =
    in
    let sg = [Sig_value(id, vd, Exported)] in
    let pat =
-     { pat_desc = Tpat_var(id, mknoloc name);
+     { pat_desc = Tpat_var(id, mknoloc name, vd.val_uid);
        pat_loc = loc;
        pat_extra = [];
        pat_type = exp.exp_type;
@@ -139,7 +139,7 @@ let name_expression ~loc ~attrs exp =
    let vb =
      { vb_pat = pat;
        vb_expr = exp;
-       vb_rec_kind = Not_recursive;
+       vb_rec_kind = Dynamic;
        vb_attributes = attrs;
        vb_loc = loc; }
    in
@@ -163,16 +163,7 @@ let execute_phrase print_outcome ppf phr =
       incr phrase_seqid;
       let phrase_name = "TOP" ^ string_of_int !phrase_seqid in
       Compilenv.reset ?packname:None phrase_name;
-      Typecore.reset_delayed_checks ();
-      let (str, sg, names, shape, newenv) =
-        Typemod.type_toplevel_phrase oldenv sstr
-      in
-      if !Clflags.dump_typedtree then Printtyped.implementation ppf str;
-      let sg' = Typemod.Signature_names.simplify newenv names sg in
-      ignore (Includemod.signatures oldenv ~mark:Mark_positive sg sg');
-      Typecore.force_delayed_checks ();
-      let shape = Shape.local_reduce shape in
-      if !Clflags.dump_shape then Shape.print ppf shape;
+      let (str, sg', newenv) = typecheck_phrase ppf oldenv sstr in
       (* `let _ = <expression>` or even just `<expression>` require special
          handling in toplevels, or nothing is displayed. In bytecode, the
          lambda for <expression> is directly executed and the result _is_ the
@@ -232,7 +223,10 @@ let execute_phrase print_outcome ppf phr =
                             outval_of_value newenv (toplevel_value id)
                               vd.val_type
                           in
-                          let ty = Printtyp.tree_of_type_scheme vd.val_type in
+                          let ty =
+                            Out_type.prepare_for_printing [vd.val_type];
+                            Out_type.tree_of_typexp Type_scheme vd.val_type
+                          in
                           Ophr_eval (outv, ty)
                       | _ -> assert false
                     else
```
