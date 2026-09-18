This test is enabled both on 5.0.0 and 5.1.0. The test makes sense for as long
as the ppxlib AST is either 5.0.0 or 5.1.0. While the ppxlib AST is on 5.0.0, the
test checks whether parsing on 5.0.0 (result of test running on 5.0.0) is the same as
parsing on 5.1.0 and then migrating down to 5.0.0 (result of test running on 5.1.0).

The test is mostly useful for debuggung problems in a full round-trip. Since Ppxlib's
`dparsetree` option doesn't compactify or strip locations, its output is very long.
So let's only keep one example.

  $ echo "let x : int = 5" > file.ml
  $ ./identity_driver.exe -dparsetree file.ml
  (((pstr_desc
     (Pstr_attribute
      ((attr_name
        ((txt ocaml.ppx.context)
         (loc
          ((loc_start
            ((pos_fname _none_) (pos_lnum 0) (pos_bol 0) (pos_cnum -1)))
           (loc_end
            ((pos_fname _none_) (pos_lnum 0) (pos_bol 0) (pos_cnum -1)))
           (loc_ghost true)))))
       (attr_payload
        (PStr
         (((pstr_desc
            (Pstr_eval
             ((pexp_desc
               (Pexp_record
                ((((txt (Lident tool_name))
                   (loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true))))
                  ((pexp_desc
                    (Pexp_constant
                     (Pconst_string ppxlib_driver
                      ((loc_start
                        ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                         (pos_cnum -1)))
                       (loc_end
                        ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                         (pos_cnum -1)))
                       (loc_ghost true))
                      ())))
                   (pexp_loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true)))
                   (pexp_loc_stack ()) (pexp_attributes ())))
                 (((txt (Lident include_dirs))
                   (loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true))))
                  ((pexp_desc
                    (Pexp_construct
                     ((txt (Lident []))
                      (loc
                       ((loc_start
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_end
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_ghost true))))
                     ()))
                   (pexp_loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true)))
                   (pexp_loc_stack ()) (pexp_attributes ())))
                 (((txt (Lident load_path))
                   (loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true))))
                  ((pexp_desc
                    (Pexp_construct
                     ((txt (Lident []))
                      (loc
                       ((loc_start
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_end
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_ghost true))))
                     ()))
                   (pexp_loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true)))
                   (pexp_loc_stack ()) (pexp_attributes ())))
                 (((txt (Lident open_modules))
                   (loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true))))
                  ((pexp_desc
                    (Pexp_construct
                     ((txt (Lident []))
                      (loc
                       ((loc_start
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_end
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_ghost true))))
                     ()))
                   (pexp_loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true)))
                   (pexp_loc_stack ()) (pexp_attributes ())))
                 (((txt (Lident for_package))
                   (loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true))))
                  ((pexp_desc
                    (Pexp_construct
                     ((txt (Lident None))
                      (loc
                       ((loc_start
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_end
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_ghost true))))
                     ()))
                   (pexp_loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true)))
                   (pexp_loc_stack ()) (pexp_attributes ())))
                 (((txt (Lident debug))
                   (loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true))))
                  ((pexp_desc
                    (Pexp_construct
                     ((txt (Lident false))
                      (loc
                       ((loc_start
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_end
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_ghost true))))
                     ()))
                   (pexp_loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true)))
                   (pexp_loc_stack ()) (pexp_attributes ())))
                 (((txt (Lident use_threads))
                   (loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true))))
                  ((pexp_desc
                    (Pexp_construct
                     ((txt (Lident false))
                      (loc
                       ((loc_start
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_end
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_ghost true))))
                     ()))
                   (pexp_loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true)))
                   (pexp_loc_stack ()) (pexp_attributes ())))
                 (((txt (Lident use_vmthreads))
                   (loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true))))
                  ((pexp_desc
                    (Pexp_construct
                     ((txt (Lident false))
                      (loc
                       ((loc_start
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_end
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_ghost true))))
                     ()))
                   (pexp_loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true)))
                   (pexp_loc_stack ()) (pexp_attributes ())))
                 (((txt (Lident recursive_types))
                   (loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true))))
                  ((pexp_desc
                    (Pexp_construct
                     ((txt (Lident false))
                      (loc
                       ((loc_start
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_end
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_ghost true))))
                     ()))
                   (pexp_loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true)))
                   (pexp_loc_stack ()) (pexp_attributes ())))
                 (((txt (Lident principal))
                   (loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true))))
                  ((pexp_desc
                    (Pexp_construct
                     ((txt (Lident false))
                      (loc
                       ((loc_start
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_end
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_ghost true))))
                     ()))
                   (pexp_loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true)))
                   (pexp_loc_stack ()) (pexp_attributes ())))
                 (((txt (Lident transparent_modules))
                   (loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true))))
                  ((pexp_desc
                    (Pexp_construct
                     ((txt (Lident false))
                      (loc
                       ((loc_start
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_end
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_ghost true))))
                     ()))
                   (pexp_loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true)))
                   (pexp_loc_stack ()) (pexp_attributes ())))
                 (((txt (Lident unboxed_types))
                   (loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true))))
                  ((pexp_desc
                    (Pexp_construct
                     ((txt (Lident false))
                      (loc
                       ((loc_start
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_end
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_ghost true))))
                     ()))
                   (pexp_loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true)))
                   (pexp_loc_stack ()) (pexp_attributes ())))
                 (((txt (Lident unsafe_string))
                   (loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true))))
                  ((pexp_desc
                    (Pexp_construct
                     ((txt (Lident false))
                      (loc
                       ((loc_start
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_end
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_ghost true))))
                     ()))
                   (pexp_loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true)))
                   (pexp_loc_stack ()) (pexp_attributes ())))
                 (((txt (Lident cookies))
                   (loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true))))
                  ((pexp_desc
                    (Pexp_construct
                     ((txt (Lident []))
                      (loc
                       ((loc_start
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_end
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_ghost true))))
                     ()))
                   (pexp_loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true)))
                   (pexp_loc_stack ()) (pexp_attributes ()))))
                ()))
              (pexp_loc
               ((loc_start
                 ((pos_fname _none_) (pos_lnum 0) (pos_bol 0) (pos_cnum -1)))
                (loc_end
                 ((pos_fname _none_) (pos_lnum 0) (pos_bol 0) (pos_cnum -1)))
                (loc_ghost true)))
              (pexp_loc_stack ()) (pexp_attributes ()))
             ()))
           (pstr_loc
            ((loc_start
              ((pos_fname _none_) (pos_lnum 0) (pos_bol 0) (pos_cnum -1)))
             (loc_end
              ((pos_fname _none_) (pos_lnum 0) (pos_bol 0) (pos_cnum -1)))
             (loc_ghost true)))))))
       (attr_loc
        ((loc_start
          ((pos_fname _none_) (pos_lnum 0) (pos_bol 0) (pos_cnum -1)))
         (loc_end ((pos_fname _none_) (pos_lnum 0) (pos_bol 0) (pos_cnum -1)))
         (loc_ghost true))))))
    (pstr_loc
     ((loc_start ((pos_fname _none_) (pos_lnum 0) (pos_bol 0) (pos_cnum -1)))
      (loc_end ((pos_fname _none_) (pos_lnum 0) (pos_bol 0) (pos_cnum -1)))
      (loc_ghost true))))
   ((pstr_desc
     (Pstr_value Nonrecursive
      (((pvb_pat
         ((ppat_desc
           (Ppat_constraint
            ((ppat_desc
              (Ppat_var
               ((txt x)
                (loc
                 ((loc_start
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
                  (loc_end
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 5)))
                  (loc_ghost false))))))
             (ppat_loc
              ((loc_start
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
               (loc_end
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 5)))
               (loc_ghost false)))
             (ppat_loc_stack ()) (ppat_attributes ()))
            ((ptyp_desc
              (Ptyp_poly ()
               ((ptyp_desc
                 (Ptyp_constr
                  ((txt (Lident int))
                   (loc
                    ((loc_start
                      ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                       (pos_cnum 8)))
                     (loc_end
                      ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                       (pos_cnum 11)))
                     (loc_ghost false))))
                  ()))
                (ptyp_loc
                 ((loc_start
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 8)))
                  (loc_end
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 11)))
                  (loc_ghost false)))
                (ptyp_loc_stack ()) (ptyp_attributes ()))))
             (ptyp_loc
              ((loc_start
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 8)))
               (loc_end
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 11)))
               (loc_ghost true)))
             (ptyp_loc_stack ()) (ptyp_attributes ()))))
          (ppat_loc
           ((loc_start
             ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
            (loc_end
             ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 11)))
            (loc_ghost true)))
          (ppat_loc_stack ()) (ppat_attributes ())))
        (pvb_expr
         ((pexp_desc
           (Pexp_constraint
            ((pexp_desc (Pexp_constant (Pconst_integer 5 ())))
             (pexp_loc
              ((loc_start
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 14)))
               (loc_end
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 15)))
               (loc_ghost false)))
             (pexp_loc_stack ()) (pexp_attributes ()))
            ((ptyp_desc
              (Ptyp_constr
               ((txt (Lident int))
                (loc
                 ((loc_start
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 8)))
                  (loc_end
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 11)))
                  (loc_ghost false))))
               ()))
             (ptyp_loc
              ((loc_start
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 8)))
               (loc_end
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 11)))
               (loc_ghost false)))
             (ptyp_loc_stack ()) (ptyp_attributes ()))))
          (pexp_loc
           ((loc_start
             ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
            (loc_end
             ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 15)))
            (loc_ghost false)))
          (pexp_loc_stack ()) (pexp_attributes ())))
        (pvb_attributes ())
        (pvb_loc
         ((loc_start
           ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
          (loc_end
           ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 15)))
          (loc_ghost false)))))))
    (pstr_loc
     ((loc_start ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
      (loc_end ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 15)))
      (loc_ghost false)))))

  $ cat > file.ml << EOF
  > module F () = struct end
  > module M = F ()
  > EOF
  $ ./identity_driver.exe -dparsetree file.ml
  (((pstr_desc
     (Pstr_attribute
      ((attr_name
        ((txt ocaml.ppx.context)
         (loc
          ((loc_start
            ((pos_fname _none_) (pos_lnum 0) (pos_bol 0) (pos_cnum -1)))
           (loc_end
            ((pos_fname _none_) (pos_lnum 0) (pos_bol 0) (pos_cnum -1)))
           (loc_ghost true)))))
       (attr_payload
        (PStr
         (((pstr_desc
            (Pstr_eval
             ((pexp_desc
               (Pexp_record
                ((((txt (Lident tool_name))
                   (loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true))))
                  ((pexp_desc
                    (Pexp_constant
                     (Pconst_string ppxlib_driver
                      ((loc_start
                        ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                         (pos_cnum -1)))
                       (loc_end
                        ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                         (pos_cnum -1)))
                       (loc_ghost true))
                      ())))
                   (pexp_loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true)))
                   (pexp_loc_stack ()) (pexp_attributes ())))
                 (((txt (Lident include_dirs))
                   (loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true))))
                  ((pexp_desc
                    (Pexp_construct
                     ((txt (Lident []))
                      (loc
                       ((loc_start
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_end
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_ghost true))))
                     ()))
                   (pexp_loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true)))
                   (pexp_loc_stack ()) (pexp_attributes ())))
                 (((txt (Lident load_path))
                   (loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true))))
                  ((pexp_desc
                    (Pexp_construct
                     ((txt (Lident []))
                      (loc
                       ((loc_start
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_end
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_ghost true))))
                     ()))
                   (pexp_loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true)))
                   (pexp_loc_stack ()) (pexp_attributes ())))
                 (((txt (Lident open_modules))
                   (loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true))))
                  ((pexp_desc
                    (Pexp_construct
                     ((txt (Lident []))
                      (loc
                       ((loc_start
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_end
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_ghost true))))
                     ()))
                   (pexp_loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true)))
                   (pexp_loc_stack ()) (pexp_attributes ())))
                 (((txt (Lident for_package))
                   (loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true))))
                  ((pexp_desc
                    (Pexp_construct
                     ((txt (Lident None))
                      (loc
                       ((loc_start
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_end
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_ghost true))))
                     ()))
                   (pexp_loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true)))
                   (pexp_loc_stack ()) (pexp_attributes ())))
                 (((txt (Lident debug))
                   (loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true))))
                  ((pexp_desc
                    (Pexp_construct
                     ((txt (Lident false))
                      (loc
                       ((loc_start
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_end
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_ghost true))))
                     ()))
                   (pexp_loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true)))
                   (pexp_loc_stack ()) (pexp_attributes ())))
                 (((txt (Lident use_threads))
                   (loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true))))
                  ((pexp_desc
                    (Pexp_construct
                     ((txt (Lident false))
                      (loc
                       ((loc_start
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_end
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_ghost true))))
                     ()))
                   (pexp_loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true)))
                   (pexp_loc_stack ()) (pexp_attributes ())))
                 (((txt (Lident use_vmthreads))
                   (loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true))))
                  ((pexp_desc
                    (Pexp_construct
                     ((txt (Lident false))
                      (loc
                       ((loc_start
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_end
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_ghost true))))
                     ()))
                   (pexp_loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true)))
                   (pexp_loc_stack ()) (pexp_attributes ())))
                 (((txt (Lident recursive_types))
                   (loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true))))
                  ((pexp_desc
                    (Pexp_construct
                     ((txt (Lident false))
                      (loc
                       ((loc_start
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_end
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_ghost true))))
                     ()))
                   (pexp_loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true)))
                   (pexp_loc_stack ()) (pexp_attributes ())))
                 (((txt (Lident principal))
                   (loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true))))
                  ((pexp_desc
                    (Pexp_construct
                     ((txt (Lident false))
                      (loc
                       ((loc_start
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_end
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_ghost true))))
                     ()))
                   (pexp_loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true)))
                   (pexp_loc_stack ()) (pexp_attributes ())))
                 (((txt (Lident transparent_modules))
                   (loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true))))
                  ((pexp_desc
                    (Pexp_construct
                     ((txt (Lident false))
                      (loc
                       ((loc_start
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_end
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_ghost true))))
                     ()))
                   (pexp_loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true)))
                   (pexp_loc_stack ()) (pexp_attributes ())))
                 (((txt (Lident unboxed_types))
                   (loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true))))
                  ((pexp_desc
                    (Pexp_construct
                     ((txt (Lident false))
                      (loc
                       ((loc_start
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_end
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_ghost true))))
                     ()))
                   (pexp_loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true)))
                   (pexp_loc_stack ()) (pexp_attributes ())))
                 (((txt (Lident unsafe_string))
                   (loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true))))
                  ((pexp_desc
                    (Pexp_construct
                     ((txt (Lident false))
                      (loc
                       ((loc_start
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_end
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_ghost true))))
                     ()))
                   (pexp_loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true)))
                   (pexp_loc_stack ()) (pexp_attributes ())))
                 (((txt (Lident cookies))
                   (loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true))))
                  ((pexp_desc
                    (Pexp_construct
                     ((txt (Lident []))
                      (loc
                       ((loc_start
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_end
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_ghost true))))
                     ()))
                   (pexp_loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true)))
                   (pexp_loc_stack ()) (pexp_attributes ()))))
                ()))
              (pexp_loc
               ((loc_start
                 ((pos_fname _none_) (pos_lnum 0) (pos_bol 0) (pos_cnum -1)))
                (loc_end
                 ((pos_fname _none_) (pos_lnum 0) (pos_bol 0) (pos_cnum -1)))
                (loc_ghost true)))
              (pexp_loc_stack ()) (pexp_attributes ()))
             ()))
           (pstr_loc
            ((loc_start
              ((pos_fname _none_) (pos_lnum 0) (pos_bol 0) (pos_cnum -1)))
             (loc_end
              ((pos_fname _none_) (pos_lnum 0) (pos_bol 0) (pos_cnum -1)))
             (loc_ghost true)))))))
       (attr_loc
        ((loc_start
          ((pos_fname _none_) (pos_lnum 0) (pos_bol 0) (pos_cnum -1)))
         (loc_end ((pos_fname _none_) (pos_lnum 0) (pos_bol 0) (pos_cnum -1)))
         (loc_ghost true))))))
    (pstr_loc
     ((loc_start ((pos_fname _none_) (pos_lnum 0) (pos_bol 0) (pos_cnum -1)))
      (loc_end ((pos_fname _none_) (pos_lnum 0) (pos_bol 0) (pos_cnum -1)))
      (loc_ghost true))))
   ((pstr_desc
     (Pstr_module
      ((pmb_name
        ((txt (F))
         (loc
          ((loc_start
            ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 7)))
           (loc_end
            ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 8)))
           (loc_ghost false)))))
       (pmb_expr
        ((pmod_desc
          (Pmod_functor Unit
           ((pmod_desc (Pmod_structure ()))
            (pmod_loc
             ((loc_start
               ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 14)))
              (loc_end
               ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 24)))
              (loc_ghost false)))
            (pmod_attributes ()))))
         (pmod_loc
          ((loc_start
            ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 9)))
           (loc_end
            ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 24)))
           (loc_ghost false)))
         (pmod_attributes ())))
       (pmb_attributes ())
       (pmb_loc
        ((loc_start
          ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
         (loc_end ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 24)))
         (loc_ghost false))))))
    (pstr_loc
     ((loc_start ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
      (loc_end ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 24)))
      (loc_ghost false))))
   ((pstr_desc
     (Pstr_module
      ((pmb_name
        ((txt (M))
         (loc
          ((loc_start
            ((pos_fname file.ml) (pos_lnum 2) (pos_bol 25) (pos_cnum 32)))
           (loc_end
            ((pos_fname file.ml) (pos_lnum 2) (pos_bol 25) (pos_cnum 33)))
           (loc_ghost false)))))
       (pmb_expr
        ((pmod_desc
          (Pmod_apply
           ((pmod_desc
             (Pmod_ident
              ((txt (Lident F))
               (loc
                ((loc_start
                  ((pos_fname file.ml) (pos_lnum 2) (pos_bol 25) (pos_cnum 36)))
                 (loc_end
                  ((pos_fname file.ml) (pos_lnum 2) (pos_bol 25) (pos_cnum 37)))
                 (loc_ghost false))))))
            (pmod_loc
             ((loc_start
               ((pos_fname file.ml) (pos_lnum 2) (pos_bol 25) (pos_cnum 36)))
              (loc_end
               ((pos_fname file.ml) (pos_lnum 2) (pos_bol 25) (pos_cnum 37)))
              (loc_ghost false)))
            (pmod_attributes ()))
           ((pmod_desc (Pmod_structure ()))
            (pmod_loc
             ((loc_start
               ((pos_fname file.ml) (pos_lnum 2) (pos_bol 25) (pos_cnum 36)))
              (loc_end
               ((pos_fname file.ml) (pos_lnum 2) (pos_bol 25) (pos_cnum 40)))
              (loc_ghost false)))
            (pmod_attributes ()))))
         (pmod_loc
          ((loc_start
            ((pos_fname file.ml) (pos_lnum 2) (pos_bol 25) (pos_cnum 36)))
           (loc_end
            ((pos_fname file.ml) (pos_lnum 2) (pos_bol 25) (pos_cnum 40)))
           (loc_ghost false)))
         (pmod_attributes ())))
       (pmb_attributes ())
       (pmb_loc
        ((loc_start
          ((pos_fname file.ml) (pos_lnum 2) (pos_bol 25) (pos_cnum 25)))
         (loc_end
          ((pos_fname file.ml) (pos_lnum 2) (pos_bol 25) (pos_cnum 40)))
         (loc_ghost false))))))
    (pstr_loc
     ((loc_start ((pos_fname file.ml) (pos_lnum 2) (pos_bol 25) (pos_cnum 25)))
      (loc_end ((pos_fname file.ml) (pos_lnum 2) (pos_bol 25) (pos_cnum 40)))
      (loc_ghost false)))))
