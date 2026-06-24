(let
  (open/400 =
     (let
       (__enabled_zeroprot_features/279 =[value<int>]
          (field_imm 0 (field_imm 1 (global Ppx_zeroprot_runtime!))))
       (makeblock 0 __enabled_zeroprot_features/279))
   build/289 =
     (function {nlocal = 0} n/291[value<int>]
       (apply (field_imm 1 (field_imm 26 (global CamlinternalQuote!)))
         (apply (field_imm 1 (field_imm 0 (global CamlinternalQuote!)))
           "example.ml" 10 212 27 753)
         (apply (field_imm 0 (field_imm 25 (global CamlinternalQuote!)))
           (apply (field_imm 4 (field_imm 24 (global CamlinternalQuote!)))
             (apply (field_imm 2 (field_imm 22 (global CamlinternalQuote!)))
               (field_imm 1 (field_imm 5 (global CamlinternalQuote!))) 0
               (apply (field_imm 1 (field_imm 0 (global CamlinternalQuote!)))
                 "example.ml" 10 215 27 750)
               0
               (function {nlocal = 0} t/613
                 (makeblock 0
                   (apply
                     (field_imm 8 (field_imm 17 (global CamlinternalQuote!)))
                     (apply
                       (field_imm 0
                         (field_imm 10 (global CamlinternalQuote!)))
                       (field_imm 3
                         (field_imm 4
                           (field_imm 4 (global CamlinternalQuote!)))))
                     0)
                   (apply
                     (field_imm 0 (field_imm 22 (global CamlinternalQuote!)))
                     (apply
                       (field_imm 0
                         (field_imm 25 (global CamlinternalQuote!)))
                       (apply
                         (field_imm 3
                           (field_imm 24 (global CamlinternalQuote!)))
                         (apply
                           (field_imm 1
                             (field_imm 0 (global CamlinternalQuote!)))
                           "example.ml" 11 230 27 750)
                         (makeblock 0
                           (apply
                             (field_imm 0
                               (field_imm 1 (global CamlinternalQuote!)))
                             "knocked_out")
                           0)
                         0
                         (makeblock 0
                           (apply
                             (field_imm 0
                               (field_imm 25 (global CamlinternalQuote!)))
                             (apply
                               (field_imm 5
                                 (field_imm 24 (global CamlinternalQuote!)))
                               (apply
                                 (field_imm 0
                                   (field_imm 25 (global CamlinternalQuote!)))
                                 (apply
                                   (field_imm 0
                                     (field_imm 24
                                       (global CamlinternalQuote!)))
                                   (apply
                                     (field_imm 0
                                       (field_imm 1
                                         (field_imm 4
                                           (global CamlinternalQuote!))))
                                     (apply
                                       (field_imm 0
                                         (field_imm 0
                                           (field_imm 4
                                             (global CamlinternalQuote!))))
                                       "Stdlib")
                                     "ref"))
                                 0)
                               (makeblock 0
                                 (makeblock 0
                                   (field_imm 1
                                     (field_imm 5
                                       (global CamlinternalQuote!)))
                                   (apply
                                     (field_imm 0
                                       (field_imm 25
                                         (global CamlinternalQuote!)))
                                     (apply
                                       (field_imm 1
                                         (field_imm 24
                                           (global CamlinternalQuote!)))
                                       (apply
                                         (field_imm 0
                                           (field_imm 3
                                             (global CamlinternalQuote!)))
                                         0))
                                     0))
                                 0))
                             0)
                           0)
                         (makeblock 0 0 0)
                         (function {nlocal = 0} t/605
                           (let (knocked_out/292 = (field_imm 0 t/605))
                             (function {nlocal = 0} t/604
                               (makeblock 0
                                 (apply
                                   (field_imm 6
                                     (field_imm 17
                                       (global CamlinternalQuote!)))
                                   (makeblock 0
                                     (makeblock 0
                                       (field_imm 0
                                         (field_imm 0
                                           (field_imm 5
                                             (global CamlinternalQuote!))))
                                       (apply
                                         (field_imm 1
                                           (field_imm 17
                                             (global CamlinternalQuote!)))
                                         knocked_out/292))
                                     0))
                                 (apply
                                   (field_imm 0
                                     (field_imm 25
                                       (global CamlinternalQuote!)))
                                   (apply
                                     (field_imm 3
                                       (field_imm 24
                                         (global CamlinternalQuote!)))
                                     (apply
                                       (field_imm 1
                                         (field_imm 0
                                           (global CamlinternalQuote!)))
                                       "example.ml" 12 262 27 750)
                                     0 0
                                     (makeblock 0
                                       (apply
                                         (field_imm 0
                                           (field_imm 25
                                             (global CamlinternalQuote!)))
                                         (apply
                                           (field_imm 25
                                             (field_imm 24
                                               (global CamlinternalQuote!)))
                                           (apply
                                             (field_imm 0
                                               (field_imm 25
                                                 (global CamlinternalQuote!)))
                                             (apply
                                               (field_imm 47
                                                 (field_imm 24
                                                   (global CamlinternalQuote!)))
                                               (region
                                                 (letrec
                                                   (loop/293
                                                      (function {nlocal = 1}
                                                        param/298[#(?, ?)]
                                                        : local
                                                        (let
                                                          (n/295 =a?
                                                             (unboxed_product_field 1 #(?, ?)
                                                               param/298)
                                                           acc/294 =a?
                                                             (unboxed_product_field 0 #(?, ?)
                                                               param/298))
                                                          (if
                                                            (%int_lessequal
                                                              n/295 0)
                                                            (apply
                                                              (field_imm 1
                                                                (field_imm 26
                                                                  (global CamlinternalQuote!)))
                                                              (apply
                                                                (field_imm 1
                                                                  (field_imm 0
                                                                    (global CamlinternalQuote!)))
                                                                "example.ml"
                                                                15 394 15
                                                                402)
                                                              (apply
                                                                (field_imm 0
                                                                  (field_imm 25
                                                                    (global CamlinternalQuote!)))
                                                                (apply
                                                                  (field_imm 9
                                                                    (field_imm 24
                                                                    (global CamlinternalQuote!)))
                                                                  (apply
                                                                    (field_imm 0
                                                                    (field_imm 10
                                                                    (global CamlinternalQuote!)))
                                                                    (field_imm 4
                                                                    (field_imm 4
                                                                    (field_imm 4
                                                                    (global CamlinternalQuote!)))))
                                                                  0)
                                                                0))
                                                            (apply
                                                              (field_imm 1
                                                                (field_imm 26
                                                                  (global CamlinternalQuote!)))
                                                              (apply
                                                                (field_imm 1
                                                                  (field_imm 0
                                                                    (global CamlinternalQuote!)))
                                                                "example.ml"
                                                                17 432 23
                                                                686)
                                                              (apply
                                                                (field_imm 0
                                                                  (field_imm 25
                                                                    (global CamlinternalQuote!)))
                                                                (apply
                                                                  (field_imm 3
                                                                    (field_imm 24
                                                                    (global CamlinternalQuote!)))
                                                                  (apply
                                                                    (field_imm 1
                                                                    (field_imm 0
                                                                    (global CamlinternalQuote!)))
                                                                    "example.ml"
                                                                    17 435 23
                                                                    683)
                                                                  (makeblock 0
                                                                    (apply
                                                                    (field_imm 0
                                                                    (field_imm 1
                                                                    (global CamlinternalQuote!)))
                                                                    "knocked_out_date")
                                                                    0)
                                                                  0
                                                                  (makeblock 0
                                                                    (apply
                                                                    (field_imm 0
                                                                    (field_imm 25
                                                                    (global CamlinternalQuote!)))
                                                                    (apply
                                                                    (field_imm 7
                                                                    (field_imm 24
                                                                    (global CamlinternalQuote!)))
                                                                    (apply
                                                                    (field_imm 0
                                                                    (field_imm 25
                                                                    (global CamlinternalQuote!)))
                                                                    (apply
                                                                    (field_imm 5
                                                                    (field_imm 24
                                                                    (global CamlinternalQuote!)))
                                                                    (apply
                                                                    (field_imm 0
                                                                    (field_imm 25
                                                                    (global CamlinternalQuote!)))
                                                                    (apply
                                                                    (field_imm 0
                                                                    (field_imm 24
                                                                    (global CamlinternalQuote!)))
                                                                    (apply
                                                                    (field_imm 0
                                                                    (field_imm 1
                                                                    (field_imm 4
                                                                    (global CamlinternalQuote!))))
                                                                    (apply
                                                                    (field_imm 0
                                                                    (field_imm 0
                                                                    (field_imm 4
                                                                    (global CamlinternalQuote!))))
                                                                    "Stdlib")
                                                                    "raise"))
                                                                    0)
                                                                    (makeblock 0
                                                                    (makeblock 0
                                                                    (field_imm 1
                                                                    (field_imm 5
                                                                    (global CamlinternalQuote!)))
                                                                    (apply
                                                                    (field_imm 0
                                                                    (field_imm 25
                                                                    (global CamlinternalQuote!)))
                                                                    (apply
                                                                    (field_imm 9
                                                                    (field_imm 24
                                                                    (global CamlinternalQuote!)))
                                                                    (apply
                                                                    (field_imm 0
                                                                    (field_imm 10
                                                                    (global CamlinternalQuote!)))
                                                                    (apply
                                                                    (field_imm 0
                                                                    (field_imm 4
                                                                    (field_imm 4
                                                                    (global CamlinternalQuote!))))
                                                                    (apply
                                                                    (field_imm 0
                                                                    (field_imm 0
                                                                    (field_imm 4
                                                                    (global CamlinternalQuote!))))
                                                                    "Stdlib")
                                                                    "Not_found"))
                                                                    0) 0)) 0))
                                                                    0)
                                                                    (makeblock 0
                                                                    (apply
                                                                    (field_imm 0
                                                                    (field_imm 20
                                                                    (global CamlinternalQuote!)))
                                                                    (apply
                                                                    (field_imm 1
                                                                    (field_imm 0
                                                                    (global CamlinternalQuote!)))
                                                                    "example.ml"
                                                                    18 476 19
                                                                    542)
                                                                    (apply
                                                                    (field_imm 8
                                                                    (field_imm 17
                                                                    (global CamlinternalQuote!)))
                                                                    (apply
                                                                    (field_imm 0
                                                                    (field_imm 10
                                                                    (global CamlinternalQuote!)))
                                                                    (apply
                                                                    (field_imm 0
                                                                    (field_imm 4
                                                                    (field_imm 4
                                                                    (global CamlinternalQuote!))))
                                                                    (apply
                                                                    (field_imm 0
                                                                    (field_imm 0
                                                                    (field_imm 4
                                                                    (global CamlinternalQuote!))))
                                                                    "Stdlib")
                                                                    "Not_found"))
                                                                    0)
                                                                    (apply
                                                                    (field_imm 0
                                                                    (field_imm 25
                                                                    (global CamlinternalQuote!)))
                                                                    (apply
                                                                    (field_imm 1
                                                                    (field_imm 24
                                                                    (global CamlinternalQuote!)))
                                                                    (apply
                                                                    (field_imm 0
                                                                    (field_imm 3
                                                                    (global CamlinternalQuote!)))
                                                                    20130322))
                                                                    0)) 0))
                                                                    0) 0)
                                                                  (makeblock 0
                                                                    0 0)
                                                                  (function
                                                                    {nlocal = 0}
                                                                    t/581
                                                                    (let
                                                                    (knocked_out_date/296 =
                                                                    (field_imm 0
                                                                    t/581))
                                                                    (function
                                                                    {nlocal = 0}
                                                                    t/580
                                                                    (makeblock 0
                                                                    (apply
                                                                    (field_imm 6
                                                                    (field_imm 17
                                                                    (global CamlinternalQuote!)))
                                                                    (makeblock 0
                                                                    (makeblock 0
                                                                    (field_imm 0
                                                                    (field_imm 0
                                                                    (field_imm 5
                                                                    (global CamlinternalQuote!))))
                                                                    (apply
                                                                    (field_imm 1
                                                                    (field_imm 17
                                                                    (global CamlinternalQuote!)))
                                                                    knocked_out_date/296))
                                                                    0))
                                                                    (apply
                                                                    (field_imm 0
                                                                    (field_imm 25
                                                                    (global CamlinternalQuote!)))
                                                                    (apply
                                                                    (field_imm 3
                                                                    (field_imm 24
                                                                    (global CamlinternalQuote!)))
                                                                    (apply
                                                                    (field_imm 1
                                                                    (field_imm 0
                                                                    (global CamlinternalQuote!)))
                                                                    "example.ml"
                                                                    21 578 23
                                                                    683)
                                                                    (makeblock 0
                                                                    (apply
                                                                    (field_imm 0
                                                                    (field_imm 1
                                                                    (global CamlinternalQuote!)))
                                                                    "date")
                                                                    0) 0
                                                                    (makeblock 0
                                                                    (apply
                                                                    (field_imm 0
                                                                    (field_imm 25
                                                                    (global CamlinternalQuote!)))
                                                                    (apply
                                                                    (field_imm 0
                                                                    (field_imm 24
                                                                    (global CamlinternalQuote!)))
                                                                    (apply
                                                                    (field_imm 1
                                                                    (field_imm 1
                                                                    (field_imm 4
                                                                    (global CamlinternalQuote!))))
                                                                    knocked_out_date/296
                                                                    (apply
                                                                    (field_imm 1
                                                                    (field_imm 0
                                                                    (global CamlinternalQuote!)))
                                                                    "example.ml"
                                                                    21 589 21
                                                                    605))) 0)
                                                                    0)
                                                                    (makeblock 0
                                                                    0 0)
                                                                    (function
                                                                    {nlocal = 0}
                                                                    t/579
                                                                    (let
                                                                    (date/297 =
                                                                    (field_imm 0
                                                                    t/579))
                                                                    (function
                                                                    {nlocal = 0}
                                                                    t/578
                                                                    (makeblock 0
                                                                    (apply
                                                                    (field_imm 6
                                                                    (field_imm 17
                                                                    (global CamlinternalQuote!)))
                                                                    (makeblock 0
                                                                    (makeblock 0
                                                                    (field_imm 0
                                                                    (field_imm 0
                                                                    (field_imm 5
                                                                    (global CamlinternalQuote!))))
                                                                    (apply
                                                                    (field_imm 1
                                                                    (field_imm 17
                                                                    (global CamlinternalQuote!)))
                                                                    date/297))
                                                                    0))
                                                                    (apply
                                                                    (field_imm 0
                                                                    (field_imm 25
                                                                    (global CamlinternalQuote!)))
                                                                    (apply
                                                                    (field_imm 16
                                                                    (field_imm 24
                                                                    (global CamlinternalQuote!)))
                                                                    (apply
                                                                    (field_imm 0
                                                                    (field_imm 25
                                                                    (global CamlinternalQuote!)))
                                                                    (apply
                                                                    (field_imm 5
                                                                    (field_imm 24
                                                                    (global CamlinternalQuote!)))
                                                                    (apply
                                                                    (field_imm 0
                                                                    (field_imm 25
                                                                    (global CamlinternalQuote!)))
                                                                    (apply
                                                                    (field_imm 0
                                                                    (field_imm 24
                                                                    (global CamlinternalQuote!)))
                                                                    (apply
                                                                    (field_imm 0
                                                                    (field_imm 1
                                                                    (field_imm 4
                                                                    (global CamlinternalQuote!))))
                                                                    (apply
                                                                    (field_imm 0
                                                                    (field_imm 0
                                                                    (field_imm 4
                                                                    (global CamlinternalQuote!))))
                                                                    "Stdlib")
                                                                    ":=")) 0)
                                                                    (makeblock 0
                                                                    (makeblock 0
                                                                    (field_imm 1
                                                                    (field_imm 5
                                                                    (global CamlinternalQuote!)))
                                                                    (apply
                                                                    (field_imm 0
                                                                    (field_imm 25
                                                                    (global CamlinternalQuote!)))
                                                                    (apply
                                                                    (field_imm 0
                                                                    (field_imm 24
                                                                    (global CamlinternalQuote!)))
                                                                    (apply
                                                                    (field_imm 1
                                                                    (field_imm 1
                                                                    (field_imm 4
                                                                    (global CamlinternalQuote!))))
                                                                    knocked_out/292
                                                                    (apply
                                                                    (field_imm 1
                                                                    (field_imm 0
                                                                    (global CamlinternalQuote!)))
                                                                    "example.ml"
                                                                    22 625 22
                                                                    636))) 0))
                                                                    (makeblock 0
                                                                    (makeblock 0
                                                                    (field_imm 1
                                                                    (field_imm 5
                                                                    (global CamlinternalQuote!)))
                                                                    (apply
                                                                    (field_imm 0
                                                                    (field_imm 25
                                                                    (global CamlinternalQuote!)))
                                                                    (apply
                                                                    (field_imm 0
                                                                    (field_imm 24
                                                                    (global CamlinternalQuote!)))
                                                                    (apply
                                                                    (field_imm 1
                                                                    (field_imm 1
                                                                    (field_imm 4
                                                                    (global CamlinternalQuote!))))
                                                                    date/297
                                                                    (apply
                                                                    (field_imm 1
                                                                    (field_imm 0
                                                                    (global CamlinternalQuote!)))
                                                                    "example.ml"
                                                                    22 640 22
                                                                    644))) 0))
                                                                    0))) 0)
                                                                    (apply
                                                                    (field_imm 0
                                                                    (field_imm 25
                                                                    (global CamlinternalQuote!)))
                                                                    (apply
                                                                    (field_imm 47
                                                                    (field_imm 24
                                                                    (global CamlinternalQuote!)))
                                                                    (region
                                                                    (apply[L]
                                                                    loop/293
                                                                    (make_unboxed_product #(?,
                                                                    value<
                                                                    int>)
                                                                    acc/294
                                                                    (%int_sub
                                                                    n/295 1)))))
                                                                    0)) 0))))))
                                                                    0))))))
                                                                0))))))
                                                   (apply[L] loop/293
                                                     (make_unboxed_product #(*,
                                                       value<int>)
                                                       (apply
                                                         (field_imm 1
                                                           (field_imm 26
                                                             (global CamlinternalQuote!)))
                                                         (apply
                                                           (field_imm 1
                                                             (field_imm 0
                                                               (global CamlinternalQuote!)))
                                                           "example.ml" 25
                                                           715 25 723)
                                                         (apply
                                                           (field_imm 0
                                                             (field_imm 25
                                                               (global CamlinternalQuote!)))
                                                           (apply
                                                             (field_imm 9
                                                               (field_imm 24
                                                                 (global CamlinternalQuote!)))
                                                             (apply
                                                               (field_imm 0
                                                                 (field_imm 10
                                                                   (global CamlinternalQuote!)))
                                                               (field_imm 4
                                                                 (field_imm 4
                                                                   (field_imm 4
                                                                    (global CamlinternalQuote!)))))
                                                             0)
                                                           0))
                                                       n/291)))))
                                             0)
                                           (apply
                                             (field_imm 0
                                               (field_imm 21
                                                 (global CamlinternalQuote!)))
                                             (apply
                                               (field_imm 0
                                                 (field_imm 16
                                                   (global CamlinternalQuote!)))
                                               0)
                                             (field_imm 0
                                               (field_imm 13
                                                 (global CamlinternalQuote!)))))
                                         0)
                                       0)
                                     (makeblock 0 0 0)
                                     (function {nlocal = 0} t/603 t/602
                                       (makeblock 0
                                         (apply
                                           (field_imm 6
                                             (field_imm 17
                                               (global CamlinternalQuote!)))
                                           (makeblock 0
                                             (makeblock 0
                                               (field_imm 0
                                                 (field_imm 0
                                                   (field_imm 5
                                                     (global CamlinternalQuote!))))
                                               (apply
                                                 (field_imm 18
                                                   (field_imm 17
                                                     (global CamlinternalQuote!)))
                                                 (field_imm 0
                                                   (field_imm 17
                                                     (global CamlinternalQuote!)))
                                                 (apply
                                                   (field_imm 0
                                                     (field_imm 16
                                                       (global CamlinternalQuote!)))
                                                   0)
                                                 (apply
                                                   (field_imm 1
                                                     (field_imm 13
                                                       (global CamlinternalQuote!)))
                                                   0)))
                                             0))
                                         (apply
                                           (field_imm 0
                                             (field_imm 25
                                               (global CamlinternalQuote!)))
                                           (apply
                                             (field_imm 1
                                               (field_imm 24
                                                 (global CamlinternalQuote!)))
                                             (apply
                                               (field_imm 0
                                                 (field_imm 3
                                                   (global CamlinternalQuote!)))
                                               20130322))
                                           0))))
                                   0))))))
                       0)
                     0)))))
           0))))
  (seq (opaque (apply build/289 10000)) (makeblock 0 build/289)))
Compiler returned: 0
