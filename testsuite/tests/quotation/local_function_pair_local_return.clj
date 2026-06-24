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
           "example.ml" 10 212 27 749)
         (apply (field_imm 0 (field_imm 25 (global CamlinternalQuote!)))
           (apply (field_imm 4 (field_imm 24 (global CamlinternalQuote!)))
             (apply (field_imm 2 (field_imm 22 (global CamlinternalQuote!)))
               (field_imm 1 (field_imm 5 (global CamlinternalQuote!))) 0
               (apply (field_imm 1 (field_imm 0 (global CamlinternalQuote!)))
                 "example.ml" 10 215 27 746)
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
                           "example.ml" 11 230 27 746)
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
                                       "example.ml" 12 262 27 746)
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
                                                      (function[L]
                                                        {nlocal = 1}
                                                        param/298[value<
                                                                   (consts ())
                                                                    (non_consts (
                                                                    [0: ?,
                                                                    value<
                                                                    int>]))>]
                                                        : local
                                                        (let
                                                          (n/295 =a?
                                                             (field_imm 1
                                                               param/298)
                                                           acc/294 =a?
                                                             (field_imm 0
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
                                                                15 392 15
                                                                400)
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
                                                                17 430 23
                                                                683)
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
                                                                    17 433 23
                                                                    680)
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
                                                                    18 474 19
                                                                    540)
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
                                                                    21 576 23
                                                                    680)
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
                                                                    21 587 21
                                                                    603))) 0)
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
                                                                    22 623 22
                                                                    634))) 0))
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
                                                                    22 638 22
                                                                    642))) 0))
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
                                                                    (makeblock 0 (?,
                                                                    value<
                                                                    int>)
                                                                    acc/294
                                                                    (%int_sub
                                                                    n/295 1)))))
                                                                    0)) 0))))))
                                                                    0))))))
                                                                0))))))
                                                   (apply[L] loop/293
                                                     (makeblock 0 (*,
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
                                                           711 25 719)
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
