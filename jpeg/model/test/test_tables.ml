open! Core
module Tables = Hardcaml_jpeg_model.Tables

let%expect_test "dc" =
  print_s
    [%message
      (Tables.Encoder.dc_table Tables.Default.dc_luma : Tables.dc Tables.coef array)];
  [%expect
    {|
    ("Tables.Encoder.dc_table Tables.Default.dc_luma"
     (((length 2) (bits 0) (data 0)) ((length 3) (bits 2) (data 1))
      ((length 3) (bits 3) (data 2)) ((length 3) (bits 4) (data 3))
      ((length 3) (bits 5) (data 4)) ((length 3) (bits 6) (data 5))
      ((length 4) (bits 14) (data 6)) ((length 5) (bits 30) (data 7))
      ((length 6) (bits 62) (data 8)) ((length 7) (bits 126) (data 9))
      ((length 8) (bits 254) (data 10)) ((length 9) (bits 510) (data 11)))) |}];
  print_s
    [%message
      (Tables.Encoder.dc_table Tables.Default.dc_chroma : Tables.dc Tables.coef array)];
  [%expect
    {|
    ("Tables.Encoder.dc_table Tables.Default.dc_chroma"
     (((length 2) (bits 0) (data 0)) ((length 2) (bits 1) (data 1))
      ((length 2) (bits 2) (data 2)) ((length 3) (bits 6) (data 3))
      ((length 4) (bits 14) (data 4)) ((length 5) (bits 30) (data 5))
      ((length 6) (bits 62) (data 6)) ((length 7) (bits 126) (data 7))
      ((length 8) (bits 254) (data 8)) ((length 9) (bits 510) (data 9))
      ((length 10) (bits 1022) (data 10)) ((length 11) (bits 2046) (data 11)))) |}]
;;

let%expect_test "ac" =
  print_s
    [%message
      (Tables.Encoder.ac_table Tables.Default.ac_luma : Tables.ac Tables.coef array array)];
  [%expect
    {|
    ("Tables.Encoder.ac_table Tables.Default.ac_luma"
     ((((length 4) (bits 10) (data ((run 0) (size 0))))
       ((length 2) (bits 0) (data ((run 0) (size 1))))
       ((length 2) (bits 1) (data ((run 0) (size 2))))
       ((length 3) (bits 4) (data ((run 0) (size 3))))
       ((length 4) (bits 11) (data ((run 0) (size 4))))
       ((length 5) (bits 26) (data ((run 0) (size 5))))
       ((length 7) (bits 120) (data ((run 0) (size 6))))
       ((length 8) (bits 248) (data ((run 0) (size 7))))
       ((length 10) (bits 1014) (data ((run 0) (size 8))))
       ((length 16) (bits 65410) (data ((run 0) (size 9))))
       ((length 16) (bits 65411) (data ((run 0) (size 10)))))
      (((length 0) (bits 0) (data ((run 0) (size 0))))
       ((length 4) (bits 12) (data ((run 1) (size 1))))
       ((length 5) (bits 27) (data ((run 1) (size 2))))
       ((length 7) (bits 121) (data ((run 1) (size 3))))
       ((length 9) (bits 502) (data ((run 1) (size 4))))
       ((length 11) (bits 2038) (data ((run 1) (size 5))))
       ((length 16) (bits 65412) (data ((run 1) (size 6))))
       ((length 16) (bits 65413) (data ((run 1) (size 7))))
       ((length 16) (bits 65414) (data ((run 1) (size 8))))
       ((length 16) (bits 65415) (data ((run 1) (size 9))))
       ((length 16) (bits 65416) (data ((run 1) (size 10)))))
      (((length 0) (bits 0) (data ((run 0) (size 0))))
       ((length 5) (bits 28) (data ((run 2) (size 1))))
       ((length 8) (bits 249) (data ((run 2) (size 2))))
       ((length 10) (bits 1015) (data ((run 2) (size 3))))
       ((length 12) (bits 4084) (data ((run 2) (size 4))))
       ((length 16) (bits 65417) (data ((run 2) (size 5))))
       ((length 16) (bits 65418) (data ((run 2) (size 6))))
       ((length 16) (bits 65419) (data ((run 2) (size 7))))
       ((length 16) (bits 65420) (data ((run 2) (size 8))))
       ((length 16) (bits 65421) (data ((run 2) (size 9))))
       ((length 16) (bits 65422) (data ((run 2) (size 10)))))
      (((length 0) (bits 0) (data ((run 0) (size 0))))
       ((length 6) (bits 58) (data ((run 3) (size 1))))
       ((length 9) (bits 503) (data ((run 3) (size 2))))
       ((length 12) (bits 4085) (data ((run 3) (size 3))))
       ((length 16) (bits 65423) (data ((run 3) (size 4))))
       ((length 16) (bits 65424) (data ((run 3) (size 5))))
       ((length 16) (bits 65425) (data ((run 3) (size 6))))
       ((length 16) (bits 65426) (data ((run 3) (size 7))))
       ((length 16) (bits 65427) (data ((run 3) (size 8))))
       ((length 16) (bits 65428) (data ((run 3) (size 9))))
       ((length 16) (bits 65429) (data ((run 3) (size 10)))))
      (((length 0) (bits 0) (data ((run 0) (size 0))))
       ((length 6) (bits 59) (data ((run 4) (size 1))))
       ((length 10) (bits 1016) (data ((run 4) (size 2))))
       ((length 16) (bits 65430) (data ((run 4) (size 3))))
       ((length 16) (bits 65431) (data ((run 4) (size 4))))
       ((length 16) (bits 65432) (data ((run 4) (size 5))))
       ((length 16) (bits 65433) (data ((run 4) (size 6))))
       ((length 16) (bits 65434) (data ((run 4) (size 7))))
       ((length 16) (bits 65435) (data ((run 4) (size 8))))
       ((length 16) (bits 65436) (data ((run 4) (size 9))))
       ((length 16) (bits 65437) (data ((run 4) (size 10)))))
      (((length 0) (bits 0) (data ((run 0) (size 0))))
       ((length 7) (bits 122) (data ((run 5) (size 1))))
       ((length 11) (bits 2039) (data ((run 5) (size 2))))
       ((length 16) (bits 65438) (data ((run 5) (size 3))))
       ((length 16) (bits 65439) (data ((run 5) (size 4))))
       ((length 16) (bits 65440) (data ((run 5) (size 5))))
       ((length 16) (bits 65441) (data ((run 5) (size 6))))
       ((length 16) (bits 65442) (data ((run 5) (size 7))))
       ((length 16) (bits 65443) (data ((run 5) (size 8))))
       ((length 16) (bits 65444) (data ((run 5) (size 9))))
       ((length 16) (bits 65445) (data ((run 5) (size 10)))))
      (((length 0) (bits 0) (data ((run 0) (size 0))))
       ((length 7) (bits 123) (data ((run 6) (size 1))))
       ((length 12) (bits 4086) (data ((run 6) (size 2))))
       ((length 16) (bits 65446) (data ((run 6) (size 3))))
       ((length 16) (bits 65447) (data ((run 6) (size 4))))
       ((length 16) (bits 65448) (data ((run 6) (size 5))))
       ((length 16) (bits 65449) (data ((run 6) (size 6))))
       ((length 16) (bits 65450) (data ((run 6) (size 7))))
       ((length 16) (bits 65451) (data ((run 6) (size 8))))
       ((length 16) (bits 65452) (data ((run 6) (size 9))))
       ((length 16) (bits 65453) (data ((run 6) (size 10)))))
      (((length 0) (bits 0) (data ((run 0) (size 0))))
       ((length 8) (bits 250) (data ((run 7) (size 1))))
       ((length 12) (bits 4087) (data ((run 7) (size 2))))
       ((length 16) (bits 65454) (data ((run 7) (size 3))))
       ((length 16) (bits 65455) (data ((run 7) (size 4))))
       ((length 16) (bits 65456) (data ((run 7) (size 5))))
       ((length 16) (bits 65457) (data ((run 7) (size 6))))
       ((length 16) (bits 65458) (data ((run 7) (size 7))))
       ((length 16) (bits 65459) (data ((run 7) (size 8))))
       ((length 16) (bits 65460) (data ((run 7) (size 9))))
       ((length 16) (bits 65461) (data ((run 7) (size 10)))))
      (((length 0) (bits 0) (data ((run 0) (size 0))))
       ((length 9) (bits 504) (data ((run 8) (size 1))))
       ((length 15) (bits 32704) (data ((run 8) (size 2))))
       ((length 16) (bits 65462) (data ((run 8) (size 3))))
       ((length 16) (bits 65463) (data ((run 8) (size 4))))
       ((length 16) (bits 65464) (data ((run 8) (size 5))))
       ((length 16) (bits 65465) (data ((run 8) (size 6))))
       ((length 16) (bits 65466) (data ((run 8) (size 7))))
       ((length 16) (bits 65467) (data ((run 8) (size 8))))
       ((length 16) (bits 65468) (data ((run 8) (size 9))))
       ((length 16) (bits 65469) (data ((run 8) (size 10)))))
      (((length 0) (bits 0) (data ((run 0) (size 0))))
       ((length 9) (bits 505) (data ((run 9) (size 1))))
       ((length 16) (bits 65470) (data ((run 9) (size 2))))
       ((length 16) (bits 65471) (data ((run 9) (size 3))))
       ((length 16) (bits 65472) (data ((run 9) (size 4))))
       ((length 16) (bits 65473) (data ((run 9) (size 5))))
       ((length 16) (bits 65474) (data ((run 9) (size 6))))
       ((length 16) (bits 65475) (data ((run 9) (size 7))))
       ((length 16) (bits 65476) (data ((run 9) (size 8))))
       ((length 16) (bits 65477) (data ((run 9) (size 9))))
       ((length 16) (bits 65478) (data ((run 9) (size 10)))))
      (((length 0) (bits 0) (data ((run 0) (size 0))))
       ((length 9) (bits 506) (data ((run 10) (size 1))))
       ((length 16) (bits 65479) (data ((run 10) (size 2))))
       ((length 16) (bits 65480) (data ((run 10) (size 3))))
       ((length 16) (bits 65481) (data ((run 10) (size 4))))
       ((length 16) (bits 65482) (data ((run 10) (size 5))))
       ((length 16) (bits 65483) (data ((run 10) (size 6))))
       ((length 16) (bits 65484) (data ((run 10) (size 7))))
       ((length 16) (bits 65485) (data ((run 10) (size 8))))
       ((length 16) (bits 65486) (data ((run 10) (size 9))))
       ((length 16) (bits 65487) (data ((run 10) (size 10)))))
      (((length 0) (bits 0) (data ((run 0) (size 0))))
       ((length 10) (bits 1017) (data ((run 11) (size 1))))
       ((length 16) (bits 65488) (data ((run 11) (size 2))))
       ((length 16) (bits 65489) (data ((run 11) (size 3))))
       ((length 16) (bits 65490) (data ((run 11) (size 4))))
       ((length 16) (bits 65491) (data ((run 11) (size 5))))
       ((length 16) (bits 65492) (data ((run 11) (size 6))))
       ((length 16) (bits 65493) (data ((run 11) (size 7))))
       ((length 16) (bits 65494) (data ((run 11) (size 8))))
       ((length 16) (bits 65495) (data ((run 11) (size 9))))
       ((length 16) (bits 65496) (data ((run 11) (size 10)))))
      (((length 0) (bits 0) (data ((run 0) (size 0))))
       ((length 10) (bits 1018) (data ((run 12) (size 1))))
       ((length 16) (bits 65497) (data ((run 12) (size 2))))
       ((length 16) (bits 65498) (data ((run 12) (size 3))))
       ((length 16) (bits 65499) (data ((run 12) (size 4))))
       ((length 16) (bits 65500) (data ((run 12) (size 5))))
       ((length 16) (bits 65501) (data ((run 12) (size 6))))
       ((length 16) (bits 65502) (data ((run 12) (size 7))))
       ((length 16) (bits 65503) (data ((run 12) (size 8))))
       ((length 16) (bits 65504) (data ((run 12) (size 9))))
       ((length 16) (bits 65505) (data ((run 12) (size 10)))))
      (((length 0) (bits 0) (data ((run 0) (size 0))))
       ((length 11) (bits 2040) (data ((run 13) (size 1))))
       ((length 16) (bits 65506) (data ((run 13) (size 2))))
       ((length 16) (bits 65507) (data ((run 13) (size 3))))
       ((length 16) (bits 65508) (data ((run 13) (size 4))))
       ((length 16) (bits 65509) (data ((run 13) (size 5))))
       ((length 16) (bits 65510) (data ((run 13) (size 6))))
       ((length 16) (bits 65511) (data ((run 13) (size 7))))
       ((length 16) (bits 65512) (data ((run 13) (size 8))))
       ((length 16) (bits 65513) (data ((run 13) (size 9))))
       ((length 16) (bits 65514) (data ((run 13) (size 10)))))
      (((length 0) (bits 0) (data ((run 0) (size 0))))
       ((length 16) (bits 65515) (data ((run 14) (size 1))))
       ((length 16) (bits 65516) (data ((run 14) (size 2))))
       ((length 16) (bits 65517) (data ((run 14) (size 3))))
       ((length 16) (bits 65518) (data ((run 14) (size 4))))
       ((length 16) (bits 65519) (data ((run 14) (size 5))))
       ((length 16) (bits 65520) (data ((run 14) (size 6))))
       ((length 16) (bits 65521) (data ((run 14) (size 7))))
       ((length 16) (bits 65522) (data ((run 14) (size 8))))
       ((length 16) (bits 65523) (data ((run 14) (size 9))))
       ((length 16) (bits 65524) (data ((run 14) (size 10)))))
      (((length 11) (bits 2041) (data ((run 15) (size 0))))
       ((length 16) (bits 65525) (data ((run 15) (size 1))))
       ((length 16) (bits 65526) (data ((run 15) (size 2))))
       ((length 16) (bits 65527) (data ((run 15) (size 3))))
       ((length 16) (bits 65528) (data ((run 15) (size 4))))
       ((length 16) (bits 65529) (data ((run 15) (size 5))))
       ((length 16) (bits 65530) (data ((run 15) (size 6))))
       ((length 16) (bits 65531) (data ((run 15) (size 7))))
       ((length 16) (bits 65532) (data ((run 15) (size 8))))
       ((length 16) (bits 65533) (data ((run 15) (size 9))))
       ((length 16) (bits 65534) (data ((run 15) (size 10))))))) |}];
  print_s
    [%message
      (Tables.Encoder.ac_table Tables.Default.ac_chroma
        : Tables.ac Tables.coef array array)];
  [%expect
    {|
    ("Tables.Encoder.ac_table Tables.Default.ac_chroma"
     ((((length 2) (bits 0) (data ((run 0) (size 0))))
       ((length 2) (bits 1) (data ((run 0) (size 1))))
       ((length 3) (bits 4) (data ((run 0) (size 2))))
       ((length 4) (bits 10) (data ((run 0) (size 3))))
       ((length 5) (bits 24) (data ((run 0) (size 4))))
       ((length 5) (bits 25) (data ((run 0) (size 5))))
       ((length 6) (bits 56) (data ((run 0) (size 6))))
       ((length 7) (bits 120) (data ((run 0) (size 7))))
       ((length 9) (bits 500) (data ((run 0) (size 8))))
       ((length 10) (bits 1014) (data ((run 0) (size 9))))
       ((length 12) (bits 4084) (data ((run 0) (size 10)))))
      (((length 0) (bits 0) (data ((run 0) (size 0))))
       ((length 4) (bits 11) (data ((run 1) (size 1))))
       ((length 6) (bits 57) (data ((run 1) (size 2))))
       ((length 8) (bits 246) (data ((run 1) (size 3))))
       ((length 9) (bits 501) (data ((run 1) (size 4))))
       ((length 11) (bits 2038) (data ((run 1) (size 5))))
       ((length 12) (bits 4085) (data ((run 1) (size 6))))
       ((length 16) (bits 65416) (data ((run 1) (size 7))))
       ((length 16) (bits 65417) (data ((run 1) (size 8))))
       ((length 16) (bits 65418) (data ((run 1) (size 9))))
       ((length 16) (bits 65419) (data ((run 1) (size 10)))))
      (((length 0) (bits 0) (data ((run 0) (size 0))))
       ((length 5) (bits 26) (data ((run 2) (size 1))))
       ((length 8) (bits 247) (data ((run 2) (size 2))))
       ((length 10) (bits 1015) (data ((run 2) (size 3))))
       ((length 12) (bits 4086) (data ((run 2) (size 4))))
       ((length 15) (bits 32706) (data ((run 2) (size 5))))
       ((length 16) (bits 65420) (data ((run 2) (size 6))))
       ((length 16) (bits 65421) (data ((run 2) (size 7))))
       ((length 16) (bits 65422) (data ((run 2) (size 8))))
       ((length 16) (bits 65423) (data ((run 2) (size 9))))
       ((length 16) (bits 65424) (data ((run 2) (size 10)))))
      (((length 0) (bits 0) (data ((run 0) (size 0))))
       ((length 5) (bits 27) (data ((run 3) (size 1))))
       ((length 8) (bits 248) (data ((run 3) (size 2))))
       ((length 10) (bits 1016) (data ((run 3) (size 3))))
       ((length 12) (bits 4087) (data ((run 3) (size 4))))
       ((length 16) (bits 65425) (data ((run 3) (size 5))))
       ((length 16) (bits 65426) (data ((run 3) (size 6))))
       ((length 16) (bits 65427) (data ((run 3) (size 7))))
       ((length 16) (bits 65428) (data ((run 3) (size 8))))
       ((length 16) (bits 65429) (data ((run 3) (size 9))))
       ((length 16) (bits 65430) (data ((run 3) (size 10)))))
      (((length 0) (bits 0) (data ((run 0) (size 0))))
       ((length 6) (bits 58) (data ((run 4) (size 1))))
       ((length 9) (bits 502) (data ((run 4) (size 2))))
       ((length 16) (bits 65431) (data ((run 4) (size 3))))
       ((length 16) (bits 65432) (data ((run 4) (size 4))))
       ((length 16) (bits 65433) (data ((run 4) (size 5))))
       ((length 16) (bits 65434) (data ((run 4) (size 6))))
       ((length 16) (bits 65435) (data ((run 4) (size 7))))
       ((length 16) (bits 65436) (data ((run 4) (size 8))))
       ((length 16) (bits 65437) (data ((run 4) (size 9))))
       ((length 16) (bits 65438) (data ((run 4) (size 10)))))
      (((length 0) (bits 0) (data ((run 0) (size 0))))
       ((length 6) (bits 59) (data ((run 5) (size 1))))
       ((length 10) (bits 1017) (data ((run 5) (size 2))))
       ((length 16) (bits 65439) (data ((run 5) (size 3))))
       ((length 16) (bits 65440) (data ((run 5) (size 4))))
       ((length 16) (bits 65441) (data ((run 5) (size 5))))
       ((length 16) (bits 65442) (data ((run 5) (size 6))))
       ((length 16) (bits 65443) (data ((run 5) (size 7))))
       ((length 16) (bits 65444) (data ((run 5) (size 8))))
       ((length 16) (bits 65445) (data ((run 5) (size 9))))
       ((length 16) (bits 65446) (data ((run 5) (size 10)))))
      (((length 0) (bits 0) (data ((run 0) (size 0))))
       ((length 7) (bits 121) (data ((run 6) (size 1))))
       ((length 11) (bits 2039) (data ((run 6) (size 2))))
       ((length 16) (bits 65447) (data ((run 6) (size 3))))
       ((length 16) (bits 65448) (data ((run 6) (size 4))))
       ((length 16) (bits 65449) (data ((run 6) (size 5))))
       ((length 16) (bits 65450) (data ((run 6) (size 6))))
       ((length 16) (bits 65451) (data ((run 6) (size 7))))
       ((length 16) (bits 65452) (data ((run 6) (size 8))))
       ((length 16) (bits 65453) (data ((run 6) (size 9))))
       ((length 16) (bits 65454) (data ((run 6) (size 10)))))
      (((length 0) (bits 0) (data ((run 0) (size 0))))
       ((length 7) (bits 122) (data ((run 7) (size 1))))
       ((length 11) (bits 2040) (data ((run 7) (size 2))))
       ((length 16) (bits 65455) (data ((run 7) (size 3))))
       ((length 16) (bits 65456) (data ((run 7) (size 4))))
       ((length 16) (bits 65457) (data ((run 7) (size 5))))
       ((length 16) (bits 65458) (data ((run 7) (size 6))))
       ((length 16) (bits 65459) (data ((run 7) (size 7))))
       ((length 16) (bits 65460) (data ((run 7) (size 8))))
       ((length 16) (bits 65461) (data ((run 7) (size 9))))
       ((length 16) (bits 65462) (data ((run 7) (size 10)))))
      (((length 0) (bits 0) (data ((run 0) (size 0))))
       ((length 8) (bits 249) (data ((run 8) (size 1))))
       ((length 16) (bits 65463) (data ((run 8) (size 2))))
       ((length 16) (bits 65464) (data ((run 8) (size 3))))
       ((length 16) (bits 65465) (data ((run 8) (size 4))))
       ((length 16) (bits 65466) (data ((run 8) (size 5))))
       ((length 16) (bits 65467) (data ((run 8) (size 6))))
       ((length 16) (bits 65468) (data ((run 8) (size 7))))
       ((length 16) (bits 65469) (data ((run 8) (size 8))))
       ((length 16) (bits 65470) (data ((run 8) (size 9))))
       ((length 16) (bits 65471) (data ((run 8) (size 10)))))
      (((length 0) (bits 0) (data ((run 0) (size 0))))
       ((length 9) (bits 503) (data ((run 9) (size 1))))
       ((length 16) (bits 65472) (data ((run 9) (size 2))))
       ((length 16) (bits 65473) (data ((run 9) (size 3))))
       ((length 16) (bits 65474) (data ((run 9) (size 4))))
       ((length 16) (bits 65475) (data ((run 9) (size 5))))
       ((length 16) (bits 65476) (data ((run 9) (size 6))))
       ((length 16) (bits 65477) (data ((run 9) (size 7))))
       ((length 16) (bits 65478) (data ((run 9) (size 8))))
       ((length 16) (bits 65479) (data ((run 9) (size 9))))
       ((length 16) (bits 65480) (data ((run 9) (size 10)))))
      (((length 0) (bits 0) (data ((run 0) (size 0))))
       ((length 9) (bits 504) (data ((run 10) (size 1))))
       ((length 16) (bits 65481) (data ((run 10) (size 2))))
       ((length 16) (bits 65482) (data ((run 10) (size 3))))
       ((length 16) (bits 65483) (data ((run 10) (size 4))))
       ((length 16) (bits 65484) (data ((run 10) (size 5))))
       ((length 16) (bits 65485) (data ((run 10) (size 6))))
       ((length 16) (bits 65486) (data ((run 10) (size 7))))
       ((length 16) (bits 65487) (data ((run 10) (size 8))))
       ((length 16) (bits 65488) (data ((run 10) (size 9))))
       ((length 16) (bits 65489) (data ((run 10) (size 10)))))
      (((length 0) (bits 0) (data ((run 0) (size 0))))
       ((length 9) (bits 505) (data ((run 11) (size 1))))
       ((length 16) (bits 65490) (data ((run 11) (size 2))))
       ((length 16) (bits 65491) (data ((run 11) (size 3))))
       ((length 16) (bits 65492) (data ((run 11) (size 4))))
       ((length 16) (bits 65493) (data ((run 11) (size 5))))
       ((length 16) (bits 65494) (data ((run 11) (size 6))))
       ((length 16) (bits 65495) (data ((run 11) (size 7))))
       ((length 16) (bits 65496) (data ((run 11) (size 8))))
       ((length 16) (bits 65497) (data ((run 11) (size 9))))
       ((length 16) (bits 65498) (data ((run 11) (size 10)))))
      (((length 0) (bits 0) (data ((run 0) (size 0))))
       ((length 9) (bits 506) (data ((run 12) (size 1))))
       ((length 16) (bits 65499) (data ((run 12) (size 2))))
       ((length 16) (bits 65500) (data ((run 12) (size 3))))
       ((length 16) (bits 65501) (data ((run 12) (size 4))))
       ((length 16) (bits 65502) (data ((run 12) (size 5))))
       ((length 16) (bits 65503) (data ((run 12) (size 6))))
       ((length 16) (bits 65504) (data ((run 12) (size 7))))
       ((length 16) (bits 65505) (data ((run 12) (size 8))))
       ((length 16) (bits 65506) (data ((run 12) (size 9))))
       ((length 16) (bits 65507) (data ((run 12) (size 10)))))
      (((length 0) (bits 0) (data ((run 0) (size 0))))
       ((length 11) (bits 2041) (data ((run 13) (size 1))))
       ((length 16) (bits 65508) (data ((run 13) (size 2))))
       ((length 16) (bits 65509) (data ((run 13) (size 3))))
       ((length 16) (bits 65510) (data ((run 13) (size 4))))
       ((length 16) (bits 65511) (data ((run 13) (size 5))))
       ((length 16) (bits 65512) (data ((run 13) (size 6))))
       ((length 16) (bits 65513) (data ((run 13) (size 7))))
       ((length 16) (bits 65514) (data ((run 13) (size 8))))
       ((length 16) (bits 65515) (data ((run 13) (size 9))))
       ((length 16) (bits 65516) (data ((run 13) (size 10)))))
      (((length 0) (bits 0) (data ((run 0) (size 0))))
       ((length 14) (bits 16352) (data ((run 14) (size 1))))
       ((length 16) (bits 65517) (data ((run 14) (size 2))))
       ((length 16) (bits 65518) (data ((run 14) (size 3))))
       ((length 16) (bits 65519) (data ((run 14) (size 4))))
       ((length 16) (bits 65520) (data ((run 14) (size 5))))
       ((length 16) (bits 65521) (data ((run 14) (size 6))))
       ((length 16) (bits 65522) (data ((run 14) (size 7))))
       ((length 16) (bits 65523) (data ((run 14) (size 8))))
       ((length 16) (bits 65524) (data ((run 14) (size 9))))
       ((length 16) (bits 65525) (data ((run 14) (size 10)))))
      (((length 10) (bits 1018) (data ((run 15) (size 0))))
       ((length 15) (bits 32707) (data ((run 15) (size 1))))
       ((length 16) (bits 65526) (data ((run 15) (size 2))))
       ((length 16) (bits 65527) (data ((run 15) (size 3))))
       ((length 16) (bits 65528) (data ((run 15) (size 4))))
       ((length 16) (bits 65529) (data ((run 15) (size 5))))
       ((length 16) (bits 65530) (data ((run 15) (size 6))))
       ((length 16) (bits 65531) (data ((run 15) (size 7))))
       ((length 16) (bits 65532) (data ((run 15) (size 8))))
       ((length 16) (bits 65533) (data ((run 15) (size 9))))
       ((length 16) (bits 65534) (data ((run 15) (size 10))))))) |}]
;;
