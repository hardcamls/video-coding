  $ . test-env.sh

Run the model encoder and decoder and check results against ffmpeg.

Test at a few different quality levels

  $ model encode frame ../test_data/mini64x64.420 64x64 model.jpg -quality 95
  $ wc -c < model.jpg
      2122
  $ ffmpeg -y -i model.jpg out_ffmpeg.yuv 
  $ model decode frame model.jpg out_model.yuv 
  $ oyuv compare max-difference yuv out_ffmpeg.yuv out_model.yuv 64x64
  1
  1
  1
  $ oyuv compare psnr yuv ../test_data/mini64x64.420 out_model.yuv 64x64
  46.76864691904693
  46.760132097139362
  47.566817670847271

  $ model encode frame ../test_data/mini64x64.420 64x64 model.jpg -quality 50
  $ wc -c < model.jpg
      1033
  $ ffmpeg -y -i model.jpg out_ffmpeg.yuv 
  $ model decode frame model.jpg out_model.yuv 
  $ oyuv compare max-difference yuv out_ffmpeg.yuv out_model.yuv 64x64
  1
  1
  1
  $ oyuv compare psnr yuv ../test_data/mini64x64.420 out_model.yuv 64x64
  36.132625346769309
  38.497906940804654
  39.514572856253487

  $ model encode frame ../test_data/mini64x64.420 64x64 model.jpg -quality 30
  $ wc -c < model.jpg
       924
  $ ffmpeg -y -i model.jpg out_ffmpeg.yuv 
  $ model decode frame model.jpg out_model.yuv 
  $ oyuv compare max-difference yuv out_ffmpeg.yuv out_model.yuv 64x64
  1
  1
  1
  $ oyuv compare psnr yuv ../test_data/mini64x64.420 out_model.yuv 64x64
  33.699396582141631
  37.782138694424944
  38.815211909823489

XXX Running this test at q=20 leads to an ffmpeg warning about EOI missing,
and for some reason the tests keeps looping...look into this bug.
