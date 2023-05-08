  $ . ./test-env.sh

  $ oyuv convert ../test_data/mini64x64.420 64x64 mini52x44.420 52x44
  $ oyuv convert ../test_data/mini64x64.420 64x64 mini52x44.420 52x44
  $ model encode frame mini52x44.420 52x44 model.jpg -quality 95
  $ ffmpeg -y -i model.jpg out_ffmpeg.yuv 
  $ model decode frame model.jpg out_model.yuv 
  $ oyuv compare max-difference yuv out_ffmpeg.yuv out_model.yuv 52x44
  1
  1
  1
  $ oyuv compare psnr yuv mini52x44.420 out_model.yuv 52x44
  46.368625775631465
  46.259937175107659
  47.575630330180793

