  $ alias ffmpeg="ffmpeg -hide_banner -loglevel warning -nostats"
  $ alias model=../bin/model.exe
  $ alias oyuv=../../tools/bin/oyuv.exe

Decode an Mouse480.jpg with the model and ffmpeg and check show the
max pixel difference is <= 1.

  $ ffmpeg -i ../test_data/Mouse480.jpg out_ffmpeg.yuv 
  $ model decode frame ../test_data/Mouse480.jpg out_model.yuv
  $ oyuv compare max-difference yuv out_ffmpeg.yuv out_model.yuv 480x320
  1
  0
  0
