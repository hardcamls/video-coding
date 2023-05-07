  $ source test-env.sh

Decode Mouse480.jpg with the model and ffmpeg and show the max pixel difference is <= 1.
I dont have much insight into what ffmpeg does, but it doesn't seem to be doing any post
processing.  A difference of 1 (or maybe even 2 or 3) is fine and is always going to
happen due to the different IDCT implementations.

  $ ffmpeg -i ../test_data/Mouse480.jpg out_ffmpeg.yuv 
  $ model decode frame ../test_data/Mouse480.jpg out_model.yuv
  $ oyuv compare max-difference yuv out_ffmpeg.yuv out_model.yuv 480x320
  1
  0
  0
