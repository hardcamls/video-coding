  $ . ./test-env.sh

Basic decoder test.

  $ simulate decoder ../../test_data/mini.jpg

perform tests at a few different quantisation levels 

  $ model encode frame ../../test_data/mini64x64.420 64x64 out.jpg -quality 99
  $ simulate decoder out.jpg

  $ model encode frame ../../test_data/mini64x64.420 64x64 out.jpg -quality 70
  $ simulate decoder out.jpg

  $ model encode frame ../../test_data/mini64x64.420 64x64 out.jpg -quality 50
  $ simulate decoder out.jpg

  $ model encode frame ../../test_data/mini64x64.420 64x64 out.jpg -quality 30
  $ simulate decoder out.jpg

  $ model encode frame ../../test_data/mini64x64.420 64x64 out.jpg -quality 20
  $ simulate decoder out.jpg

  $ model encode frame ../../test_data/mini64x64.420 64x64 out.jpg -quality 10
  $ simulate decoder out.jpg
