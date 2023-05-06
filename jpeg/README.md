# Status

(this will change as features are added)

* JPEG decoder
  - Baseline profile
  - Interleaved only
  - Arbitrary huffman tables
  - 2 quant tables
  - 1024 cycle dct
  - Relatively complete component and scan management

# TODO 

* JPEG Encoder
  - flexible scan specs for software model
  - With an encoder we can simplify our feature set dramatically.
  - 4:2:0 (maybe 4:2:2 and mono, but it can be a build time option)
  - Fixed huffman tables
* Decoder features
  - More testing!
    - Find more images and formats to test
    - Custom encoder would be a good start
    - Fix known bug on last block(s)
  - More parallelism in IDCT.  Should be able to get down to 128 cycles.
  - Multiplex the huffman table specifications
  - Fixed huffman table option
  - More quant tables
  - Non-interleaved images
  - Optimise Mhz - currently quite low through bitstream+codeword decoder, but
    there are cycles available to optimise this.
  - Extended profile 12 bit support
  - Motion JPEG support (just testing processing of multiple frames)
  - Error handling support
    - ... and restart markers
  - Integrate soft-CPU with accelerator design
- Other potential features
  - These are much less common .. probably not going to do any of these
  - Progressive mode
  - Arithmetic coding
  - Lossless mode

## References

Interesting looking JPEG test suite:

https://github.com/drewnoakes/metadata-extractor-images/tree/master/jpg

## Using ffmpeg

Decode with ffmpeg

```
ffmpeg -i Mouse480.jpg -pix_fmt yuv420p out.yuv
```

- `-y` to overwrite output 

Encode with ffmpeg

```
ffmpeg -s 640x480 -pix_fmt yuv420p -i test-yuv420p.yuv test-640x480.jpg
ffmpeg -s 640x480 -pix_fmt uyvy422 -i test-yuv422uyvy.yuv test-640x480.jpg
```