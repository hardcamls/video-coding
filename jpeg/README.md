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
  - Software model
    - Probably want this anyway to test various features of the decoder.
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