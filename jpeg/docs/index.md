# Hardcaml_jpeg 

[Hardcaml_jpeg] implements a baseline JPEG decoder suitable for FPGA implementation using Hardcaml.

It is designed as a learning tool to show current best practices for designing hardware in Hardcaml.

## About JPEG

JPEG is still image compression CODEC that has been around for many, many years.  It sees 
heavy use in digital cameras as it can significantly reduce storage requirements with the ability 
to trade file size for image quality.

Like most (still or motion based) image compression it is based around the Discrete Cosine 
transformation, a lossy quantization step, following by lossless entropy coding.  In fact, 
JPEG offers a number of extra features over this base line set though we will restrict ourselves to
this set here.

# Overview of this documentation 

1. Review of the JPEG algorithm
2. Tour of the software model
3. Overview of the hardware design
4. IDCT implementations
5. Entropy decoding
6. Putting it all together

```ocaml
# module Jpeg = Hardcaml_jpeg;;
Line 1, characters 15-28:
Error: Unbound module Hardcaml_jpeg
```
