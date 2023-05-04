# oplay

## YUV video player 

Playback (and optionally difference) uncompressed video files in various YUV file formats.

YUV is a colour space often used for video compression.

### Command line

```
oplay [options] [in_file] [diff_file]

  -s      input size
  -S      output size
  -f      YUV format
  -r      frame rate (FPS)
  -d      start in fullscreen mode
  -v      be verbose
  -help   Display this list of options
  --help  Display this list of options
```

Sizes are specified with a string like `640x480` or an identifer 
like `qcif`, `vga`, `720p`(see stdsizes.ml for all supported).

The format is one of `yuy2`, `uyvy`, `yvyu`, `yv12`, `iyuv`, `420`.

If a `file` is not provided it is read from stdin.  In this case
only stepping and playing is supported.

### Keys

* `escape` quit
* `space`, `return` play/stop
* `tab` toggle full screen
* `right`, `left` step one frame forward/backward
* `home`, `end` go to first/last frame
* `1`-`9` go _n\*10%_ into the file
* `g` draw 16x16 grid
* `d` cycle difference mode
* `y` cycle colour plane

### Differencing 

2 YUV files can be supplied and a pixel by pixel difference shown.

### TODO

* Strange behaviour when selecting different colour planes/diffs.

