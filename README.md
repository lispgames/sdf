# SDF glyph atlas generator

Generates [sdf](https://steamcdn-a.akamaihd.net/apps/valve/2007/SIGGRAPH2007_AlphaTestedMagnification.pdf), psdf and [msdf](https://github.com/Chlumsky/msdfgen/files/3050967/thesis.pdf)

### basic usage:

`(save-atlas (make-atlas "font.ttf" pixel-size) "atlas.png" "metric.met")`

Writes an atlas named `atlas.png` containing the characters in
`sdf:*default-characters*` from font `font.ttf`, with tallest
characters about `pixel-size` pixels tall.

### more options:

`make-atlas font-name pixel-size &key scale spread string width height mode auto-size-granularity-x auto-size-granularity-y optimize-pack trim`

* `font-name` : name of a font file, passed to zpb-ttf

* `pixel-size` : size in pixels of tallest character in font

* `scale`: default 8, only used when `mode` is `:sdf-ms`, see `mode` for more info

* `spread`: default 2.5, range of pixels covered by distance field. lower gives a bit better precision close to shape edges, larger allows for wider blur effects

* `string`: default `*default-characters*`, vector of characters to include in atlas

* `width`, `height`: default :auto, size of texture to generate. If `:auto`, will guess a reasonable size and expand to fit. Due to incremental resizing, packing returned by `:auto` might not be same as returned by same sizes passed directlly.

* `mode`: default `:sdf`, one of the following options:

    * `:sdf`: calculate a single channel sdf directly from vector data in font
    * `:sdf-ms`: render a bitmap from font, scaled up by `scale`, and calculate single-channnel distance field from that
    * `:msdf`: calculate a multichannel sdf directly from vector data in font
    * `:psdf`: calculate a single channel signed pseudo-distance field directly from vector data in font

* `auto-size-granularity-x`,`auto-size-granularity-y`: default 1,1: granularity of increasing texture size when using `:auto` for `width` and/or `height`. Separate to allow for multiple-of-4 width if desired for 32 bit row stride

* `optimize-pack`: default `NIL`, if true, try various sizes to try to get a better packing (seems to help compared to just `:auto`, but might still be flaky and/or slow)

* `trim`: default `NIL`, if `:y-only`, trim any empty space from vertical edges of texture. if `t`, trim empty space from all sides of texture.

