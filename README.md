# SDF glyph atlas generator

Generates [sdf](https://steamcdn-a.akamaihd.net/apps/valve/2007/SIGGRAPH2007_AlphaTestedMagnification.pdf), psdf and [msdf](https://github.com/Chlumsky/msdfgen/files/3050967/thesis.pdf)

### basic usage:

`(save-atlas (make-atlas "font.ttf" pixel-size) "atlas.png" "metric.met")`

Writes an atlas named `atlas.png` containing all the available characters
from font `font.ttf`, with tallest characters about `pixel-size` pixels tall.

### more options:

__make-atlas__ *font-name pixel-size* &key *scale spread string width height mode auto-size-granularity-x auto-size-granularity-y optimize-pack trim*

* `font-name` : name of a font file, passed to zpb-ttf

* `pixel-size` : size in pixels of tallest character in font

* `scale` : default `8`. Only used when `mode` is `:sdf-ms`, see `mode` for more info

* `spread` : default `2.5`. Range of pixels covered by distance field. lower gives a bit better precision close to shape edges, larger allows for wider blur effects

* `string` : vector of characters to include in atlas, or NIL for all available.

* `width`, `height` : default `:auto`. Size of texture to generate. If `:auto`, will guess a reasonable size and expand to fit. Due to incremental resizing, packing returned by `:auto` might not be same as returned by same sizes passed directly.

* `mode` : default `:sdf`. One of the following options:

    * `:sdf` : calculate a single channel sdf directly from vector data in font
    * `:sdf-ms` : render a bitmap from font, scaled up by `scale`, and calculate single-channel distance field from that
    * `:msdf` : calculate a multichannel sdf directly from vector data in font
    * `:psdf` : calculate a single channel signed pseudo-distance field directly from vector data in font

* `auto-size-granularity-x`, `auto-size-granularity-y` : default `1`,`1`. Granularity of increasing texture size when using `:auto` for `width` and/or `height`. Separate to allow for multiple-of-4 width if desired for 32 bit row stride

* `trim` : default `NIL`. If `:y-only`, trim any empty space from vertical edges of texture. If non-`NIL`, trim empty space from all sides of texture.

* `expand-mode`: default `:restart`. When using `:auto` dimensions or
  `:optimize-pack t`, tells whether to `:restart` packing from scratch
  after each increase in size, or to `:continue` with current pack
  after expanding size. `:restart` usually gives smaller atlas, but
  might be slower, especially with `optimize-pack`.

* `optimize-pack` : default `NIL`. If true, try various sizes to try
  to get a better packing (seems to help compared to just `:auto`, but
  might still be flaky and/or slow). If set to `:both`, try both
  `:restart` and `:continue` `expand-mode`s (very slow, but might save
  another few %)
