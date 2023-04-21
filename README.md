# SDF glyph atlas generator

Generates [sdf](https://steamcdn-a.akamaihd.net/apps/valve/2007/SIGGRAPH2007_AlphaTestedMagnification.pdf), psdf and [msdf](https://github.com/Chlumsky/msdfgen/files/3050967/thesis.pdf)

### basic usage:

`(save-atlas (make-atlas "font.ttf" pixel-size) "atlas.png" "metric.met")`

Writes an atlas named `atlas.png` containing all the available characters
from font `font.ttf`, with tallest characters about `pixel-size` pixels tall.

Alternately, for metrics in more portable `bmfont` format, load `sdf/bmfont` and one or more of `3b-bmfont/json`, `3b-bmfont/xml` or `3b-bmfont/text`, then

`(sdf-bmfont:save-bmfont (make-atlas "font.ttf" pixel-size) "atlas.png" "atlas.json") ;; or replace "atlas.json" with "atlas.txt" or "atlas.xml"`

see [3b-bmfont](https://github.com/3b/3b-bmfont) for utilities to load and use bmfont format atlases.

### more options:

__make-atlas__ *font-name pixel-size* &key *scale spread string width height mode auto-size-granularity-x auto-size-granularity-y optimize-pack trim*

* `font-name` : name of a font file, passed to zpb-ttf

* `pixel-size` : size in texels of em-square in font.

Values in the range of 16 to 40 work for many fonts, depending on size of features in the font and intended final size. Icon or logo fonts with complex glyphs or fonts with very thin features may need larger values.

* `spread` : default `2.5`. Range of texels covered by distance field.

Lower gives a bit better precision close to shape edges and slightly
smaller atlas, larger allows for wider blur effects. Note that the
range of effects possible with a particular `spread` is in units of
texels in the atlas, but you will usually want to specify them either
in screen pixels or relative to font size. If the final render will be
significantly smaller you might need to adjust `spread` to make sure
the range is still large enough when converted to pixels. Similarly,
if you want effects that scale with font size, `spread` should be
calculated from `pixel-size`.

* `string` : vector of characters to include in atlas, or NIL for all available.

* `width`, `height` : default `:auto`. Size of texture to generate. If `:auto`, will guess a reasonable size and expand to fit. Due to incremental resizing, packing returned by `:auto` might not be same as returned by same sizes passed directly.

* `mode` : default `:sdf`. One of the following options:

    * `:sdf` : calculate a single channel sdf directly from vector data in font
    * `:msdf` : calculate a multichannel sdf directly from vector data in font
    * `:psdf` : calculate a single channel signed pseudo-distance field directly from vector data in font
    * `:msdf+a` : combination of `:msdf` stored in RGB channels and `:sdf` stored in alpha channel
    * `:mtsdf` : alias for `:msdf+a`


* `auto-size-granularity-x`, `auto-size-granularity-y` : default `1`,`1`. Granularity of increasing texture size when using `:auto` for `width` and/or `height`. Separate to allow for multiple-of-4 width if desired for 32 bit row stride

* `trim` : default `NIL`. If `:y-only`, trim any empty space from vertical edges of texture. If non-`NIL`, trim empty space from all sides of texture. (might cause final atlas to not be a multiple of `auto-size-granularity-*`

* `optimize-pack` : default `NIL`. If true, try various initial sizes to try
  to get a better packing. Currently accepts small integers to indicate how much effort to spend optimizing, but exact meaning is subject to change. Can add significant amounts of time, and usually only saves a few rows/columns from final size at best.

* `verbose` : if `:dots` print out `#\.` (and a few other characters) as progress indicator. Otherwise, if true print out verbose info about what it is doing. (might accept a function at some point to be called periodically, but API for that isn't finalized yet.)