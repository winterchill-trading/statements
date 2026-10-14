# Bundled font

`NotoSans-Regular.ttf` is Noto Sans 2.015 (Latin, Greek and Cyrillic), unmodified,
copyright 2022 The Noto Project Authors, licensed under the SIL Open Font License 1.1
(`OFL.txt` in this directory). The licence allows bundling and redistribution with
software; it declares no Reserved Font Name.

The library compiles the file into itself and embeds a subset of it (only the glyphs a
document uses) into PDF statements whose text the standard PDF fonts cannot show, such
as Russian or Kyrgyz. The licence's conditions do not apply to documents made with the
font. Nothing is downloaded at build time.

SHA-256: `f5f552c8c5edb61fe6efb824baf4d4de47b1a8689ab4925ff43f7bd6a4ebece5`
