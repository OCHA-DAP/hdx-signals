# Chart font sizes vs. campaign display scaling

Font sizes as they stand now, plus what actually reaches the screen once the
campaign's fixed image width shrinks the whole PNG:

| Chart | Title | Caption | Native canvas @300dpi | Scale factor (660÷native width) | Title on screen | Caption on screen |
|---|---|---|---|---|---|---|
| Line chart (`plot_ts.R`) | 16px | 12px | 6×4in → 1800×1200px | 0.367 | ~5.9px | ~4.4px |
| Heatmap (`plot_agricultural_hotspots.R`) | 16px | 12px | 6×3in → 1800×900px | 0.367 | ~5.9px | ~4.4px |
| Market monitor (`plot_market_monitor.R`) | 14px | 10px | 6×4in → 1800×1200px | 0.367 | ~5.1px | ~3.7px |
| Map (`map_theme.R`) | 14px | 8px | varies per ISO3, 4–6in → 1200–1800px | 0.55 (narrow) – 0.367 (wide) | ~7.7px (small country) – ~5.1px (large) | ~4.4px – ~2.9px |

**The scaling factor, simply:** every image is drawn once at a fixed pixel
size — `width_inches × 300 dpi` (set by `ggsave()` in `images.R:139-147`).
The campaign then displays it in an `<img>` tag pinned to a fixed CSS width,
`660px` (`image_block.R:29`), with `height:auto`. A browser/email client
doesn't re-render the text — it just shrinks the whole raster image down to
fit that 660px box, so every pixel (font included) gets multiplied by
`660 / native_pixel_width`.

That's why line chart/heatmap/market monitor all share one constant factor
(0.367) — they always render at a fixed 6in width, so the ratio never
changes. Maps are the outlier: their canvas width comes from
`input/iso3_map_settings.json` per country, so the ratio (and thus the
effective on-screen size) shifts country to country.

## Not yet implemented / open items

- **Map's scale factor still isn't normalized.** Unlike the line
  chart/heatmap/market monitor (all fixed at 0.367), maps range from 0.367
  to 0.55 depending on the country's canvas width in
  `iso3_map_settings.json`. No fix has been applied to make the *effective
  on-screen* map title/caption size match the other charts' ~5-6px — only
  the nominal (pre-scaling) px values were aligned with main (14px/8px).
- **`gt` table image (`table_inform.R` / `save_table()`) not reviewed.**
  Its font sizing and effective on-screen scale weren't checked in this
  pass — it also renders at a fixed 6×4in canvas but displays at a
  different CSS width (`500px`, `location_block.R:69`), so its scale
  factor (500/1800 = 0.278) differs from every chart above and hasn't been
  tuned to match.
- **No empirical verification.** All scale-factor math here is derived
  from the `ggsave`/`img_width` code paths, not confirmed against an actual
  rendered Mailchimp campaign send.
- **Market monitor's new 14px/10px sizing is untested** against a real
  send — chosen to match the map's nominal sizes, not verified to stop the
  overflow in practice.
- **Heatmap rounded corners**: spec asked for 2px-radius corners on the panel frame/legend swatches; ggplot2 has no native rounded-rect support, so these are plain square hairline borders instead.
- **Line chart entity/category chips** (e.g. "Pakistan"/"Armed conflict" pill badges): not implemented — flagged optional in the original ask, and there's no existing data plumbing to attach category labels to a chart.
- **Line chart hex tokens**: that review specified a slightly different hairline/muted-grey (#DCE3E4/#7D8B8D) than what maps/heatmap already shipped (#D8E0E1/#9DB1B3). Kept the existing tokens rather than introduce a second inconsistent set — flagging the mismatch between reviews rather than resolving it unilaterally.
