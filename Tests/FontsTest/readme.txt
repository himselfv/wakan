We need to draw CJK characters in a predefined box, e.g. 100x100. We would like to occupy the whole box, and we would like this to work with all CJK fonts, or at least with most fonts, and all the common ones.

Common fonts we care about
 - MS Gothic + variations
 - MS Mincho + variations
 - MingLiU + variations
 - SimSun + variations
 - Yu Gothic
 - Yu Mincho
 - Meiryo / Meiryo UI
 - Microsoft JhengHei
 - Microsoft YaHei
 - Arial Unicode MS.

Options considered:
1. By line height ("simply draw")
All fonts are guaranteed to fit in the box, but different fonts are drawn in different sizes, and leave empty space. This is because fonts have internal leading (space at the top of the character) which is included in the line height.

2. By cell height (~= character height)
All characters are now roughly of the same size, the size of our box. So the font size selection is solved. But when we draw the text, the internal leading is still drawn, and our character ends up much lower, partially outside of the box.
We need to reposition them.

3. By cell height + centered
One way to reposition the characters is to DrawText(DT_VCENTER). This does not, strictly speaking, do what we want, but in practice it usually ends up in the box. Still, not precise: many fonts are slightly offset to the bottom.

4. By cell height + raise by leading
Old-time fonts have no "internal leading", modern fonts have different values of it. But CJK characters need no internal leading (it is for stuff like diacritics). Can't we just raise modern fonts by its internal leading value and end up with the beginning of the character?
This works, but not perfectly. Old-style fonts actually do have some internal leading, just hardcoded. So when we raise by softcoded internal leading old-style fonts continue to look good while modern fonts stick too much to the top.
Also some fonts (Meiryo is a notable offender) have broken internal leading values, having up to a third of the character in that zone. That part is going to be drawn outside of the box.

5. By cell height + raise by leading + autoadjust
 - Raise all fonts by internal leading.
 - Lower all fonts a bit to produce a nice fixed-width internal leading
 - Since we've now lowered all fonts including old-style fonts which already had hardcoded internal leading, apply hardcoded adjustments for some of those old-time fonts to raise them a bit back.
Since we also have offender fonts like Meiryo, whose internal leading values don't mean anything at all, we apply some hardcoded adjustments to these too.
This ALMOST WORKS. This is even a tolerable option: we support:
 - standard old-time fonts (via fixes)
 - unknown modern fonts with correct internal leading
 - known correct and known broken modern fonts via adjustments
These fonts are standard on Windows so hardcoding fixes to make them work is acceptable.
Yet maybe there's a better way?

RESULTS: Acceptable but not completely future-proof (many unknown fonts can still have weird values for internal leading).

6. By baseline.
The idea: Even if the font's ascent and internal leading are broken, it's baseline HAS to be correct, or it wouldn't look good even when you simply use it on one line with other fonts (e.g. latin) in Word.
There can be differences in how different fonts position CJK characters relative to the baseline, but they HAVE to be minimal. A bit higher or a bit lower, but nothing significant.
Therefore:
 - Note how the old-time (Gothic, Mincho) baseline is positioned in the box, when you draw a character
 - Adjust all fonts so that their baselines are positioned like that.

For a 100-pixel-high cell, Gothic and Mincho have ascent=86, descent=14.

RESULTS:
  Most fonts look fine, even though they are positioned slightly differently. Microsoft YaHei and especially MingLiU cross the lower border of the box!