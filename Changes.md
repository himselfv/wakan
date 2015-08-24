# Introduction

This page lists changes in various versions of Wakan, in historical order. For changes in versions earlier than 1.67, see [old history page](http://wakan.manga.cz/?page=history&lang=en).

# 1.9
  * Redesigned kanji search panel - share your thoughts
> > ![http://wiki.wakan.googlecode.com/hg/images/kanji-search-panel.png](http://wiki.wakan.googlecode.com/hg/images/kanji-search-panel.png)

  * [Dictionary and component downloader](Downloader.md) with auto-import
> > ![http://wiki.wakan.googlecode.com/hg/images/downloader-select.png](http://wiki.wakan.googlecode.com/hg/images/downloader-select.png)

  * Streamlined manual dictionary import
  * [Links](CustomLinks.md) and [copy formats](CopyFormats.md) for both kanji and words are now stored as standard XSLT and LNK files
  * Support for ENAMDICT tags


# 1.87
**Warning**: `wakan.chr` format changed. Wakan 1.87+ cannot load older `wakan.chr`, and Wakan <= 1.87 cannot load new ones.

  * New `wakan.chr` format -- older `wakan.chr`s won't load.
  * New compatible `wakan.chr` with latest information from KANJIDIC and Unihan included.
  * You can now make your own language versions of `wakan.chr` (although this is a little clumsy). Go to `Settings-> Database managment-> Import character data`.

# 1.86

  * Redesigned KanjiDetails with less visual clutter, cleaner categories (older combobox is also available).
> > ![http://wiki.wakan.googlecode.com/hg/images/chardetails-category-menu.png](http://wiki.wakan.googlecode.com/hg/images/chardetails-category-menu.png)

  * "Word examples" button in Kanji details.
  * [Custom links](CustomLinks.md) for kanji and expressions.
  * Ctrl-C in dictionary lets you copy results [in different formats](CopyFormats.md) — right-click dictionary results.
> > ![http://wiki.wakan.googlecode.com/hg/images/dict-popup-menu-mini.png](http://wiki.wakan.googlecode.com/hg/images/dict-popup-menu-mini.png)

  * Unified JP->EN, EN->JP lookup in dictionary
> > ![http://wiki.wakan.googlecode.com/hg/images/dict-any-matches.png](http://wiki.wakan.googlecode.com/hg/images/dict-any-matches.png)

  * Redesigned transliteration selection, you can now create [custom romaji](CustomRomaji.md) systems.
> > ![http://wiki.wakan.googlecode.com/hg/images/custom-romaji.png](http://wiki.wakan.googlecode.com/hg/images/custom-romaji.png)

# Since Wakan 1.67 to Wakan 1.85

**Warning**: Dictionary format changed. Wakan 1.80+ can load older dictionaries, but new dictionaries will not load in older Wakans.

## Dictionaries
  * New dictionary format with full support for EDICT2 (multi-kana multi-kanji multi-sense articles), full multi-lingual support.
  * EDICT2 import
  * CC-EDICT import **1.8.17**
  * Auto-import and auto-update for dictionaries. Just place the latest EDICT/EDICT2/CEDICT/etc file in the Wakan folder.
  * Multilingual dict support. You can now import versions of EDICT in your local language (i.e. japanese->czech).
  * Dynamic priority for dictionaries (reorder your dictionaries in dictionary manager)

## Character data
  * Import non-English language KANJIDIC (Settings> Database> Import char data) (parts of Issue 120 (on Google Code)) **1.8.18**

## Text translator
  * Greatly increased translation speed. A text which took Wakan 1.67 6 minutes to translate now takes about 4 seconds.
  * Multithreaded translation feature (further speedup)
  * Option to disable "You're about to translate a long text fragment" warning.
  * You can now abort the translation process at any time.
  * Aozora-Ruby support. Load or paste text with aozora-ruby and it'll be displayed with ruby above the text. Save auto-generated readings in Aozora-Ruby format and read the text in your favorite aozora-bunko reader.
  * Export as HTML with Ruby (Issue 24 (on Google Code)), OpenDocument Text with Ruby (opens in LibreOffice) (Issue 113 (on Google Code)), kanji+kana with spaces (Issue 58 (on Google Code))  **1.8.16**
  * Copy as HTML (Ctrl-C, Ctrl-V into Word with Ruby), annotated text as text with ruby (Ctrl-Shift-C) (Issue 94 (on Google Code)) **1.8.16**
  * Copy text with translations between Wakan instances (Ctrl-C, Ctrl-V in another Wakan) (Issue 114 (on Google Code)) **1.8.16**

## Optimization
  * Kanji list now refreshes several times faster.
  * Faster loading, up to 8 times compared to Wakan 1.67, even with big user vocabularies.

## Misc features
  * Fullscreen Mode (press F11)
  * Option to not show splash screen on start.
  * [CommandLine](CommandLine.md) enhancements:
    * You can open files for editing by passing them to Wakan.
    * [Dictionaries](Dictionaries.md) can be imported from the command line.
  * Nicer settings dialog, more pages and more logical grouping of options
  * Dictionary tag list updated to the lastes one for EDICT.
  * Dictionary tags translation. It is now possible to have all those "pop" and "adj-na" tags in your native language.
  * Raine radicals list updated to the latest version.
  * Option to disable "Do you want to save changes?" warning.
  * Option to disable automatic priority adjustment based on what you type (no more "User data changed" messages after editing a text).
  * Portable Wakan (Issue 108 (on Google Code), Issue 19 (on Google Code)) **1.8.15** - See [Portable](Portable.md)
  * Multi-select kanji and mass-add to groups (Issue 20 (on Google Code)) **1.8.15**
  * Any font size in Editor (Issue 56 (on Google Code)) **1.8.15**
  * Category manager, merge and duplicate categories (Issue 57 (on Google Code)) **1.8.15**
  * Option to save search params\save column widths on exit (Issue 98 (on Google Code)) **1.8.15**
  * Allow resizing for some panels (Issue 92 (on Google Code)) **1.8.16**
  * Portrait layout (Tools> Portrait mode) (Issue 100 (on Google Code)) **1.8.16**

## Bugfixes and misc
  * WordIndex and CharIndex are incorrectly reported as missing when dictionary isn't loaded.
  * Do not write wakan.lay since it's not being read anymore
  * Temporary data in `annot\` not being deleted
  * Text editor: Finalize insert before selecting all (Ctrl-A)
  * Wakan resets currently selected group in KanjiDetails from time to time
  * Reload kanji details when cancelling user changes
  * `user\char*.bin` files being left in Wakan folder
  * Flicker when selecting text/scrolling through selected text in the editor.
  * Last, partially visible line in the editor is not drawn
  * First paragraph not being rendered correctly when visible only partially
  * Process messages while displaying the translation progress
  * Dictionary manager, Settings now close by ESC key
  * Progress bar for import/export vocab (Issue 105 (on Google Code)) **1.8.15**
  * Popup hint might make vocabulary edits go to the wrong word (Issue 35 (on Google Code)) **1.8.15**
  * Shift-Click does not select text in the editor (Issue 104 (on Google Code)) **1.8.15**
  * Text selection in several controls is buggy (Issue 117 (on Google Code)) **1.8.16**
  * Fonts are not auto-detected properly when Japanese locale is active (Issue 122 (on Google Code)) **1.8.16**

## Files changed
  * wakan.exe -- recompiled
  * en.lng -- added translation lines
  * ru.lng -- added translation lines, corrected translation
  * all languages -- minor changes
  * wakan.rad -- radical list updated from RADKFILE
  * wakan.sod -- recompiled
  * wakan.cfg -- kana conversion params added, format of various sections changed
  * wakan.ini -- added, optional
  * 7z.dll -- added, needed for exporting in OpenDocument, also for auto-downloading