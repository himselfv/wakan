# Overview
Wakan 1.69 introduced a new feature - the ability to read special "annotation" files that can make Wakan display a webpage, picture or text whenever you display a specific word inside the Dictionary or select a specific character in Character mode. For example, when typing word "bara" (a rose in Japanese) to the Dictionary, Wakan can display a picture of a rose.

It was apparently originally scheduled to be fully implemented in Wakan 1.70, but that version never came to be. The feature as implemented in Wakan 1.69 is probably half-baked, although it does work.

At this point it is unclear if the feature is in demand. Those who actively use Annotations are asked to get in touch by creating Issues on Issue tracker detailing further development needed.

_This page has been ported from the old Wakan help and has not yet been updated. The information may be obsolete_

# Enabling and disabling annotations
Annotations feature can be switched on and off on `Settings -> Annotations` page. Uncheck "`Enable annotations`" off to slightly lower startup time.

# .ANO files
The annotation files are normal text files which you can create in any text editor. They can be either plain-text (8-bit) or Unicode UCS2 (16-bit), but I strongly recommend that you create them in Unicode UCS2 encoding. The files must have the extension ".ANO" and must be stored in Wakan folder.

The .ANO file can contain any number of "annotation commands" - one command on each line. The format of the annotation command is as follows:

`<tag>:<annotation>`

Where `<tag>` is specifying for what word of character the annotation should be displayed and `<annotation>` is giving details about the annotation.

# Tags
`<tag>` can be one of these:

  1. "U"+Unicode hex code of the character-to-be-annotated (ex. "U4E00")
  1. character-to-be-annotated (only in Unicode files) (ex. "乾")
  1. written form of the word-to-be-annotated (only in Unicode files) (ex. "産業")
  1. phonetic form of the word-to-be-annotated (only in Unicode files) (ex. "さんぎょう")
  1. romanized phonetic form of the word-to-be-annotated (ex. "sangyou")
  1. written form followed by phonetic form separated by "+" sign of the word-to-be-annotated (only in UCS2 files) - this is to prevent ambiguities caused by common written or phonetic form (ex. "橋+はし")

# Annotations
`<annotation>` can contain several specific commands to be performed on word or character specified by `<tag>`.

The format of `<annotation>` is:
```
  <command>:<parameter>,<command>:<parameter>, ...
```

Possibilities for `<command>` are:
  * "T" - show the text in `<parameter>` on word description line (for words only)
  * "C" - show the character/word in given foreground color
  * "I" - show the image whose location is given in `<parameter>` in special "media" window
  * "W" - show the webpage whose URL is given in `<parameter>` in special "media" window (specify the URL without "`http://`" part)

# Compilation process
To speed up working with annotations, Wakan automatically compiles .ANO files into a binary package file ANNOTATE.PKG. This file is recreated automatically whenever Wakan finds a new .ANO file in its folder or it detects change in any of the existing .ANO files. If you want to force the recompilation process, just delete ANNOTATE.PKG and Wakan will recompile it upon startup.