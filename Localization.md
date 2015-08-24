Wakan UI can be displayed in a number of languages, to change the translation click "Database > Change language..."

Available languages are stored as .lng files in Wakan folder. You can download more languages manually or through [Downloader](Downloader.md)


## Custom localizations
**This article is intended towards advanced users**

If the translation is not available for your language, you can make one yourself.

Copy `en.lng` stored in Wakan folder and rename it according to your language code. Edit the file, translating the text.

The format of the file is as follows:
```
; Comment
; Another comment
#LANG>Language name visible to the user
#AUTH>Translation author
#VERS>Translation version

[Numbered messages]

[Grammar tags]

[Kanji properties]
```

Numbered messages, Grammar tags and Kanji properties are sections, each will now be explained in detail.


### Numbered messages

These are blocks of text displayed somewhere in the application.

```
00001>Numbered text messages
00002>These usually go one after another
00003>Each numbered line is a single block of text which is displayed somewhere in the application
00065+Some messages can span multiple lines,
00065+which is denoted by using + instead of >.
00065+Line break will be inserted between such lines.
00065>Such messages end with a normal > line.
```

They may contain params such as %d or %s appear, which have to be preserved with their order intact. That's where textual (%s) or numeric (%d) information will be inserted at runtime.

### Grammar tags

Grammar and contextual markers are translated to target language too.

```
mark-rare=s
mark-sens=1
mark-sl=1slang
mark-uK=skanji
mark-uk=skana
```


### Character properties

Wakan handles a number of character properties taken from different sources (KANJIDIC, Unihan). This is a description of those which is shown in UI:

```
cprop-0=Separator,Can be used to separate groups of items
cprop-1=Korean reading,Romanized form of the Korean reading(s) of the character
cprop-2=Mandarin reading,"Pinyin" of each character, i.e. the (Mandarin or Beijing) Chinese romanization
```


## Hints
When translating, make sure you understand where in the UI the line appears to choose the appropriate translation. There's no clear way to know that, but you can usually guess from the lines around.

Sometimes guessing doesn't work and the line is to vague to translate conclusively, then make the safest bet and later correct the translation if you notice it in the UI.