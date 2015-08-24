Wakan uses dictionaries in its own format which has .DIC extension. Dictionary files must be located in the application folder. Dictionaries can be managed from `Main Menu > Database > Dictionary manager`.

# Dictionary manager
![http://wakan.googlecode.com/svn/wiki/images/dictman.png](http://wakan.googlecode.com/svn/wiki/images/dictman.png)

You can select dictionary files you want to use at this time from all the available dictionaries. Unchecked dictionaries will not be used.

  * **Refresh & Rescan** - Scans the current dictionary again for available .DIC files. Please note that only dictionaries for the current language (Japanese or Chinese) are displayed.

For every dictionary you can choose to include it into Group 1, 2 and/or 3 so that you can quickly switch between different sets of dictionaries when looking up a word. You can also choose whether to use each dictionary for compound words / kanji usage examples and for popup hints.

Up and down buttons are available to reorder dictionaries. Entries higher in the list will appear higher in search results.

  * **Import from EDICT** - Converts EDICT compatible dictionary to .DIC file, see below.


# Where to get more dictionaries
Some dictionaries are included in the distribution. You can download others and add those to Wakan:

  * Download ready-to-use dictionaries from Downloads section and put them into Wakan folder.
  * Download ready-to-use dictionaries (outdated) from [Wakan main site](http://wakan.manga.cz/?page=download&lang=en).
  * Download latest [EDICT2](http://www.csse.monash.edu.au/~jwb/edict.html) (japanese) / [CEDICT](http://www.mdbg.net/chindict/chindict.php?page=cedict) (chinese) from respective sites and put them into Wakan folder. They will be imported/updated when you run the application.
  * Download and import other EDICT2-compatible dictionaries (see below).

## Well-known dictionaries
These are external dictionaries you can import to Wakan. If you know a dictionary which is not listed here, add it or contact project owners.

Japanese to English:

  * [EDICT2 and EDICT](http://www.csse.monash.edu.au/~jwb/edict.html) are the same thing, only in different formats, so you only have to install one (preferably EDICT2).

  * SCICOMP, BUDDHIC and other small EDICT-like dictionaries got imported into EDICT. If you have a separate copy of it, you can delete it after downloading latest EDICT.

  * [ENAMDICT](http://www.csse.monash.edu.au/~jwb/enamdict_doc.html) is a dictionary of names, it's installed separately.

Japanese to Russian:

  * [JR-Edict](http://rowaasr13.narod.ru/jr-edict/index.html) (also available from Jim Breen's page)

  * [Warodai](http://e-lib.ua/dic/) (big japanese-russian dictionary) is not in EDICT form, but [a converter is available](http://code.google.com/p/jp-tools/).


# Importing dictionaries
To import dictionaries you need the import package: UNICONV.exe and WORDFREQ\_CK. See [here](http://code.google.com/p/wakan/issues/detail?id=67) and [here](http://code.google.com/p/wakan/issues/detail?id=66). Alternatively, download [Wakan Import Pack](.md) which contains everything you need to import dictionaries.

Wakan supports EDICT, EDICT2 and CEDICT, with full support to all features of EDICT2 (multi-kana, multi-kanji, multi-sense articles).

**Wakan <=1.67** supports EDICT (version 1) and CEDICT only. Dictionaries imported in Wakan <=1.67 will work on all versions of Wakan, but dictionaries imported in Wakan 1.80+ will only work in Wakan 1.80+.


### Auto import
Wakan automatically imports some well-known dictionaries. This includes EDICT2, EDICT, CEDICT and others. To add or update these dictionaries, just download them and put into Wakan folder. Next time you run Wakan they'll be re-imported.

### Custom import
Custom dictionaries can be manually imported by going to "Database->Manage dictionaries" (Wakan 1.80+) or "Tools->Manage dictionaries" (Wakan <=1.67) or  and doing "Import from EDICT".

  1. Add any number of dictionary files to the file list. It's recommended that you create a separate import for each file.
  1. Choose a dictionary file name which does not conflict with any of the already imported dictionaries.
  1. Enter some description/copyright/version information (anything you like).
  1. Set priority to 0, set language to "Japanese" if you import EDICT-formatted file, and "Chinese" if you import CEDICT.
  1. Make sure "Build with word index", "Build with character index" and "Build with frequency information" checkboxes are all set.
  1. Press "Build".

After the dictionary is built, it'll be automatically added to the application.

Not all of the entries might be converted, in which case a file "roma\_problems.txt" will be created in the application directory, listing the problems. This is fine, unless a large number of entries got cut.

### Command-line
**Wakan 1.80+**. Dictionaries can be imported by running Wakan with a command line of "makedic". See more at "CommandLine".

## Auto update
**Wakan 1.80+**. Wakan will automatically update any of your custom imported dictionaries if you place new versions of all the files which went into it in the application directory.

For example, if you have a dictionary "my.dic" which was compiled from "MY\_EDICT\_A" and "MY\_EDICT\_B", as long as "MY\_EDICT\_A" and "MY\_EDICT\_B" are available in Wakan folder, "my.dic" will automatically be updated every time any of those two files changes.

## Multilingual dictionaries
Dictionaries in any language are supported by Wakan. Japanese->English, Japanese->Czech, Japanese->Russian, even Japanese->Japanese definition dictionaries can be imported.

Only EDICT style tags are supported at this point. Other tags are treated as text.

**Wakan <=1.67** Only Japanese->English dictionaries are supported. Dictionaries in other languages can be used, as long as the system "Language for non-unicode programs" is set to that language.

## What if I have a dictionary in other format
Ask the dictionary owner to convert the dictionary into EDICT2/JMDict format. This is the universally accepted format for Japanese dictionaries. Many applications support it, so many people will benefit from the conversion.