_This page has been ported from the old Wakan help and has not yet been updated. The information may be obsolete_

# Main window
![http://wakan.googlecode.com/svn/wiki/images/main.png](http://wakan.googlecode.com/svn/wiki/images/main.png)

The control panel at the top of the main window has four buttons, labeled Characters, Dictionary, Editor and Translator and Vocabulary. These correspond to the four [main areas](#Areas.md) of the program], which are described below. Each displays the corresponding tool in the lower part of the main window.

## Menus
The [Database menu](#DatabaseMenu.md) provides access to the user Settings and the Dictionary Manager. It also provides commands for saving and undoing changes in the user database, and displaying [statistics](#Statistics.md).

The Characters, Dictionary, Editor and Vocabulary menus provide commands relevant to the corresponding tools (see [below](#Areas.md)).

The Help menu provides access to this help, and to the "About" page for Wakan.

## Language Selector
  * ![http://wakan.googlecode.com/svn/wiki/images/btn_japanese.png](http://wakan.googlecode.com/svn/wiki/images/btn_japanese.png) - Switches program to Japanese mode.
  * ![http://wakan.googlecode.com/svn/wiki/images/btn_chinese.png](http://wakan.googlecode.com/svn/wiki/images/btn_chinese.png) - Switches program to Chinese mode.

Switching languages alters the entire user interface. The [Character list](KanjiList.md) displays only characters relevant to the selected language, the [Dictionary Manager](Dictionary#Dictionary_manager.md) displays  dictionary files only for that language, and the [Vocabulary](Vocabulary.md) shows only word lists for that language.

## Clipboard viewer
![http://wakan.googlecode.com/svn/wiki/images/clip.png](http://wakan.googlecode.com/svn/wiki/images/clip.png)

Shows the contents of the Windows clipboard (or the beginning of the contents, if it is too long to fit in the viewer). Many commands in the Wakan can change the contents of the clipboard.

**Note:** Program can handle only UNICODE clipboard format. If you want to paste some data from this program make sure the application supports UNICODE.

  * ![http://wakan.googlecode.com/svn/wiki/images/btn_cliperase.png](http://wakan.googlecode.com/svn/wiki/images/btn_cliperase.png) - Clears clipboard contents.


# Areas
Main areas of the program:
  * **[Character list](KanjiList.md)** - The character list can display all of the characters in the database for the selected language, or the results of a [search](KanjiList#Search.md) for
characters with specified properties. The list can be [sorted](KanjiList#Sort.md) in various ways, and [detailed information](KanjiDetails.md) can be displayed for a selected character.
  * **[Dictionary](Dictionary.md)** - The dictionary window provides the definition for a Japanese or Chinese word, or searches the dictionary for words whose definitions contain an English word
or phrase.
  * **[Editor and translator](Editor.md)** - Provides an input method (a way of typing words in Roman letters and having them converted to characters) and other features for text editing. In addition, it can display phonetic readings for characters in the context of words, and the meanings of individual words. Integrated dictionary window can be used to translate individual words and to assist in translating longer pieces of text.
  * **[Vocabulary](Vocabulary.md)** - The Vocabulary Manager maintains word lists for study purposes, and groups them by categories.


# Database menu
  * **Save user changes** (Ctrl+F2): Writes all changes made in [vocabulary](Vocabulary.md) database to disk.
  * **Cancel user changes** (Ctrl+F3): Cancels all changes in [vocabulary](Vocabulary.md) database, causing it to revert to the last version saved to disk.
  * **Statistics**: Shows information about the dictionary files, and statistics on information stored in the user database (learned words and characters, vocabulary entries, etc).
  * **Dictionary Manager** (Ctrl+F8): Shows the [dictionary manager](Dictionary#Dictionary_manager.md) (a utility for managing or importing dictionary files).
  * **Settings**: Shows user preference [settings](Settings.md).
  * **Change language**: Lets you select Wakan user interface language.
  * **Exit** (Alt+X): Closes the program, and asks whether unsaved changes in the databases or the contents of the editor should be saved.


# Printing
![http://wakan.googlecode.com/svn/wiki/images/printpreview.png](http://wakan.googlecode.com/svn/wiki/images/printpreview.png)

WYSIWYG print preview window.

  * ![http://wakan.googlecode.com/svn/wiki/images/btn_printfirst.png](http://wakan.googlecode.com/svn/wiki/images/btn_printfirst.png) - Displays first page.
  * ![http://wakan.googlecode.com/svn/wiki/images/btn_printprior.png](http://wakan.googlecode.com/svn/wiki/images/btn_printprior.png) - Displays previous page.
  * ![http://wakan.googlecode.com/svn/wiki/images/btn_printnext.png](http://wakan.googlecode.com/svn/wiki/images/btn_printnext.png) - Displays next page.
  * ![http://wakan.googlecode.com/svn/wiki/images/btn_printlast.png](http://wakan.googlecode.com/svn/wiki/images/btn_printlast.png) - Displays last page.
  * ![http://wakan.googlecode.com/svn/wiki/images/btn_printzoomin.png](http://wakan.googlecode.com/svn/wiki/images/btn_printzoomin.png) - Displays smaller portion of page.
  * ![http://wakan.googlecode.com/svn/wiki/images/btn_printzoomfull.png](http://wakan.googlecode.com/svn/wiki/images/btn_printzoomfull.png) - Displays entire page.
  * ![http://wakan.googlecode.com/svn/wiki/images/btn_printzoomout.png](http://wakan.googlecode.com/svn/wiki/images/btn_printzoomout.png) - Displays larger portion of page.
  * ![http://wakan.googlecode.com/svn/wiki/images/btn_printsettings.png](http://wakan.googlecode.com/svn/wiki/images/btn_printsettings.png) - Displays [character cards settings](Settings#Character_cards.md), [vocabulary list settings](Settings#Vocabulary.md) or [text translator settings](Settings#Editor.md) depending on what you are printing.
  * ![http://wakan.googlecode.com/svn/wiki/images/btn_printpage.png](http://wakan.googlecode.com/svn/wiki/images/btn_printpage.png) - Displays printer and page settings dialog.
  * ![http://wakan.googlecode.com/svn/wiki/images/btn_printprint.png](http://wakan.googlecode.com/svn/wiki/images/btn_printprint.png) - Begins printing.
  * ![http://wakan.googlecode.com/svn/wiki/images/btn_printbmp.png](http://wakan.googlecode.com/svn/wiki/images/btn_printbmp.png) - Saves print into a series of BMP files.