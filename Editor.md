_This page has been ported from the old Wakan help and has not yet been updated. The information may be obsolete_

# Text editor
![http://wakan.googlecode.com/svn/wiki/images/dicttranslate.png](http://wakan.googlecode.com/svn/wiki/images/dicttranslate.png)

This utility serves as a text editor and assists in translating text from Japanese or Chinese to English. Its features include an input method (converts syllables typed in Roman letters to characters), recognizing word boundaries (only available in Japanese, not in Chinese) and printing vertical text with furigana and meaning. It can be very useful for reading web pages or written text.

**How to access:** **Editor** button on the main control panel, or **Editor window** (Ctrl+E) on the **Editor** menu. (Also appears in part of the main window when **Translator** button on the main control panel is pressed.)

**Quick tip:** After loading Japanese or Chinese text, press **Fill** button to fill reading and meanings.

**Warning:** Only active dictionaries are searched. You can specify which dictionaries are active in the [dictionary manager](Dictionary#Dictionary_manager.md).

## Window controls
  * ![http://wakan.googlecode.com/svn/wiki/images/btn_transopen.png](http://wakan.googlecode.com/svn/wiki/images/btn_transopen.png) - Loads text for translation from 16-bit Unicode file or from a special application format that already contains translation information.
  * ![http://wakan.googlecode.com/svn/wiki/images/btn_transsave.png](http://wakan.googlecode.com/svn/wiki/images/btn_transsave.png) - Saves text into 16-bit Unicode file or into special application format with the translation information.
  * ![http://wakan.googlecode.com/svn/wiki/images/btn_transkanji.png](http://wakan.googlecode.com/svn/wiki/images/btn_transkanji.png) - Kanji mode (converts kana to kanji).
  * ![http://wakan.googlecode.com/svn/wiki/images/btn_transkana.png](http://wakan.googlecode.com/svn/wiki/images/btn_transkana.png) - Kana mode (disables kanji conversion).
  * ![http://wakan.googlecode.com/svn/wiki/images/btn_transalpha.png](http://wakan.googlecode.com/svn/wiki/images/btn_transalpha.png) - ASCII mode (writes ASCII text).
  * ![http://wakan.googlecode.com/svn/wiki/images/btn_transfurig.png](http://wakan.googlecode.com/svn/wiki/images/btn_transfurig.png) - Toggles furigana display.
  * ![http://wakan.googlecode.com/svn/wiki/images/btn_transmean.png](http://wakan.googlecode.com/svn/wiki/images/btn_transmean.png) - Toggles displaying meaning under text.
  * ![http://wakan.googlecode.com/svn/wiki/images/btn_transclear.png](http://wakan.googlecode.com/svn/wiki/images/btn_transclear.png) - Deletes translation information from whole text.
  * ![http://wakan.googlecode.com/svn/wiki/images/btn_transfill.png](http://wakan.googlecode.com/svn/wiki/images/btn_transfill.png) - Automatically fills reading and meaning to whole text. Program tries to locate every word of the text in the dictionary and stores its reading and meaning. Located words are displayed in different colors (see below). This process can be configured in the [settings](Settings#Editor.md).
  * ![http://wakan.googlecode.com/svn/wiki/images/btn_transchange.png](http://wakan.googlecode.com/svn/wiki/images/btn_transchange.png) - Sets the translation of the currently selected word in text to the currently selected word in dictionary.
  * ![http://wakan.googlecode.com/svn/wiki/images/btn_transprint.png](http://wakan.googlecode.com/svn/wiki/images/btn_transprint.png) - Displays [print preview](MainWindow#Printing.md) window with the currently loaded text. Furigana and meaning can be printed too. Text can be printed vertically (right-to-left). All of this can be configured in [settings](Settings#Editor.md).

## Colors in translated text
  * **White** - Untranslated word.
  * **Red** - Word which was not found in dictionary.
  * **Yellow** - Translated word.
  * **Orange** - Translated verb.
  * **Blue** - Estimated particle.
  * **Cyan** - Hiragana-only word.
  * **Violet** - Katakana-only word.

# How to write Japanese text
You can freely write text in hiragana or katakana in the editor window. Text entered in romaji is automatically converted to kana using the current [romanization settings](Settings#Romanization.md). See [romanization tips](Settings#Romanization.md) for more info. Text written UPPER-CASE is converted to katakana, text written in lower-case is converted to hiragana. Whenever you press SPACE the last entered text is converted to kanji of currently selected word in [dictionary](Dictionary.md) (you can change the selection by up and down arrow keys). By pressing ENTER you leave the text as it is performing no conversion.

## Keyboard summary
  * **SPACE** - Convert lastly entered word (shown in blue) to kanji.
  * **ENTER** - Perform no conversion to lastly entered word.
  * **UP/DOWN ARROW** - Change currently selected word in dictionary (to alter the conversion).

## Example
**This tutorial will show you how to write the sentence**
> `watashi ha hon wo yomimasu ga, terebi wo mimasen.`

  1. Open the editor window and select kanji mode (by pressing button btn\_transkanji.png).
  1. Type `watashi`. Note that the text is displayed in hiragana (since you typed it in lowercase) and that it is coloured blue (because it is not yet converted to kanji).<br><img src='http://wakan.googlecode.com/svn/wiki/images/tutorial1.png' />
<ol><li>Press SPACE. Watashi is then converted to kanji.<br><img src='http://wakan.googlecode.com/svn/wiki/images/tutorial2.png' />
</li><li>Type <code>ha</code> and press SPACE. Program identifies it as a particle and displays it in a different color.<br><img src='http://wakan.googlecode.com/svn/wiki/images/tutorial3.png' />
</li><li>Type <code>hon</code>, press SPACE to convert it and type <code>wo</code> and SPACE for the particle.<br><img src='http://wakan.googlecode.com/svn/wiki/images/tutorial4.png' />
</li><li>Type <code>yomimasu</code> and SPACE for conversion. Note that WaKan can handle inflected words and also that it is displayed in different color because it is a verb. If you meant yomimasu as to recite, you must press DOWN ARROW before the conversion to change the actual verb in dictionary.<br><img src='http://wakan.googlecode.com/svn/wiki/images/tutorial5.png' />
</li><li>Type <code>ga,</code>. You don't need to press SPACE before the comma because WaKan can automatically identify the word has ended.<br><img src='http://wakan.googlecode.com/svn/wiki/images/tutorial6.png' />
</li><li>Type <code>TEREBI</code> or <code>terebi</code> and SPACE. If you are entering katakana words that are not in dictionary you must type it in upper-case, however terebi is common and program converts it to katakana automatically.<br><img src='http://wakan.googlecode.com/svn/wiki/images/tutorial7.png' />
</li><li>Type <code>wo mimasen.</code> to finish the sentence.<br><img src='http://wakan.googlecode.com/svn/wiki/images/tutorial8.png' /></li></ol>

<h1>Tips</h1>

<h2>Translating a web page</h2>
You can open the <a href='Dictionary.md'>dictionary</a> window, select <b>Search by clipboard</b>, leave the window open. Then you can select some text on the web page, put it into the clipboard and immediately see the word translation (without even clicking on Wakan window). See the screenshot below.<br>
<br>
<img src='http://wakan.googlecode.com/svn/wiki/images/shot_translateweb.png' />

<h2>Translating a Japanese text document</h2>
You can load Unicode text document into text translator and use the <b>fill</b> function to fill meaning and reading to words in the text. Then you can print the document with furigana and meaning. You can also click on any word in the text to automatically display all dictionary matches (and correct the automatically filled meaning).