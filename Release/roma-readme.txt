; Comments start with a semicolon on a separate line
Anywhere where unicode is expected, hex-unicode is also allowed (though discouraged).

[Romaji]
Lists all kana-romaji assignments in the format:
  hiragana,katakana,any,number,of,romaji
First romaji is the one used in Kana->Romaji, the rest are supported as alternatives for Romaji->Kana (e.g. you can type both zo and dzo, but only zo will be printed).

Any of hiragana, katakana can be empty but if they aren't, they must match.

At this point only one or two character hiragana/katakana entries are supported. See comments below on how to handle long vowels, double consonants etc.

[Priority]
If different kana has the same transcription, usually due to similar romanization:
  を,ヲ,o,wo
  お,オ,o
Then it's unclear which one should be inserted when you type "o". For that is this section:
  o,お,を
Since を has alternative input formula, お has more priority. Any other uses for "o" which are not listed explicitly are assumed to be after the end of the list.
Either hiragana or katakana can be used at every position, but only one is needed. This is okay:
  o,お,ヲ
This is not:
  o,お,オ,を,ヲ

[KanaToRomaji]
Additional replacements to make on the string after doing basic Kana->Romaji transformation. Stuff like:
  nn,mn
  nb,mb

[RomajiToKana]
Replacements to make on the string before doing Romaji->Kana. Usually the reversal of KanaToRomaji beautifications, and maybe something else:
  mn,nn
  mb,nb

[RomajiToHiragana]
[RomajiToKatakana]
Yet more replacements to make in these particular cases. There may be specific rules, e.g. in katakana it's traditionally written "E-" instead of "EE".
If you're good without these sections then by all means do not use them.


Double consonants:
The usual way to handle this is to replace っ with D (having everything else in lowercase), and then in KanaToRomaji:
  Dt,tt
  Ds,ss
  etc.
Same thing backwards with RomajiToKana.

Long vowels:
The usual way is to not have any special table entries for these. Instead, transliterate stuff letter by letter, then replace:
  a-,aa
  i-,ia
  etc.
If stuff like ou needs to be represented differently, add that too:
  ou,o:

