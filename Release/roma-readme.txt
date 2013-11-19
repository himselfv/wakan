KanaConv transcription format documentation.

; Comments start with a semicolon on a separate line
Anywhere where unicode is expected, hex-unicode (C037B5AA) is also allowed (though discouraged).


[Romaji]
Lists all kana-romaji assignments in the format:
  kana,any,number,of,romaji
  ぞ,zo,dzo

First romaji is the one which will be used in Kana->Romaji, the rest are supported as alternatives for Romaji->Kana, e.g. you can type both ZO and DZO for ぞ, but ぞ is translated to romaji as ZO:
  zo -> ぞ
  dzo -> ぞ
  ぞ -> zo

Kana can be hiragana or katakana, it's your choice. Some rare syllables are only available in katakana.
At this point only one or two character kana entries are supported. See comments below on how to handle long vowels, double consonants etc.

You can specify multiple items on the same line by separating them with ;. This allows for tables
	か,ka;	き,ki;	く,ku;	け,ke;	こ,ko;
	が,ga;	ぎ,gi;	ぐ,gu;	げ,ge;	ご,go;


[Priority]
Sometimes different kana has the same romanization:
  を,o,wo
  お,o
Then it's unclear which one should be inserted when you type "o".
  o -> を? お?
You can give backward translation priorities in this section:
  romaji,any,number,of,kana
  o,お,を
  ; Since を has alternative input formula, お has more priority.
The first kana in the list will be used as a translation of romaji by default. The order of the rest can be used by some software to, say, present a list of probable alternatives. Any other uses for "o" which are not listed explicitly are assumed to be after the end of the list.

Either hiragana or katakana can be used at every position, but only one is needed. This is okay:
  o,お,ヲ
This is not:
  o,お,オ,を,ヲ

You can specify multiple items on the same line by separating them with ;.


[KanaToRomaji]
Additional replacements to make on the string after doing basic Kana->Romaji transformation. Stuff like:
  nn,mn
  nb,mb
See below for usage examples.

[RomajiToKana]
Replacements to make on the string before doing Romaji->Kana. Usually the reversal of KanaToRomaji beautifications, and maybe something else:
  mn,nn
  mb,nb

[RomajiToHiragana]
[RomajiToKatakana]
Yet more replacements to make in these particular cases. There may be specific rules, e.g. in katakana it's traditionally written "E-" instead of "EE".
If you're good without these sections then by all means do not use them.



Double consonants
====================
The usual way to handle double consonants is to replace っ with D (having everything else in lowercase), and then in KanaToRomaji:
  Dt,tt
  Ds,ss
  etc.
Same thing backwards with RomajiToKana.

Long vowels
==============
The usual way is to not have any special table entries for these. Instead, transliterate stuff letter by letter, then replace:
  a-,aa
  i-,ia
  etc.
If syllables like ou needs to be represented differently, add that too:
  ou,o:
When translating back, it's usual to *not* translate "aa" to "a-" except for hiragana:
  [RomajiToKana]
  ; nothing on this case
  [RomajiToKatakana]
  aa,a-
If you need to type a- in hiragana, just type that.

MNP
======
Many transliterations replace N with M when followed by B, P or M. This is pretty straightforward:
  [KanaToRomaji]
  nb,mb
  np,mp
  nm,mm
  [RomajiToKana]
  mb,nb
  mp,np
  mm,nm