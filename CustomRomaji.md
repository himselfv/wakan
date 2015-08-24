**This article is intended for advanced users**

Romanization systems are stored in Wakan folder as files with `.roma` extension. Those are text files in the following format:
```
# Comment
; Comment

[Romaji]
あ,a; い,i; う,u;
か,ka; き,ki; く,ku;
わ,wa;
ウァ,wa;
は,wa,ha;
を,o,wo;
お,o;

[Priority]
wa,わ; e,え; o,お;
```

`[Romaji]` section describes which latin sequences correspond to a given kana syllable. In the example above, you can type "a" to get "あ", and you can type "o" or "wo" to get "を".

Rules in this section can come in any order but if several entries are on the same string they must be `<tab>` and/or semicolon-separated. It doesn't matter if you use katakana or hiragana, they're considered equivalent.

When kana text needs to be translated to romaji, each syllable is located in this table and its first transliteration is taken, for example:
```
  わは -> wawa
  おを -> oo
```

When romaji input needs to be translated to kana, first syllable with that translatiteration is taken:
```
  wawa -> わわ
  waha -> わは
  oo -> をを
```

Note that there are several syllables which translate to "wa", but only one has to be chosen when translating "wa" back to kana. `[Priority]` describes the order of preference of syllable matches for a latin sequence. For example:
```
  o,お,を;
```
Tells us to convert "o" to "お". At this time it is not required to provide more than one element in the sequence since only the first element will be used, but you may also prioritize further entries. Any missing syllables from `[Romaji]` will be assumed to be at the end of the list.

## Pre/post-conversions
After translating kana to romaji there's an additional step where custom replacements take place. This allows for some customization of romaji (e.g. presenting long vowels as desired or replacing NN with NM as some transliterations do). Similarly, there's a replacement step just before translating romaji to kana.

The replacement rules for these steps are defined in four optional sections:
```
[KanaToRomaji]
',
; Long vowels
a-,aa
i-,ii
u-,uu
e-,ee
o-,oo
; nmp
nb,mb
np,mp
nm,mm

[RomajiToKana]
; nmp
mb,nb
mp,np
mm,nm

[RomajiToKatakana]
; Long vowels
aa,a-
ii,i-
uu,u-
ee,e-
oo,o-

[RomajiToHiragana]
; Nothing
```

`[RomajiToKana]` runs on every romaji->kana conversion while `[RomajiToKatakana]` and `[RomajiToHiragana]` additionally run only in these specific cases. This allows for some differences in transliteration to hiragana and katakana, as sometimes happens.

An example of how this is used is double-consonant handling. It's common to treat it like this:
```
[Romaji]
っ,D,xtsu;
# convert っ to "D" but allow typing it as "xtsu"
# ごっこ will be converted to goDko

[KanaToRomaji]
; Double consonants
Dk,kk
Dg,gg
Ds,ss
Dz,zz
; ...etc
# goDko -> gokko

[RomajiToKana]
; Double consonants
kk,Dk
gg,Dg
ss,Ds
zz,Dz
; ...etc
# gokko -> goDko -> ごっこ
```

Study existing romaji files to see how other common problems are solved:
  * Long vowels / ー
  * ん handling
  * nn/nm handling