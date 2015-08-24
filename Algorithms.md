_This page has been ported from the old Wakan documentation and has not yet been updated. The information is not up to date, but it's still a useful read for those getting into Wakan code._

# Introduction
This article is intended for developers. It describes the details of implementation of several core Wakan routines.

This document concerns Japanese mode unless noted otherwise, Chinese mode routines are similar but much more simple at this point.

List of algorithms:
  * DicSearch - performs dictionary search for a given word, returns a list of results sorted by priority
  * Deflex - returns a list of all possible original words from which the inputted word was inflected (deflection routine)
  * GetDocWord - returns largest possible word at a given position within the text
  * AutoTranslate - automatically assigns readings & meanings to a given text

# DicSearch
**Purpose**: searches all active dictionaries for a given word, returns a list of results sorted by priority

**Input**:
  * s - search string in Unicode
  * mode - mode of search (1-by reading, 2-by meaning, 3-by written-search whole, 4-by written-find word at beginning)
    * modes 1/2/3 are used in dictionary mode, mode 4 is used for editor and popup tool
  * modeflags - can contain these flags: partial (partial search), reverse (search end of word)
  * dctgroup - which dictionary group to use
  * ...and some other params which are not important

**Output**: a list of records (one for each result) sorted by priority. Each record contains:
  * phonetic/written/meaning of the result
  * index to .DIC and .USR,
  * wordlen - length of word in the search input

Steps:
  1. Generate a list of all search inputs (has to deflect the input word, locate the word, etc..). Each search input has its priority (9-highest, 0-lowest).
  1. For each search input try to search all active dictionaries (according to dictgroup) and put results into a list with priority
  1. Sort search results list according to priority

## Step (1) in detail
  * mode=1 -> convert s to kana/Bopomofo, call Deflex on s (priorities 9/8, sufokay mode), take its output as a list of search inputs
  * mode=2 -> s is the only item on search inputs
  * mode=3 -> call Deflex on s (priorities 9/8, sufokay mode), take its output as a list of search inputs
  * mode=4 -> this is complicated :o) s is a longest possible word, but the word can be smaller than that, we must generate a list of all possible words
    1. if s is in katakana, put it as the only word on the list of search input, we're done!
    1. s begins with kanji, call Deflex on it (priorities 9/8, alwaysdeflect mode) and continue with next option - this means that hiragana-only words are _not_ deflected (this would be too slow and inaccurate)
    1. if s consists only of kanji & hiragana, put all prefixes of s on the search input list (with priority = 2\*length of the prefix), prefix must comply to at least one of the following criteria:
      * we are in partial mode
      * it ends with a kanji
      * it is followed by a particle (eg. s must equal to prefix+particle). Particles are taken from .cfg `[Particles]`
      * it equals to s (eg. we always search the whole word)

### Examples for step (1)
  1. s=kawanakatta, mode=1 -> s converted to かわなかった
    * Deflex returns かわなかった(9), かわなかっつ(8), かわなかつ(8), かわなかう(8), かわなかる(8), かわない(8), かう(8)
  1. s=呼び込んでみてくださいませんか, mode=4
    * Starts with kanji -> try Deflex on s (alwaysdeflect mode, sufokay would probably return nothing)
    * Deflex returns 呼び込んでみつ(1),　呼び込んでる(1),　呼び込ぬ(1),　呼ぶ(1),　呼び込ぶ(1),　呼びる(1),　呼び込んでむ(1), 呼び込む(1),　呼び込んでみる(1),　呼る(1)
    * Now add all valid prefixes: 呼(0)  呼び(2#)　呼び込(4)　呼び込ん(6#)　呼び込んで(8#)　呼び込んでみ(8#)　呼び込んでみて(8#)　呼び込んでみて(8#)　く呼び込んでみてくだ(8#) 呼び込んでみてくださ(8#)　呼び込んでみてください(8#)　呼び込んでみてくださいま(8#)　呼び込んでみてくださいませ(8#)　呼び込んでみてくださいません(8) 呼び込んでみてくださいませんか(9)
    * Prefixes marked by # would be added only in partial mode

So you can see that we will search the dictionary for 14 different words (25 in partial mode).


## Step (2) in detail
  * search all dictionaries in dicgroup by phonetic (mode 1), written (modes 3/4) or meaning (mode 2 - uses word index)
    * if written search fails for modes 3/4 and s is hiragana-only, tries phonetic then
  * for each located word do this:
    1. if we used Deflex before, check whether it complies to the type Deflect returned,
      * for example if s was きって - Deflex returned (among others) きる, we would accept only u-verbs, and so
      * 切る, 斬る and 剪る will pass, but 着る will not. (this feature needs Edict tags present in dictionary)
    1. now calculate popclass ("popularity class") according to the following rules:
      * base popclass is 40
      * if "prefer nouns and verbs" is checked
        * lower popclass by 10 for verbs & nouns, by 5 for words without gram.marker
      * if "prefer polite words" is checked - lower popclass by 1-6 for honorific/humble/polite words
      * for obsolete words raise popclass by 20, the same for obscure words
      * if "prefer popular words" is checked - lower popclass by 150 for pop words
      * lower popclass by 10 for each conversion done (in the editor) for this entry (is remembered in .USR)
    1. now calculate priority (inp is input priority - from step (1), wordlen is length of the word):
      * mode=1 -> `priority=10000*(10-inp)+1000*wordlen+popclass*10`
      * mode=2 -> `priority=20000+(1000 if exact match)+popclass*100`
      * mode=3 or 4 -> `priority=10000*(10-inp)+popclass*10`
        * if "prefer words in user dictionary" is checked
          * lower priority by 1000 for user dic.words and further by 1000 for learned words
        * increase priority by 1000 for katakana words
        * increase priority by `(dicpriority*20000)` (dicpriority is the priority of the .DIC file)
        * The resulting priority/100 is displayed as "p" marker in Editor mode

Results are sorted by priority (ascending order).


# Deflex
**Purpose**: remove inflection from a word

**Input**:
  * s - input word
  * priorities - priority for inflected and original words
  * mode - sufokay or alwaysdeflect

**Output**: list of possible dictionary-form words from which s was inflected along with priority and word type (u-verb/ru-verb/etc.)

Algorithm steps:
  1. Find first non-kanji character in s and divide s by it into two parts: core and noncore
  1. Take each suffix of noncore and try this:
    * try to locate an inflection rule (in .cfg `[Deflection]`) which would have some prefix of suf in {inflected suffix} (see .cfg)
      * there are a special set of rules ("KKKK" rules) which conform to words with empty prefix and non-empty core
    * for each located rule try whether the rest of the noncore (without the prefix of suf) conforms to some known suffix (see example below if its unclear) in .cfg `[Suffixes]` - {suffix category} in `[Suffixes]` must match {suffix category} in `[Deflection]`
    * if the rule does not conform to any suffix - in sufokay mode do _not_ add it to results,
      * in alwaysdeflect mode _do_ add it to results but with priority 1
  1. original word is always returned

Example: s=飲んだら, priorities=9/8
  * core=飲, noncore=んだら
  * try to find rules for んだら, だら, ら and "KKKK"(we have empty prefix and nonempty core).
  * following rules are located
    1. u-verb rules with A {suffix category} (-ta form) for suffix んだら: んだ->む, んだ->ぶ and んだ->ぬ
    1. u-verb rules with a {suffix category} (negative form) for suffix ら: ら->る
    1. ru-verb rules with **{suffix category} for suffix "KKKK": KKKK->る
  * check suffixes:
    1. for A {suffix category} ら and り are valid (both kana can follow -ta form) we have 飲(core)+んだ(suf)+ら(remaining suffix) - suffix conforms to ら -> all 1) rules are valid
    1. for a {suffix category} we accept ない and なかった, but we have empty (remaining suffix) and so we accept 2) rule anyways (users can enter negative form alone)
    1. for** {suffix category} we would accept any defined suffix - but (remaining suffix), which is んだら confirms to neither to them -> we won't accept 3) rule in sufokay mode
  * and so the results are (priority in parentheses): 飲んだら(9), 飲む(8), 飲ぶ(8), 飲ぬ(8) and 飲んだる(8)
    * in alwaysdeflect mode there is one more result: 飲る(1)

For more examples see examples for Dicsearch step (1)


# GetDocWord
**Purpose**: find longest prefix of the string which could be a word

**Input**:
  * s - some part of a larger document

**Output**:
  * w - longest prefix of s which can be a Japanese word

There are four types of characters: hiragana, katakana, kanji and other

The algorithm works differently according to the type of the first character of s:
  1. if it is katakana, then w is longest katakana-only prefix of s
  1. if it is お or ご (honorific prefixes), then this char is ignored and the second char is taken as the significant one
  1. if it is hiragana, then w is longest hiragana-only prefix of s
  1. if it is kanji, then w is longest kanji-only prefix of s followed by longest hiragana-only prefix of s

Exception to rule 4: `<kanji><hiragana><kanji-sequence><hiragana-sequence>` words are also accepted

Example:
  * s=呼び込んでみて本を読んでいる, GetDocWord returns 呼び込んでみて
  * s=新聞を読みます, GetDocWord returns 新聞を

# AutoTranslate
**Purpose**: Assign reading and meaning to words within text

**Input**:
  * s - some Japanese/Chinese text

**Output**:
> annotated s

Algorithm works in these steps:
  1. move to the beginning of s
  1. call GetDocWord on the rest of s starting from current position
  1. call !Dicsearch (mode=4,partial) on GetDocWord result
  1. take the first Dicsearch result as the annotation
  1. move after the located word
  1. continue with 2) until end of the text is reached