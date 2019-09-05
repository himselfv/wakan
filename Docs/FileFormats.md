_This page has been ported from the old Wakan help and has not yet been updated. The information may be obsolete_

# File formats in Wakan

## Vocabulary list export
_Any subset of [vocabulary](Vocabulary) can be exported to this format from Vocabulary window._

First line must contain "`WaKan Word List 1`".
Lines beginning with `;` are comments.
Blank lines are ignored.

Each word is identified by four lines:
  1. reading of the word (Unicode in hex)
  1. writing of the word (Unicode in hex)
  1. english meaning
  1. category in the format `<language>~<category type><category name>`

## CSV vocabulary export
_The entire vocabulary can be exported to this format from [Settings\Database maintenance](Settings#Database_maintenance) page._

First character of each line:
  * `$` - Table with the given table name begins here
  * `>` - List of table columns separated by ;
  * `+` - Record in the table, each field is separated by ;
  * `.` - End of table

Comments are allowed before the first `>`, comment lines must start with `;` symbol.
**Note:** Wakan versions prior to **1.8.19-dev** do not allow comments in this way.

Exported file contains following tables:

  * `User`: List of vocabulary words (`PhoneticSort` is a sort order of readings, dates are given in YYYYMMDD format, `Score`: 0-problematic, 1-unlearned, 2-learned, 3-mastered, `MaxScore`: maximum attained score)
  * `UserIdx`: List of characters appearing in vocabulary words (`Kanji` is Unicode index of the character, `Begin` is T or F - T when the character is at the beginning of the word)
  * `UserCat`: Categories of words (Type is ASCII code of the category type)
  * `UserSheet`: Links words to categories
  * `KnownKanji`: List of learned characters

Table references:

  * `UserIdx.Word` references `User.Index`
  * `UserSheet.Word` references `User.Index`
  * `UserSheet.Number` references `UserCat.Index`