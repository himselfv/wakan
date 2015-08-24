#summary Describes what CopyFormats are and how to configure them

**Information on this page applies to Wakan 1.9+**


Wakan can copy search results and character data by Ctrl-C in various formats, e.g.:
```
  伴船 [ともぶね] 1. consort ship; 2. joint boarding; boarding a ship together ——EDICT2

  <dt>伴船<rt>ともぶね</rt></dt>
  <dd><li>consort ship</li><li>joint boarding, boarding a ship together</li><cite>EDICT2</cite></dd>

  間 [あいだ、ま、あい、カン、ケン] interval, space
  間 (あいだ、ま、あい、カン、ケン interval, space)
```
This makes it easier to do routine work which involves copy-pasting dictionary entries, e.g. pasting results to a friend as text, to a blog as HTML or hand-picking examples for your Anki deck.

To copy an entry in any format, right-click it and choose "`Copy as->[Format name]`". To choose which format is used by default when you press Ctrl-C, do "`Copy as->Configure...`" and select the format from the list.

There's a bunch of predefined formats shipped with Anki, but you can add your own ones, tailored for your needs.


# Custom formats
Formats are defined as .xslt files in Wakan user data folder (see [Where does Wakan keep my data?](Portable.md)). Formats for copying character data are in `KanjiCopyFormats\` subfolder and those shown for a word or expression are in `ExprCopyFormats\`.

Each .xslt file defines a [XSLT Transformation](http://en.wikipedia.org/wiki/XSLT) which is the industry standard language for reformatting XML. Wakan copies data in an XML format (see below) and automatically does one such transformation.

It is behind the scope of this help to explain how to code in XSLT, but you can find some tips below.


## Kanji XML format
Wakan keeps track of plenty of info about each kanji, which is stored as kanji properties. Properties are numbered, the full table of properties is available in `[CharInfo]` section of `wakan.cfg`.

When copying a kanji, Wakan initially lists all of it's properties. XSLT may decide to only keep those it needs:
```
<kanji>
  <char>間</char>
  <prop type="1">gan</prop>
  <prop type="1">KAN</prop>
  <prop type="2">jian1</prop>
  <prop type="2">jiān</prop>
  <prop type="2">jian4</prop>
  <prop type="3">interval</prop>
  <prop type="3">space</prop>
  <prop type="4">カン</prop>
  <prop type="4">ケン</prop>
  <prop type="5">あいだ</prop>
  <prop type="5">ま</prop>
  <prop type="5">あい</prop>
  <!-- ...etc... -->
```

XSLT Hint: Here's how to filter for only definition properties:
```
 <xsl:apply-templates select="prop[@type=3]" />
```

If there are several kanji selected for copying, Wakan wraps them in one parent `<chars>` element:
```
<chars>
  <kanji>...</kanji>
  <kanji>...</kanji>
</chars>
```


## Expression XML format
Expression format is similar to [JMDICT format](http://www.edrdg.org/jmdict/edict_doc.html), but greatly simplified and somewhat modified.

```
<entry>
  <k_ele>...</k_ele> <!-- expression -->
  <r_ele>...</r_ele> <!-- reading, optional -->
  <r_ele>...</r_ele>
  <sense>...</sense>
  <sense>...</sense>
  <dict>Dictionary name from which this entry is taken</dict>
</entry>
```
Note: JMDict has no `<dict>` tag. All entries in JMDict are from JMDict.

If there are several entries selected for copying, Wakan wraps them in one parent `<entries>` element:
```
<entries>
  <entry>...</entry>
  <entry>...</entry>
</entries>
```
Otherwise `<entry>` is the sole top-level element.

Note: JMDict groups `<entry>` items in a top-level `<JMDict>` instead.


Expression and reading are exactly as they appear in JMDict but with less info:
```
<k_ele>
  <keb>右翼</keb>
</k_ele>
<r_ele>
  <reb>うよく</reb>
</r_ele>
```
At this time Wakan never produces any other tags in `<k_ele>` and `<r_ele>`.


If the results are grouped from several dictionaries, as some modules do (e.g. dictionary lookup), they are grouped in `<article>` tags:
```
<entry>
  <k_ele>...</k_ele> <!-- expression -->
  <r_ele>...</r_ele> <!-- reading, optional -->
  <r_ele>...</r_ele>
  <article>
    <sense>...</sense>
    <sense>...</sense>
    <dict>Dictionary 1</dict>
  </article>
  <article>
    <sense>...</sense>
    <dict>Dictionary 2</dict>
  </article>
</entry>
```
For instance, if there's an entry for a given expression both in EDICT and in SAMPLEDIC, there will be two `<articles>`.

Note: JMDict always lists `<sense>`s under `<entry>`.


`<sense>` equates to JMDict's `<sense>`, but we don't support JMDict markers, instead allowing `<m>markers</m>`, `<c>comments</c>` and `<g>grammar tags</g>` both in glosses and sense-wide:
```
<sense>
  <m>adj</m>
  <m>marker</m>
  <gloss>right-wing<m>marker</m><c>comment</c></gloss>
  <gloss>right flank</gloss>
  <gloss>right wing</gloss>
</sense>
```


## What can be customized, some ideas
  1. Sense numbering:
    * (1)
    * 1.
    * 1)
    * No numbering
  1. Sense separator:
    * / (like in EDICT)
    * ;
  1. Gloss separator:
    * ;
    * ,
  1. Show/omit dictionary names.
  1. Show/omit grammar marks, comments.
  1. Format as single/multiple lines.


## Example patterns
Some patterns to use in XSLT with explanations:

```
<xsl:template match="dict">
```
Rules to apply to any occurence of `<dict>` anywhere (when processing that block).

```
<xsl:value-of select="." />
```
Used inside a template, inserts full text of currently matched block, not doing any processing inside it.

```
<xsl:apply-templates />
```
Used inside a template, inserts full text of currently matched block, processing all sub-templates in it.

```
<xsl:apply-templates select="r_ele" />
```
Used inside a template, inserts full text of currently matched block, processing all `<r_ele>` nodes in it and deleting everything else.

```
<xsl:if test="preceding-sibling::node()[name()='entry']">
<xsl:if test="following-sibling::node()[name()='entry']">
```
Tests that there were `<entry>` nodes ANYWHERE on the same level before/after this element.

```
<xsl:when test="preceding-sibling::*[1][self::entry]">
<xsl:when test="following-sibling::*[1][self::entry]">
```
Tests that then node right before/after this one is an `<entry>`.