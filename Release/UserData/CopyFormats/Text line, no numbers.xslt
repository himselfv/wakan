<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:output method="text" indent="no" encoding="UTF-8"/>

<!--
# Single line with non-numbered senses
# 伴船 [ともぶね] consort ship; joint boarding, boarding a ship together ——EDICT2
marker=<%text>
group=
comment=(%text%)
-->

<xsl:template match="entry">
  <xsl:if test="preceding-sibling::node()[name()='entry']"><xsl:text>&#xA;</xsl:text></xsl:if>
  <xsl:apply-templates select="k_ele" />
  <xsl:if test="r_ele">
    <xsl:text> [</xsl:text>
    <xsl:apply-templates select="r_ele" />
    <xsl:text>]</xsl:text>
  </xsl:if>
  <xsl:text> </xsl:text>
  <xsl:apply-templates select="article" />
</xsl:template>

<xsl:template match="k_ele/keb">
  <xsl:if test="preceding-sibling::node()[name()='keb']"><xsl:text>、</xsl:text></xsl:if>
  <xsl:value-of select="."/>
</xsl:template>

<xsl:template match="r_ele/reb">
  <xsl:if test="preceding-sibling::node()[name()='reb']"><xsl:text>、</xsl:text></xsl:if>
  <xsl:value-of select="."/>
</xsl:template>

<!-- article=%clauses%{%dict%? ——%dict%}{%last%!?; } -->
<xsl:template match="article">
  <xsl:if test="preceding-sibling::node()[name()='article']"><xsl:text>; </xsl:text></xsl:if>
  <xsl:apply-templates select="sense" />
  <xsl:if test="dict">
    <xsl:text> ——</xsl:text>
    <xsl:value-of select="dict"/>
  </xsl:if>
</xsl:template>

<xsl:template match="sense">
  <xsl:if test="count(../sense) &gt; 1">
    <xsl:if test="preceding-sibling::node()[name()='sense']"><xsl:text>; </xsl:text></xsl:if>
  </xsl:if>
  <xsl:apply-templates select="gloss" />
</xsl:template>

<xsl:template match="gloss">
  <xsl:value-of select="."/><xsl:text>, </xsl:text>
</xsl:template>

<xsl:template match="gloss[position() = last()]">
  <xsl:value-of select="."/>
</xsl:template>

</xsl:stylesheet>