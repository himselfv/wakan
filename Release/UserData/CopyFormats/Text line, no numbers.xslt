<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:output method="text" indent="no" encoding="UTF-8"/>

<!--
# Single line with non-numbered senses
# 伴船 [ともぶね] consort ship; joint boarding, boarding a ship together ——EDICT2
# Markers and groups are ignored
# comment=(%text%)
-->

<!-- Expression -->
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


<!-- Article -->
<xsl:template match="article">
  <xsl:if test="preceding-sibling::node()[name()='article']"><xsl:text>; </xsl:text></xsl:if>
  <xsl:apply-templates />
</xsl:template>

<xsl:template match="dict">
  <xsl:text> ——</xsl:text>
  <xsl:apply-templates />
</xsl:template>

<xsl:template match="sense">
  <xsl:if test="preceding-sibling::node()[name()='sense']"><xsl:text>; </xsl:text></xsl:if>
  <xsl:apply-templates />
</xsl:template>

<xsl:template match="gloss">
  <xsl:apply-templates /><xsl:text>, </xsl:text>
</xsl:template>

<xsl:template match="gloss[position() = last()]">
  <xsl:apply-templates />
</xsl:template>


<!-- Flags -->
<xsl:template match="m|g" />
<xsl:template match="c">
  <xsl:choose>
    <xsl:when test="preceding-sibling::*[1][self::c]"><xsl:text>; </xsl:text></xsl:when>
    <xsl:when test="preceding-sibling::node()"><xsl:text> (</xsl:text></xsl:when>
    <xsl:otherwise><xsl:text>(</xsl:text></xsl:otherwise>
  </xsl:choose>
  <xsl:value-of select="."/>
  <xsl:if test="not(following-sibling::*[1][self::c])"><xsl:text>)</xsl:text></xsl:if>
</xsl:template>

</xsl:stylesheet>