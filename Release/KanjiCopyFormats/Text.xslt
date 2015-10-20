<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:output method="text" indent="no" encoding="UTF-8"/>

<!--
Kanji [KUN, ON] definition
-->

<xsl:template match="kanji">
  <xsl:if test="preceding-sibling::node()[name()='kanji']"><xsl:text>&#xA;</xsl:text></xsl:if>
  <xsl:apply-templates select="char" />
  <xsl:text> [</xsl:text>
    <xsl:apply-templates select="prop[@type=5]" /><!-- kun -->
    <xsl:if test="prop[@type=5]"><xsl:text>、</xsl:text></xsl:if>
    <xsl:apply-templates select="prop[@type=4]" /><!-- on -->
  <xsl:text>]</xsl:text>
  <xsl:if test="prop[@type=3]"><xsl:text> </xsl:text></xsl:if>
  <xsl:apply-templates select="prop[@type=3]" /><!-- definition -->
</xsl:template>

<xsl:template match="prop[@type='5' or @type='4']">
  <xsl:variable name="text" select="."/>
  <xsl:value-of select="translate($text,'.','')"/>
  <xsl:if test="position() != last()"><xsl:text>、</xsl:text></xsl:if>
</xsl:template>

<xsl:template match="prop[@type='3']">
  <xsl:value-of select="."/>
  <xsl:if test="position() != last()"><xsl:text>, </xsl:text></xsl:if>
</xsl:template>

</xsl:stylesheet>
