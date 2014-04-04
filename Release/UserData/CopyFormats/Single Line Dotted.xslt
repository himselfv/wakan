<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:output method="text" indent="no" encoding="UTF-8"/>

<xsl:template match="/entry">
  <xsl:apply-templates select="sense" />
</xsl:template>

<xsl:template match="sense">
  <xsl:if test="count(../sense) &gt; 1">
    <xsl:if test="preceding-sibling::node()[name()='sense']"><xsl:text>; </xsl:text></xsl:if>
    <xsl:value-of select="position()"/><xsl:text>. </xsl:text>
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