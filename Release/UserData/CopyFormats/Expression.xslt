<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:output method="text" indent="no" encoding="UTF-8"/>

<xsl:template match="/entry">
  <xsl:apply-templates select="expr" />
  <xsl:if test="read">
    <xsl:text>[</xsl:text>
    <xsl:apply-templates select="read" />
    <xsl:text>]</xsl:text>
  </xsl>
</xsl:template>

<xsl:template match="expr">
  <xsl:if test="preceding-sibling::node()[name()='expr']"><xsl:text>、</xsl:text></xsl:if>
  <xsl:value-of select="."/>
</xsl:template>

<xsl:template match="expr">
  <xsl:if test="preceding-sibling::node()[name()='expr']"><xsl:text>、</xsl:text></xsl:if>
  <xsl:value-of select="."/>
</xsl:template>


</xsl:stylesheet>