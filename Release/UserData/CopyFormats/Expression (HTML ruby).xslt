<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:output method="text" indent="no" encoding="UTF-8"/>

<!--
<ruby>友船<rt>ともぶね</rt></ruby>
-->

<xsl:template match="entry">
  <xsl:if test="preceding-sibling::node()[name()='entry']"><xsl:text>&#xA;</xsl:text></xsl:if>
  <xsl:text>&lt;ruby&gt;</xsl:text>
  <xsl:apply-templates select="k_ele" />
  <xsl:if test="r_ele">
    <xsl:text>&lt;rt&gt;</xsl:text>
    <xsl:apply-templates select="r_ele" />
    <xsl:text>&lt;/rt&gt;</xsl:text>
  </xsl:if>
  <xsl:text>&lt;/ruby&gt;</xsl:text>
</xsl:template>

<xsl:template match="k_ele/keb">
  <xsl:if test="preceding-sibling::node()[name()='keb']"><xsl:text>、</xsl:text></xsl:if>
  <xsl:value-of select="."/>
</xsl:template>

<xsl:template match="r_ele/reb">
  <xsl:if test="preceding-sibling::node()[name()='reb']"><xsl:text>、</xsl:text></xsl:if>
  <xsl:value-of select="."/>
</xsl:template>

</xsl:stylesheet>