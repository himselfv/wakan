<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:output method="text" indent="no" encoding="UTF-8"/>

<!--
# Proper HTML definition list entry
# <dt>伴船<span class=read>ともぶね</span></dt>
# <dd><ol><li>consort ship</li><li>joint boarding, boarding a ship together</li></ol><cite>EDICT2</cite></dd>
marker=<%text>
group=
comment=(%text%)
-->

<!-- Expression -->
<xsl:template match="entry">
  <xsl:if test="preceding-sibling::node()[name()='entry']"><xsl:text>&#xA;</xsl:text></xsl:if>
  <xsl:text>&lt;dt&gt;</xsl:text>
  <xsl:apply-templates select="k_ele" />
  <xsl:if test="r_ele">
    <xsl:text>&lt;span class=read&gt;</xsl:text>
    <xsl:apply-templates select="r_ele" />
    <xsl:text>&lt;/span&gt;</xsl:text>
  </xsl:if>
  <xsl:text>&lt;/dt&gt;&#13;&#10;</xsl:text>
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
  <xsl:text>&lt;dd&gt;</xsl:text>
  <xsl:apply-templates />
  <xsl:text>&lt;/dd&gt;&#13;&#10;</xsl:text>
</xsl:template>

<xsl:if test="sense">
  <xsl:text>&lt;ol&gt;</xsl:text>
  <xsl:apply-templates />
  <xsl:text>&lt;/ol&gt;</xsl:text>
</xsl:if>

<xsl:if test="dict">
  <xsl:text>&lt;cite&gt;</xsl:text>
  <xsl:apply-templates />
  <xsl:text>&lt;/cite&gt;</xsl:text>
</xsl:if>

<xsl:template match="sense">
  <xsl:text>&lt;li&gt;</xsl:text>
  <xsl:apply-templates />
  <xsl:text>&lt;/li&gt;</xsl:text>
</xsl:template>

<xsl:template match="gloss">
  <xsl:apply-templates /><xsl:text>, </xsl:text>
</xsl:template>

<xsl:template match="gloss[position() = last()]">
  <xsl:apply-templates />
</xsl:template>


<!-- Flags -->
<xsl:template match="m|g" />

<xsl:template match="c[position()=1]">
  <xsl:text> (</xsl:text><xsl:value-of select="."/><xsl:text>,</xsl:text>
</xsl:template>

<xsl:template match="c">
  <xsl:value-of select="."/><xsl:text>; </xsl:text>
</xsl:template>

<xsl:template match="c[position()=last()]">
  <xsl:value-of select="."/><xsl:text>)</xsl:text>
</xsl:template>

</xsl:stylesheet>