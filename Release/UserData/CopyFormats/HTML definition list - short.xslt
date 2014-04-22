<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:output method="text" indent="no" encoding="UTF-8"/>

<!--
# Breaks some of HTML rules - uses <rt> outside of <ruby> and <li> outside of list,
# but it's shorter and it works anyway.
# <dt>伴船<rt>ともぶね</rt></dt>
# <dd><li>consort ship</li><li>joint boarding, boarding a ship together</li><cite>EDICT2</cite></dd>
marker=<%text>
group=
comment=(%text%)
-->

<xsl:template match="entry">
  <xsl:if test="preceding-sibling::node()[name()='entry']"><xsl:text>&#xA;</xsl:text></xsl:if>
  <xsl:text>&lt;dt&gt;</xsl:text>
  <xsl:apply-templates select="k_ele" />
  <xsl:if test="r_ele">
    <xsl:text>&lt;rt&gt;</xsl:text>
    <xsl:apply-templates select="r_ele" />
    <xsl:text>&lt;/rt&gt;</xsl:text>
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

<!-- article=%clauses%{%dict%? ——%dict%}{%last%!?; } -->
<xsl:template match="article">
  <xsl:text>&lt;dd&gt;</xsl:text>
  <xsl:apply-templates select="sense" />
  <xsl:if test="dict">
    <xsl:text>&lt;cite&gt;</xsl:text>
    <xsl:value-of select="dict"/>
    <xsl:text>&lt;/cite&gt;</xsl:text>
  </xsl:if>
  <xsl:text>&lt;/dd&gt;&#13;&#10;</xsl:text>
</xsl:template>

<xsl:template match="sense">
  <xsl:text>&lt;li&gt;</xsl:text>
  <xsl:apply-templates select="gloss" />
  <xsl:text>&lt;/li&gt;</xsl:text>
</xsl:template>

<xsl:template match="gloss">
  <xsl:value-of select="."/><xsl:text>, </xsl:text>
</xsl:template>

<xsl:template match="gloss[position() = last()]">
  <xsl:value-of select="."/>
</xsl:template>

</xsl:stylesheet>