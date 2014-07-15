<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:output method="html" indent="no" encoding="UTF-8"/>

<!--
<div class="kanji">
<span class="char"></span>
<span class="prop-5">prop</span>
...
</div>
-->

<xsl:template match="chars">
  <xsl:apply-templates />
</xsl:template>

<xsl:template match="kanji">
  <div class="kanji">
  <xsl:apply-templates />
  </div>
</xsl:template>

<xsl:template match="char">
  <span class="char">
  <xsl:apply-templates />
  </span>
</xsl:template>

<xsl:template match="prop">
  <span>
  <xsl:attribute name="class">prop-<xsl:value-of select="@type" /></xsl:attribute>
  <xsl:apply-templates />
  </span>
</xsl:template>

</xsl:stylesheet>
