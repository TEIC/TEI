<?xml version="1.0"?>
<!--
#######################################
Roma Stylesheet

#######################################
author: Arno Mittelbach <arno-oss@mittelbach-online.de>
version: 0.9
date: 10.06.2004

#######################################
Description

-->
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:param name="module"/>
<xsl:param name="class"/>

  <xsl:template match="/">
    <p class="roma">
      <form method="POST">
	<table>
	  <tr><td class="headline" colspan="7">List of attribute
	  classes</td></tr>
	  <tr class="header">
	    <td>Class name</td>
	    <td width="580">Description</td>
	    <td width="">Attributes</td>
	  </tr>
	  <xsl:call-template name="generateList"/>
	</table>
      </form>
    </p>
  </xsl:template>
  
  <xsl:template name="generateList">
    <xsl:for-each select="//attClassList/attClass">
      <tr>
	<td><xsl:value-of select="className"/></td>
	<td width="580"><xsl:value-of select="classDesc"/></td>
	<td>
	  <a><xsl:attribute name="href">?class=<xsl:value-of
	select="className"/>&amp;mode=listAddedAttributes&amp;module=<xsl:value-of select="module"/></xsl:attribute>change Attributes
	  </a>
	</td>
      </tr>
    </xsl:for-each>
  </xsl:template>
</xsl:stylesheet>