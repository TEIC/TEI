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
<xsl:param name="host"/>

  <xsl:template match="/">
    <p class="roma">
      <form method="POST">
	<table>
	  <tr><td class="headline" colspan="7"><xsl:value-of disable-output-escaping="yes" select="$res_form_headline"/></td></tr>
	  <tr class="header">
	    <td><xsl:value-of disable-output-escaping="yes" select="$res_form_className"/></td>
	    <td width="580"><xsl:value-of disable-output-escaping="yes" select="$res_form_description"/></td>
	    <td width=""><xsl:value-of disable-output-escaping="yes" select="$res_form_attributes"/></td>
	  </tr>
	  <xsl:call-template name="generateList"/>
	</table>
      </form>
    </p>
  </xsl:template>
  
  <xsl:template name="generateList">
    <xsl:for-each select="//attClassList/attClass">
      <tr>
	<td><a><xsl:attribute name="href">http://<xsl:value-of select="$host"/>/TEI/Roma/xquery/class.xq?name=<xsl:value-of select="className"/></xsl:attribute><xsl:value-of select="className"/></a></td>
	<td width="580"><xsl:value-of select="classDesc"/></td>
	<td>
	  <a><xsl:attribute name="href">?class=<xsl:value-of
	select="className"/>&amp;mode=listAddedAttributes&amp;module=<xsl:value-of select="module"/></xsl:attribute><xsl:value-of disable-output-escaping="yes" select="$res_form_changeAttributes"/>
	  </a>
	</td>
      </tr>
    </xsl:for-each>
  </xsl:template>
</xsl:stylesheet>