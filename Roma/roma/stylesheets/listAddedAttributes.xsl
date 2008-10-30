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
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:param name="element"/>
  <xsl:param name="module"/>
  <xsl:param name="class"/>
  <xsl:template match="/">
    <p class="roma">
      <form method="POST">
        <xsl:attribute name="action">?mode=changeListAddedAttributes</xsl:attribute>
        <input type="hidden" name="module">
          <xsl:attribute name="value">
            <xsl:value-of select="$module"/>
          </xsl:attribute>
        </input>
        <input type="hidden" name="element">
          <xsl:attribute name="value">
            <xsl:value-of select="$element"/>
          </xsl:attribute>
        </input>
        <input type="hidden" name="class">
          <xsl:attribute name="value">
            <xsl:value-of select="$class"/>
          </xsl:attribute>
        </input>
        <table>
          <tr>
            <td class="headline" colspan="6"><xsl:value-of select="$res_form_headline"/>: <xsl:value-of select="$element"/></td>
          </tr>
          <tr>
            <td colspan="6" class="spacer">
              <a>
                <xsl:attribute name="href">?mode=addAttribute&amp;module=<xsl:value-of select="$module"/>&amp;class=<xsl:value-of select="$class"/>&amp;element=<xsl:value-of select="$element"/>&amp;added=true</xsl:attribute>
                <xsl:value-of disable-output-escaping="yes" select="$res_form_add"/>
              </a>
            </td>
          </tr>
          <tr class="header">
            <td>
              <xsl:value-of disable-output-escaping="yes" select="$res_form_changeAttribute"/>
            </td>
            <td width="400">
              <xsl:value-of disable-output-escaping="yes" select="$res_form_description"/>
            </td>
            <td width="">
              <xsl:value-of disable-output-escaping="yes" select="$res_form_delete"/>
            </td>
          </tr>
          <xsl:call-template name="generateList"/>
<!--

          <tr>
            <td class="button" colspan="6">
              <input type="submit" value="Save"/>
            </td>
          </tr>
-->
        </table>
      </form>
    </p>
  </xsl:template>
  <xsl:template name="generateList">
    <xsl:for-each select="//Element/att">
      <tr>
        <td>
          <a>
            <xsl:attribute name="href">?mode=addAttribute&amp;attribute=<xsl:value-of select="name"/>&amp;class=<xsl:value-of select="$class"/>&amp;element=<xsl:value-of select="$element"/>&amp;module=<xsl:value-of select="$module"/>&amp;type=change</xsl:attribute>
            <xsl:value-of select="name"/>
          </a>
        </td>
        <td width="400">
          <xsl:value-of select="desc"/>
        </td>
        <td>
	  <a>
	    <xsl:attribute name="href">?mode=deleteAttribute&amp;element=<xsl:value-of select="$element"/>&amp;module=<xsl:value-of select="$module"/>&amp;class=<xsl:value-of select="$class"/>&amp;attribute=<xsl:value-of select="name"/></xsl:attribute>
	    <xsl:value-of disable-output-escaping="yes" select="$res_form_delete"/>
	  </a>
        </td>
      </tr>
    </xsl:for-each>
  </xsl:template>
</xsl:stylesheet>
