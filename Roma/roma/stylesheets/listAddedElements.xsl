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
  <xsl:template match="/">
    <p class="roma">
      <form  accept-charset="utf-8"  method="POST">
        <xsl:attribute name="action">?mode=changeListAddedElements</xsl:attribute>
        <table>
          <tr>
            <td class="headline" colspan="7">
              <xsl:value-of disable-output-escaping="yes" select="$res_form_headline"/>
            </td>
          </tr>
          <tr>
            <td colspan="7" class="spacer">
              <a href="?mode=addElements">
                <xsl:value-of disable-output-escaping="yes" select="$res_form_add"/>
              </a>
            </td>
          </tr>
          <tr class="header">
            <td>
              <xsl:value-of select="$res_form_changeElement"/>
            </td>
            <td>
              <xsl:value-of disable-output-escaping="yes" select="$res_form_tagName"/>
            </td>
            <td width="400">
              <xsl:value-of disable-output-escaping="yes" select="$res_form_description"/>
            </td>
            <td width="">
              <xsl:value-of disable-output-escaping="yes" select="$res_form_attributes"/>
            </td>
            <td width="">
              <xsl:value-of disable-output-escaping="yes" select="$res_form_delete"/>
            </td>
          </tr>
          <xsl:call-template name="generateList"/>
	  <!--
	  <tr>
	    <td class="button">
	      <input class="submit" type="submit" value="Save"/>
	    </td>
          </tr>
	  -->
        </table>
      </form>
    </p>
  </xsl:template>
  <xsl:template name="generateList">
    <xsl:for-each select="//addedElements/Element">
      <tr>
        <td>
          <a>
            <xsl:attribute name="href">?mode=addElements&amp;element=<xsl:value-of select="elementName"/></xsl:attribute>
            <xsl:value-of select="elementName"/>
          </a>
        </td>
        <td>
          <xsl:value-of select="elementName"/>
        </td>
        <td width="400">
          <xsl:value-of select="desc"/>
        </td>
        <td>
          <a>
            <xsl:attribute name="href">?mode=listAddedAttributes&amp;element=<xsl:value-of select="elementName"/>&amp;added=true</xsl:attribute>
            <xsl:value-of disable-output-escaping="yes" select="$res_form_changeAttributes"/>
          </a>
        </td>
        <td>
          <a>
            <xsl:attribute name="href">?mode=deleteAddedElement&amp;element=<xsl:value-of select="elementName"/></xsl:attribute>
            <xsl:value-of disable-output-escaping="yes" select="$res_form_delete"/>
          </a>
        </td>
      </tr>
    </xsl:for-each>
  </xsl:template>
</xsl:stylesheet>
