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

  <xsl:template match="/">
    <p class="roma">
      <xsl:call-template name="topLinks"/>
      <h1>Added Elements </h1>
      <form method="POST">
	<xsl:attribute name="action">?mode=changeListAddedElements</xsl:attribute>
	<table>
	  <tr><td class="headline" colspan="7">List of added
	  elements</td></tr>
	  <tr><td colspan="7" class="spacer"><a href="?mode=addElements">Add new Element</a></td></tr>
	  <tr class="header">
	    <td>Change Element</td>
	    <td>Include</td>
	    <td>Exclude</td>
	    <td>Tag Name</td>
	    <td width="400">Description</td>
	    <td width="">Attributes</td>
	    <td width="">Delete</td>
	  </tr>
	  <xsl:call-template name="generateList"/>
	  <tr><td class="button" colspan="7"><input type="submit"/></td></tr>
	</table>
      </form>
    </p>
  </xsl:template>
  
  <xsl:template name="topLinks">
    <table class="topLinks">
      <tr>
	<td><a href="?mode=main">Change Modules</a></td>
	<td class="selected"><a href="?mode=listAddedElements">Add
	Elements</a></td>
	<td><a href="?mode=changeClasses">Change Classes</a></td>
	<td><a href="?mode=customizeLanguage">Customize language</a></td>
	<td><a href="?mode=createSchema">Create Schema</a></td>
	<td><a href="?mode=createDocumentation">Create Documentation</a></td>
	<td><a href="?mode=saveCustomization">Save Customization</a></td>
	<td class="newCustomization"><a href="?mode=newCustomization">Create new Customization</a></td>
      </tr>
    </table>
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
	  <input class="radio" type="radio" value="yes">
	    <xsl:if test="not(string(include)='delete')">
	      <xsl:attribute name="checked">1</xsl:attribute>
	    </xsl:if>
	    <xsl:attribute name="name">include_<xsl:value-of select="elementName"/></xsl:attribute>
	  </input>
	</td>
	<td>
	  <input class="radio" type="radio" value="no">
	    <xsl:if test="string(include)='delete'">
	      <xsl:attribute name="checked">1</xsl:attribute>
	    </xsl:if>
	    <xsl:attribute name="name">include_<xsl:value-of
	    select="elementName"/></xsl:attribute>
	  </input>
	</td>
	<td>
	  <xsl:value-of select="elementName"/>
	</td>
	<td width="400"><xsl:value-of select="desc"/></td>
	<td>
	  <a>
	    <xsl:attribute
	     name="href">?mode=listAddedAttributes&amp;element=<xsl:value-of select="elementName"/>&amp;added=true</xsl:attribute>
	    change Attributes
	  </a>
	</td>
	<td>
	    <a>
	      <xsl:attribute
	       name="href">?mode=deleteAddedElement&amp;element=<xsl:value-of
	       select="elementName"/></xsl:attribute>
	      delete
	    </a>
	</td>
      </tr>
    </xsl:for-each>
  </xsl:template>
</xsl:stylesheet>