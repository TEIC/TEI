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
<xsl:param name="element"/>
<xsl:param name="module"/>
<xsl:param name="class"/>
<xsl:param name="MESSAGE"/>
<xsl:param name="ERRORS"/>


  <xsl:template match="/">
    <p class="roma">
      <xsl:call-template name="topLinks"/>
      <h1>Added Attributes </h1>
      <xsl:if test="not($MESSAGE='')">
	<p class="success">
	  <xsl:value-of select="$MESSAGE"/>
	</p>
      </xsl:if>
      <xsl:if test="not($ERRORS='')">
	<p class="error"><xsl:value-of
	select="$ERRORS"/></p>
      </xsl:if>
      <form method="POST">
	<xsl:attribute
	name="action">?mode=changeListAddedAttributes</xsl:attribute>
	<input type="hidden" name="module">
	  <xsl:attribute name="value"><xsl:value-of select="$module"/></xsl:attribute>
	</input>
	<input type="hidden" name="element">
	  <xsl:attribute name="value"><xsl:value-of select="$element"/></xsl:attribute>
	</input>
	<input type="hidden" name="class">
	  <xsl:attribute name="value"><xsl:value-of select="$class"/></xsl:attribute>
	</input>
	<table>
	  <tr><td class="headline" colspan="6">List of attributes</td></tr>
	  <tr>
	    <td colspan="6" class="spacer">
	      <a>
	      <xsl:attribute
name="href">?mode=addAttribute&amp;module=<xsl:value-of
		select="$module"/>&amp;class=<xsl:value-of select="$class"/>&amp;element=<xsl:value-of select="$element"/>&amp;added=true</xsl:attribute>
	      Add new Attribute</a>
	    </td>
	  </tr>
	  <tr class="header">
	    <td>Change Attribute</td>
	    <td>Include</td>
	    <td>Exclude</td>
	    <td>Tag Name</td>
	    <td width="400">Description</td>
	    <td width="">Delete</td>
	  </tr>
	  <xsl:call-template name="generateList"/>
	  <tr><td class="button" colspan="6"><input type="submit"/></td></tr>
	</table>
      </form>
    </p>
  </xsl:template>
  
  <xsl:template name="topLinks">
    <table class="topLinks">
      <tr>
	<td>
	  <xsl:if test="not($module='') and $class=''">
	    <xsl:attribute name="class">selected</xsl:attribute>
	  </xsl:if>
	  <a href="?mode=main">Change Modules</a>
	</td>
	<td>
	  <xsl:if test="$module='' and $class=''">
	    <xsl:attribute name="class">selected</xsl:attribute>
	  </xsl:if>
	  <a href="?mode=listAddedElements">Add Elements</a>
	</td>
	<td>
	  <xsl:if test="not($class='')">
	    <xsl:attribute name="class">selected</xsl:attribute>
	  </xsl:if>
	  <a href="?mode=changeClasses">Change Classes</a>
	</td>
	<td><a href="?mode=customizeLanguage">Customize language</a></td>
	<td><a href="?mode=createSchema">Create Schema</a></td>
	<td><a href="?mode=createDocumentation">Create Documentation</a></td>
	<td><a href="?mode=saveCustomization">Save Customization</a></td>
	<td class="newCustomization"><a href="?mode=newCustomization">Create new Customization</a></td>
      </tr>
    </table>
  </xsl:template>

  <xsl:template name="generateList">
    <xsl:for-each select="//Element/att">
      <tr>
	<td>
	  <a>
	    <xsl:attribute
	    name="href">?mode=addAttribute&amp;attribute=<xsl:value-of
select="name"/>&amp;class=<xsl:value-of select="$class"/>&amp;element=<xsl:value-of select="$element"/>&amp;module=<xsl:value-of select="$module"/>&amp;type=change</xsl:attribute>
	    <xsl:value-of select="name"/>
	  </a>
	</td>
	<td>
	  <input class="radio" type="radio" value="yes">
	    <xsl:if test="not(string(include)='delete')">
	      <xsl:attribute name="checked">1</xsl:attribute>
	    </xsl:if>
	    <xsl:attribute name="name">include_<xsl:value-of select="name"/></xsl:attribute>
	  </input>
	</td>
	<td>
	  <input class="radio" type="radio" value="no">
	    <xsl:if test="string(include)='delete'">
	      <xsl:attribute name="checked">1</xsl:attribute>
	    </xsl:if>
	    <xsl:attribute name="name">include_<xsl:value-of
	    select="name"/></xsl:attribute>
	  </input>
	</td>
	<td>
	  <xsl:if test="@added='true'">
	    <xsl:value-of select="name"/>
	  </xsl:if>
	  <xsl:if test="not(@added='true')">
	    <input type="text">
	      <xsl:attribute name="name">name_<xsl:value-of select="name"/></xsl:attribute>
	      <xsl:if test="not(string(altName)='')">
		<xsl:attribute name="value"><xsl:value-of
		select="altName"/></xsl:attribute>
	      </xsl:if>
	      <xsl:if test="string(altName)=''">
		<xsl:attribute name="value"><xsl:value-of
		select="name"/></xsl:attribute>
	      </xsl:if>
	    </input>
	  </xsl:if>
	</td>
	<td width="400"><xsl:value-of select="desc"/></td>
	<td>
	  <xsl:if test="@added='true'">
	    <a>
	      <xsl:attribute
	       name="href">?mode=deleteAttribute&amp;element=<xsl:value-of
	       select="$element"/>&amp;module=<xsl:value-of
	      select="$module"/>&amp;class=<xsl:value-of
select="$class"/>&amp;attribute=<xsl:value-of select="name"/></xsl:attribute>
	      delete
	    </a>
	  </xsl:if>
	</td>
      </tr>
    </xsl:for-each>
  </xsl:template>
</xsl:stylesheet>