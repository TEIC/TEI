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
<xsl:param name="excludedElements"/>
<xsl:param name="changedElementNames"/>
<xsl:param name="module"/>


  <xsl:template match="/">
  <p class="roma">
    <a href="?mode=main">
     back
    </a><br/>

    <form method="POST">
     <xsl:attribute name="action">
      ?mode=moduleChanged&amp;module=<xsl:value-of select="$module"/>
     </xsl:attribute>
     <table>
      <tr><td class="headline" colspan="6">List of elements in module: <xsl:value-of select="$module"/></td></tr>
      <tr class="header">
       <td></td>
       <td>Include</td>
       <td>Exclude</td>
       <td>Tag Name</td>
       <td width="400">Description</td>
       <td width="">Attributes</td>
      </tr>
      <xsl:call-template name="listElements"/>
      <tr><td class="button" colspan="6"><input type="submit"/></td></tr>
     </table>
    </form>
    <br/><br/>
    </p>
  </xsl:template>


  <xsl:template name="listElements">
    <xsl:for-each select="//list/elementList/teiElement">
      <xsl:variable name="currentElement"><xsl:value-of
      select="elementName"/></xsl:variable>
      <tr>
	<xsl:if test="//errorList/error/value[node()=$currentElement]">
	  <xsl:attribute name="class">error</xsl:attribute>
	</xsl:if>
	<td>
	  <a>
	    <xsl:attribute name="href">?mode=changeElement&amp;element=<xsl:value-of select="$currentElement"/>&amp;module=<xsl:value-of select="$module"/></xsl:attribute>
	    <xsl:value-of select="$currentElement"/>
	  </a>
	</td> 
	<td>
	  <input class="radio" type="radio" value="include">
	    <xsl:attribute name="name">element_<xsl:value-of select="$currentElement"/></xsl:attribute>
	    <xsl:if test="not(contains( $excludedElements, $currentElement ))">
	      <xsl:attribute name="checked">1</xsl:attribute>
	    </xsl:if>
	  </input>
	</td>
	<td>
	  <input class="radio" type="radio" value="exclude">
	    <xsl:attribute name="name">element_<xsl:value-of select="$currentElement"/></xsl:attribute>
	    <xsl:if test="//changes/excludedElements/element[node()=$currentElement]">
	      <xsl:attribute name="checked">1</xsl:attribute>
	    </xsl:if>
	  </input>
	</td>
	<td>
	  <input type="text" size="30">
	    <xsl:attribute name="name">elementName_<xsl:value-of select="$currentElement"/></xsl:attribute>
	    <xsl:if test="//changes/changedNames/element[@ident=$currentElement]">
	      <xsl:variable name="newName">
		<xsl:value-of select="//changes/changedNames/element[@ident=$currentElement]/altIdent"/>
	      </xsl:variable>
	      <xsl:attribute name="value"><xsl:value-of select="$newName"/></xsl:attribute>
	    </xsl:if>
	    <xsl:if test="not(//changes/changedNames/element[@ident=$currentElement])">
	      <xsl:attribute name="value"><xsl:value-of select="$currentElement"/></xsl:attribute>
	    </xsl:if>
	  </input>
	</td>
	<td width="400"><xsl:value-of select="elementDesc"/></td>
	<td>
	  <a>
	    <xsl:attribute name="href">?element=<xsl:value-of
	    select="$currentElement"/>&amp;module=<xsl:value-of
	    select="$module"/>&amp;mode=listAddedAttributes</xsl:attribute>change
	    Attributes
	  </a>
	</td>
      </tr>
    </xsl:for-each>
  </xsl:template>


</xsl:stylesheet>