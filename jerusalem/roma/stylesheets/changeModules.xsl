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
<xsl:param name="lang"/>
<xsl:param name="TEISERVER">http://tei.oucs.ox.ac.uk/Query/</xsl:param>

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
      <tr><td class="headline" colspan="7"><xsl:value-of disable-output-escaping="yes" select="$res_form_headline"/> <xsl:value-of select="$module"/></td></tr>
      <tr class="header">
       <td></td>
       <td><a href="javascript:includeAllElements()"><xsl:value-of disable-output-escaping="yes" select="$res_form_include"/></a></td>
       <td><a href="javascript:excludeAllElements()"><xsl:value-of disable-output-escaping="yes" select="$res_form_exclude"/></a></td>
       <td><xsl:value-of disable-output-escaping="yes"
       select="$res_form_tagName"/></td>
       <td/>
       <td width="400"><xsl:value-of disable-output-escaping="yes" select="$res_form_description"/></td>
       <td width=""><xsl:value-of disable-output-escaping="yes" select="$res_form_attributes"/></td>
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
      <xsl:sort select="elementName"/>
      <xsl:variable name="currentElement">
	<xsl:value-of select="elementName"/>
      </xsl:variable>
      <tr>
	<xsl:if test="//errorList/error/value[node()=$currentElement]">
	  <xsl:attribute name="class">error</xsl:attribute>
	</xsl:if>
	<td>
	  <a onmouseover="javascript:document.getElementById('{$currentElement}_utils').style.display=''"
		onmouseout="javascript:document.getElementById('{$currentElement}_utils').style.display='none'">
	    <xsl:attribute name="href">?mode=changeElement&amp;element=<xsl:value-of select="$currentElement"/>&amp;module=<xsl:value-of select="$module"/></xsl:attribute>
	    <xsl:value-of select="$currentElement"/>
	  </a>
	  <div id="{$currentElement}_utils" style="padding: 5px; border: 1px solid gray; background: #DDDDFF; display: none; width: 150px; font-size: 10px"
	  onmouseover="javascript:document.getElementById('{$currentElement}_utils').style.display=''"
	  onmouseout="javascript:document.getElementById('{$currentElement}_utils').style.display='none'">
	    Element <b><xsl:value-of select="$currentElement"/></b><br/>
	    - <a href="#" onclick="javascript:window.open('tei_space/childs.php?element={$currentElement}', '{$currentElement} childs', 'directories=no,scrollbars=yes,width=400, height=300,toolbar=no,status=no,menubar=no,location=no')" style="color: black; text-decoration: none" onmouseover="this.style.color='red'" onmouseout="this.style.color='black'">Possible children in TEI space</a><br />
	    - <a href="#" onclick="javascript:window.open('tei_space/parents.php?element={$currentElement}', '{$currentElement} parents', 'directories=no,scrollbars=yes,width=400, height=300,toolbar=no,status=no,menubar=no,location=no')" style="color: black; text-decoration: none" onmouseover="this.style.color='red'" onmouseout="this.style.color='black'">Possible parents in TEI space</a><br />
	    - <a href="#" onclick="javascript:window.open('tei_space/model_classes.php?element={$currentElement}', '{$currentElement} parents', 'directories=no,scrollbars=yes,width=400, height=300,toolbar=no,status=no,menubar=no,location=no')" style="color: black; text-decoration: none" onmouseover="this.style.color='red'" onmouseout="this.style.color='black'">Model class</a><br />
	    - <a href="#" onclick="javascript:window.open('tei_space/attributes.php?element={$currentElement}', '{$currentElement} parents', 'directories=no,scrollbars=yes,width=500, height=400,toolbar=no,status=no,menubar=no,location=no')" style="color: black; text-decoration: none" onmouseover="this.style.color='red'" onmouseout="this.style.color='black'">Attributes</a>
	  </div>
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
	<td>
	 <a target="_new">
	   <xsl:attribute name="href">
	     <xsl:value-of select="$TEISERVER"/>
	     <xsl:text>tag.xq?documentationLanguage=</xsl:text>
	     <xsl:value-of select="$lang"/>
	     <xsl:text>&amp;doclang=</xsl:text>
	     <xsl:value-of select="$lang"/>
	     <xsl:text>&amp;name=</xsl:text>
	     <xsl:value-of select="elementName"/>
	   </xsl:attribute>
	   <span class="helpMe">?</span>
	 </a>
	</td>
	<td width="400"><xsl:value-of select="elementDesc"/></td>
	<td>
	  <a>
	    <xsl:attribute name="href">?element=<xsl:value-of
	    select="$currentElement"/>&amp;module=<xsl:value-of
	    select="$module"/>&amp;mode=listAddedAttributes</xsl:attribute>
	    <xsl:value-of disable-output-escaping="yes" select="$res_form_changeAttributes"/>
	  </a>
	</td>
      </tr>
    </xsl:for-each>
  </xsl:template>


</xsl:stylesheet>
