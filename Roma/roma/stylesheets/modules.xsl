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
    <table cellspacing="30">
     <tr>
      <td>
       <form>
        <table>
         <tr><td class="headline" colspan="5"><xsl:value-of disable-output-escaping="yes" select="$res_form_headline"/></td></tr>
         <tr class="header">
          <td></td>
          <td><xsl:value-of  disable-output-escaping="yes"
	  select="$res_form_moduleName"/></td>
	  <td/>
          <td><xsl:value-of  disable-output-escaping="yes" select="$res_form_description"/></td>
          <td><xsl:value-of  disable-output-escaping="yes" select="$res_form_changes"/></td>
         </tr>
         <xsl:call-template name="processListModules"/>
        </table>
       </form>
      </td>
      <td class="selectedModulesBox">
       <form>
       <table>
        <tr>
         <td class="headline" colspan="2"><xsl:value-of
	 disable-output-escaping="yes"
select="$res_selectedModules_headline"/></td>
	</tr>
	<xsl:call-template name="processSelectedModules"/>
       </table>
	</form>
      </td>
      </tr>
    </table>
   </p>
  </xsl:template>


 <xsl:template name="processSelectedModules">
   <xsl:for-each select=".//selectedModules/module">
     <xsl:variable name="thisModule"><xsl:value-of select="."/></xsl:variable>
     <tr>
       <td>
	 <a class="action">
	   <xsl:attribute name="href">?module=<xsl:value-of select="$thisModule"/>&amp;mode=removeModule</xsl:attribute>
	   <xsl:value-of  disable-output-escaping="yes" select="$res_form_remove"/>
	 </a>
       </td>
       <td>
	 <a class="display">
	   <xsl:attribute name="href">?mode=changeModule&amp;module=<xsl:value-of select="$thisModule"/></xsl:attribute>
	   <xsl:value-of select="$thisModule"/>
	 </a>
       </td>
     </tr>
   </xsl:for-each>
 </xsl:template>

 <xsl:template name="processListModules">
   <xsl:for-each select=".//teiModulesList/teiModule">
     <xsl:variable name="currentModule"><xsl:value-of
     select="moduleName"/></xsl:variable>
     <tr>
       <xsl:if
	test="//changes/changedModules/module[text()=$currentModule]
	and not(//changes/selectedModules/module[text()=$currentModule])">
	 <xsl:attribute name="class">notAdded</xsl:attribute>
       </xsl:if>
       <td>
	 <a class="action">
	   <xsl:attribute name="href">?module=<xsl:value-of select="moduleName"/>&amp;mode=addModule</xsl:attribute>
	   <xsl:value-of  disable-output-escaping="yes" select="$res_form_add"/>
	 </a>
       </td>
       <td>
	 <a class="display">
	   <xsl:attribute name="href">?mode=changeModule&amp;module=<xsl:value-of select="moduleName"/></xsl:attribute>
	   <xsl:value-of select="moduleName"/>
	 </a>
       </td>
       <td>
	 <a target="_new">
	   <xsl:attribute name="href">
	     <xsl:text>http://www.tei-c.org/P5/Guidelines/</xsl:text>
	     <xsl:value-of select="moduleChapter"/>
	     <xsl:text>.html</xsl:text>
	   </xsl:attribute>
	   <span class="helpMe">?</span>
	 </a>
       </td>
       <td><xsl:value-of select="moduleDesc"/></td>
       <td>
	 <xsl:if test="//changes/changedModules/module[text()=$currentModule]"><xsl:value-of select="$res_form_changed"/></xsl:if>
       </td>
     </tr>
   </xsl:for-each>
  </xsl:template>


</xsl:stylesheet>