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
    <h1>Modules</h1>
    <table cellspacing="30">
     <tr>
      <td>
       <form>
        <table>
         <tr><td class="headline" colspan="4">List of Modules</td></tr>
         <tr class="header">
          <td></td>
          <td>Module name</td>
          <td>A short description</td>
          <td>Changes</td>
         </tr>
         <xsl:call-template name="processListModules"/>
        </table>
       </form>
      </td>
      <td class="selectedModulesBox">
       <form>
       <table>
        <tr>
         <td class="headline" colspan="2">List of selected Modules</td>
	</tr>
	<xsl:call-template name="processSelectedModules"/>
       </table>
	</form>
      </td>
      </tr>
    </table>
    <br/><br/>
   </p>
  </xsl:template>

  <xsl:template name="topLinks">
   <table class="topLinks">
    <tr>
     <td class="selected"><a href="?mode=main">Change Modules</a></td>
     <td><a href="?mode=listAddedElements">Add Elements</a></td>
     <td><a href="?mode=changeClasses">Change Classes</a></td>
     <td><a href="?mode=customizeLanguage">Customize language</a></td>
     <td><a href="?mode=createSchema">Create Schema</a></td>
     <td><a href="?mode=createDocumentation">Create Documentation</a></td>
     <td><a href="?mode=saveCustomization">Save Customization</a></td>
     <td class="newCustomization" rowspan="3"><a
     href="?mode=newCustomization">Create new Customization</a></td>
    </tr>
   </table>
  </xsl:template>


 <xsl:template name="processSelectedModules">
   <xsl:for-each select=".//selectedModules/module">
     <xsl:variable name="thisModule"><xsl:value-of select="."/></xsl:variable>
     <tr>
       <td>
	 <a>
	   <xsl:attribute name="href">?module=<xsl:value-of select="$thisModule"/>&amp;mode=removeModule</xsl:attribute>
	   remove
	 </a>
       </td>
       <td>
	 <a>
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
	 <a>
	   <xsl:attribute name="href">?module=<xsl:value-of select="moduleName"/>&amp;mode=addModule</xsl:attribute>
	   add
	 </a>
       </td>
       <td>
	 <a>
	   <xsl:attribute name="href">?mode=changeModule&amp;module=<xsl:value-of select="moduleName"/></xsl:attribute>
	   <xsl:value-of select="moduleName"/>
	 </a>
       </td>
       <td><xsl:value-of select="moduleDesc"/></td>
       <td>
	 <xsl:if test="//changes/changedModules/module[text()=$currentModule]">changed</xsl:if>
       </td>
     </tr>
   </xsl:for-each>
  </xsl:template>


</xsl:stylesheet>