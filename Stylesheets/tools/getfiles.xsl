<?xml version="1.0" encoding="utf-8"?>
<!--
    Copyright TEI Consortium.
    Dual-licensed under CC-by and BSD2 licences
    See the file COPYING.txt for details
    $Date$
    $Id$
-->
<xsl:stylesheet xmlns:n="www.example.com" xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:xs="http://www.w3.org/2001/XMLSchema"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="2.0"
                exclude-result-prefixes="n tei xs">

  <xsl:param name="debug">false</xsl:param>
  <xsl:param name="corpus"/>
  <xsl:param name="suffix">xml</xsl:param>
  <xsl:param name="corpusList"/>
  <xsl:param name="processP4">false</xsl:param>
  <xsl:param name="processP5">true</xsl:param>
  <xsl:param name="verbose">false</xsl:param>
  <xsl:key name="All" match="*" use="1"/>
  <xsl:key name="AllTEI" match="tei:*" use="1"/>
  <xsl:key name="E" match="*" use="local-name()"/>

  <xsl:template name="main">
      <xsl:variable name="pathlist">
         <xsl:choose>
	   <xsl:when test="$corpusList=''">
	     <xsl:value-of select="concat($corpus,'?select=*.',$suffix,';recurse=yes;on-error=warning')"/>
	   </xsl:when>
	   <xsl:otherwise>
	     <xsl:value-of select="$corpusList"/>
	   </xsl:otherwise>
         </xsl:choose>
      </xsl:variable>
      <xsl:if test="$debug='true'">
	<xsl:message>Process <xsl:value-of select="$pathlist"/></xsl:message>
      </xsl:if>
      <xsl:variable name="docs" select="collection($pathlist)"/> 
      <xsl:variable name="all">
         <n:ROOT>
	   <xsl:if test="$processP4='true'">
	     <xsl:for-each select="$docs/TEI.2">
	       <xsl:if test="$verbose='true'">
		 <xsl:message>processing <xsl:value-of select="base-uri(.)"/>
		 </xsl:message>
	       </xsl:if>
	       <TEI.2 xn="{base-uri(.)}">
		 <xsl:apply-templates select="*|@*" mode="copy"/>
	       </TEI.2>
	     </xsl:for-each>
	   </xsl:if>
	   <xsl:if test="$processP5='true'">
	     <xsl:for-each select="$docs/tei:*">
	       <xsl:if test="$verbose='true'">
		 <xsl:message>processing <xsl:value-of select="base-uri(.)"/>
		 </xsl:message>
	       </xsl:if>
	       <tei:TEI xn="{base-uri(.)}">
		 <xsl:apply-templates select="*|@*" mode="copy"/>
	       </tei:TEI>
	     </xsl:for-each>
	     <xsl:for-each select="$docs/tei:teiCorpus">
	       <xsl:if test="$verbose='true'">
		 <xsl:message>processing <xsl:value-of select="base-uri(.)"/>
		 </xsl:message>
	       </xsl:if>
	       <tei:teiCorpus xn="{base-uri(.)}">
		 <xsl:copy-of select="@*|*"/>
	       </tei:teiCorpus>
	     </xsl:for-each>
	   </xsl:if>
         </n:ROOT>
      </xsl:variable>
      <xsl:for-each select="$all/*">
	<xsl:call-template name="processAll"/>
      </xsl:for-each>
  </xsl:template>

  <xsl:template name="processAll">
      <html>
         <body>
	   <table border="1">
	     <xsl:for-each-group select="key('All',1)" group-by="local-name()">
	       <xsl:sort select="current-grouping-key()"/>
	       <tr style="vertical-align:top;">
		 <td> 
		   <xsl:value-of select="current-grouping-key()"/>
		 </td> 
		 <td> 
		   <xsl:value-of select="count(current-group())"/>
		 </td> 
	       </tr>
	     </xsl:for-each-group>
	   </table>
         </body>
      </html>
  </xsl:template>

  <xsl:template match="text()|@*" mode="copy">
      <xsl:copy-of select="."/>
  </xsl:template>

  <xsl:template match="*" mode="copy">
      <xsl:copy>
         <xsl:apply-templates select="*|@*|text()" mode="copy"/>
      </xsl:copy>
  </xsl:template>
  
</xsl:stylesheet>