<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:s="http://www.ascc.net/xml/schematron"
		xmlns:xs="http://www.w3.org/2001/XMLSchema"                
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                exclude-result-prefixes="s xs tei"
                version="2.0">
  <xsl:import href="../common/tei.xsl"/>
  <xsl:import href="tei-param.xsl"/>
  <xsl:import href="../common/verbatim.xsl"/>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p>
    TEI stylesheet for making LaTeX output.
      </p>
         <p>
    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

   
   
      </p>
         <p>Author: See AUTHORS</p>
         <p>Id: $Id$</p>
         <p>Copyright: 2011, TEI Consortium</p>
      </desc>
   </doc>
  <xsl:output method="text" encoding="utf8"/>
  <xsl:strip-space elements="*"/>
  <xsl:preserve-space elements="tei:hi tei:emph tei:foreign tei:p"/>
  <xsl:include href="core.xsl"/>
  <xsl:include href="corpus.xsl"/>
  <xsl:include href="drama.xsl"/>
  <xsl:include href="figures.xsl"/>
  <xsl:include href="header.xsl"/>
  <xsl:include href="linking.xsl"/>
  <xsl:include href="namesdates.xsl"/>
  <xsl:include href="tagdocs.xsl"/>
  <xsl:include href="textstructure.xsl"/>
  <xsl:include href="transcr.xsl"/>
  <xsl:include href="verse.xsl"/>
  <xsl:include href="textcrit.xsl"/>

  <xsl:param name="startNamespace">\color{red}</xsl:param>
  <xsl:param name="startElement">{</xsl:param>
  <xsl:param name="highlightStartElementName">\textcolor{red}{</xsl:param>
  <xsl:param name="highlightEndElementName">}</xsl:param>
  <xsl:param name="startElementName">\textbf{</xsl:param>
  <xsl:param name="startAttribute">{</xsl:param>
  <xsl:param name="startAttributeValue">{</xsl:param>
  <xsl:param name="startComment">\begin{it}</xsl:param>
  <xsl:param name="endElement">}</xsl:param>
  <xsl:param name="endElementName">}</xsl:param>
  <xsl:param name="endComment">\end{it}</xsl:param>
  <xsl:param name="endAttribute">}</xsl:param>
  <xsl:param name="endAttributeValue">}</xsl:param>
  <xsl:param name="endNamespace"/>
  <xsl:param name="spaceCharacter">\hspace*{6pt}</xsl:param>
  <xsl:variable name="docClass">
      <xsl:choose>
         <xsl:when test="/tei:TEI[@rend='letter']">
            <xsl:text>letter</xsl:text>
         </xsl:when>
         <xsl:when test="/tei:TEI[@rend='book']">
            <xsl:text>book</xsl:text>
         </xsl:when>
         <xsl:otherwise>
            <xsl:text>article</xsl:text>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:variable>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process elements  processing-instruction()[name(.)='tex']</desc>
   </doc>
  <xsl:template match="processing-instruction()[name(.)='tex']">
      <xsl:value-of select="."/>
  </xsl:template>

  <xsl:template name="verbatim-lineBreak">
      <xsl:param name="id"/>
      <xsl:text>\mbox{}\newline 
</xsl:text>
  </xsl:template>

  <xsl:template name="verbatim-createElement">
      <xsl:param name="name"/>
      <xsl:param name="special"/>
      <xsl:text>\textbf{</xsl:text>
      <xsl:value-of select="$name"/>
      <xsl:text>}</xsl:text>
  </xsl:template>

  <xsl:template name="verbatim-newLine"/>

  <xsl:template name="verbatim-createAttribute">
      <xsl:param name="name"/>
      <xsl:value-of select="$name"/>
  </xsl:template>

  <xsl:template name="verbatim-Text">
      <xsl:param name="words"/>
      <xsl:choose>
         <xsl:when test="parent::*/@xml:lang='zh-tw'">
	           <xsl:text>{\textChinese </xsl:text>
		   <xsl:value-of select="tei:escapeCharsVerbatim($words)"/>
	           <xsl:text>}</xsl:text>
         </xsl:when>
         <xsl:when test="parent::*/@xml:lang='ja'">
	           <xsl:text>{\textJapanese </xsl:text>
		   <xsl:value-of select="tei:escapeCharsVerbatim($words)"/>
	           <xsl:text>}</xsl:text>
         </xsl:when>
         <xsl:when test="parent::*/@xml:lang='kr'">
	           <xsl:text>{\textKorean </xsl:text>
		   <xsl:value-of select="tei:escapeCharsVerbatim($words)"/>
	           <xsl:text>}</xsl:text>
         </xsl:when>
         <xsl:otherwise>
	   <xsl:value-of select="tei:escapeCharsVerbatim($words)"/>
         </xsl:otherwise>
      </xsl:choose>

  </xsl:template>


  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc><p>We need the backslash and two curly braces to insert LaTeX
      commands into the output, so these characters need to replaced when they
      are found in running text. </p></desc>
  </doc>
  <xsl:function name="tei:escapeCharsVerbatim" as="xs:string">
    <xsl:param name="letters"/>
    <xsl:value-of select="translate($letters, '\{}','⃥❴❵')"/>
  </xsl:function>

  <xsl:function name="tei:escapeChars" as="xs:string">
    <xsl:param name="letters"/>
      <xsl:value-of
	  select="replace(replace(replace(replace(translate($letters,'&#10;',' '), 
		  '\\','\\textbackslash '),
		  '\{','\\{'),
		  '\}','\\}'),
		  '~','\\textasciitilde ')"/>
  </xsl:function>


</xsl:stylesheet>
