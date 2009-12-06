<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns="http://www.tei-c.org/ns/1.0"
                xmlns:ve="http://schemas.openxmlformats.org/markup-compatibility/2006"
                xmlns:o="urn:schemas-microsoft-com:office:office"
                xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"
                xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math"
                xmlns:v="urn:schemas-microsoft-com:vml"
                xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing"
                xmlns:w10="urn:schemas-microsoft-com:office:word"
                xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"
                xmlns:wne="http://schemas.microsoft.com/office/word/2006/wordml"
                xmlns:mml="http://www.w3.org/1998/Math/MathML"
                xmlns:tbx="http://www.lisa.org/TBX-Specification.33.0.html"
                
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="2.0"
                exclude-result-prefixes="ve o r m v wp w10 w wne mml tbx">

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p> TEI Utility stylesheet for making Word docx files from TEI XML (see tei-docx.xsl)</p>
         <p> This library is free software; you can redistribute it and/or
      modify it under the terms of the GNU Lesser General Public License as
      published by the Free Software Foundation; either version 2.1 of the
      License, or (at your option) any later version. This library is
      distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
      without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
      PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
      details. You should have received a copy of the GNU Lesser General Public
      License along with this library; if not, write to the Free Software
      Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA </p>
         <p>Author: See AUTHORS</p>
         <p>Id: $Id: normalize-word-style.xsl 5451 2009-01-20 10:09:54Z rahtz $</p>
         <p>Copyright: 2008, TEI Consortium</p>
      </desc>
   </doc>

  <xsl:key name="STYLES" match="w:style" use="@w:styleId"/>
  <xsl:param name="word-directory">..</xsl:param>
  <xsl:param name="debug">false</xsl:param>  
  
  <xsl:template match="@*|text()|comment()|processing-instruction()" mode="part0">
      <xsl:copy-of select="."/>
  </xsl:template>
  
  
  <xsl:template match="*" mode="part0">
      <xsl:copy>
         <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()" mode="part0"/>
      </xsl:copy>
  </xsl:template>

  <xsl:template match="w:pStyle/@w:val|w:rStyle/@w:val" mode="part0">
      <xsl:variable name="old" select="."/>
      <xsl:variable name="new">
	<xsl:for-each select="document(concat($word-directory,'/word/styles.xml'),/)">
	  <xsl:value-of select="key('STYLES',$old)/w:name/@w:val"/>
	</xsl:for-each>
      </xsl:variable>
      <xsl:attribute name="w:val">
         <xsl:choose>
	   <xsl:when test="$new=''">
	     <xsl:value-of select="$old"/>
	     <xsl:if test="$debug='true'">
	       <xsl:message>! style <xsl:value-of select="$old"/> ... NOT FOUND    </xsl:message>
	     </xsl:if>
	   </xsl:when>
	   <xsl:when test="not($new=$old)">
	     <xsl:if test="$debug='true'">
	       <xsl:message>! style <xsl:value-of select="$old"/> ... CHANGED ...  <xsl:value-of select="$new"/>
	       </xsl:message>
	     </xsl:if>
	     <xsl:value-of select="$new"/>
	   </xsl:when>
	   <xsl:otherwise>
	     <xsl:value-of select="$old"/>
	   </xsl:otherwise>
         </xsl:choose>
      </xsl:attribute>
  </xsl:template>
  

</xsl:stylesheet>