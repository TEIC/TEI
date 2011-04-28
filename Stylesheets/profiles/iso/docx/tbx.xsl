<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main"
                xmlns:cals="http://www.oasis-open.org/specs/tm9901"
                xmlns:iso="http://www.iso.org/ns/1.0"
                xmlns:its="http://www.w3.org/2005/11/its"
                xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math"
                xmlns:mml="http://www.w3.org/1998/Math/MathML"
                xmlns:o="urn:schemas-pmicrosoft-com:office:office"
                xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture"
                xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"
                xmlns:tbx="http://www.lisa.org/TBX-Specification.33.0.html"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:teidocx="http://www.tei-c.org/ns/teidocx/1.0"
                xmlns:v="urn:schemas-microsoft-com:vml"
                xmlns:fn="http://www.w3.org/2005/02/xpath-functions"
                xmlns:ve="http://schemas.openxmlformats.org/markup-compatibility/2006"
                xmlns:w10="urn:schemas-microsoft-com:office:word"
                xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"
                xmlns:wne="http://schemas.microsoft.com/office/word/2006/wordml"
                xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing"
                xmlns:xs="http://www.w3.org/2001/XMLSchema"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:html="http://www.w3.org/1999/xhtml"                
                version="2.0"
                exclude-result-prefixes="teidocx cals ve o r m v wp w10 w html wne mml tbx iso tei a xs pic fn its">
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
    <desc>
      <p> TEI stylesheet for simplifying TEI ODD markup </p>
      <p> This library is free software; you can redistribute it and/or modify it under the
      terms of the GNU Lesser General Public License as published by the Free Software Foundation;
      either version 2.1 of the License, or (at your option) any later version. This library is
      distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
      implied warranty of MAINTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser
      General Public License for more details. You should have received a copy of the GNU Lesser
      General Public License along with this library; if not, write to the Free Software Foundation,
      Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA </p>
      <p>Author: See AUTHORS</p>
      <p>Id: $Id$</p>
      <p>Copyright: 2008, TEI Consortium</p>
    </desc>
  </doc>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl"  >
    <desc>TBX processing</desc>
  </doc>
  
    <xsl:template match="tbx:term" mode="get-style">
      <xsl:if test="following-sibling::tbx:termNote[@type='termType'
	      and .='abbreviation']">abbreviatedForm</xsl:if>
    </xsl:template>

    <xsl:template match="tbx:admin[@type='source']" mode="get-style">source</xsl:template>
    <xsl:template match="tbx:descrip" mode="get-style">Definition</xsl:template>

    <xsl:template match="tbx:hi[@type='entailedTerm']" mode="get-style">
      <xsl:text>termRef</xsl:text>
    </xsl:template>

   <xsl:template match="tbx:descrip[@type='definition']">
     <w:p>    
       <w:pPr>
	 <w:pStyle w:val="Definition"/>
       </w:pPr>
       <xsl:for-each
	   select="ancestor::tbx:termEntry">
	 <xsl:for-each
	     select="tbx:descripGrp/tbx:descrip[@type='subjectField']|tbx:descrip[@type='subjectField']">
	   <w:r>
	     <w:rPr>
	       <w:rStyle w:val="domain"/>
	     </w:rPr>
	     <w:t>
	       <xsl:text>〈</xsl:text>
	       <xsl:value-of select="."/>
	       <xsl:text>〉</xsl:text>
	     </w:t>
	   </w:r>
	   <w:r>
	     <w:t xml:space='preserve'> </w:t>
	   </w:r>
	 </xsl:for-each>
       </xsl:for-each>
       <xsl:apply-templates/>
       <xsl:for-each select="../../tbx:admin[@type='source']">
	 <w:r>
	   <w:rPr>
	     <w:rStyle w:val="source"/>
	   </w:rPr>
	   <w:t xml:space='preserve'>[SOURCE: </w:t>	   
	 </w:r>
	 <xsl:apply-templates select="."/>
	 <w:r>
	   <w:rPr>
	     <w:rStyle w:val="source"/>
	   </w:rPr>
	   <w:t xml:space='preserve'>]</w:t>	   
	 </w:r>
       </xsl:for-each>
     </w:p>
   </xsl:template>

  <xsl:template match="tbx:termNote">
    <xsl:choose>
      <xsl:when test="@type='grammaticalGender'">
	<w:r><w:t xml:space='preserve'>, </w:t></w:r>
	<w:r>
	  <w:rPr>
	    <w:rStyle w:val="gender"/>
	    <w:b w:val="0"/>
	  </w:rPr>
	  <w:t>
	    <xsl:choose>
	      <xsl:when test=".='masculine'">m</xsl:when>
	      <xsl:when test=".='feminine'">f</xsl:when>
	      <xsl:when test=".='neuter'">n</xsl:when>
	    </xsl:choose>
	  </w:t>
	</w:r>
      </xsl:when>
      <xsl:when test="@type='grammaticalNumber'">
	<w:r><w:t xml:space='preserve'>, </w:t></w:r>
	<w:r>
	  <w:rPr>
	    <w:rStyle w:val="number"/>
	    <w:b w:val="0"/>
	  </w:rPr>
	  <w:t>
	    <xsl:value-of select="."/>
	  </w:t>
	</w:r>
      </xsl:when>
      <xsl:when test="@type='partOfSpeech'">
	<xsl:if test="not(.='noun')">
	  <w:r>
	  <w:t xml:space='preserve'>, </w:t></w:r>
	  <w:r>
	    <w:rPr>
	      <w:rStyle w:val="partOfSpeech"/>
	      <w:b w:val="0"/>
	    </w:rPr>
	    <w:t>
	      <xsl:value-of select="."/>
	    </w:t>
	  </w:r>
	</xsl:if>
      </xsl:when>  
      <xsl:when test="@type='pronunciation'">
	<w:r>
	  <w:rPr>
	    <w:b w:val="0"/>
	  </w:rPr>
	  <w:t xml:space='preserve'>, </w:t>
	</w:r>
	<w:r>
	  <w:rPr>
          <w:rStyle w:val="pronunciation"/>
	  <w:b w:val="0"/>
	  </w:rPr>
	  <w:t>
	    <xsl:text>/ </xsl:text>
	    <xsl:value-of select="."/>
	    <xsl:text> /</xsl:text>
	  </w:t>
	</w:r>
      </xsl:when>
      <xsl:when test="@type='geographicalUsage'">
	<xsl:analyze-string select="." regex="^([^\-]+)-([^\-]+)(-x-)?([A-z]*)">
	  <xsl:matching-substring>
	    <w:r><w:t xml:space='preserve'><xsl:text> </xsl:text></w:t></w:r>
	    <w:r>
	      <w:rPr>
		<w:rStyle w:val="language"/>
		<w:b w:val="0"/>
	      </w:rPr>
	      <w:t xml:space='preserve'><xsl:value-of select="regex-group(1)"/></w:t>
	    </w:r>
	    <w:r><w:t xml:space='preserve'><xsl:text> </xsl:text></w:t></w:r>
	    <w:r>
	      <w:rPr>
		<w:rStyle w:val="geographicalUse"/>
		<w:b w:val="0"/>
	      </w:rPr>
	      <w:t xml:space='preserve'><xsl:value-of select="regex-group(2)"/></w:t>
	    </w:r>
	    <xsl:if test="not(regex-group(4)='')">
	      <w:r><w:t xml:space='preserve'><xsl:text> </xsl:text></w:t></w:r>
	      <w:r>
		<w:rPr>
		  <w:rStyle w:val="script"/>
		  <w:b w:val="0"/>
		</w:rPr>
		<w:t xml:space='preserve'><xsl:value-of select="regex-group(4)"/></w:t>
	      </w:r>
	    </xsl:if>
	  </xsl:matching-substring>
	  <xsl:non-matching-substring>
	    <w:r>
	      <w:rPr>
		<w:rStyle w:val="geographicalUse"/>
		<w:b w:val="0"/>
	      </w:rPr>
	      <w:t xml:space='preserve'><xsl:value-of select="."/></w:t>
	    </w:r>
	  </xsl:non-matching-substring>
	</xsl:analyze-string>
      </xsl:when>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="termNum">
    <xsl:choose>
      <xsl:when test="starts-with(../@id,'autoTermNum')">
	<w:p>
	  <w:pPr>
	    <w:pStyle w:val="{substring-before(../@id,'_')}"/>
	  </w:pPr>
	  <xsl:if test="../@xml:id">
	    <xsl:for-each select="..">
	      <xsl:variable name="N">
		<xsl:number level="any"/>
	      </xsl:variable>
	      <w:bookmarkStart w:id="{number($N) + 30000}" w:name="_{@xml:id}"/>
	      <w:bookmarkEnd  w:id="{number($N) + 30000}"/>
	    </xsl:for-each>
	  </xsl:if>
	</w:p>
      </xsl:when>
      <xsl:when test="starts-with(../@id,'user_')">
	<w:p>
	  <w:pPr>
	    <w:pStyle w:val="TermNum"/>
	  </w:pPr>
	  <w:r>
	    <w:t>
	      <xsl:value-of select="substring-after(../@id,'user_')"/>
	    </w:t>
	  </w:r>
	</w:p>
      </xsl:when>
      <xsl:otherwise>
	<w:p>
	  <w:pPr>
	    <w:pStyle w:val="TermNum"/>
	  </w:pPr>
	  <w:r>
	    <w:t>
	      <xsl:value-of select="../@id"/>
	    </w:t>
	  </w:r>
	</w:p>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

</xsl:stylesheet>