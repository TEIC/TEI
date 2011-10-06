<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xs="http://www.w3.org/2001/XMLSchema"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:prop="http://schemas.openxmlformats.org/officeDocument/2006/custom-properties"
                xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main"
                xmlns:cp="http://schemas.openxmlformats.org/package/2006/metadata/core-properties"
                xmlns:dc="http://purl.org/dc/elements/1.1/"
                xmlns:dcterms="http://purl.org/dc/terms/"
                xmlns:dcmitype="http://purl.org/dc/dcmitype/"
                xmlns:iso="http://www.iso.org/ns/1.0"
                xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math"
                xmlns:mml="http://www.w3.org/1998/Math/MathML"
                xmlns:mo="http://schemas.microsoft.com/office/mac/office/2008/main"
                xmlns:mv="urn:schemas-microsoft-com:mac:vml"
                xmlns:o="urn:schemas-microsoft-com:office:office"
                xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture"
                xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"
                xmlns:rel="http://schemas.openxmlformats.org/package/2006/relationships"
                xmlns:tbx="http://www.lisa.org/TBX-Specification.33.0.html"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:teidocx="http://www.tei-c.org/ns/teidocx/1.0"
                xmlns:v="urn:schemas-microsoft-com:vml"
                xmlns:ve="http://schemas.openxmlformats.org/markup-compatibility/2006"
                xmlns:w10="urn:schemas-microsoft-com:office:word"
                xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"
                xmlns:wne="http://schemas.microsoft.com/office/word/2006/wordml"
                xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing"
                
                xmlns="http://www.tei-c.org/ns/1.0"
                version="2.0"
                exclude-result-prefixes="a cp dc dcterms dcmitype prop     iso m mml mo mv o pic r rel     tbx tei teidocx v xs ve w10 w wne wp">
    
    
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p> TEI stylesheet for converting Word docx files to TEI </p>
         <p> This library is free software; you can redistribute it and/or modify it under
            the terms of the GNU Lesser General Public License as published by the Free Software
            Foundation; either version 2.1 of the License, or (at your option) any later version.
            This library is distributed in the hope that it will be useful, but WITHOUT ANY
            WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
            PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details. You
            should have received a copy of the GNU Lesser General Public License along with this
            library; if not, write to the Free Software Foundation, Inc., 59 Temple Place, Suite
            330, Boston, MA 02111-1307 USA </p>
         <p>Author: See AUTHORS</p>
         <p>Id: $Id$</p>
         <p>Copyright: 2008, TEI Consortium</p>
      </desc>
   </doc>
    
    
    <!-- 
        This template handles lists and takes care of nested lists.
    -->
    <xsl:template name="listSection">
        <xsl:variable name="level">
            <xsl:value-of select="w:pPr/w:pStyle/@w:val"/>
        </xsl:variable>
        <list>
	  <xsl:call-template name="listType"/>
	  
	  <!-- ISO Notes are be handled by a specific handler -->
	  <xsl:for-each-group select="current-group()"
			      group-adjacent="if(w:pPr/w:pStyle/@w:val=$level)  then 0 
					      else if(w:pPr/w:pStyle/@w:val='Note') then 0                 
					      else if(w:pPr/w:pStyle/@w:val='dl') then 1                
					      else 1">
	    <xsl:choose>
	      <!-- we are still on the same level -->
	      <xsl:when test="current-grouping-key()=0">
		<xsl:for-each select="current-group()">
		  <!-- put items and notes as siblings  for now -->
		  <xsl:choose>
		    <xsl:when test="tei:is-list(.)">
		      <item>
			<xsl:apply-templates/>
		      </item>
		    </xsl:when>
		    <xsl:otherwise>
		      <xsl:apply-templates select="." mode="paragraph"/>
		    </xsl:otherwise>
		  </xsl:choose>
		</xsl:for-each>
	      </xsl:when>
	      <xsl:otherwise>
		<xsl:call-template name="listSection"/>
	      </xsl:otherwise>
	    </xsl:choose>
	    
	  </xsl:for-each-group>
        </list>
    </xsl:template>
    
    <!--
        Trying to figure out the style of a list.
    -->
    <xsl:template name="listType">
        <xsl:variable name="style">
            <xsl:value-of select="w:pPr/w:pStyle/@w:val"/>
        </xsl:variable>
        <xsl:variable name="type" select="tei:get-listtype($style)"/>
        
        <xsl:attribute name="type">
            <xsl:choose>
                <xsl:when test="string-length($type) &gt; 0">
                    <xsl:value-of select="$type"/>
                </xsl:when>
                
                <!-- try to figure it out by looking at the corresponding numbering file -->
                <xsl:otherwise>
                    
                    <!-- look up the numbering definition .. either in document.xml or in styles.xml  -->
                    <xsl:variable name="numbering-def">
                        <xsl:choose>
                            <xsl:when test="w:pPr/w:numPr/w:numId/@w:val">
                                <xsl:value-of select="w:pPr/w:numPr/w:numId/@w:val"/>
                            </xsl:when>
                            <xsl:otherwise>
                                <!-- we might want to follow the basedOn reference, but not at the moment -->
                                <xsl:value-of select="document(concat($wordDirectory,'/word/styles.xml'))//w:style[w:name/@w:val=$style]/w:pPr/w:numPr/w:numId/@w:val"/>
                            </xsl:otherwise>
                        </xsl:choose>
                    </xsl:variable>
                    
                    <!-- look up the level .. either in document.xml or in styles.xml  -->
                    <xsl:variable name="numbering-level">
                        <xsl:choose>
                            <xsl:when test="w:pPr/w:numPr/w:ilvl/@w:val">
                                <xsl:value-of select="w:pPr/w:numPr/w:ilvl/@w:val"/>
                            </xsl:when>
                            <xsl:otherwise>
                                <!-- we might want to follow the basedOn reference, but not at the moment -->
                                <xsl:value-of select="document(concat($wordDirectory,'/word/styles.xml'))//w:style[w:name/@w:val=$style]/w:pPr/w:numPr/w:ilvl/@w:val"/>
                            </xsl:otherwise>
                        </xsl:choose>
                    </xsl:variable>
                    
                    <!-- find the abstract numbering definition and then the corresponding numfmt -->
                    <xsl:variable name="abstract-def"
                             select="document(concat($wordDirectory,'/word/numbering.xml'))//w:num[@w:numId=$numbering-def]/w:abstractNumId/@w:val"/>
                    <xsl:variable name="numfmt">
                        <xsl:value-of select="document(concat($wordDirectory,'/word/numbering.xml'))//w:abstractNum[@w:abstractNumId=$abstract-def]/w:lvl[@w:ilvl=$numbering-level]/w:numFmt/@w:val"/>
                    </xsl:variable>
                    
                    
                    <!-- figure out what numbering scheme to use -->
                    <xsl:choose>
		        <xsl:when test="$style='dl'">gloss</xsl:when>
                        <xsl:when test="string-length($numfmt)=0">unordered</xsl:when>
                        <xsl:when test="$numfmt='bullet'">unordered</xsl:when>
                        <xsl:otherwise>ordered</xsl:otherwise>
                    </xsl:choose>
                </xsl:otherwise>
            </xsl:choose>
        </xsl:attribute>
    </xsl:template>
    
    
</xsl:stylesheet>
