<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="2.0" xmlns:xs="http://www.w3.org/2001/XMLSchema"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:prop="http://schemas.openxmlformats.org/officeDocument/2006/custom-properties"
    xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main"
    xmlns:cp="http://schemas.openxmlformats.org/package/2006/metadata/core-properties"
    xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:dcterms="http://purl.org/dc/terms/"
    xmlns:dcmitype="http://purl.org/dc/dcmitype/" xmlns:iso="http://www.iso.org/ns/1.0"
    xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math"
    xmlns:mml="http://www.w3.org/1998/Math/MathML"
    xmlns:mo="http://schemas.microsoft.com/office/mac/office/2008/main"
    xmlns:mv="urn:schemas-microsoft-com:mac:vml" xmlns:o="urn:schemas-microsoft-com:office:office"
    xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture"
    xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"
    xmlns:rel="http://schemas.openxmlformats.org/package/2006/relationships"
    xmlns:tbx="http://www.lisa.org/TBX-Specification.33.0.html"
    xmlns:tei="http://www.tei-c.org/ns/1.0" xmlns:teidocx="http://www.tei-c.org/ns/teidocx/1.0"
    xmlns:v="urn:schemas-microsoft-com:vml"
    xmlns:ve="http://schemas.openxmlformats.org/markup-compatibility/2006"
    xmlns:w10="urn:schemas-microsoft-com:office:word"
    xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"
    xmlns:wne="http://schemas.microsoft.com/office/word/2006/wordml"
    xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing"
    xmlns:xd="http://www.pnp-software.com/XSLTdoc" xmlns="http://www.tei-c.org/ns/1.0"
    exclude-result-prefixes="a cp dc dcterms dcmitype prop
    iso m mml mo mv o pic r rel
    tbx tei teidocx v xs ve w10 w wne wp xd">
    
    
    <xd:doc type="stylesheet">
        <xd:short> TEI stylesheet for converting Word docx files to TEI </xd:short>
        <xd:detail> This library is free software; you can redistribute it and/or modify it under
            the terms of the GNU Lesser General Public License as published by the Free Software
            Foundation; either version 2.1 of the License, or (at your option) any later version.
            This library is distributed in the hope that it will be useful, but WITHOUT ANY
            WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
            PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details. You
            should have received a copy of the GNU Lesser General Public License along with this
            library; if not, write to the Free Software Foundation, Inc., 59 Temple Place, Suite
            330, Boston, MA 02111-1307 USA </xd:detail>
        <xd:author>See AUTHORS</xd:author>
        <xd:cvsId>$Id: from.xsl 6832 2009-10-12 22:42:59Z rahtz $</xd:cvsId>
        <xd:copyright>2008, TEI Consortium</xd:copyright>
    </xd:doc>
    
    <xd:doc>
        <xd:short></xd:short>
    </xd:doc>
    <xsl:template match="w:r">
      <xsl:call-template name="ins-or-del"/>
    </xsl:template>

    <xsl:template name="ins-or-del">
        <xsl:variable name="style">
            <xsl:value-of select="w:rPr/w:rStyle/@w:val"/>
        </xsl:variable>
        <xsl:choose>
            <xsl:when test="$style='mentioned'">
                <mentioned>
                    <xsl:apply-templates/>
                </mentioned>
            </xsl:when>
            
            <xsl:when test="$style='ref'">
                <ref>
                    <xsl:apply-templates/>
                </ref>
            </xsl:when>
            
            <xsl:when test="$style='date'">
                <date>
                    <xsl:apply-templates/>
                </date>
            </xsl:when>
            
            <xsl:when test="$style='orgName'">
                <orgName>
                    <xsl:apply-templates/>
                </orgName>
            </xsl:when>
            
            <xsl:when test="w:rPr/w:position[number(@w:val)&lt;-2]">
                <hi>
		  <xsl:attribute name="rend">
		    <xsl:text>subscript</xsl:text>
		    <xsl:if test="w:rPr/w:i">
		      <xsl:text> italic</xsl:text>
		    </xsl:if>
		    <xsl:if test="w:rPr/w:b[not(@w:val='0')]">
		      <xsl:text> bold</xsl:text>
		    </xsl:if>
		  </xsl:attribute>
		  <xsl:apply-templates/>
                </hi>
            </xsl:when>
            
            <xsl:when test="w:rPr/w:position[number(@w:val)&gt;2]">
                <hi>
		  <xsl:attribute name="rend">
		    <xsl:text>superscript</xsl:text>
		    <xsl:if test="w:rPr/w:i">
		      <xsl:text> italic</xsl:text>
		    </xsl:if>
		    <xsl:if test="w:rPr/w:b[not(@w:val='0')]">
		      <xsl:text> bold</xsl:text>
		    </xsl:if>
		  </xsl:attribute>
		  <xsl:apply-templates/>
                </hi>
            </xsl:when>
            
            <xsl:when test="w:rPr/w:vertAlign">
                <hi>
		  <xsl:attribute name="rend">
		    <xsl:value-of select="w:rPr/w:vertAlign/@w:val"/>
		    <xsl:if test="w:rPr/w:i">
		      <xsl:text> italic</xsl:text>
		    </xsl:if>
		    <xsl:if test="w:rPr/w:b[not(@w:val='0')]">
		      <xsl:text> bold</xsl:text>
		    </xsl:if>
		  </xsl:attribute>
		  <xsl:apply-templates/>
		</hi>
            </xsl:when>
            
            <xsl:when test="w:rPr/w:i">
                <hi rend="italic">
                    <xsl:apply-templates/>
                </hi>
            </xsl:when>
            
            <xsl:when test="w:rPr/w:b[not(@w:val='0')]">
                <hi rend="bold">
                    <xsl:apply-templates/>
                </hi>
            </xsl:when>
            
            <xsl:otherwise>
                <xsl:apply-templates/>
            </xsl:otherwise>
        </xsl:choose>
        
    </xsl:template>
    
    <xd:doc>
        <xd:short>Handle Text, Comments, Tabs, Symbols etc. </xd:short>
    </xd:doc>
    <xsl:template match="w:t">
        <xsl:variable name="t">
            <xsl:choose>
                <xsl:when test="@xml:space='preserve' and string-length(normalize-space(.))=0">
                    <seg>
                        <xsl:value-of select="."/>
                    </seg>
                </xsl:when>
                <xsl:when test="@xml:space='preserve'">
                    <xsl:value-of select="."/>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:value-of select="normalize-space(.)"/>
                </xsl:otherwise>
            </xsl:choose>
        </xsl:variable>
        <xsl:choose>
            <xsl:when test="parent::w:r/w:rPr/w:rFonts[starts-with(@w:ascii,'ISO')]">
                <seg iso:font="{parent::w:r/w:rPr/w:rFonts/@w:ascii}">
                    <xsl:value-of select="$t"/>
                </seg>
            </xsl:when>
            <xsl:otherwise>
                <xsl:copy-of select="$t"/>
            </xsl:otherwise>
            
        </xsl:choose>
    </xsl:template>
    
    <xd:doc>
        <xd:short>Convert special characters (w:syms) into c tags.</xd:short>
    </xd:doc>
    <xsl:template match="w:sym">
        <c iso:font="{@w:font}" n="{@w:char}"/>
    </xsl:template>
    
    <xd:doc>
        <xd:short>handle tabs</xd:short>
    </xd:doc>
    <xsl:template match="w:r/w:tab">
      <xsl:text>&#009;</xsl:text>
    </xsl:template>
    
    <xd:doc>
        <xd:short>handle ptabs (absolute position tab character)</xd:short>
    </xd:doc>
    <xsl:template match="w:r/w:ptab">
        <c rend="ptab" type="{@w:alignment}">
            <xsl:text>&#009;</xsl:text>
        </c>
    </xsl:template>
    
    
    <xd:doc>
        <xd:short>capture line breaks</xd:short>
    </xd:doc>
    <xsl:template match="w:br">
        <xsl:choose>
            <xsl:when test="@w:type='page'">
                <pb/>
            </xsl:when>
            <xsl:otherwise>
                <lb/>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>
    
    <xd:doc>
        <xd:short>Contains text that has been tracked as a revision. </xd:short>
    </xd:doc>
    <xsl:template match="w:p/w:del">
      <del when="{@w:date}" resp="#{translate(@w:author,' ','_')}">
	<xsl:apply-templates/>
      </del>
    </xsl:template>

    <xsl:template match="w:delText">
	<xsl:apply-templates/>
    </xsl:template>

    <xsl:template match="w:p/w:ins">
      <add when="{@w:date}" resp="#{translate(@w:author,' ','_')}">
	<xsl:call-template name="ins-or-del"/>
      </add>
    </xsl:template>

 
    
</xsl:stylesheet>