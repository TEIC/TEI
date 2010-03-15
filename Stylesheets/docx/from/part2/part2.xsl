<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:cals="http://www.oasis-open.org/specs/tm9901"
                xmlns:xs="http://www.w3.org/2001/XMLSchema"
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
                exclude-result-prefixes="a cp dc dcterms dcmitype prop     iso m mml mo mv o pic r rel cals     tbx tei teidocx v xs ve w10 w wne wp">
    
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p> TEI stylesheet for converting Word docx files to TEI </p>
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
         <p>Id: $Id: docx-tei.xsl 6820 2009-10-10 20:03:31Z rahtz $</p>
         <p>Copyright: 2008, TEI Consortium</p>
      </desc>
   </doc>
    
    
    <xsl:template match="@*|comment()|processing-instruction()" mode="part2">
        <xsl:copy-of select="."/>
    </xsl:template>
    
    <xsl:template match="*" mode="part2">
        <xsl:copy>
            <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()" mode="part2"/>
        </xsl:copy>
    </xsl:template>
    
    
    <xsl:template match="text()" mode="part2">
        <xsl:value-of select="."/>
    </xsl:template>
    
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>
         <p>     Zap empty hi, p and item </p>
      </desc>
    </doc>
    <xsl:template match="tei:hi[not(*) and string-length(.)=0]" mode="part2"/>
    <xsl:template match="tei:item[not(*) and string-length(.)=0]" mode="part2"/>
    <xsl:template match="tei:p[not(*) and string-length(.)=0]" mode="part2"/>
    
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>
         <p>     Inner lists in lists must be moved to inside items
	 </p>
      </desc>
    </doc>
    <xsl:template match="tei:list/tei:list" mode="part2"/>
    <xsl:template match="tei:item" mode="part2">
        <item>
            <xsl:copy-of select="@*"/>
            <xsl:variable name="me" select="generate-id()"/>
            <xsl:apply-templates mode="part2"/>
            <!-- find following sibling lists and notes -->
            <xsl:for-each select="following-sibling::tei:list[preceding-sibling::tei:item[1][generate-id()=$me]]">
                <list>
                    <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()" mode="part2"/>
                </list>
            </xsl:for-each>
        </item>
    </xsl:template>
    
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>
         <p>     Bold emdash in title, forget it </p>
      </desc>
    </doc>
    <xsl:template match="tei:head/tei:hi[.=' ']" mode="part2"/>
    
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>
         <p>     Zap emdashes at start of head </p>
      </desc>
    </doc>
    <xsl:template match="tei:head/text()" mode="part2">
        <xsl:choose>
            <xsl:when test="starts-with(.,'— ')">
                <xsl:value-of select="substring(.,3)"/>
            </xsl:when>
            <xsl:when test="starts-with(.,' — ')">
                <xsl:value-of select="substring(.,4)"/>
            </xsl:when>
            <xsl:when test="starts-with(.,' — ')">
                <xsl:value-of select="substring(.,4)"/>
            </xsl:when>
            <xsl:otherwise>
                <xsl:value-of select="."/>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>
    
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>
         <p>     A &lt;seg&gt; which does nothing is not worth having </p>
      </desc>
    </doc>
    <xsl:template match="tei:seg[not(@*)]" mode="part2">
        <xsl:choose>
            <xsl:when test="parent::tei:formula and normalize-space(.)=''"/>
            <xsl:when test="parent::*/text()">
                <xsl:value-of select="."/>
            </xsl:when>
            <xsl:otherwise>
                <xsl:copy-of select="."/>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>
    
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>
         <p>     Look at the sections we have generated, and put
        them in &lt;front&gt; or &lt;body&gt; as appropriate</p>
      </desc>
    </doc>

    <xsl:template match="tei:text" mode="part2">
        <text>
            <xsl:for-each select="tei:fw">
                <xsl:copy-of select="."/>
            </xsl:for-each>
            <body>
                <xsl:for-each select="tei:body/tei:*">
                    <xsl:apply-templates select="." mode="part2"/>
                </xsl:for-each>
            </body>
        </text>
    </xsl:template>
    
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>
         <p>     A &lt;p&gt; inside a listBibl is moved out</p>
      </desc>
    </doc>
    <xsl:template match="tei:listBibl/tei:p" mode="part2"/>
    
    <xsl:template match="tei:listBibl" mode="part2">
        <xsl:for-each select="tei:p">
            <p>
                <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()" mode="part2"/>
            </p>
        </xsl:for-each>
        <listBibl>
            <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()" mode="part2"/>
        </listBibl>
    </xsl:template>
    
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>
         <p>     A tab (in a hi)? in a gloss list </p>
      </desc>
    </doc>
    <xsl:template match="tei:list[@type='gloss']/tei:item/tei:hi[tei:c[@rend='tab']]"
                 mode="part2"/>
    <xsl:template match="tei:list[@type='gloss']/tei:item/tei:c[@rend='tab']" mode="part2"/>
    
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>
         <p>     Top of a weird gloss list </p>
      </desc>
    </doc>
    <xsl:template match="tei:list[@type='gloss']/tei:label[.='where']" mode="part2">
        <head>
            <xsl:apply-templates/>
        </head>
    </xsl:template>
    
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>
         <p>     A tab in a &lt;bibl&gt;? no. </p>
      </desc>
    </doc>
    <xsl:template match="tei:bibl/tei:c[@rend='tab']" mode="part2"/>
    
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>
         <p>     A tab in a &lt;gloss&gt;? no. </p>
      </desc>
    </doc>
    <xsl:template match="tei:gloss//tei:c[@rend='tab']" mode="part2"/>
    
    
    <!-- removed 2010-03-15, seems to screw up formulae
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>
         <p>     A tab in a &lt;formula&gt;? no. </p>
      </desc>
    </doc>

    <xsl:template match="tei:formula//tei:c[@rend='tab']" mode="part2"/>
    -->
    
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>
         <p>     A bold line break??? </p>
      </desc>
    </doc>
    <xsl:template match="tei:hi[count(*)=1 and not(text()) and tei:lb]" mode="part2">
        <tei:lb/>
    </xsl:template>
    
    
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>
         <p>     A tab in a &lt;head&gt;? no. </p>
      </desc>
    </doc>
    <xsl:template match="tei:head/tei:c[@rend='tab']" mode="part2"/>
    

    <!-- 
     remove Table and Figure from start 
     <xsl:template match="cals:table/cals:title[starts-with(.,'Table  — ')]" mode="part2">
     <title xmlns="http://www.oasis-open.org/specs/tm9901">
     <xsl:value-of select="substring-after(.,'Table  — ')"/>
     </title>
     </xsl:template>
     
     <xsl:template match="tei:figure/tei:head[starts-with(.,'Figure  — ')]" mode="part2">
     <head>
     <xsl:value-of select="substring-after(.,'Figure  — ')"/>
     </head>
     </xsl:template>
    -->
</xsl:stylesheet>