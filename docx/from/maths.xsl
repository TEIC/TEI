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
         <p>This software is dual-licensed:

1. Distributed under a Creative Commons Attribution-ShareAlike 3.0
Unported License http://creativecommons.org/licenses/by-sa/3.0/ 

2. http://www.opensource.org/licenses/BSD-2-Clause
		


Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

* Redistributions of source code must retain the above copyright
notice, this list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright
notice, this list of conditions and the following disclaimer in the
documentation and/or other materials provided with the distribution.

This software is provided by the copyright holders and contributors
"as is" and any express or implied warranties, including, but not
limited to, the implied warranties of merchantability and fitness for
a particular purpose are disclaimed. In no event shall the copyright
holder or contributors be liable for any direct, indirect, incidental,
special, exemplary, or consequential damages (including, but not
limited to, procurement of substitute goods or services; loss of use,
data, or profits; or business interruption) however caused and on any
theory of liability, whether in contract, strict liability, or tort
(including negligence or otherwise) arising in any way out of the use
of this software, even if advised of the possibility of such damage.
</p>
         <p>Author: See AUTHORS</p>
         
         <p>Copyright: 2013, TEI Consortium</p>
      </desc>
   </doc>
    
    
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Handles a single paragraph that contains a math object</desc>
   </doc>
    <xsl:template match="w:p[w:pPr/w:pStyle/@w:val='Formula']" mode="paragraph">
        <xsl:call-template name="paragraph-formula"/>
    </xsl:template>
    
    
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>
        
            //TODO: figure out why there is a priority=-1
        
    </desc>
   </doc>
    <xsl:template match="w:p[m:oMathPara]" mode="paragraph" priority="-1">
        <p>
            <formula>
                <xsl:apply-templates select="m:oMathPara/m:oMath"/>
            </formula>
        </p>
    </xsl:template>
    
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>
        
            //TODO: figure out why there is a priority=-1
        
    </desc>
   </doc>
    <xsl:template match="w:p[m:oMath]" mode="paragraph" priority="-1">
        <p>
            <formula>
                <xsl:apply-templates select="m:oMath"/>
            </formula>
        </p>
    </xsl:template>
    
    
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>named template for w:p[w:pPr/w:pStyle/@w:val='Formula'</desc>
   </doc>
    <xsl:template name="paragraph-formula">
        <p>
            <formula>
                <xsl:if test="w:r/w:rPr/w:rStyle/@w:val='FormulaReference'">
                    <xsl:attribute name="n">
                        <xsl:value-of select="w:r[w:rPr/w:rStyle/@w:val='FormulaReference']/w:t"/>
                    </xsl:attribute>
                </xsl:if>
                <xsl:apply-templates/>
            </formula>
        </p>
    </xsl:template>

    
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Depending on the math mode, omml is either converted to mathML or an identity transformation is started</desc>
   </doc>
    <xsl:template match="m:oMath">
        <xsl:choose>
            <xsl:when test="$mathMethod='omml'">
                <xsl:apply-templates select="." mode="iden"/>
            </xsl:when>
            <xsl:otherwise>
                <mml:math>
                    <xsl:apply-templates/>
                </mml:math>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    
    
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>
         <p>Handles a w:object, possibly by just copying it else
	 giving error</p>
      </desc>
   </doc>
    <xsl:template match="w:object">
      <xsl:choose>
	<xsl:when test="preserveObject='true'">
	  <xsl:copy>
	    <xsl:apply-templates mode="iden"/>
	  </xsl:copy>
	  </xsl:when>
          <xsl:otherwise>
	    <xsl:sequence select="tei:docxError('unable to handle Word
				  object, possibly  embedded
				  spreadsheet or equation')"/>
	  </xsl:otherwise>
      </xsl:choose>
    </xsl:template>
    
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Guides the identity transformation of imagedata in math</desc>
   </doc>
    <xsl:template match="v:imagedata" mode="iden">
            <xsl:variable name="rid" select="@r:id"/>
            <xsl:variable name="file">
                <xsl:value-of
		    select="document(concat($wordDirectory,'/word/_rels/document.xml.rels'))//rel:Relationship[@Id=$rid]/@Target"/>
	    </xsl:variable>
	    <v:imagedata r:id="{$file}"/>
    </xsl:template>
    
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Guides the identity transformation of ole in maths</desc>
   </doc>
    <xsl:template match="o:OLEObject" mode="iden">
        <o:OLEObject>
            <xsl:copy-of select="@*"/>
            <xsl:variable name="rid" select="@r:id"/>
            <xsl:variable name="file">
                <xsl:value-of
		    select="document(concat($wordDirectory,'/word/_rels/document.xml.rels'))//rel:Relationship[@Id=$rid]/@Target"/>
	    </xsl:variable>
            <xsl:attribute name="r:id">
                <xsl:value-of select="$file"/>
            </xsl:attribute>
        </o:OLEObject>
    </xsl:template>
    

    
</xsl:stylesheet>
