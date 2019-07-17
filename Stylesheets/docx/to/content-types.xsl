<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main"
                xmlns:cals="http://www.oasis-open.org/specs/tm9901"
                xmlns:contypes="http://schemas.openxmlformats.org/package/2006/content-types"
                xmlns:cp="http://schemas.openxmlformats.org/package/2006/metadata/core-properties"
                xmlns:dc="http://purl.org/dc/elements/1.1/"
                xmlns:dcmitype="http://purl.org/dc/dcmitype/"
                xmlns:dcterms="http://purl.org/dc/terms/"
                xmlns:html="http://www.w3.org/1999/xhtml"
                xmlns:iso="http://www.iso.org/ns/1.0"
                xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math"
                xmlns:mml="http://www.w3.org/1998/Math/MathML"
                xmlns:o="urn:schemas-microsoft-com:office:office"
                xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture"
                xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"
                xmlns:tbx="http://www.lisa.org/TBX-Specification.33.0.html"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:teidocx="http://www.tei-c.org/ns/teidocx/1.0"
                xmlns:teix="http://www.tei-c.org/ns/Examples"
                xmlns:v="urn:schemas-microsoft-com:vml"
                xmlns:fn="http://www.w3.org/2005/02/xpath-functions"
                xmlns:ve="http://schemas.openxmlformats.org/markup-compatibility/2006"
                xmlns:w10="urn:schemas-microsoft-com:office:word"
                xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"
                xmlns:wne="http://schemas.microsoft.com/office/word/2006/wordml"
                xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing"
                
                xmlns:xs="http://www.w3.org/2001/XMLSchema"
                xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="2.0"
                exclude-result-prefixes="#all">
    
    
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p> TEI stylesheet for making Word docx files from TEI XML </p>
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
    
    <xsl:template name="write-docxfile-content-types">
	     <xsl:if test="$debug='true'">
	        <xsl:message>Writing out <xsl:value-of select="concat($wordDirectory,'/%5BContent_Types%5D.xml')"/>
         </xsl:message>
	     </xsl:if>

        <xsl:result-document href="{concat($wordDirectory,'/%5BContent_Types%5D.xml')}">
            
            <Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">
                
                <Default Extension="jpeg" ContentType="image/jpeg"/>
                <Default Extension="jpg" ContentType="image/jpeg"/>
                <Default Extension="png" ContentType="image/png"/>
                <Default Extension="tiff" ContentType="image/tiff"/>
                <Default Extension="rels"
                     ContentType="application/vnd.openxmlformats-package.relationships+xml"/>
                <Default Extension="xml" ContentType="application/xml"/>
                <Default Extension="bin"
                     ContentType="application/vnd.openxmlformats-officedocument.oleObject"/>
                <Default Extension="wmf" ContentType="image/x-wmf"/>
                <Default Extension="emf" ContentType="image/x-emf"/>
                
                
                <!-- docprops -->
                <Override PartName="/docProps/core.xml"
                      ContentType="application/vnd.openxmlformats-package.core-properties+xml"/>
                <Override PartName="/docProps/app.xml"
                      ContentType="application/vnd.openxmlformats-officedocument.extended-properties+xml"/>
                <Override PartName="/docProps/custom.xml"
                      ContentType="application/vnd.openxmlformats-officedocument.custom-properties+xml"/>
                
                <!-- word -->
                <Override PartName="/word/document.xml"
                      ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.document.main+xml"/>
                <Override PartName="/word/styles.xml"
                      ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.styles+xml"/>
                <Override PartName="/word/numbering.xml"
                      ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.numbering+xml"/>
                <Override PartName="/word/webSettings.xml"
                      ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.webSettings+xml"/>
                
                <Override PartName="/word/endnotes.xml"
                      ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.endnotes+xml"/>
                <Override PartName="/word/fontTable.xml"
                      ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.fontTable+xml"/>
                <Override PartName="/word/footnotes.xml"
                      ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.footnotes+xml"/>
                <Override PartName="/word/settings.xml"
                      ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.settings+xml"/>
		<Override PartName="/word/comments.xml"
                      ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.comments+xml"/>

                <!-- others -->
                <Override PartName="/word/theme/theme1.xml"
                      ContentType="application/vnd.openxmlformats-officedocument.theme+xml"/>
                
                <!-- headers -->
                <xsl:choose>
                    <xsl:when test="count(key('ALLHEADERS',1))=0 and doc-available($defaultHeaderFooterFile)">
		      <xsl:for-each select="doc($defaultHeaderFooterFile)">
			<xsl:call-template name="write-docxfile-content-types-header-references"/>
		      </xsl:for-each>
                    </xsl:when>
                    <xsl:otherwise>
		      <xsl:call-template name="write-docxfile-content-types-header-references"/>
                    </xsl:otherwise>
                </xsl:choose>
                
                
                <!-- footers -->
                <xsl:choose>
                    <xsl:when test="count(key('ALLFOOTERS',1))=0  and doc-available($defaultHeaderFooterFile)">
		      <xsl:for-each select="doc($defaultHeaderFooterFile)">
			<xsl:call-template name="write-docxfile-content-types-footer-references"/>
		      </xsl:for-each>
                    </xsl:when>
                    <xsl:otherwise>
		      <xsl:call-template name="write-docxfile-content-types-footer-references"/>
                    </xsl:otherwise>
                </xsl:choose>
            </Types>
            
        </xsl:result-document>
    </xsl:template>
    
    
    
    <xsl:template
	name="write-docxfile-content-types-footer-references">
        <xsl:for-each select="key('ALLFOOTERS',1)">
            <Override xmlns="http://schemas.openxmlformats.org/package/2006/content-types"
                   ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.footer+xml">
                <xsl:attribute name="PartName" select="concat('/word/footer', position(), '.xml')"/>
            </Override>            
        </xsl:for-each>
    </xsl:template>
    
    <xsl:template name="write-docxfile-content-types-header-references">
        <xsl:for-each select="key('ALLHEADERS',1)">
            <Override xmlns="http://schemas.openxmlformats.org/package/2006/content-types"
                   ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.header+xml">
                <xsl:attribute name="PartName" select="concat('/word/header', position(), '.xml')"/>
            </Override>
        </xsl:for-each>
    </xsl:template>
    
    
    
</xsl:stylesheet>
