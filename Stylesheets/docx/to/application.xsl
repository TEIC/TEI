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
		xmlns:ep="http://schemas.openxmlformats.org/officeDocument/2006/extended-properties"
                xmlns:xs="http://www.w3.org/2001/XMLSchema"
                xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="2.0"
                exclude-result-prefixes="cp ep ve o r m v wp w10 w wne mml tbx iso      tei a xs pic fn xsi dc dcterms dcmitype     contypes teidocx teix html cals">

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

    <xsl:template name="write-docxfile-docprops-core">
        <xsl:variable name="now" select="tei:whatsTheDate()"/>
        <xsl:variable name="createdDate">
	  <xsl:for-each select="document($coreFile)">
	    <xsl:value-of select="cp:coreProperties/dcterms:created"/>
	  </xsl:for-each>
        </xsl:variable>

        <xsl:variable name="revision">
	  <xsl:for-each select="document($coreFile)">
	    <xsl:value-of select="cp:coreProperties/cp:revision + 1"/>
	  </xsl:for-each>
        </xsl:variable>

        <!-- after opening core.xml, we cannot write back to it; so save
            under new name -->
	<xsl:if test="$debug='true'">
	        <xsl:message>Writing out <xsl:value-of select="concat($wordDirectory,'/docProps/newcore.xml')"/>
         </xsl:message>
	     </xsl:if>

        <xsl:result-document href="{concat($wordDirectory,'/docProps/newcore.xml')}" standalone="yes">
            <cp:coreProperties>
                <dc:title>
		  <xsl:sequence select="tei:generateMetadataTitle(.)"/>
                </dc:title>
                <dc:creator>
		  <xsl:value-of
		      select="string-join(tei:generateMetadataAuthor(.),'')"/>
                </dc:creator>
                <cp:lastModifiedBy>TEI stylesheets</cp:lastModifiedBy>
                <cp:revision>
                    <xsl:value-of select="$revision"/>
                </cp:revision>
                <dcterms:created xsi:type="dcterms:W3CDTF">
		  <xsl:value-of select="$createdDate"/>
                </dcterms:created>
                <dcterms:modified xsi:type="dcterms:W3CDTF">
		  <xsl:value-of select="$now"/>
                </dcterms:modified>
            </cp:coreProperties>
        </xsl:result-document>
    </xsl:template>


    <xsl:template name="write-docxfile-docprops-app">
        <xsl:variable name="template">
	  <xsl:for-each select="document($appFile)">
	    <xsl:value-of select="ep:Properties/ep:Template"/>
	  </xsl:for-each>
        </xsl:variable>
        <!-- after opening app.xml, we cannot write back to it; so save
            under new name -->
	<xsl:if test="$debug='true'">
	  <xsl:message>Writing out <xsl:value-of select="concat($wordDirectory,'/docProps/newapp.xml')"/>
	  </xsl:message>
	</xsl:if>

        <xsl:result-document href="{concat($wordDirectory,'/docProps/newapp.xml')}" standalone="yes">
	  <Properties xmlns="http://schemas.openxmlformats.org/officeDocument/2006/extended-properties" xmlns:vt="http://schemas.openxmlformats.org/officeDocument/2006/docPropsVTypes">
	    <Template><xsl:value-of select="$template"/></Template>
	    <TotalTime>1</TotalTime>
	    <Pages>1</Pages>
	    <Words>0</Words>
	    <Characters>0</Characters>
	    <Application>TEI XSL stylesheets</Application>
	    <DocSecurity>0</DocSecurity>
	    <Lines>0</Lines>
	    <Paragraphs>0</Paragraphs>
	    <ScaleCrop>false</ScaleCrop>
	    <Company>Text Encoding Initiative</Company>
	    <LinksUpToDate>false</LinksUpToDate>
	    <CharactersWithSpaces>0</CharactersWithSpaces>
	    <SharedDoc>false</SharedDoc>
	    <HyperlinksChanged>false</HyperlinksChanged>
	    <AppVersion>7.3.0</AppVersion>
	  </Properties>
        </xsl:result-document>
    </xsl:template>

    <!-- after opening custom.xml, we cannot write back to it; so save
	 under new name -->
    <xsl:template name="write-docxfile-docprops-custom">
	     <xsl:if test="$debug='true'">
	        <xsl:message>Writing out <xsl:value-of select="concat($wordDirectory,'/docProps/newcustom.xml')"/>    </xsl:message>
	     </xsl:if>
        <xsl:result-document href="{concat($wordDirectory,'/docProps/newcustom.xml')}" standalone="yes">
            <Properties xmlns="http://schemas.openxmlformats.org/officeDocument/2006/custom-properties"
                     xmlns:vt="http://schemas.openxmlformats.org/officeDocument/2006/docPropsVTypes">
                <property pid="1001" name="TEI_toDOCX">
                    <xsl:attribute name="fmtid">
                        <xsl:text>{D5CDD505-2E9C-101B-9397-08002B2CF9AE}</xsl:text>
                    </xsl:attribute>
                    <vt:lpwstr>2.15.0</vt:lpwstr>
                </property>
                <xsl:for-each select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:encodingDesc/tei:appInfo/tei:application">
                    <xsl:if test="not(@ident='TEI_toDOCX')">
                        <property name="{@ident}">
                            <xsl:attribute name="pid">
                                <xsl:value-of select="position()+1001"/>
                            </xsl:attribute>
                            <xsl:attribute name="fmtid">
                                <xsl:text>{D5CDD505-2E9C-101B-9397-08002B2CF9AE}</xsl:text>
                            </xsl:attribute>
                            <vt:lpwstr>
                                <xsl:value-of select="@version"/>
                            </vt:lpwstr>
                        </property>
                    </xsl:if>
                </xsl:for-each>
            </Properties>
        </xsl:result-document>
    </xsl:template>

</xsl:stylesheet>
