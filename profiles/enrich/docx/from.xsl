<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns="http://www.tei-c.org/ns/1.0"
                xmlns:xs="http://www.w3.org/2001/XMLSchema"
                xmlns:iso="http://www.iso.org/ns/1.0"
                xmlns:teidocx="http://www.tei-c.org/ns/teidocx/1.0"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:ve="http://schemas.openxmlformats.org/markup-compatibility/2006"
                xmlns:o="urn:schemas-microsoft-com:office:office"
                xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"
                xmlns:rel="http://schemas.openxmlformats.org/package/2006/relationships"
                xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math"
                xmlns:v="urn:schemas-microsoft-com:vml"
                xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing"
                xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main"
                xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture"
                xmlns:w10="urn:schemas-microsoft-com:office:word"
                xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"
                xmlns:wne="http://schemas.microsoft.com/office/word/2006/wordml"
                xmlns:mml="http://www.w3.org/1998/Math/MathML"
                xmlns:tbx="http://www.lisa.org/TBX-Specification.33.0.html"
                version="2.0"
                exclude-result-prefixes="ve o r m v wp w10 w wne mml tbx pic rel a         tei teidocx xs iso">
    
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
    <desc>
      <p> TEI stylesheet for simplifying TEI ODD markup </p>
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
    <!-- import base conversion style -->


    <xsl:import href="../../default/docx/from.xsl"/>

    <xsl:template name="create-tei-header">
      <teiHeader>
        <fileDesc>
          <titleStmt>
            <title>
              <xsl:call-template name="getDocTitle"/>
            </title>
                    <author>
                      <xsl:call-template name="getDocAuthor"/>
                    </author>
          </titleStmt>
          <editionStmt>
            <edition>
              <date>
                <xsl:call-template name="getDocDate"/>
              </date>
                    </edition>
                </editionStmt>
                <publicationStmt>
                  <p>
		  </p>
                </publicationStmt>
                <sourceDesc>
                  <p>Converted from a Word document </p>
                </sourceDesc>
            </fileDesc>
            <revisionDesc>
	      <change>
		<date>
		  <xsl:text>$LastChangedDate: </xsl:text>
		  <xsl:value-of select="tei:whatsTheDate()"/>
		  <xsl:text>$</xsl:text>
		</date>
		<name type="person">
		  <xsl:call-template name="getDocAuthor"/>
		</name>
	      </change>
            </revisionDesc>
      </teiHeader>
    </xsl:template>

    <xsl:template match="tei:text" mode="pass2">
      <text>
	<body>
	  <xsl:for-each select="tei:body">
	    <xsl:variable name="MS">
	      <xsl:apply-templates mode="pass2"/>
	    </xsl:variable>
	    <xsl:for-each select="$MS">
	      <msDesc>
		<xsl:attribute name="xml:lang">
		  <xsl:text>en</xsl:text>
		</xsl:attribute>
		<xsl:attribute name="xml:id">
		  <xsl:text>m1</xsl:text>
		</xsl:attribute>
		<msIdentifier>
		</msIdentifier>
		<xsl:copy-of select="tei:msContents"/>
		<physDesc>
		  <xsl:copy-of select="tei:physDesc/tei:p"/>
		  <xsl:if test="tei:supportDesc or tei:layoutDesc">
		    <objectDesc>
		      <xsl:copy-of select="tei:supportDesc"/>
		      <xsl:copy-of select="tei:layoutDesc"/>
		    </objectDesc>
		  </xsl:if>
		  <xsl:copy-of select="tei:handDesc"/>
		  <xsl:copy-of select="tei:decoDesc"/>
		  <xsl:copy-of select="tei:bindingDesc"/>
		</physDesc>
		<history>
		</history>
	      </msDesc>
	      <xsl:copy-of select="tei:title|tei:p|tei:div|tei:head"/>
	    </xsl:for-each>
	  </xsl:for-each>
	</body>
      </text>
    </xsl:template>

    <xsl:template match="tei:div[tei:head='Decoration']" mode="pass2">
      <decoDesc>
	        <xsl:apply-templates mode="pass2"/>
      </decoDesc>
    </xsl:template>

    <xsl:template match="tei:div[tei:head='Text']" mode="pass2">
	     <xsl:apply-templates mode="pass2"/>
    </xsl:template>

    <xsl:template match="tei:div[tei:head='Binding']" mode="pass2">
      <bindingDesc>
	        <xsl:apply-templates mode="pass2"/>
      </bindingDesc>
    </xsl:template>

    <xsl:template match="tei:div[tei:head='Physical Description']" mode="pass2">
      <physDesc>
	        <xsl:apply-templates mode="pass2"/>
      </physDesc>
    </xsl:template>

    <xsl:template match="tei:body/tei:div" mode="pass2">
      <xsl:if test="not(tei:head[string-length(.)=0] and count(*)=1)">
	        <xsl:apply-templates mode="pass2"/>
      </xsl:if>
    </xsl:template>

    <xsl:template match="tei:body/tei:div/tei:head" mode="pass2">
      <title>
	        <xsl:apply-templates mode="pass2"/>
      </title>
    </xsl:template>

    <xsl:template match="tei:head[.='Decoration']" mode="pass2"/>
    <xsl:template match="tei:head[.='Text']" mode="pass2"/>
    <xsl:template match="tei:head[.='Binding']" mode="pass2"/>
    <xsl:template match="tei:head[.='Physical Description']" mode="pass2"/>

    <xsl:template match="@rend[.='Body Text']" mode="pass2"/>
    <xsl:template match="@rend[.='Body Text Indent']" mode="pass2"/>

    <xsl:template match="tei:hi/@rend[.='superscript']" mode="pass2">
      <xsl:attribute name="rend">
	        <xsl:text>sup</xsl:text>
      </xsl:attribute>
    </xsl:template>

    <xsl:template match="tei:publicationStmt[.='']" mode="pass2">
      <publicationStmt>
	        <p>Unpublished </p>
      </publicationStmt>
    </xsl:template>

    <xsl:template match="tei:c[tei:match(@rend,'tab')]" mode="pass2">
      <xsl:text> </xsl:text>
    </xsl:template>

    <xsl:template match="tei:c[@iso:font='Symbol']" mode="pass2">
      <xsl:choose>
	        <xsl:when test="@n='F0B4'">×</xsl:when>
	        <xsl:when test="@n='F05B'">[</xsl:when>
	        <xsl:when test="@n='F05D'">]</xsl:when>
	        <xsl:otherwise>
	           <xsl:message>Panic: character <xsl:value-of select="@n"/> unknown</xsl:message>
	        </xsl:otherwise>
      </xsl:choose>
    </xsl:template>


    <xsl:template match="tei:fw" mode="pass2"/>

    <xsl:template match="tei:milestone[@unit='section']" mode="pass2"/>


</xsl:stylesheet>
