<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:iso="http://www.iso.org/ns/1.0" xmlns="http://www.w3.org/1999/xhtml" xmlns:html="http://www.w3.org/1999/xhtml" xmlns:tei="http://www.tei-c.org/ns/1.0" xmlns:teix="http://www.tei-c.org/ns/Examples" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:ncx="http://www.daisy.org/z3986/2005/ncx/" version="2.0" exclude-result-prefixes="iso tei teix dc html ncx" xpath-default-namespace="http://www.tei-c.org/ns/1.0">
  <xsl:import href="../html5/tei.xsl"/>
  <xsl:import href="../epub/epub-common.xsl"/>
  <xsl:import href="../epub/epub-preflight.xsl"/>
  <xsl:output method="xml" encoding="utf-8" doctype-system="" indent="no"/>
  <xsl:key match="tei:graphic[not(ancestor::teix:egXML)]" use="1" name="G"/>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
    <desc>
      <p>
	TEI stylesheet for making ePub output. A lot learnt from
	http://www.hxa.name/articles/content/epub-guide_hxa7241_2007.xhtml and
	the stylesheets of the NZETC.
      </p>
      <p>This software is dual-licensed:

1. Distributed under a Creative Commons Attribution-ShareAlike 3.0
Unported License http://creativecommons.org/licenses/by-sa/3.0/ 

2. http://www.opensource.org/licenses/BSD-2-Clause
		
All rights reserved.

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
      <p>Id: $Id$</p>
      <p>Copyright: 2008, TEI Consortium</p>
    </desc>
  </doc>
  <xsl:param name="opfPackageVersion">3.0</xsl:param>
  <xsl:param name="STDOUT">false</xsl:param>
  <xsl:param name="autoHead">true</xsl:param>
  <xsl:param name="autoToc">true</xsl:param>
  <xsl:param name="bottomNavigationPanel">false</xsl:param>
  <xsl:param name="coverimage"/>
  <xsl:param name="createanttask">false</xsl:param>
  <xsl:param name="cssFile">../tei.css</xsl:param>
  <xsl:param name="cssODDFile">../odd.css</xsl:param>
  <xsl:param name="cssPrintFile">../epub-print.css</xsl:param>
  <xsl:param name="debug">false</xsl:param>
  <xsl:param name="directory">.</xsl:param>
  <xsl:param name="doctypePublic"/>
  <xsl:param name="doctypeSystem"/>
  <xsl:param name="fixgraphicsurl">false</xsl:param>
  <xsl:param name="inputDir">.</xsl:param>
  <xsl:param name="institution"/>
  <xsl:param name="linkPanel">false</xsl:param>
  <xsl:param name="odd">false</xsl:param>
  <xsl:param name="outputDir"><xsl:value-of select="$directory"/>/OPS</xsl:param>
  <xsl:param name="outputMethod">xml</xsl:param>
  <xsl:param name="outputSuffix">.xhtml</xsl:param>
  <xsl:param name="outputTarget">epub3</xsl:param>
  <xsl:param name="publisher"/>
  <xsl:param name="splitLevel">0</xsl:param>
  <xsl:param name="subject"/>
  <xsl:param name="tocDepth">5</xsl:param>
  <xsl:param name="tocFront">true</xsl:param>
  <xsl:param name="topNavigationPanel">false</xsl:param>
  <xsl:param name="uid"/>
  <xsl:param name="useHeaderFrontMatter">false</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>(extensible) wrapper for root element</desc>
  </doc>
  <xsl:template match="/">
    <xsl:call-template name="processTEI"/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>[epub] Override of top-level template. This does most of
  the work: performing the normal transformation, fixing the links to graphics files so that they are
  all relative, creating the extra output files, etc</desc>
  </doc>
  <xsl:template name="processTEI">
    <xsl:variable name="stage1">
      <xsl:apply-templates mode="preflight"/>
    </xsl:variable>
    <xsl:for-each select="$stage1">
      <xsl:call-template name="processTEIHook"/>
      <xsl:variable name="coverImageOutside">
        <xsl:choose>
          <xsl:when test="/tei:TEI/tei:text/tei:front/tei:titlePage[@facs]">
            <xsl:for-each select="/tei:TEI/tei:text/tei:front/tei:titlePage[@facs][1]">
              <xsl:for-each select="id(substring(@facs,2))">
                <xsl:choose>
                  <xsl:when test="count(tei:graphic)=1">
                    <xsl:value-of select="tei:graphic/@url"/>
                  </xsl:when>
                  <xsl:otherwise>
                    <xsl:value-of select="tei:graphic[2]/@url"/>
                  </xsl:otherwise>
                </xsl:choose>
              </xsl:for-each>
            </xsl:for-each>
          </xsl:when>
          <xsl:when test="not($coverimage='')">
            <xsl:value-of select="$coverimage"/>
          </xsl:when>
        </xsl:choose>
      </xsl:variable>
      <xsl:variable name="coverImageInside">
        <xsl:choose>
          <xsl:when test="/tei:TEI/tei:text/tei:front/tei:titlePage[@facs]">
            <xsl:for-each select="/tei:TEI/tei:text/tei:front/tei:titlePage[@facs][1]">
              <xsl:for-each select="id(substring(@facs,2))">
                <xsl:value-of select="tei:graphic[1]/@url"/>
              </xsl:for-each>
            </xsl:for-each>
          </xsl:when>
          <xsl:when test="not($coverimage='')">
            <xsl:value-of select="$coverimage"/>
          </xsl:when>
        </xsl:choose>
      </xsl:variable>
      <xsl:choose>
        <xsl:when test="$splitLevel='-1'">
          <xsl:apply-templates/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:apply-templates mode="split"/>
        </xsl:otherwise>
      </xsl:choose>
      <xsl:for-each select="*">
        <xsl:variable name="TOC">
          <TOC xmlns="http://www.w3.org/1999/xhtml">
            <xsl:call-template name="mainTOC"/>
          </TOC>
        </xsl:variable>
        <!--
	    <xsl:result-document href="/tmp/TOC">
	    <xsl:copy-of select="$TOC"/>
	    </xsl:result-document>
-->
        <xsl:for-each select="tokenize($javascriptFiles,',')">
          <xsl:variable name="file" select="normalize-space(.)"/>
          <xsl:variable name="name" select="tokenize($file,'/')[last()]"/>
          <xsl:if test="$verbose='true'">
            <xsl:message>write Javascript file <xsl:value-of select="$name"/></xsl:message>
          </xsl:if>
          <xsl:result-document method="text" href="{concat($directory,'/OPS/',$name)}">
            <xsl:for-each select="unparsed-text($file)">
              <xsl:copy-of select="."/>
            </xsl:for-each>
          </xsl:result-document>
        </xsl:for-each>
        <xsl:if test="$verbose='true'">
          <xsl:message>write file OPS/stylesheet.css</xsl:message>
        </xsl:if>
        <xsl:result-document method="text" href="{concat($directory,'/OPS/stylesheet.css')}">
          <xsl:if test="not($cssFile='')">
            <xsl:if test="$verbose='true'">
              <xsl:message>reading file <xsl:value-of select="$cssFile"/></xsl:message>
            </xsl:if>
            <xsl:for-each select="tokenize(unparsed-text($cssFile),     '\r?\n')">
              <xsl:call-template name="purgeCSS"/>
            </xsl:for-each>
          </xsl:if>
          <xsl:if test="not($cssSecondaryFile='')">
            <xsl:if test="$verbose='true'">
              <xsl:message>reading secondary file <xsl:value-of select="$cssSecondaryFile"/></xsl:message>
            </xsl:if>
            <xsl:for-each select="tokenize(unparsed-text($cssSecondaryFile),       '\r?\n')">
              <xsl:call-template name="purgeCSS"/>
            </xsl:for-each>
          </xsl:if>
          <xsl:if test="$odd='true'">
            <xsl:if test="$verbose='true'">
              <xsl:message>reading file <xsl:value-of select="$cssODDFile"/></xsl:message>
            </xsl:if>
            <xsl:for-each select="tokenize(unparsed-text($cssODDFile),         '\r?\n')">
              <xsl:call-template name="purgeCSS"/>
            </xsl:for-each>
          </xsl:if>
          <xsl:if test="$odd='true'">
            <xsl:if test="$verbose='true'">
              <xsl:message>reading file <xsl:value-of select="$cssODDFile"/></xsl:message>
            </xsl:if>
            <xsl:for-each select="tokenize(unparsed-text($cssODDFile),         '\r?\n')">
              <xsl:call-template name="purgeCSS"/>
            </xsl:for-each>
          </xsl:if>
          <xsl:if test="$filePerPage='true'">
            <xsl:text>body { width: </xsl:text>
            <xsl:value-of select="$viewPortWidth"/>
            <xsl:text>px;
 height: </xsl:text>
            <xsl:value-of select="$viewPortHeight"/>
            <xsl:text>px;
}
img.fullpage {
position: absolute;
height: </xsl:text>
            <xsl:value-of select="$viewPortHeight"/>
            <xsl:text>px; left:0px; top:0px;}
</xsl:text>
          </xsl:if>
        </xsl:result-document>
        <xsl:if test="$verbose='true'">
          <xsl:message>write file OPS/print.css</xsl:message>
        </xsl:if>
        <xsl:result-document method="text" href="{concat($directory,'/OPS/print.css')}">
          <xsl:if test="$verbose='true'">
            <xsl:message>reading file <xsl:value-of select="$cssPrintFile"/></xsl:message>
          </xsl:if>
          <xsl:for-each select="tokenize(unparsed-text($cssPrintFile),     '\r?\n')">
            <xsl:call-template name="purgeCSS"/>
          </xsl:for-each>
        </xsl:result-document>
        <xsl:if test="$verbose='true'">
          <xsl:message>write file mimetype</xsl:message>
        </xsl:if>
        <xsl:result-document method="text" href="{concat($directory,'/mimetype')}">
          <xsl:value-of select="$epubMimetype"/>
        </xsl:result-document>
        <xsl:if test="$verbose='true'">
          <xsl:message>write file META-INF/container.xml</xsl:message>
        </xsl:if>
        <xsl:result-document method="xml" href="{concat($directory,'/META-INF/container.xml')}">
          <container xmlns="urn:oasis:names:tc:opendocument:xmlns:container" version="1.0">
            <rootfiles>
              <rootfile full-path="OPS/package.opf" media-type="application/oebps-package+xml"/>
            </rootfiles>
          </container>
        </xsl:result-document>
        <xsl:if test="$verbose='true'">
          <xsl:message>write file OPS/package.opf</xsl:message>
        </xsl:if>
	<xsl:variable name="A">
	  <xsl:call-template name="generateAuthor"/>
	</xsl:variable>
	<xsl:variable name="printA">
	  <xsl:analyze-string select="$A" regex="([^,]+), ([^,]+), (.+)">
	    <xsl:matching-substring>
	      <xsl:value-of select="regex-group(1)"/>
	      <xsl:text>, </xsl:text>
	      <xsl:value-of select="regex-group(2)"/>
	    </xsl:matching-substring>
	    <xsl:non-matching-substring>
	      <xsl:value-of select="."/>
	    </xsl:non-matching-substring>
	  </xsl:analyze-string>
	</xsl:variable>	
        <xsl:result-document
	    href="{concat($directory,'/OPS/package.opf')}"
	    method="xml" indent="yes">
          <package xmlns="http://www.idpf.org/2007/opf" unique-identifier="pub-id" version="{$opfPackageVersion}">
	    <xsl:call-template name="opfmetadata">
	      <xsl:with-param name="author" select="$A"/>
	      <xsl:with-param name="printAuthor" select="$printA"/>
	      <xsl:with-param name="coverImageOutside" select="$coverImageOutside"/>
	    </xsl:call-template>
            <manifest>
              <!-- deal with intricacies of overlay files -->
              <xsl:variable name="TL" select="key('Timeline',1)"/>
              <xsl:if test="$mediaoverlay='true' and key('Timeline',1)">
                <xsl:if test="$verbose='true'">
                  <xsl:message>write file SMIL files</xsl:message>
                </xsl:if>
                <xsl:for-each select="key('Timeline',1)">
                  <xsl:variable name="TLnumber">
                    <xsl:number level="any"/>
                  </xsl:variable>
                  <xsl:variable name="audio">
                    <xsl:text>media/audio</xsl:text>
                    <xsl:number level="any"/>
                    <xsl:text>.</xsl:text>
                    <xsl:value-of select="tokenize(@corresp,'\.')[last()]"/>
                  </xsl:variable>
                  <item id="timeline-audio{$TLnumber}" href="{$audio}">
                    <xsl:attribute name="media-type">
                      <xsl:choose>
                        <xsl:when test="contains($audio,'.m4a')">audio/m4a</xsl:when>
                        <xsl:otherwise>audio/m4a</xsl:otherwise>
                      </xsl:choose>
                    </xsl:attribute>
                  </item>
                  <xsl:for-each select="key('PB',1)">
                    <xsl:variable name="page">
                      <xsl:value-of select="generate-id()"/>
                    </xsl:variable>
                    <xsl:variable name="target">
                      <xsl:apply-templates select="." mode="ident"/>
                    </xsl:variable>
                    <xsl:if test="count(key('objectOnPage',$page))&gt;0">
                      <item id="{$target}-audio" href="{$target}-overlay.smil" media-type="application/smil+xml"/>
                      <xsl:result-document href="{concat($directory,'/OPS/',$target,'-overlay.smil')}" method="xml">
                        <smil xmlns="http://www.w3.org/ns/SMIL" version="3.0" profile="http://www.ipdf.org/epub/30/profile/content/">
                          <body>
                            <xsl:for-each select="key('objectOnPage',$page)">
                              <xsl:variable name="object" select="@xml:id"/>
                              <xsl:for-each select="$TL">
                                <xsl:for-each select="key('Object',$object)">
                                  <par id="{@xml:id}">
                                    <text src="{$target}.xhtml{@corresp}"/>
                                    <audio src="{$audio}" clipBegin="{@from}{../@unit}" clipEnd="{@to}{../@unit}"/>
                                  </par>
                                </xsl:for-each>
                              </xsl:for-each>
                            </xsl:for-each>
                          </body>
                        </smil>
                      </xsl:result-document>
                    </xsl:if>
                  </xsl:for-each>
                </xsl:for-each>
              </xsl:if>
              <xsl:if test="not($coverImageOutside='')">
                <item href="{$coverImageOutside}" id="cover-image" media-type="image/jpeg"/>
              </xsl:if>
              <xsl:if test="not($coverimage='') and not($coverimage=$coverImageOutside)">
                <item href="{$coverimage}" id="cover-image-extra" media-type="image/jpeg"/>
              </xsl:if>
              <xsl:for-each select="tokenize($javascriptFiles,',')">
                <xsl:variable name="name" select="tokenize(normalize-space(.),'/')[last()]"/>
                <item href="{$name}" id="javascript{position()}" media-type="text/javascript"/>
              </xsl:for-each>
              <item id="css" href="stylesheet.css" media-type="text/css"/>
              <item id="print.css" href="print.css" media-type="text/css"/>
              <item href="titlepage.xhtml" id="titlepage" media-type="application/xhtml+xml"/>
              <xsl:if test="$filePerPage='true'">
                <item href="titlepageverso.xhtml" id="titlepageverso" media-type="application/xhtml+xml"/>
              </xsl:if>
              <xsl:for-each select="tei:text/tei:front/tei:titlePage">
                <xsl:variable name="N" select="position()"/>
                <item href="titlepage{$N}.xhtml" id="titlepage{$N}" media-type="application/xhtml+xml"/>
              </xsl:for-each>
              <item href="titlepageback.xhtml" id="titlepageback" media-type="application/xhtml+xml"/>
              <item id="toc" properties="nav" href="toc.xhtml" media-type="application/xhtml+xml"/>
              <item id="start" href="index.xhtml" media-type="application/xhtml+xml"/>
              <xsl:choose>
                <xsl:when test="$filePerPage='true'">
                  <xsl:for-each select="key('PB',1)">
                    <xsl:variable name="target">
                      <xsl:apply-templates select="." mode="ident"/>
                    </xsl:variable>
                    <xsl:if test="@facs">
                      <xsl:variable name="facstarget">
                        <xsl:apply-templates select="." mode="ident"/>
                        <xsl:text>-facs.xhtml</xsl:text>
                      </xsl:variable>
                      <item href="{$facstarget}" media-type="application/xhtml+xml">
                        <xsl:attribute name="id">
                          <xsl:text>pagefacs</xsl:text>
                          <xsl:number level="any"/>
                        </xsl:attribute>
                      </item>
                    </xsl:if>
                    <item href="{$target}.xhtml" media-type="application/xhtml+xml">
                      <xsl:if test="$mediaoverlay='true'">
                        <xsl:attribute name="media-overlay">
                          <xsl:value-of select="$target"/>
                          <xsl:text>-audio</xsl:text>
                        </xsl:attribute>
                      </xsl:if>
                      <xsl:attribute name="id">
                        <xsl:text>page</xsl:text>
                        <xsl:number level="any"/>
                      </xsl:attribute>
                    </item>
                  </xsl:for-each>
                </xsl:when>
                <xsl:otherwise>
                  <xsl:for-each select="$TOC/html:TOC/html:ul/html:li">
                    <xsl:choose>
                      <xsl:when test="not(html:a)"/>
                      <xsl:when test="starts-with(html:a/@href,'#')"/>
                      <xsl:otherwise>
                        <item href="{html:a[1]/@href}" media-type="application/xhtml+xml">
                          <xsl:attribute name="id">
                            <xsl:text>section</xsl:text>
                            <xsl:number count="html:li" level="any"/>
                          </xsl:attribute>
                        </item>
                      </xsl:otherwise>
                    </xsl:choose>
                    <xsl:if test="html:ul">
                      <xsl:for-each select="html:ul//html:li[html:a          and          not(contains(html:a/@href,'#'))]">
                        <item href="{html:a[1]/@href}" media-type="application/xhtml+xml">
                          <xsl:attribute name="id">
                            <xsl:text>section</xsl:text>
                            <xsl:number count="html:li" level="any"/>
                          </xsl:attribute>
                        </item>
                      </xsl:for-each>
                    </xsl:if>
                  </xsl:for-each>
                </xsl:otherwise>
              </xsl:choose>
              <!-- images -->
              <xsl:for-each select="key('GRAPHICS',1)">
                <xsl:variable name="img" select="@url|@facs"/>
                <xsl:if test="not($img=$coverImageOutside)">
                  <xsl:variable name="ID">
                    <xsl:number level="any"/>
                  </xsl:variable>
                  <xsl:variable name="mimetype">
                    <xsl:choose>
                      <xsl:when test="@mimeType != ''">
                        <xsl:value-of select="@mimeType"/>
                      </xsl:when>
                      <xsl:when test="contains($img,'.gif')">image/gif</xsl:when>
                      <xsl:when test="contains($img,'.png')">image/png</xsl:when>
                      <xsl:when test="contains($img,'.mpeg')">video/mpeg4</xsl:when>
                      <xsl:when test="contains($img,'.mp4')">video/mpeg4</xsl:when>
                      <xsl:when test="contains($img,'.m4v')">video/mpeg4</xsl:when>
                      <xsl:otherwise>image/jpeg</xsl:otherwise>
                    </xsl:choose>
                  </xsl:variable>
                  <item href="{$img}" id="image-{$ID}" media-type="{$mimetype}"/>
                </xsl:if>
              </xsl:for-each>
              <!-- page images -->
              <xsl:for-each select="key('PBGRAPHICS',1)">
                <xsl:variable name="img" select="@facs"/>
                <xsl:variable name="ID">
                  <xsl:number level="any"/>
                </xsl:variable>
                <xsl:variable name="mimetype">
                  <xsl:choose>
                    <xsl:when test="@mimeType != ''">
                      <xsl:value-of select="@mimeType"/>
                    </xsl:when>
                    <xsl:when test="contains($img,'.gif')">image/gif</xsl:when>
                    <xsl:when test="contains($img,'.png')">image/png</xsl:when>
                    <xsl:otherwise>image/jpeg</xsl:otherwise>
                  </xsl:choose>
                </xsl:variable>
                <item href="{$img}" id="pbimage-{$ID}" media-type="{$mimetype}"/>
              </xsl:for-each>
              <item id="ncx" href="toc.ncx" media-type="application/x-dtbncx+xml"/>
              <xsl:call-template name="epubManifestHook"/>
            </manifest>
            <spine toc="ncx">
              <itemref idref="titlepage" linear="yes"/>
              <xsl:if test="$filePerPage='true'">
                <itemref idref="titlepageverso" linear="yes"/>
              </xsl:if>
              <xsl:for-each select="tei:text/tei:front/tei:titlePage">
                <xsl:variable name="N" select="position()"/>
                <itemref idref="titlepage{$N}" linear="yes"/>
              </xsl:for-each>
              <itemref idref="start" linear="yes"/>
              <xsl:choose>
                <xsl:when test="$filePerPage='true'">
                  <xsl:for-each select="key('PB',1)">
                    <xsl:if test="@facs">
                      <itemref>
                        <xsl:attribute name="idref">
                          <xsl:text>pagefacs</xsl:text>
                          <xsl:number level="any"/>
                        </xsl:attribute>
                        <xsl:attribute name="linear">yes</xsl:attribute>
                      </itemref>
                    </xsl:if>
                    <itemref>
                      <xsl:attribute name="idref">
                        <xsl:text>page</xsl:text>
                        <xsl:number level="any"/>
                      </xsl:attribute>
                      <xsl:attribute name="linear">yes</xsl:attribute>
                    </itemref>
                  </xsl:for-each>
                </xsl:when>
                <xsl:otherwise>
                  <xsl:for-each select="$TOC/html:TOC/html:ul/html:li">
                    <xsl:choose>
                      <xsl:when test="not(html:a)"/>
                      <xsl:when test="starts-with(html:a/@href,'#')"/>
                      <xsl:otherwise>
                        <itemref>
                          <xsl:attribute name="idref">
                            <xsl:text>section</xsl:text>
                            <xsl:number count="html:li" level="any"/>
                          </xsl:attribute>
                          <xsl:attribute name="linear">yes</xsl:attribute>
                        </itemref>
                      </xsl:otherwise>
                    </xsl:choose>
                    <xsl:if test="html:ul">
                      <xsl:for-each select="html:ul//html:li[html:a and not(contains(html:a/@href,'#'))]">
                        <itemref>
                          <xsl:attribute name="idref">
                            <xsl:text>section</xsl:text>
                            <xsl:number count="html:li" level="any"/>
                          </xsl:attribute>
                          <xsl:attribute name="linear">yes</xsl:attribute>
                        </itemref>
                      </xsl:for-each>
                    </xsl:if>
                  </xsl:for-each>
                </xsl:otherwise>
              </xsl:choose>
              <itemref idref="titlepageback">
                <xsl:attribute name="linear">
                  <xsl:choose>
                    <xsl:when test="$filePerPage='true'">yes</xsl:when>
                    <xsl:otherwise>no</xsl:otherwise>
                  </xsl:choose>
                </xsl:attribute>
              </itemref>
              <xsl:call-template name="epubSpineHook"/>
            </spine>
            <guide>
              <reference type="text" href="titlepage.xhtml" title="Cover"/>
              <reference type="text" title="Start" href="index.xhtml"/>
              <xsl:choose>
                <xsl:when test="$filePerPage='true'">
		</xsl:when>
                <xsl:otherwise>
                  <xsl:for-each select="$TOC/html:TOC/html:ul/html:li">
                    <xsl:if test="html:a">
                      <reference type="text" href="{html:a[1]/@href}">
                        <xsl:attribute name="title">
                          <xsl:value-of select="normalize-space(html:a[1])"/>
                        </xsl:attribute>
                      </reference>
                    </xsl:if>
                    <xsl:if test="contains(parent::html:ul/@class,'group')">
                      <xsl:for-each select="html:ul//html:li">
                        <xsl:choose>
                          <xsl:when test="not(html:a)"/>
                          <xsl:when test="contains(html:a/@href,'#')"/>
                          <xsl:otherwise>
                            <reference type="text" href="{html:a/@href}">
                              <xsl:attribute name="title">
                                <xsl:value-of select="normalize-space(html:a[1])"/>
                              </xsl:attribute>
                            </reference>
                          </xsl:otherwise>
                        </xsl:choose>
                      </xsl:for-each>
                    </xsl:if>
                  </xsl:for-each>
                </xsl:otherwise>
              </xsl:choose>
              <reference href="titlepageback.xhtml" type="text" title="About this book"/>
            </guide>
          </package>
        </xsl:result-document>
        <xsl:if test="$verbose='true'">
          <xsl:message>write file OPS/titlepage.xhtml</xsl:message>
        </xsl:if>
        <xsl:result-document href="{concat($directory,'/OPS/titlepage.xhtml')}" method="xml">
          <html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en">
            <head>
              <xsl:call-template name="metaHTML">
                <xsl:with-param name="title">Title page</xsl:with-param>
              </xsl:call-template>
              <meta name="calibre:cover" content="true"/>
              <title>Title page</title>
              <style type="text/css" title="override_css">
		@page {padding: 0pt; margin:0pt}
		body { text-align: center; padding:0pt; margin: 0pt; }
	      </style>
            </head>
            <body>
              <xsl:choose>
                <xsl:when test="$coverImageInside=''">
                  <div>
                    <xsl:attribute name="style">
		      font-family: serif; 
		      height:860;          
		      font-size:30pt; 
		      font-weight: bold;
		      padding-top: 15pt;
		      margin: 12pt;
		      border: solid red 1pt; 
		      text-align:center;
		    </xsl:attribute>
                    <xsl:call-template name="generateTitle"/>
                  </div>
                </xsl:when>
                <xsl:otherwise>
                  <div>
                    <img width="{$viewPortWidth}" height="{$viewPortHeight}" alt="cover picture" src="{$coverImageInside}"/>
                  </div>
                </xsl:otherwise>
              </xsl:choose>
            </body>
          </html>
        </xsl:result-document>
        <xsl:for-each select="tei:text/tei:front/tei:titlePage">
          <xsl:variable name="N" select="position()"/>
          <xsl:if test="$verbose='true'">
            <xsl:message>write file OPS/titlepage<xsl:value-of select="$N"/>.xhtml</xsl:message>
          </xsl:if>
          <xsl:result-document href="{concat($directory,'/OPS/titlepage',$N,'.xhtml')}" method="xml">
            <html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en">
              <head>
                <xsl:call-template name="metaHTML">
                  <xsl:with-param name="title">Title page</xsl:with-param>
                </xsl:call-template>
                <xsl:call-template name="linkCSS">
		  <xsl:with-param
		      name="file">stylesheet.css</xsl:with-param>
		</xsl:call-template>
                <title>Title page</title>
              </head>
              <body>
                <div class="titlePage">
                  <xsl:apply-templates/>
                </div>
              </body>
            </html>
          </xsl:result-document>
        </xsl:for-each>
        <xsl:if test="$filePerPage='true'">
          <xsl:if test="$verbose='true'">
            <xsl:message>write file OPS/titlepageverso.xhtml</xsl:message>
          </xsl:if>
          <xsl:result-document href="{concat($directory,'/OPS/titlepageverso.xhtml')}" method="xml">
            <html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en">
              <head>
                <xsl:call-template name="metaHTML">
                  <xsl:with-param name="title">title page verso</xsl:with-param>
                </xsl:call-template>
                <title>title page verso</title>
              </head>
              <body>
                <p/>
              </body>
            </html>
          </xsl:result-document>
        </xsl:if>
        <xsl:if test="$verbose='true'">
          <xsl:message>write file OPS/titlepageback.xhtml</xsl:message>
        </xsl:if>
        <xsl:result-document href="{concat($directory,'/OPS/titlepageback.xhtml')}" method="xml">
          <html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en">
            <head>
              <xsl:call-template name="metaHTML">
                <xsl:with-param name="title">About this book</xsl:with-param>
              </xsl:call-template>
              <title>About this book</title>
            </head>
            <body>
              <div style="text-align: left; font-size: larger">
                <h2>Information about this book</h2>
                <xsl:for-each select="/*/tei:teiHeader/tei:fileDesc">
                  <xsl:apply-templates mode="metadata"/>
                </xsl:for-each>
                <xsl:for-each select="/*/tei:teiHeader/tei:encodingDesc">
                  <xsl:apply-templates mode="metadata"/>
                </xsl:for-each>
              </div>
            </body>
          </html>
        </xsl:result-document>
        <xsl:if test="$verbose='true'">
          <xsl:message>write file OPS/toc.ncx</xsl:message>
        </xsl:if>
        <xsl:result-document href="{concat($directory,'/OPS/toc.ncx')}" method="xml">
          <ncx xmlns="http://www.daisy.org/z3986/2005/ncx/" version="2005-1">
            <head>
              <meta name="dtb:uid">
                <xsl:attribute name="content">
                  <xsl:call-template name="generateID"/>
                </xsl:attribute>
              </meta>
              <meta name="dtb:totalPageCount" content="0"/>
              <meta name="dtb:maxPageNumber" content="0"/>
            </head>
            <docTitle>
              <text>
                <xsl:call-template name="generateSimpleTitle"/>
              </text>
            </docTitle>
            <navMap>
              <xsl:variable name="navPoints">
                <navPoint>
                  <navLabel>
                    <text>[Cover]</text>
                  </navLabel>
                  <content src="titlepage.xhtml"/>
                </navPoint>
                <xsl:for-each select="tei:text/tei:front/tei:titlePage[1]">
                  <xsl:variable name="N" select="position()"/>
                  <navPoint>
                    <navLabel>
                      <text>[Title page]</text>
                    </navLabel>
                    <content src="titlepage{$N}.xhtml"/>
                  </navPoint>
                </xsl:for-each>
                <xsl:choose>
                  <xsl:when test="not($TOC/html:TOC/html:ul[@class='toc toc_body']/html:li)">
                    <xsl:for-each select="$TOC/html:TOC/html:ul[@class='toc toc_front']">
                      <xsl:apply-templates select="html:li"/>
                    </xsl:for-each>
                    <navPoint>
                      <navLabel>
                        <text>[The book]</text>
                      </navLabel>
                      <content src="index.xhtml"/>
                    </navPoint>
                    <xsl:for-each select="$TOC/html:TOC/html:ul[contains(@class,'group')]">
                      <xsl:apply-templates select=".//html:li[not(contains(html:a/@href,'#'))]"/>
                    </xsl:for-each>
                    <xsl:for-each select="$TOC/html:TOC/html:ul[@class='toc toc_back']">
                      <xsl:apply-templates select="html:li"/>
                    </xsl:for-each>
                  </xsl:when>
                  <xsl:otherwise>
                    <xsl:for-each select="$TOC/html:TOC/html:ul">
                      <xsl:apply-templates select="html:li"/>
                    </xsl:for-each>
                  </xsl:otherwise>
                </xsl:choose>
                <navPoint>
                  <navLabel>
                    <text>[About this book]</text>
                  </navLabel>
                  <content src="titlepageback.xhtml"/>
                </navPoint>
              </xsl:variable>
              <xsl:for-each select="$navPoints/ncx:navPoint">
                <xsl:variable name="pos" select="position()"/>
                <navPoint id="navPoint-{$pos}" playOrder="{$pos}">
                  <xsl:copy-of select="*"/>
                </navPoint>
              </xsl:for-each>
            </navMap>
          </ncx>
        </xsl:result-document>
        <xsl:if test="$filePerPage='true'">
          <xsl:if test="$verbose='true'">
            <xsl:message>write file META-INF/com.apple.ibooks.display-options.xml</xsl:message>
          </xsl:if>
          <xsl:result-document href="{concat($directory,'/META-INF/com.apple.ibooks.display-options.xml')}">
            <display_options xmlns="">
              <platform name="*">
                <option name="fixed-layout">true</option>
              </platform>
            </display_options>
          </xsl:result-document>
        </xsl:if>
        <xsl:if test="$debug='true'">
          <xsl:message>write file OPS/toc.html</xsl:message>
        </xsl:if>
        <xsl:result-document href="{concat($directory,'/OPS/toc.xhtml')}" method="xml" doctype-system="">
          <html xmlns="http://www.w3.org/1999/xhtml" xmlns:epub="http://www.idpf.org/2007/ops">
            <head>
              <title>
                <xsl:call-template name="generateSimpleTitle"/>
              </title>
                <xsl:call-template name="linkCSS">
		  <xsl:with-param
		      name="file">stylesheet.css</xsl:with-param>
		</xsl:call-template>
            </head>
            <body>
              <section class="TableOfContents">
                <header>
                  <h1>Contents</h1>
                </header>
                <nav xmlns:epub="http://www.idpf.org/2007/ops" epub:type="toc" id="toc">
                  <ol>
                    <xsl:for-each select="$TOC/html:TOC/html:ul/html:li">
                      <xsl:choose>
                        <xsl:when test="not(html:a)"/>
                        <xsl:when test="starts-with(html:a/@href,'#')"/>
                        <xsl:when test="contains(@class,'headless')"/>
                        <xsl:otherwise>
                          <li>
                            <a href="{html:a/@href}">
                              <xsl:value-of select="html:span[@class='headingNumber']"/>
                              <xsl:value-of select="normalize-space(html:a[1])"/>
                            </a>
                          </li>
                        </xsl:otherwise>
                      </xsl:choose>
                    </xsl:for-each>
                    <li>
                      <a href="titlepageback.xhtml">[About this book]</a>
                    </li>
                  </ol>
                </nav>
                <nav xmlns:epub="http://www.idpf.org/2007/ops" epub:type="landmarks" id="guide">
                  <h2>Guide</h2>
                  <ol>
                    <li>
                      <a epub:type="titlepage" href="titlepage.xhtml">[Title page]</a>
                    </li>
                    <li>
                      <a epub:type="bodymatter" href="index.xhtml">[The book]</a>
                    </li>
                    <li>
                      <a epub:type="colophon" href="titlepageback.xhtml">[About this book]</a>
                    </li>
                  </ol>
                </nav>
              </section>
            </body>
          </html>
        </xsl:result-document>

      </xsl:for-each>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="opfmetadata">
    <xsl:param name="author"/>
    <xsl:param name="printAuthor"/>
    <xsl:param name="coverImageOutside"/>
    <metadata xmlns="http://www.idpf.org/2007/opf"
	      xmlns:dc="http://purl.org/dc/elements/1.1/"
	      xmlns:dcterms="http://purl.org/dc/terms/" 
	      xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" 
	      xmlns:opf="http://www.idpf.org/2007/opf">
      <dc:title id="title">
	<xsl:call-template name="generateSimpleTitle"/>
      </dc:title>
      <meta refines="#title" property="title-type">main</meta>
      <dc:creator id="creator">
	<xsl:value-of select="$printAuthor"/>
      </dc:creator>
      <meta refines="#creator" property="file-as">
	<xsl:value-of select="$author"/>
      </meta>
      <meta refines="#creator" property="role" scheme="marc:relators">aut</meta>
      <dc:language>
	<xsl:call-template name="generateLanguage"/>
      </dc:language>
      <xsl:call-template name="generateSubject"/>
      <dc:identifier id="pub-id">
	<xsl:call-template name="generateID"/>
      </dc:identifier>
      <meta refines="#pub-id" property="identifier-type" scheme="onix:codelist5">15</meta>
      <dc:description>
	<xsl:call-template name="generateSimpleTitle"/>
	<xsl:text> / </xsl:text>
	<xsl:value-of select="$author"/>
      </dc:description>
      <dc:publisher>
	<xsl:call-template name="generatePublisher"/>
      </dc:publisher>
      <xsl:for-each select="tei:teiHeader/tei:profileDesc/tei:creation/tei:date[@notAfter]">
	<dc:date id="creation">
	  <xsl:value-of select="@notAfter"/>
	</dc:date>
      </xsl:for-each>
      <xsl:for-each select="tei:teiHeader/tei:fileDesc/tei:sourceDesc//tei:date[@when][1]">
	<dc:date id="original-publication">
	  <xsl:value-of select="@when"/>
	</dc:date>
      </xsl:for-each>
      <dc:date id="epub-publication">
	<xsl:call-template name="generateDate"/>
      </dc:date>
      <dc:rights>
	<xsl:call-template name="generateLicence"/>
      </dc:rights>
      <xsl:if test="not($coverImageOutside='')">
	<meta name="cover" content="cover-image"/>
      </xsl:if>
      <meta property="dcterms:modified">
	<xsl:call-template name="generateRevDate"/>
      </meta>
    </metadata>
  </xsl:template>

  <xsl:template name="epubSpineHook"/>
  <xsl:template name="epubManifestHook"/>
  <xsl:template name="processTEIHook"/>
</xsl:stylesheet>
