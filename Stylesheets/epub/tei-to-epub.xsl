<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:dc="http://purl.org/dc/elements/1.1/"
		xmlns:iso="http://www.iso.org/ns/1.0"
		xmlns="http://www.w3.org/1999/xhtml"
		xmlns:html="http://www.w3.org/1999/xhtml"
		xmlns:tei="http://www.tei-c.org/ns/1.0"
		xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		xmlns:ncx="http://www.daisy.org/z3986/2005/ncx/"
		version="2.0" exclude-result-prefixes="iso tei dc html ncx">
  <xsl:import href="../xhtml2/tei.xsl"/>
  <xsl:output method="xml" encoding="utf-8" indent="yes"/>
  <xsl:key name="GRAPHICS" use="1"
	   match="tei:graphic"/>
  <xsl:key name="PAGEIMAGES" use="1" 
	   match="tei:pb[@facs]"/>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
    <desc>
      <p>
	TEI stylesheet for making ePub output. A lot learnt from
	http://www.hxa.name/articles/content/epub-guide_hxa7241_2007.html and
	the stylesheets of the NZETC.
      </p>
      <p>
	This library is free software; you can redistribute it and/or
	modify it under the terms of the GNU Lesser General Public
	License as published by the Free Software Foundation; either
	version 2.1 of the License, or (at your option) any later version.
	
	This library is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
	Lesser General Public License for more details.
	
	You should have received a copy of the GNU Lesser General Public
	License along with this library; if not, write to the Free Software
	Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
	
      </p>
      <p>Author: See AUTHORS</p>
      <p>Id: $Id$</p>
      <p>Copyright: 2008, TEI Consortium</p>
    </desc>
  </doc>
  <xsl:param name="fixgraphicsurl">false</xsl:param>
  <xsl:param name="doctypePublic">-//W3C//DTD XHTML 1.1//EN</xsl:param>
  <xsl:param name="doctypeSystem">http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd</xsl:param>
  <xsl:param name="directory"></xsl:param>
  <xsl:param name="splitLevel">0</xsl:param>
  <xsl:param name="STDOUT">false</xsl:param>
  <xsl:param name="outputDir">OEBPS</xsl:param>
  <xsl:param name="cssFile">../tei.css</xsl:param>
  <xsl:param name="cssPrintFile">../tei-print.css</xsl:param>
  <xsl:param name="cssODDFile">../odd.css</xsl:param>
  <xsl:param name="topNavigationPanel">false</xsl:param>
  <xsl:param name="bottomNavigationPanel">false</xsl:param>
  <xsl:param name="autoToc">true</xsl:param>
  <xsl:param name="autoHead">true</xsl:param>
  <xsl:param name="tocDepth">3</xsl:param>
  <xsl:param name="linkPanel">false</xsl:param>
  <xsl:param name="institution"/>
  <xsl:param name="subject"/>
  <xsl:param name="uid"/>
  <xsl:param name="publisher"/>
  <xsl:param name="coverimage"/>
  <xsl:param name="odd">false</xsl:param>
  <xsl:param name="debug">false</xsl:param>

  <xsl:variable name="coverimageFile">
    <xsl:choose>
      <xsl:when test="not($coverimage='')">
	<xsl:value-of select="$coverimage"/>
      </xsl:when>
      <xsl:when
	  test="/tei:TEI/tei:text/tei:front/tei:titlePage/@facs">
        <xsl:text>media/imagetitlePage</xsl:text>
	<xsl:for-each select="/tei:TEI/tei:text/tei:front/tei:titlePage[@facs]">
	  <xsl:number level="any"/>
	  <xsl:text>.</xsl:text>
	  <xsl:value-of select="tokenize(@url|@facs,'\.')[last()]"/>
	</xsl:for-each>
      </xsl:when>
    </xsl:choose>
  </xsl:variable>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[epub] Suppress normal page footer      </desc>
   </doc>
  <xsl:template name="stdfooter">
    <xsl:param name="file"/>
  </xsl:template>
					
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[epub] Set licence</desc>
   </doc>
  <xsl:template name="generateLicence">
    <xsl:text>Creative Commons Attribution</xsl:text>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[epub] Set language</desc>
   </doc>
  <xsl:template name="generateLanguage">
    <xsl:choose>
      <xsl:when test="@xml:lang">
	<xsl:value-of select="@xml:lang"/>
      </xsl:when>
      <xsl:when test="tei:text/@xml:lang">
	<xsl:value-of select="tei:text/@xml:lang"/>
      </xsl:when>

      <xsl:otherwise>
	<xsl:text>en</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[epub] Set subject</desc>
   </doc>
  <xsl:template name="generateSubject">
    <xsl:choose>
      <xsl:when test="$subject=''">
	<xsl:text>TEI Text</xsl:text>
      </xsl:when>
      <xsl:otherwise>
	<xsl:value-of select="$subject"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[epub] Set name of publisher</desc>
   </doc>
  <xsl:template name="generatePublisher">
    <xsl:choose>
      <xsl:when test="not($publisher='')">
	<xsl:value-of select="$publisher"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:value-of
	    select="normalize-space(tei:teiHeader/tei:fileDesc/tei:publicationStmt)"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[epub] Set unique identifier for output
      </desc>
   </doc>
  <xsl:template name="generateID">
    <xsl:choose>
      <xsl:when test="$uid=''">
        <xsl:text>http://www.example.com/TEIEPUB/</xsl:text>
        <xsl:value-of select="format-dateTime(current-dateTime(),'[Y][M02][D02][H02][m02][s02]')"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$uid"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[epub] Override addition of CSS links. We force a simple
      name of "stylesheet.css"
      </desc>
   </doc>
  <xsl:template name="includeCSS">
    <link xmlns="http://www.w3.org/1999/xhtml" href="stylesheet.css" rel="stylesheet" type="text/css"/>
    <xsl:if test="not($cssPrintFile='')">
      <link xmlns="http://www.w3.org/1999/xhtml" rel="stylesheet" media="print" type="text/css" href="print.css"/>
    </xsl:if>
    <link xmlns="http://www.w3.org/1999/xhtml" rel="stylesheet" type="application/vnd.adobe-page-template+xml" href="page-template.xpgt"/>
  </xsl:template>

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
      <xsl:apply-templates mode="fixgraphics"/>
    </xsl:variable>
    <xsl:for-each select="$stage1">
      <xsl:apply-templates mode="split"/>
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
	<xsl:if test="$debug='true'">
	  <xsl:message>write file OEBPS/stylesheet.css</xsl:message>
	</xsl:if>
        <xsl:result-document method="text" href="{concat($directory,'OEBPS/stylesheet.css')}">
	<xsl:if test="$debug='true'">
	  <xsl:message>reading file <xsl:value-of select="$cssFile"/></xsl:message>
	</xsl:if>
          <xsl:for-each select="tokenize(unparsed-text($cssFile),
				'\r?\n')">
	    <xsl:call-template name="purgeCSS"/>
          </xsl:for-each>
          <xsl:if test="$odd='true'">
	      <xsl:if test="$debug='true'">
		<xsl:message>reading file <xsl:value-of
		select="$cssODDFile"/></xsl:message>
	      </xsl:if>
            <xsl:for-each select="tokenize(unparsed-text($cssODDFile),         '\r?\n')">
	      <xsl:call-template name="purgeCSS"/>
            </xsl:for-each>
          </xsl:if>
        </xsl:result-document>
	<xsl:if test="$debug='true'">
	  <xsl:message>write file OEBPS/print.css</xsl:message>
	</xsl:if>
        <xsl:result-document method="text" href="{concat($directory,'OEBPS/print.css')}">
	    <xsl:if test="$debug='true'">
	      <xsl:message>reading file <xsl:value-of
	      select="$cssPrintFile"/></xsl:message>
	    </xsl:if>
          <xsl:for-each select="tokenize(unparsed-text($cssPrintFile),
				'\r?\n')">
	    <xsl:call-template name="purgeCSS"/>
          </xsl:for-each>
        </xsl:result-document>
	<xsl:if test="$debug='true'">
	  <xsl:message>write file mimetype</xsl:message>
	</xsl:if>
        <xsl:result-document method="text" href="{concat($directory,'mimetype')}">
          <xsl:text>application/epub+zip</xsl:text>
        </xsl:result-document>
	<xsl:if test="$debug='true'">
	  <xsl:message>write file META-INF/container.xml</xsl:message>
	</xsl:if>
        <xsl:result-document method="xml" href="{concat($directory,'META-INF/container.xml')}">
          <container xmlns="urn:oasis:names:tc:opendocument:xmlns:container" version="1.0">
            <rootfiles>
              <rootfile full-path="OEBPS/content.opf" media-type="application/oebps-package+xml"/>
            </rootfiles>
          </container>
	</xsl:result-document>
	  <xsl:if test="$debug='true'">
	    <xsl:message>write file OEBPS/content.opf</xsl:message>
	</xsl:if>
        <xsl:result-document href="{concat($directory,'OEBPS/content.opf')}" method="xml">
          <package xmlns="http://www.idpf.org/2007/opf" unique-identifier="dcidid" version="2.0">
            <metadata xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:dcterms="http://purl.org/dc/terms/" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:opf="http://www.idpf.org/2007/opf">
              <dc:title>
                <xsl:call-template name="generateSimpleTitle"/>
              </dc:title>
              <dc:language xsi:type="dcterms:RFC3066">
                <xsl:call-template name="generateLanguage"/>
              </dc:language>
	      <dc:subject>
                <xsl:call-template name="generateSubject"/>
	      </dc:subject>
	      <xsl:for-each
		  select="tei:teiHeader/tei:textClass/tei:keywords[@scheme='#lcsh']/tei:term">
		<dc:subject><xsl:value-of select="."/></dc:subject>
	      </xsl:for-each>
              <dc:identifier id="dcidid" opf:scheme="URI">
                <xsl:call-template name="generateID"/>
              </dc:identifier>
              <dc:creator>
                <xsl:call-template name="generateAuthor"/>
              </dc:creator>
              <dc:publisher>
                <xsl:call-template name="generatePublisher"/>
              </dc:publisher>
              <dc:date xsi:type="dcterms:W3CDTF">
                <xsl:call-template name="generateDate"/>
              </dc:date>
              <dc:rights>
                <xsl:call-template name="generateLicence"/>
              </dc:rights>
	      <xsl:if test="not($coverimageFile='')">
		<meta name="cover"  content="cover-image" />
	      </xsl:if>
            </metadata>

	    <manifest>
	      <xsl:if test="not($coverimageFile='')">
		<item href="{$coverimageFile}" id="cover-image" media-type="image/jpeg"/>
	      </xsl:if>
	      <item href="stylesheet.css" id="css" media-type="text/css"/>
	      <item href="titlepage.html" id="titlepage" media-type="application/xhtml+xml"/>	      
	      <item href="titlepageback.html" id="titlepageback" media-type="application/xhtml+xml"/>	      
              <item id="print.css" href="print.css"
		    media-type="text/css"/>
	      <item id="apt" href="page-template.xpgt" media-type="application/adobe-page-template+xml"/>
              <item id="start" href="index.html" media-type="application/xhtml+xml"/>
              <xsl:for-each select="$TOC/html:TOC/html:ul/html:li">
		<xsl:if test="html:a and not(starts-with(html:a/@href,'#'))">
		  <item href="{html:a[1]/@href}" media-type="application/xhtml+xml">
		    <xsl:attribute name="id">
		      <xsl:text>section</xsl:text>
		      <xsl:number count="html:li" level="any"/>
		    </xsl:attribute>
		  </item>
		</xsl:if>
		<xsl:if test="html:ul">
		  <xsl:for-each select="html:ul//html:li[not(contains(html:a/@href,'#'))]">
		    <item href="{html:a[1]/@href}" media-type="application/xhtml+xml">
		      <xsl:attribute name="id">
			<xsl:text>section</xsl:text>
			<xsl:number count="html:li" level="any"/>
		      </xsl:attribute>
		    </item>
		  </xsl:for-each>
		</xsl:if>
              </xsl:for-each>
              <!-- images -->
              <xsl:for-each select="key('GRAPHICS',1)">
                <xsl:variable name="ID">
                  <xsl:number level="any"/>
                </xsl:variable>
                <xsl:variable name="mimetype">
                  <xsl:choose>
                    <xsl:when test="contains(@url,'.gif')">image/gif</xsl:when>
                    <xsl:when test="contains(@url,'.png')">image/png</xsl:when>
                    <xsl:otherwise>image/jpeg</xsl:otherwise>
                  </xsl:choose>
		</xsl:variable>
                <item href="{@url}" id="image-{$ID}" media-type="{$mimetype}"/>
              </xsl:for-each>
              <!-- page images -->
              <xsl:for-each select="key('PAGEIMAGES',1)">
                <xsl:variable name="ID">
		  <xsl:text>pb</xsl:text>
                  <xsl:number level="any"/>
                </xsl:variable>
                <xsl:variable name="mimetype">
                  <xsl:choose>
                    <xsl:when test="contains(@url,'.gif')">image/gif</xsl:when>
                    <xsl:when test="contains(@url,'.png')">image/png</xsl:when>
                    <xsl:otherwise>image/jpeg</xsl:otherwise>
                  </xsl:choose>
		</xsl:variable>
                <item href="{@facs}" id="image-{$ID}" media-type="{$mimetype}"/>
              </xsl:for-each>
              <item id="ncx" href="toc.ncx" media-type="application/x-dtbncx+xml"/>
            </manifest>
            <spine toc="ncx">
              <itemref idref="titlepage"/>
              <itemref idref="start"/>
              <xsl:for-each select="$TOC/html:TOC/html:ul/html:li">
                  <xsl:if test="html:a and not(starts-with(html:a/@href,'#'))">
                    <itemref>
                      <xsl:attribute name="idref">
                        <xsl:text>section</xsl:text>
                        <xsl:number count="html:li" level="any"/>
                      </xsl:attribute>
                    </itemref>
		  </xsl:if>
                  <xsl:if test="html:ul">
                    <xsl:for-each select="html:ul//html:li[not(contains(html:a/@href,'#'))]">
                      <itemref>
                        <xsl:attribute name="idref">
                          <xsl:text>section</xsl:text>
                          <xsl:number count="html:li" level="any"/>
                        </xsl:attribute>
                      </itemref>
                    </xsl:for-each>
		  </xsl:if>
              </xsl:for-each>
              <itemref idref="titlepageback"/>
            </spine>

	    <guide>
	      <reference href="titlepage.html" type="cover"
			 title="Cover"/>
              <reference type="text" title="Start" href="index.html"/>
              <xsl:for-each select="$TOC/html:TOC/html:ul/html:li">
		 
                  <xsl:if test="html:a">
                    <reference type="text" href="{html:a[1]/@href}">
                      <xsl:attribute name="title">
                        <xsl:value-of select="normalize-space(html:a[1])"/>
                      </xsl:attribute>
                    </reference>
		  </xsl:if>
		  <!--
		      <xsl:if test="html:ul">
		      <xsl:for-each select="html:ul//html:li[not(contains(html:a/@href,'#'))]">
                      <reference type="text" href="{html:a/@href}">
		      <xsl:attribute name="title">
		      <xsl:value-of select="normalize-space(html:a[1])"/>
		      </xsl:attribute>
                      </reference>
		      </xsl:for-each>
		      </xsl:if>
		  -->
              </xsl:for-each>
	      <reference href="titlepageback.html" type="text"
			 title="About this book"/>
            </guide>
          </package>
	</xsl:result-document>
	<xsl:if test="$debug='true'">
	  <xsl:message>write file OEBPS/titlepage.html</xsl:message>
	</xsl:if>
        <xsl:result-document href="{concat($directory,'OEBPS/titlepage.html')}" method="xml">
	  <html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en">
	    <head>
	      <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
	      <meta name="calibre:cover" content="true"/>
	      <title>Title page</title>
	      <style type="text/css" title="override_css">
		@page {padding: 0pt; margin:0pt}
		body { text-align: center; padding:0pt; margin: 0pt; }
	      </style>
	    </head>
	    <body>
	      <xsl:choose>
		<xsl:when
		    test="tei:text/tei:front/tei:titlePage">
		  <xsl:apply-templates select="tei:text/tei:front/tei:titlePage"/>
		</xsl:when>
		<xsl:when test="$coverimageFile=''">
		  <div style="font-family: serif; height:860;
			      font-size:36pt; border: bold red 1pt; text-align:center">
		    <xsl:call-template name="generateTitle"/>
		  </div>
		</xsl:when>
		<xsl:otherwise>
		  <div>
		    <img width="600" height="860"
			 alt="cover picture"
			 src="{$coverimageFile}"/>
		  </div>
		</xsl:otherwise>
	      </xsl:choose>
	    </body>
	  </html>
	</xsl:result-document>

	<xsl:if test="$debug='true'">
	  <xsl:message>write file OEBPS/titlepageback.html</xsl:message>
	</xsl:if>
        <xsl:result-document href="{concat($directory,'OEBPS/titlepageback.html')}" method="xml">
	  <html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en">
	    <head>
	      <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
	      <title>About this book</title>
	    </head>
	    <body>
	      <div style="text-align: left; font-size: smaller">
		<h2>Information about this book</h2>
		<xsl:for-each select="/*/tei:teiHeader/tei:fileDesc">
		  <xsl:apply-templates mode="metadata"/>
		</xsl:for-each>
	      </div>
	    </body>
	  </html>
	</xsl:result-document>

	<xsl:if test="$debug='true'">
	  <xsl:message>write file OEBPS/toc.ncx</xsl:message>
	</xsl:if>
        <xsl:result-document href="{concat($directory,'OEBPS/toc.ncx')}" method="xml">
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
		    <text>Title Page and Contents</text>
		  </navLabel>
		  <content src="index.html"/>
		</navPoint>
		<xsl:for-each select="$TOC/html:TOC/html:ul/html:li">
		  <xsl:if test="html:a and not(starts-with(html:a/@href,'#'))">
		    <navPoint>
		      <navLabel>
			<text>
			  <xsl:value-of select="html:span[@class='headingNumber']"/>
			  <xsl:value-of select="normalize-space(html:a[1])"/>
			</text>
		      </navLabel>
		      <content src="{html:a/@href}"/>
		    </navPoint>
		  </xsl:if>
		  <!--		<xsl:if test="html:ul">
                    <xsl:for-each select="html:ul/html:li">
		    <xsl:variable name="pos">
		    <xsl:number level="any"/>
		    </xsl:variable>
		    <navPoint id="navPoint-{$pos+1}" playOrder="{$pos+1}">
		    <navLabel>
		    <text>
		    <xsl:value-of select="normalize-space(html:a[1])"/>
		    </text>
		    </navLabel>
		    <content src="{html:a/@href}"/>
		    </navPoint>
                    </xsl:for-each>
		    </xsl:if>
		-->
		</xsl:for-each>
		<navPoint>
		  <navLabel>
		    <text>About this book</text>
		  </navLabel>
		  <content src="titlepageback.html"/>
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
	  <xsl:if test="$debug='true'">
	    <xsl:message>write file OEBPS/page-template.xpgt</xsl:message>
	  </xsl:if>
        <xsl:result-document method="xml" href="{concat($directory,'OEBPS/page-template.xpgt')}">
          <ade:template xmlns="http://www.w3.org/1999/xhtml" xmlns:ade="http://ns.adobe.com/2006/ade" xmlns:fo="http://www.w3.org/1999/XSL/Format">
            <fo:layout-master-set>
              <fo:simple-page-master master-name="single_column">
                <fo:region-body margin-bottom="3pt" margin-top="0.5em" margin-left="3pt" margin-right="3pt"/>
              </fo:simple-page-master>
              <fo:simple-page-master master-name="single_column_head">
                <fo:region-before extent="8.3em"/>
                <fo:region-body margin-bottom="3pt" margin-top="6em" margin-left="3pt" margin-right="3pt"/>
              </fo:simple-page-master>
              <fo:simple-page-master master-name="two_column" margin-bottom="0.5em" margin-top="0.5em" margin-left="0.5em" margin-right="0.5em">
                <fo:region-body column-count="2" column-gap="10pt"/>
              </fo:simple-page-master>
              <fo:simple-page-master master-name="two_column_head" margin-bottom="0.5em" margin-left="0.5em" margin-right="0.5em">
                <fo:region-before extent="8.3em"/>
                <fo:region-body column-count="2" margin-top="6em" column-gap="10pt"/>
              </fo:simple-page-master>
              <fo:simple-page-master master-name="three_column" margin-bottom="0.5em" margin-top="0.5em" margin-left="0.5em" margin-right="0.5em">
                <fo:region-body column-count="3" column-gap="10pt"/>
              </fo:simple-page-master>
              <fo:simple-page-master master-name="three_column_head" margin-bottom="0.5em" margin-top="0.5em" margin-left="0.5em" margin-right="0.5em">
                <fo:region-before extent="8.3em"/>
                <fo:region-body column-count="3" margin-top="6em" column-gap="10pt"/>
              </fo:simple-page-master>
              <fo:page-sequence-master>
                <fo:repeatable-page-master-alternatives>
                  <fo:conditional-page-master-reference master-reference="three_column_head" page-position="first" ade:min-page-width="80em"/>
                  <fo:conditional-page-master-reference master-reference="three_column" ade:min-page-width="80em"/>
                  <fo:conditional-page-master-reference master-reference="two_column_head" page-position="first" ade:min-page-width="50em"/>
                  <fo:conditional-page-master-reference master-reference="two_column" ade:min-page-width="50em"/>
                  <fo:conditional-page-master-reference master-reference="single_column_head" page-position="first"/>
                  <fo:conditional-page-master-reference master-reference="single_column"/>
                </fo:repeatable-page-master-alternatives>
              </fo:page-sequence-master>
            </fo:layout-master-set>
            <ade:style>
              <ade:styling-rule selector=".title_box" display="adobe-other-region" adobe-region="xsl-region-before"/>
            </ade:style>
          </ade:template>
        </xsl:result-document>
      </xsl:for-each>
    </xsl:for-each>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[epub] Add specific linebreak in verbatim output, as
      readers do not seem to grok the CSS
      </desc>
   </doc>
  <xsl:template name="verbatim-lineBreak">
    <xsl:param name="id"/>
    <br/>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[epub] Local mode to rewrite names of graphics inclusions;
      default is identity transform
      </desc>
   </doc>
  <xsl:template match="@*|text()|comment()|processing-instruction()" mode="fixgraphics">
    <xsl:copy-of select="."/>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[epub] Local mode to rewrite names of graphics inclusions;
      default is identifty transform
      </desc>
   </doc>
  <xsl:template match="*" mode="fixgraphics">
    <xsl:copy>
      <xsl:apply-templates
	  select="*|@*|processing-instruction()|comment()|text()" mode="fixgraphics"/>
    </xsl:copy>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[epub] Local mode to rewrite names of graphics inclusions
      </desc>
   </doc>
  <xsl:template match="tei:titlePage[@facs]|tei:pb[@facs]"
		mode="fixgraphics">
    <xsl:copy>
      <xsl:variable name="newName">
        <xsl:text>media/image</xsl:text>
	<xsl:if test="@facs">
	  <xsl:value-of select="local-name()"/>
	</xsl:if>
        <xsl:number level="any"/>
        <xsl:text>.</xsl:text>
        <xsl:value-of select="tokenize(@url|@facs,'\.')[last()]"/>
      </xsl:variable>
      <xsl:attribute name="facs">
	<xsl:value-of select="$newName"/>
      </xsl:attribute>
      <xsl:copy-of select="@n"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="tei:graphic" mode="fixgraphics">
    <xsl:copy>
      <xsl:choose>
	<xsl:when test="$fixgraphicsurl='true'">
	  <xsl:variable name="newName">
	    <xsl:text>media/image</xsl:text>
	    <xsl:if test="self::tei:pb">pb</xsl:if>
	    <xsl:number level="any"/>
	    <xsl:text>.</xsl:text>
	    <xsl:value-of select="tokenize(@url|@facs,'\.')[last()]"/>
	  </xsl:variable>
	  <xsl:attribute name="url">
	    <xsl:value-of select="$newName"/>
	  </xsl:attribute>
	  <xsl:copy-of select="@n"/>
	  <xsl:copy-of select="@height"/>
	  <xsl:copy-of select="@width"/>
	  <xsl:copy-of select="@scale"/>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:copy-of select="@*"/>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:copy>
  </xsl:template>
  
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[epub] Remove unwanted things from CSS
      </desc>
   </doc>
  <xsl:template name="purgeCSS">
    <xsl:choose>
      <xsl:when test="contains(.,'line-height:')"/>
      <xsl:when test="contains(.,'max-width:')"/>
      <xsl:when test="contains(.,'height:')"/>
      <xsl:when test="contains(.,'text-indent:')"/>
<!--
      <xsl:when test="contains(.,'clear:')"/>
      <xsl:when test="contains(.,'padding')"/>
      <xsl:when test="contains(.,'float:')"/>
      <xsl:when test="contains(.,'font-size:')"/>
      <xsl:when test="contains(.,'width:')"/>
      <xsl:when test="contains(.,'margin')"/>
      <xsl:when test="contains(.,'border')"/>
-->
      <xsl:otherwise>
	<xsl:value-of select="."/>
	<xsl:text>&#10;</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="tei:div/tei:bibl">
    <div class="biblfree">
      <xsl:apply-templates/>
    </div>
  </xsl:template>

  <xsl:template match="tei:l">
    <div class="l">
      <xsl:choose>
	<xsl:when test="ancestor::tei:div[contains(@rend,'linenumber')]">
	  <xsl:variable name="n">
	    <xsl:number/>
	  </xsl:variable>
	  <div class="linenumber">
	    <xsl:choose>
	      <xsl:when test="$n mod 5 = 0">
		<xsl:value-of select="$n"/>
	      </xsl:when>
	      <xsl:otherwise>&#160;</xsl:otherwise>
	    </xsl:choose>
	  </div>
	  <xsl:apply-templates/>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:apply-templates/>
	</xsl:otherwise>
      </xsl:choose>
    </div>
  </xsl:template>

  <xsl:template name="addLangAtt"/>

  <xsl:template match="tei:pb[@facs]">
    <xsl:choose>
      <xsl:when test="parent::tei:div | parent::tei:body">
	<div><img src="{@facs}" alt="page image"/></div>
      </xsl:when>
      <xsl:otherwise>
	<img width="600" height="860" src="{@facs}" alt="page image"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="tei:lb[@rend='space']">
    <xsl:text> </xsl:text>
  </xsl:template>

  <xsl:template match="tei:titleStmt" mode="metadata">
    <h3>Title</h3>
    <xsl:apply-templates mode="metadata"/>
</xsl:template>

  <xsl:template match="tei:editionStmt" mode="metadata">
    <h3>Edition</h3>
    <xsl:apply-templates mode="metadata"/>
</xsl:template>

  <xsl:template match="tei:publicationStmt" mode="metadata">
    <h3>Publication</h3>
    <xsl:choose>
      <xsl:when test="tei:p">
	<xsl:apply-templates/>
      </xsl:when>
      <xsl:otherwise>
	<dl>
	  <xsl:apply-templates mode="metadata"/>
	</dl>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
  <xsl:template match="tei:seriesStmt" mode="metadata">
    <h3>Series</h3>
    <xsl:apply-templates mode="metadata"/>
  </xsl:template>
  
  <xsl:template match="tei:notesStmt" mode="metadata">
    <h3>Notes</h3>
    <xsl:apply-templates mode="metadata"/>
  </xsl:template>

  <xsl:template match="tei:sourceDesc" mode="metadata">
    <h3>Source</h3>
    <xsl:apply-templates mode="metadata"/>
  </xsl:template>

  <xsl:template match="tei:sourceDesc/tei:bibl" mode="metadata">
    <p> — <xsl:apply-templates mode="metadata"/></p>
  </xsl:template>

  <xsl:template match="tei:respStmt" mode="metadata">
    <p><i><xsl:value-of select="tei:resp"/></i>:
      <xsl:value-of select="tei:name"/>
    </p>
  </xsl:template>

  <xsl:template match="tei:relatedItem[@target]" mode="metadata">
    <a href="{@target}"><xsl:value-of select="@target"/></a>
</xsl:template>

  <xsl:template match="tei:extent" mode="metadata"/>

  <xsl:template match="tei:authority" mode="metadata">
    <dt>Authority</dt>
    <dd>
    <xsl:apply-templates/>
    </dd>
  </xsl:template>

  <xsl:template match="tei:publicationStmt/tei:address" mode="metadata">
    <dt>Address</dt>
    <dd>
    <xsl:apply-templates/>
    </dd>
  </xsl:template>

  <xsl:template match="tei:publicationStmt/tei:publisher" mode="metadata">
    <dt>Publisher</dt>
    <dd>
    <xsl:apply-templates/>
    </dd>
  </xsl:template>

  <xsl:template match="tei:distributor" mode="metadata">
    <dt>Distributor</dt>
    <dd>
    <xsl:apply-templates/>
    </dd>
  </xsl:template>

  <xsl:template match="tei:idno" mode="metadata">
    <dt>ID [<xsl:value-of select="@type|@iso:meta"/>]</dt>
    <dd>
    <xsl:apply-templates/>
    </dd>
  </xsl:template>

  <xsl:template match="tei:availability[not(@n) and preceding-sibling::tei:availability/@n]" mode="metadata"/>

  <xsl:template match="tei:availability" mode="metadata">
    <dt>Availability</dt>
    <dd>
    <xsl:for-each select="tei:p">
      <xsl:apply-templates/>
    </xsl:for-each>
    </dd>
  </xsl:template>

  <xsl:template match="tei:bibl/tei:title" mode="metadata">
    <i>
    <xsl:apply-templates/>
    </i>
  </xsl:template>

  <xsl:template match="tei:date" mode="metadata">
    <dt>Date</dt>
    <dd>
    <xsl:apply-templates/>
    </dd>
  </xsl:template>

  <xsl:template match="*" mode="metadata">
    <p>
      <xsl:apply-templates/>
    </p>
  </xsl:template>

  <xsl:template match="tei:seriesStmt/tei:p">
      <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="tei:distributor/tei:address">
      <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="tei:authority/tei:address">
      <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="tei:authority/tei:addLine">
      <xsl:apply-templates/><br/>
  </xsl:template>

  <xsl:template match="tei:title[@type='uniform']"/>

  <xsl:template match="tei:editionStmt/tei:p">
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="tei:editor">
    <xsl:apply-templates/>
    <xsl:text> (editor)</xsl:text>
  </xsl:template>

  <xsl:template match="tei:title[@type='main']">
    <i>
      <xsl:apply-templates/>
    </i>
  </xsl:template>

  <xsl:template match="tei:title[@type='alternative']">
    <xsl:apply-templates/>
    <xsl:text> (alternative title)</xsl:text>
  </xsl:template>
 </xsl:stylesheet>
