<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet                 xmlns:dc="http://purl.org/dc/elements/1.1/"
                xmlns:html="http://www.w3.org/1999/xhtml"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="2.0"
                exclude-result-prefixes="tei dc html">

  <xsl:import href="../xhtml2/tei.xsl"/>

  <xsl:output method="xml" encoding="utf-8" indent="yes"/>


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
         <p>Id: $Id: tei.xsl 7025 2009-11-29 19:47:02Z rahtz $</p>
         <p>Copyright: 2008, TEI Consortium</p>
      </desc>
   </doc>


  <xsl:param name="splitLevel">0</xsl:param>
  <xsl:param name="STDOUT">false</xsl:param>
  <xsl:param name="outputDir">OEBPS</xsl:param>
  <xsl:param name="cssFile">stylesheet.css</xsl:param>
  <xsl:param name="cssPrintFile">print.css</xsl:param>
  <xsl:param name="topNavigationPanel">false</xsl:param>
  <xsl:param name="bottomNavigationPanel">false</xsl:param>
  <xsl:param name="autoToc">false</xsl:param>
  <xsl:param name="tocDepth">1</xsl:param>
  <xsl:param name="linkPanel">false</xsl:param>

  <xsl:template name="generateLicence">
    <xsl:text>Creative Commons Attribution</xsl:text>
  </xsl:template>

  <xsl:template name="generateLanguage">
    <xsl:text>en</xsl:text>
  </xsl:template>

  <xsl:template name="generatePublisher">
    <xsl:text>TEI</xsl:text>
  </xsl:template>

  <xsl:template name="generateID">
    <xsl:text>1</xsl:text>
  </xsl:template>

  <xsl:template name="generateLocalCSS">
    <link xmlns="http://www.w3.org/1999/xhtml" rel="stylesheet" type="application/vnd.adobe-page-template+xml" 
	href="page-template.xpgt"/>
  </xsl:template>

  <xsl:template match="/">

    <xsl:apply-templates mode="split"/>

    <xsl:for-each select="*">

    <xsl:variable name="TOC">
      <TOC xmlns="http://www.w3.org/1999/xhtml">
	<xsl:call-template name="mainTOC"/>
      </TOC>
    </xsl:variable>

    <xsl:result-document method="text" href="OEBPS/stylesheet.css">
      <xsl:for-each
	  select="tokenize(unparsed-text('../tei.css'),
		  '\r?\n')">
	<xsl:if test="not(contains(.,'clear:'))">
	  <xsl:value-of select="."/>
	</xsl:if>
      </xsl:for-each>
    </xsl:result-document>

    <xsl:result-document method="text" href="OEBPS/print.css">
      <xsl:for-each
	  select="tokenize(unparsed-text('../tei-print.css'),
		  '\r?\n')">
	<xsl:if test="not(contains(.,'clear:'))">
	  <xsl:value-of select="."/>
	</xsl:if>
      </xsl:for-each>
    </xsl:result-document>

    <xsl:result-document method="text" href="mimetype">
      <xsl:text>application/epub+zip</xsl:text>
    </xsl:result-document>

    <xsl:result-document method="xml" href="META-INF/container.xml">
      <container version="1.0" xmlns="urn:oasis:names:tc:opendocument:xmlns:container">
	<rootfiles>
	  <rootfile full-path="OEBPS/content.opf" media-type="application/oebps-package+xml"/>
	</rootfiles>
      </container>
    </xsl:result-document>

    <xsl:result-document href="OEBPS/content.opf" method="xml">
        <package xmlns="http://www.idpf.org/2007/opf" unique-identifier="dcidid" 
                 version="2.0">
          
          <metadata xmlns:dc="http://purl.org/dc/elements/1.1/"
                    xmlns:dcterms="http://purl.org/dc/terms/"
                    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
                    xmlns:opf="http://www.idpf.org/2007/opf">
            <dc:title>
	      <xsl:call-template name="generateTitle"/>
	    </dc:title>

            <dc:language xsi:type="dcterms:RFC3066">
	      <xsl:call-template name="generateLanguage"/>
	    </dc:language>

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
          </metadata>
          
          <manifest>
            <item id="stylesheet.css"      
                  href="stylesheet.css"           
                  media-type="text/css" />
            <item id="print.css"      
                  href="print.css"           
                  media-type="text/css" />
            <item id="head"      
                  href="index.html"           
                  media-type="application/xhtml+xml" />
	    <xsl:for-each select="$TOC/html:TOC/html:ul/html:li">
	      <item
                  href="{html:a/@href}"           
                  media-type="application/xhtml+xml" >
		<xsl:attribute name="id">
		  <xsl:text>section</xsl:text>
		  <xsl:number count="html:li"
                     level="any"/>
		</xsl:attribute>
	      </item>
	    </xsl:for-each>
            <item id="ncx" href="toc.ncx"                 
                  media-type="application/x-dtbncx+xml" />
          </manifest>
          
          <spine toc="ncx">
            <itemref idref="head" />
	    <xsl:for-each select="$TOC/html:TOC/html:ul/html:li">
              <itemref>
		<xsl:attribute name="idref">
		  <xsl:text>section</xsl:text>
		  <xsl:number count="html:li"

                     level="any"/>
		</xsl:attribute>
	      </itemref>
            </xsl:for-each>
          </spine>

          <guide>
            <reference type="text"       
                       title="Text"              
                       href="index.html" />
            
	    <xsl:for-each select="$TOC/html:TOC/html:ul/html:li">
              <reference type="text"       
                         title="{normalize-space(.)}"              
                         href="{html:a/@href}" />
            </xsl:for-each>
          </guide>
        </package>
      </xsl:result-document>
      
      
      <!-- daisybook ncx table of contents -->

      <xsl:result-document href="OEBPS/toc.ncx"  method="xml">
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
	      <xsl:call-template name="generateTitle"/>
	    </text>
          </docTitle>

          <navMap>
            <navPoint id="navPoint-1" playOrder="1">
              <navLabel>
                <text>Titlepage</text>
              </navLabel>
              <content src="index.html"/>
            </navPoint>
	    <xsl:for-each select="$TOC/html:TOC/html:ul/html:li">
	      <navPoint id="navPoint-{position()}" playOrder="{position()+1}">
                <navLabel>
                  <text>
		    <xsl:value-of select="normalize-space(.)"/>
                  </text>
                </navLabel>
                <content src="{html:a/@href}"/>
              </navPoint>
            </xsl:for-each>

          </navMap>
        </ncx>
      </xsl:result-document>



    <xsl:result-document method="xml" href="OEBPS/page-template.xpgt">
      <ade:template xmlns="http://www.w3.org/1999/xhtml" xmlns:ade="http://ns.adobe.com/2006/ade"
		    xmlns:fo="http://www.w3.org/1999/XSL/Format">
	
	<fo:layout-master-set>
	  <fo:simple-page-master master-name="single_column">
	    <fo:region-body margin-bottom="3pt" margin-top="0.5em" margin-left="3pt" margin-right="3pt"/>
	  </fo:simple-page-master>
	  
	  <fo:simple-page-master master-name="single_column_head">
	    <fo:region-before extent="8.3em"/>
	    <fo:region-body margin-bottom="3pt" margin-top="6em" margin-left="3pt" margin-right="3pt"/>
	  </fo:simple-page-master>
	  
	  <fo:simple-page-master master-name="two_column"	margin-bottom="0.5em" margin-top="0.5em" margin-left="0.5em" margin-right="0.5em">
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
	      <fo:conditional-page-master-reference master-reference="single_column_head" page-position="first" />
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

    </xsl:template>

</xsl:stylesheet>