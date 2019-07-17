<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:teidocx="http://www.tei-c.org/ns/teidocx/1.0" xmlns:chart="urn:oasis:names:tc:opendocument:xmlns:chart:1.0" xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:dom="http://www.w3.org/2001/xml-events" xmlns:dr3d="urn:oasis:names:tc:opendocument:xmlns:dr3d:1.0" xmlns:draw="urn:oasis:names:tc:opendocument:xmlns:drawing:1.0" xmlns:fo="urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0" xmlns:form="urn:oasis:names:tc:opendocument:xmlns:form:1.0" xmlns:math="http://www.w3.org/1998/Math/MathML" xmlns:meta="urn:oasis:names:tc:opendocument:xmlns:meta:1.0" xmlns:number="urn:oasis:names:tc:opendocument:xmlns:datastyle:1.0" xmlns:office="urn:oasis:names:tc:opendocument:xmlns:office:1.0" xmlns:m="http://www.w3.org/1998/Math/MathML" xmlns:atom="http://www.w3.org/2005/Atom" xmlns:estr="http://exslt.org/strings" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:xhtml="http://www.w3.org/1999/xhtml" xmlns:dbk="http://docbook.org/ns/docbook" xmlns:rng="http://relaxng.org/ns/structure/1.0" xmlns:ooo="http://openoffice.org/2004/office" xmlns:oooc="http://openoffice.org/2004/calc" xmlns:ooow="http://openoffice.org/2004/writer" xmlns:script="urn:oasis:names:tc:opendocument:xmlns:script:1.0" xmlns:style="urn:oasis:names:tc:opendocument:xmlns:style:1.0" xmlns:svg="urn:oasis:names:tc:opendocument:xmlns:svg-compatible:1.0" xmlns:table="urn:oasis:names:tc:opendocument:xmlns:table:1.0" xmlns:text="urn:oasis:names:tc:opendocument:xmlns:text:1.0" xmlns:xforms="http://www.w3.org/2002/xforms" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:tei="http://www.tei-c.org/ns/1.0" xmlns:teix="http://www.tei-c.org/ns/Examples" version="2.0" office:version="1.0">
  <xsl:import href="../common/common.xsl"/>
  <xsl:import href="../common/verbatim.xsl"/>
  <xsl:param name="useFixedDate">false</xsl:param>
  <xsl:param name="debug">false</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
    <desc>
      <p> TEI stylesheet for making OpenOffice files from TEI XML.
	 Originally derived from the OpenOffice /Docbook
	 conversion, but largely rewritten </p>
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
 <xsl:strip-space elements="tei:additional tei:address tei:adminInfo
			    tei:altGrp tei:altIdentifier tei:analytic
			    tei:app tei:appInfo tei:application
			    tei:arc tei:argument tei:attDef
			    tei:attList tei:availability tei:back
			    tei:biblFull tei:biblStruct tei:bicond
			    tei:binding tei:bindingDesc tei:body
			    tei:broadcast tei:cRefPattern tei:calendar
			    tei:calendarDesc tei:castGroup
			    tei:castList tei:category tei:certainty
			    tei:char tei:charDecl tei:charProp
			    tei:choice tei:cit tei:classDecl
			    tei:classSpec tei:classes tei:climate
			    tei:cond tei:constraintSpec tei:correction
			    tei:custodialHist tei:decoDesc
			    tei:dimensions tei:div tei:div1 tei:div2
			    tei:div3 tei:div4 tei:div5 tei:div6
			    tei:div7 tei:divGen tei:docTitle tei:eLeaf
			    tei:eTree tei:editionStmt
			    tei:editorialDecl tei:elementSpec
			    tei:encodingDesc tei:entry tei:epigraph
			    tei:epilogue tei:equipment tei:event
			    tei:exemplum tei:fDecl tei:fLib
			    tei:facsimile tei:figure tei:fileDesc
			    tei:floatingText tei:forest tei:front
			    tei:fs tei:fsConstraints tei:fsDecl
			    tei:fsdDecl tei:fvLib tei:gap tei:glyph
			    tei:graph tei:graphic tei:group
			    tei:handDesc tei:handNotes tei:history
			    tei:hom tei:hyphenation tei:iNode tei:if
			    tei:imprint tei:incident tei:index
			    tei:interpGrp tei:interpretation tei:join
			    tei:joinGrp tei:keywords tei:kinesic
			    tei:langKnowledge tei:langUsage
			    tei:layoutDesc tei:leaf tei:lg tei:linkGrp
			    tei:list tei:listBibl tei:listChange
			    tei:listEvent tei:listForest tei:listNym
			    tei:listOrg tei:listPerson tei:listPlace
			    tei:listRef tei:listRelation
			    tei:listTranspose tei:listWit tei:location
			    tei:locusGrp tei:macroSpec tei:metDecl
			    tei:moduleRef tei:moduleSpec tei:monogr
			    tei:msContents tei:msDesc tei:msIdentifier
			    tei:msItem tei:msItemStruct tei:msPart
			    tei:namespace tei:node tei:normalization
			    tei:notatedMusic tei:notesStmt tei:nym
			    tei:objectDesc tei:org tei:particDesc
			    tei:performance tei:person tei:personGrp
			    tei:physDesc tei:place tei:population
			    tei:postscript tei:precision
			    tei:profileDesc tei:projectDesc
			    tei:prologue tei:publicationStmt
			    tei:quotation tei:rdgGrp tei:recordHist
			    tei:recording tei:recordingStmt
			    tei:refsDecl tei:relatedItem tei:relation
			    tei:relationGrp tei:remarks tei:respStmt
			    tei:respons tei:revisionDesc tei:root
			    tei:row tei:samplingDecl tei:schemaSpec
			    tei:scriptDesc tei:scriptStmt tei:seal
			    tei:sealDesc tei:segmentation
			    tei:seriesStmt tei:set tei:setting
			    tei:settingDesc tei:sourceDesc
			    tei:sourceDoc tei:sp tei:spGrp tei:space
			    tei:spanGrp tei:specGrp tei:specList
			    tei:state tei:stdVals tei:subst
			    tei:substJoin tei:superEntry
			    tei:supportDesc tei:surface tei:surfaceGrp
			    tei:table tei:tagsDecl tei:taxonomy
			    tei:teiCorpus tei:teiHeader tei:terrain
			    tei:text tei:textClass tei:textDesc
			    tei:timeline tei:titlePage tei:titleStmt
			    tei:trait tei:transpose tei:tree
			    tei:triangle tei:typeDesc tei:vAlt
			    tei:vColl tei:vDefault tei:vLabel
			    tei:vMerge tei:vNot tei:vRange tei:valItem
			    tei:valList tei:vocal teix:* rng:* xsl:* xhtml:* atom:* m:*"/>
  <xsl:key name="W" match="image" use="@url"/>
  <xsl:key name="H" match="image" use="@url"/>
  <xsl:output method="xml" omit-xml-declaration="no"/>
  <xsl:decimal-format name="staff" digit="D"/>
  <xsl:variable name="doc_type">TEI</xsl:variable>
  <xsl:param name="useHeaderFrontMatter">false</xsl:param>
  <xsl:param name="postQuote">’</xsl:param>
  <xsl:param name="preQuote">‘</xsl:param>
  <xsl:param name="outputDir">.</xsl:param>
  <xsl:param name="freestanding">false</xsl:param>
  <xsl:param name="outputTarget">odt</xsl:param>
  <xsl:param name="urlMarkup"/>
  <xsl:param name="linkElement">a</xsl:param>
  <xsl:param name="linkAttribute">href</xsl:param>
  <xsl:param name="linkElementNamespace">urn:oasis:names:tc:opendocument:xmlns:text:1.0</xsl:param>
  <xsl:param name="linkAttributeNamespace">http://www.w3.org/1999/xlink</xsl:param>
  <xsl:key name="IDS" match="tei:*[@xml:id]" use="@xml:id"/>
  <xsl:key name="GRAPHICS" match="tei:graphic" use="1"/>
  <xsl:key name="GRAPHICS" match="tei:media" use="1"/>
  <xsl:key name="PB" match="tei:pb[@facs]" use="1"/>
  <xsl:key name="Page" match="style:page-layout-properties" use="1"/>
  <xsl:template match="/">
    <xsl:choose>
      <xsl:when test="$freestanding='true'">
        <xsl:result-document href="{concat($outputDir,'/meta.xml')}" exclude-result-prefixes="#all">
          <xsl:call-template name="META"/>
        </xsl:result-document>
        <office:document-content office:version="1.2">
          <xsl:if test="$freestanding='true'">
            <xsl:for-each select="document(concat($outputDir,'/content.xml'))/office:document-content">
              <xsl:copy-of select="office:scripts"/>
              <xsl:copy-of select="office:font-face-decls"/>
              <xsl:copy-of select="office:automatic-styles"/>
            </xsl:for-each>
          </xsl:if>
          <office:body>
            <office:text>
              <xsl:apply-templates/>
            </office:text>
          </office:body>
        </office:document-content>
      </xsl:when>
      <xsl:otherwise>
        <office:document>
          <xsl:call-template name="META"/>
          <office:body>
            <office:text>
              <xsl:apply-templates/>
            </office:text>
          </office:body>
        </office:document>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:result-document href="{concat($outputDir,'/META-INF/manifest.xml')}">
      <manifest:manifest xmlns:manifest="urn:oasis:names:tc:opendocument:xmlns:manifest:1.0" manifest:version="1.2">
        <manifest:file-entry manifest:media-type="application/vnd.oasis.opendocument.text" manifest:version="1.2" manifest:full-path="/"/>
        <manifest:file-entry manifest:media-type="" manifest:full-path="Configurations2/accelerator/current.xml"/>
        <manifest:file-entry manifest:media-type="application/vnd.sun.xml.ui.configuration" manifest:full-path="Configurations2/"/>
        <manifest:file-entry manifest:media-type="text/xml" manifest:full-path="content.xml"/>
        <manifest:file-entry manifest:media-type="application/rdf+xml" manifest:full-path="manifest.rdf"/>
        <manifest:file-entry manifest:media-type="text/xml" manifest:full-path="styles.xml"/>
        <manifest:file-entry manifest:media-type="text/xml" manifest:full-path="meta.xml"/>
        <manifest:file-entry manifest:media-type="" manifest:full-path="Thumbnails/thumbnail.png"/>
        <manifest:file-entry manifest:media-type="text/xml" manifest:full-path="settings.xml"/>
        <xsl:if test="count(key('GRAPHICS',1))&gt;0">
          <xsl:for-each select="key('GRAPHICS',1)">
            <manifest:file-entry>
            <xsl:variable name="imagetype" select="tokenize(@url,'\.')[last()]"/>
              <xsl:attribute name="manifest:full-path">
                <xsl:text>Pictures/resource</xsl:text>
                <xsl:number level="any"/>
                <xsl:text>.</xsl:text>
                <xsl:value-of select="$imagetype"/>
              </xsl:attribute>
              <xsl:attribute name="manifest:media-type">
		<xsl:value-of  select="tei:generateMimeType(@url,@mimeType)"/>
              </xsl:attribute>
            </manifest:file-entry>
          </xsl:for-each>
        </xsl:if>
        <xsl:if test="count(key('PB',1))&gt;0">
          <xsl:for-each select="key('PB',1)">
            <manifest:file-entry>
              <xsl:variable name="imagetype" select="tokenize(@facs,'\.')[last()]"/>
              <xsl:attribute name="manifest:full-path">
                <xsl:text>Pictures/pageimage</xsl:text>
                <xsl:number level="any"/>
                <xsl:text>.</xsl:text>
                <xsl:value-of select="$imagetype"/>
              </xsl:attribute>
              <xsl:attribute name="manifest:media-type">
                <xsl:value-of  select="tei:generateMimeType(@facs,@mimeType)"/>
              </xsl:attribute>
            </manifest:file-entry>
          </xsl:for-each>
        </xsl:if>
      </manifest:manifest>
    </xsl:result-document>
  </xsl:template>
  <xsl:template name="META">
    <xsl:for-each select="*">
      <office:document-meta office:version="1.2">
	<office:meta>
        <meta:generator>TEI to OpenOffice XSLT</meta:generator>
        <dc:title>
          <xsl:sequence select="tei:generateMetadataTitle(.)"/>
        </dc:title>
        <dc:description/>
        <dc:subject/>
        <meta:creation-date>
          <xsl:sequence select="tei:generateRevDate(.)"/>
        </meta:creation-date>
        <dc:date>
          <xsl:sequence select="tei:generateRevDate(.)"/>
        </dc:date>
        <dc:language>
          <xsl:choose>
            <xsl:when test="/tei:TEI/@xml:lang">
              <xsl:value-of select="/tei:TEI/@xml:lang"/>
            </xsl:when>
            <xsl:otherwise>en</xsl:otherwise>
          </xsl:choose>
        </dc:language>
        <meta:editing-cycles>1</meta:editing-cycles>
        <meta:editing-duration>PT00H00M00S</meta:editing-duration>
        <meta:user-defined meta:name="Info 1"/>
        <meta:user-defined meta:name="Info 2"/>
        <meta:user-defined meta:name="Info 3"/>
        <meta:user-defined meta:name="Info 4"/>
        <meta:document-statistic meta:table-count="1" meta:image-count="0" meta:object-count="0" meta:page-count="1" meta:paragraph-count="42" meta:word-count="144" meta:character-count="820"/>
	</office:meta>
      </office:document-meta>
    </xsl:for-each>
  </xsl:template>
  <!-- base structure -->
  <xsl:template match="tei:TEI">
    <xsl:apply-templates select="tei:text"/>
  </xsl:template>
  <xsl:template match="tei:body|tei:front|tei:back">
    <xsl:apply-templates/>
  </xsl:template>
  <xsl:template match="tei:head">
    <xsl:choose>
      <xsl:when test="parent::tei:figure"/>
      <xsl:when test="parent::tei:list"/>
      <xsl:when test="parent::tei:div"/>
      <xsl:when test="parent::tei:div1"/>
      <xsl:when test="parent::tei:div2"/>
      <xsl:when test="parent::tei:div3"/>
      <xsl:when test="parent::tei:div4"/>
      <xsl:when test="parent::tei:div5"/>
      <xsl:when test="parent::tei:div6"/>
      <xsl:when test="parent::tei:table"/>
      <xsl:otherwise>
        <xsl:choose>
          <xsl:when test="parent::tei:appendix">
            <text:p text:style-name="Appendix">
              <xsl:apply-templates/>
            </text:p>
          </xsl:when>
          <xsl:when test="parent::tei:body">
            <text:h text:outline-level="1">
              <xsl:apply-templates/>
            </text:h>
          </xsl:when>
          <xsl:otherwise>
            <text:p>
              <xsl:apply-templates/>
            </text:p>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="tei:div">
    <xsl:variable name="depth">
      <xsl:value-of select="count(ancestor::tei:div)"/>
    </xsl:variable>
    <text:h>
      <xsl:attribute name="text:outline-level">
        <xsl:value-of select="$depth + 1"/>
      </xsl:attribute>
      <xsl:call-template name="test.id"/>
      <xsl:apply-templates select="tei:head" mode="show"/>
    </text:h>
    <xsl:apply-templates/>
  </xsl:template>
  <xsl:template match="tei:div1|tei:div2|tei:div3|tei:div4|tei:div5|tei:div6">
    <xsl:variable name="depth">
      <xsl:value-of select="substring-after(name(.),'div')"/>
    </xsl:variable>
    <text:h>
      <xsl:attribute name="text:outline-level">
        <xsl:value-of select="$depth + 1"/>
      </xsl:attribute>
      <xsl:call-template name="test.id"/>
      <xsl:apply-templates select="tei:head" mode="show"/>
    </text:h>
    <xsl:apply-templates/>
  </xsl:template>
  <!-- paragraphs -->
  <xsl:template match="tei:pb">
    <text:soft-page-break/>
  </xsl:template>
  <xsl:template match="tei:p">
    <xsl:variable name="style">
      <xsl:choose>
        <xsl:when test="ancestor::tei:note[@place='foot']">
          <xsl:text>Footnote</xsl:text>
        </xsl:when>
        <xsl:otherwise>
          <xsl:text>Text_20_body</xsl:text>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:for-each-group select="node()" group-adjacent="if        (self::text())        then 1        else if        (tei:isInline(.))        then 1        else 2        ">
      <xsl:choose>
        <xsl:when test="current-grouping-key()=2">
          <xsl:apply-templates select="current-group()"/>
        </xsl:when>
        <xsl:otherwise>
          <text:p text:style-name="{$style}">
            <xsl:call-template name="test.id"/>
            <xsl:apply-templates select="current-group()"/>
          </text:p>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:for-each-group>
  </xsl:template>
  <!-- figures -->
  <xsl:template match="tei:figure">
    <text:p text:style-name="Standard">
      <xsl:call-template name="test.id"/>
      <xsl:apply-templates select="tei:graphic|tei:media"/>
    </text:p>
    <xsl:if test="tei:head">
      <text:p text:style-name="Caption">
        <text:span text:style-name="Figurenum">
          <xsl:text>Figure </xsl:text>
          <text:sequence text:ref-name="refFigure0" text:name="Figure" text:formula="Figure+1" style:num-format="1">
            <xsl:number level="any"/>
            <xsl:text>.</xsl:text>
          </text:sequence>
        </text:span>
        <xsl:text> </xsl:text>
        <xsl:apply-templates select="tei:head" mode="show"/>
      </text:p>
    </xsl:if>
  </xsl:template>
  <xsl:template match="tei:graphic|tei:media">
    <xsl:variable name="id">
      <xsl:choose>
        <xsl:when test="@xml:id">
          <xsl:value-of select="@xml:id"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:text>Figure</xsl:text>
          <xsl:number level="any"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:variable name="filename">
      <xsl:text>Pictures/resource</xsl:text>
      <xsl:number level="any"/>
      <xsl:text>.</xsl:text>
      <xsl:value-of select="tokenize(@url,'\.')[last()]"/>
    </xsl:variable>
    <xsl:variable name="origheight">
      <xsl:choose>
        <xsl:when test="@teidocx:height">
          <xsl:value-of select="@teidocx:height"/>
        </xsl:when>
        <xsl:when test="doc-available(concat($outputDir,'/image-size-info.xml'))">
          <xsl:for-each select="document(concat($outputDir,'/image-size-info.xml'))">
            <xsl:value-of select="(number(key('H',$filename)/height) div 72) * 9144"/>
          </xsl:for-each>
        </xsl:when>
        <xsl:otherwise>0</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:variable name="origwidth">
      <xsl:choose>
        <xsl:when test="@teidocx:width">
          <xsl:value-of select="@teidocx:width"/>
        </xsl:when>
        <xsl:when test="doc-available(concat($outputDir,'/image-size-info.xml'))">
          <xsl:for-each select="document(concat($outputDir,'/image-size-info.xml'))">
            <xsl:value-of select="(number(key('W',$filename)/width) div 72) * 9144"/>
          </xsl:for-each>
        </xsl:when>
        <xsl:otherwise>0</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="$filename and  ( ($origwidth and $origheight) or (@width and @height))">
        <!-- work out page width / height and subtract 1inch on all sides -->
        <xsl:variable name="pageWidth">
          <xsl:for-each select="document(concat($outputDir,'/styles.xml'))/office:document-styles/office:automatic-styles/style:page-layout[1]/style:page-layout-properties">
            <xsl:value-of select="number(tei:convert-dim-pt(@fo:page-width) - 144)"/>
          </xsl:for-each>
        </xsl:variable>
        <xsl:variable name="pageHeight">
          <xsl:for-each select="document(concat($outputDir,'/styles.xml'))/office:document-styles/office:automatic-styles/style:page-layout[1]/style:page-layout-properties">
            <xsl:value-of select="number(tei:convert-dim-pt(@fo:page-height) - 144)"/>
          </xsl:for-each>
        </xsl:variable>
        <xsl:variable name="Width">
          <xsl:choose>
            <xsl:when test="contains(@width,'%')">
              <xsl:value-of select="(($pageWidth div 100) * number(substring-before(@width,'%'))) cast as xs:integer"/>
            </xsl:when>
            <xsl:when test="@width">
              <xsl:value-of select="tei:convert-dim-pt(@width)"/>
            </xsl:when>
            <xsl:when test="@scale and $origwidth">
              <xsl:value-of select="number($origwidth * number(@scale)) div 127 cast as xs:integer"/>
            </xsl:when>
            <xsl:when test="@height[not(contains(.,'%'))] and $origheight">
              <xsl:variable name="h">
                <xsl:value-of select="number(tei:convert-dim-pt(@height))"/>
              </xsl:variable>
              <xsl:value-of select="($h * number($origwidth)) div number($origheight)"/>
            </xsl:when>
            <xsl:when test="$origwidth">
              <xsl:value-of select="number($origwidth) div 127"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:message terminate="yes">no way to work out image width for
	      <xsl:value-of select="$filename"/>
	      </xsl:message>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:variable>
        <xsl:variable name="Height">
          <xsl:choose>
            <xsl:when test="contains(@height,'%')">
              <xsl:value-of select="(($pageHeight div 100) * (number(substring-before(@height,'%')))) cast as xs:integer"/>
            </xsl:when>
            <xsl:when test="@height">
              <xsl:value-of select="tei:convert-dim-pt(@height)"/>
            </xsl:when>
            <xsl:when test="@scale and $origheight">
              <xsl:value-of select="($origheight *         number(@scale)) div 127 cast as xs:integer"/>
            </xsl:when>
	    <xsl:when test="@width[contains(.,'%')]">
	      <xsl:value-of select="(($pageHeight div 100) * (number(substring-before(@width,'%')))) cast as xs:integer"/>
	    </xsl:when>	  
            <xsl:when test="@width[not(contains(.,'%'))] and $origheight and $origwidth">
              <xsl:variable name="w">
                <xsl:value-of select="number(tei:convert-dim-pt(@width))"/>
              </xsl:variable>
              <xsl:value-of select="($w * number($origheight)) div number($origwidth)"/>
            </xsl:when>
            <xsl:when test="$origheight">
              <xsl:value-of select="number($origheight) div 127"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:message terminate="yes">no way to work out image height for  <xsl:value-of select="$filename"/>
	      </xsl:message>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:variable>
        <!-- check for sense -->
        <xsl:variable name="imageHeight">
          <xsl:choose>
            <xsl:when test="number($Height) &lt; 0">
              <xsl:value-of select="$pageHeight"/>
            </xsl:when>
            <xsl:when test="number($Height) &gt; $pageHeight">
              <xsl:value-of select="$pageHeight"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:value-of select="$Height"/>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:variable>
        <xsl:variable name="imageWidth">
          <xsl:choose>
            <xsl:when test="number($Width) &lt; 0">
              <xsl:value-of select="$pageWidth"/>
            </xsl:when>
            <xsl:when test="number($Width) &gt; $pageWidth">
              <xsl:value-of select="$pageWidth"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:value-of select="$Width"/>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:variable>
	<xsl:if test="$debug='true'">
<xsl:message>
  <xsl:for-each select="@*">
    - @<xsl:value-of select="name(.)"/>: <xsl:value-of select="."/>
</xsl:for-each>
    - pageWidth: <xsl:value-of select="$pageWidth"/>
    - pageHeight: <xsl:value-of select="$pageHeight"/>
    - Width: <xsl:value-of select="$Width"/>
    - Height: <xsl:value-of select="$Height"/>
    - imageWidth: <xsl:value-of select="$imageWidth"/>
    - imageHeight: <xsl:value-of select="$imageHeight"/>
</xsl:message>
	</xsl:if>
        <draw:frame draw:style-name="fr1" draw:name="{$id}" draw:z-index="0">
          <xsl:attribute name="text:anchor-type">
            <xsl:choose>
              <xsl:when test="parent::tei:figure">
                <xsl:text>paragraph</xsl:text>
              </xsl:when>
              <xsl:otherwise>
                <xsl:text>as-char</xsl:text>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:attribute>
          <xsl:attribute name="svg:width">
            <xsl:value-of select="$imageWidth"/>
            <xsl:text>pt</xsl:text>
          </xsl:attribute>
          <xsl:attribute name="svg:height">
            <xsl:value-of select="$imageHeight"/>
            <xsl:text>pt</xsl:text>
          </xsl:attribute>
          <draw:image xlink:href="{$filename}" xlink:type="simple" xlink:show="embed" xlink:actuate="onLoad" draw:filter-name="&lt;All formats&gt;"/>
        </draw:frame>
      </xsl:when>
      <xsl:otherwise>
        <xsl:message terminate="yes">ERROR. no image size info for  <xsl:value-of select="$filename"/>, cannot proceed</xsl:message>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <!-- lists -->
  <xsl:template match="tei:listBibl">
    <xsl:choose>
      <xsl:when test="tei:msDesc">
	<xsl:apply-templates/>
      </xsl:when>
      <xsl:otherwise>
	<text:list text:style-name="L2">
	  <xsl:apply-templates/>
	</text:list>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="tei:list">
    <xsl:if test="tei:head">
      <text:p>
        <xsl:attribute name="text:style-name">
          <xsl:choose>
            <xsl:when test="tei:isOrderedList(.)">P2</xsl:when>
            <xsl:otherwise>P1</xsl:otherwise>
          </xsl:choose>
        </xsl:attribute>
        <xsl:apply-templates select="tei:head" mode="show"/>
      </text:p>
    </xsl:if>
    <text:list>
      <xsl:attribute name="text:style-name">
        <xsl:choose>
          <xsl:when test="tei:isOrderedList(.)">L2</xsl:when>
          <xsl:otherwise>L1</xsl:otherwise>
        </xsl:choose>
      </xsl:attribute>
      <xsl:apply-templates/>
    </text:list>
  </xsl:template>
  <xsl:template match="tei:list[tei:isGlossList(.)]" priority="10">
    <xsl:apply-templates/>
  </xsl:template>
  <xsl:template match="tei:list[tei:isGlossList(.)]/tei:item">
    <xsl:choose>
      <xsl:when test="count(*)=1 and tei:list">
      <xsl:apply-templates/>
      </xsl:when>
      <xsl:otherwise>
	<text:p text:style-name="List_20_Contents">
	  <xsl:apply-templates/>
	</text:p>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="tei:list[tei:isGlossList(.)]/tei:label">
    <text:p text:style-name="List_20_Heading">
      <xsl:apply-templates/>
    </text:p>
  </xsl:template>

  <xsl:template match="tei:item/tei:p">
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="tei:item">
    <text:list-item>
      <xsl:for-each-group select="node()" group-adjacent="if
							  (self::text())			  then 1
							  else if  (self::tei:list
							  or
							  self::tei:p
							  or self::teix:egXML)        then 2       else 1        ">
	<xsl:choose>
	  <xsl:when test="current-grouping-key()=2">
	    <xsl:apply-templates select="current-group()"/>
	    </xsl:when>
	    <xsl:otherwise>
            <text:p>
              <xsl:attribute name="text:style-name">
		<xsl:choose>
                  <xsl:when test="tei:isOrderedList(..)">P2</xsl:when>
                  <xsl:otherwise>P1</xsl:otherwise>
		</xsl:choose>
              </xsl:attribute>
		<xsl:apply-templates select="current-group()"/>
	      </text:p>
	    </xsl:otherwise>
	  </xsl:choose>
	</xsl:for-each-group>
    </text:list-item>
  </xsl:template>
  <xsl:template name="displayNote">
    <text:p text:style-name="tei_displayNote">
      <xsl:apply-templates/>
    </text:p>
  </xsl:template>
  <!-- inline stuff -->
  <xsl:template match="tei:emph">
    <text:span text:style-name="Emphasis">
      <xsl:apply-templates/>
    </text:span>
  </xsl:template>
  <xsl:template match="tei:gi">
    <xsl:text>&lt;</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>&gt;</xsl:text>
  </xsl:template>
  <xsl:template match="tei:caesura">
    <xsl:text>   </xsl:text>
  </xsl:template>
  <xsl:template match="tei:q[not(tei:l)]">
    <text:span text:style-name="q">
      <xsl:text>‘</xsl:text>
      <xsl:apply-templates/>
      <xsl:text>’</xsl:text>
    </text:span>
  </xsl:template>
  <xsl:template name="plainNote">
    <text:span text:style-name="tei_inlineNote">
      <xsl:text> [</xsl:text>
      <xsl:choose>
        <xsl:when test="@n">
          <xsl:value-of select="@n"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:sequence select="tei:i18n('Note')"/>
          <xsl:text>: </xsl:text>
        </xsl:otherwise>
      </xsl:choose>
      <xsl:apply-templates/>
      <xsl:text>] </xsl:text>
    </text:span>
  </xsl:template>
  <xsl:template name="footNote">
    <text:note text:note-class="footnote">
      <text:note-citation>
        <xsl:number level="any" count="tei:note[@place='foot']"/>
      </text:note-citation>
      <text:note-body>

	<xsl:for-each-group select="node()" group-adjacent="if        (self::text())        then 1        else if        (tei:isInline(.))        then 1        else 2        ">
	  <xsl:choose>
	    <xsl:when test="current-grouping-key()=2">
	      <xsl:apply-templates select="current-group()"/>
	    </xsl:when>
	    <xsl:otherwise>
	      <text:p text:style-name="Footnote">
		<xsl:call-template name="test.id"/>
		<xsl:apply-templates select="current-group()"/>
	      </text:p>
	    </xsl:otherwise>
	  </xsl:choose>
	</xsl:for-each-group>
      </text:note-body>
    </text:note>
  </xsl:template>
  <xsl:template name="endNote">
    <text:note text:note-class="endnote">
      <text:note-citation>
        <xsl:number format="i" level="any" count="tei:note[@place='end']"/>
      </text:note-citation>
      <text:note-body>
        <text:p text:style-name="Endnote">
          <xsl:apply-templates/>
        </text:p>
      </text:note-body>
    </text:note>
  </xsl:template>
  <xsl:template match="tei:table|tei:figure|tei:item" mode="crossref">
    <xsl:number level="any"/>
  </xsl:template>
  <xsl:template match="tei:div" mode="crossref">
    <xsl:number format="1.1.1.1.1" level="multiple" count="tei:div"/>
  </xsl:template>
  <xsl:template match="tei:div1|tei:div2|tei:div3|tei:div4|tei:div5|tei:div6" mode="crossref">
    <xsl:number format="1.1.1.1.1" count="tei:div1|tei:div2|tei:div3|tei:div4|tei:div5|tei:div6" level="multiple"/>
  </xsl:template>
  <xsl:template name="test.id">
    <xsl:if test="@xml:id">
      <text:bookmark text:name="{@xml:id}"/>
    </xsl:if>
  </xsl:template>
  <xsl:template match="tei:hi">
    <text:span>
      <xsl:attribute name="text:style-name">
        <xsl:choose>
          <xsl:when test="tei:match(@rend,'normalweight')">
            <xsl:text>Standard</xsl:text>
          </xsl:when>
          <xsl:when test="tei:match(@rend,'code') or tei:match(@rend,'typewriter')">
            <xsl:text>Source_20_Text</xsl:text>
          </xsl:when>
          <xsl:when test="tei:match(@rend,'sup') or tei:match(@rend,'superscript')">
            <xsl:text>Superscript</xsl:text>
          </xsl:when>
          <xsl:when test="tei:match(@rend,'sub') or tei:match(@rend,'subscript')">
            <xsl:text>Subscript</xsl:text>
          </xsl:when>
          <xsl:when test="tei:match(@rend,'bold')">
            <xsl:text>Highlight</xsl:text>
          </xsl:when>
          <xsl:when test="tei:match(@rend,'label')">
            <xsl:text>Highlight</xsl:text>
          </xsl:when>
          <xsl:when test="tei:match(@rend,'it') or tei:match(@rend,'i') or tei:match(@rend,'italic')">
            <xsl:text>Emphasis</xsl:text>
          </xsl:when>
          <xsl:when test="tei:match(@rend,'underline')">
            <xsl:text>Underline</xsl:text>
          </xsl:when>
          <xsl:when test="tei:match(@rend,'sc') or tei:match(@rend,'smallcaps')">
            <xsl:text>SmallCaps</xsl:text>
          </xsl:when>
          <xsl:when test="tei:match(@rend,'sc') or tei:match(@rend,'allcaps')">
            <xsl:text>AllCaps</xsl:text>
          </xsl:when>
          <xsl:when test="tei:match(@rend,'strikethrough')">
            <xsl:text>StrikeThrough</xsl:text>
          </xsl:when>
          <xsl:when test="tei:match(@rend,'doublestrikethrough')">
            <xsl:text>Doublestrikethrough</xsl:text>
          </xsl:when>
          <xsl:when test="tei:match(@rend,'underline')">
            <xsl:text>UnderLine</xsl:text>
          </xsl:when>
          <xsl:when test="tei:match(@rend,'doubleunderline')">
            <xsl:text>Doubleunderline</xsl:text>
          </xsl:when>
          <xsl:otherwise>
            <xsl:text>Emphasis</xsl:text>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:attribute>
      <xsl:apply-templates/>
    </text:span>
  </xsl:template>
  <xsl:template match="tei:index"/>
  <xsl:template match="tei:eg">
    <xsl:call-template name="Literal">
      <xsl:with-param name="Text">
        <xsl:value-of select="."/>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  <xsl:template match="teix:egXML">
    <xsl:call-template name="Literal">
      <xsl:with-param name="Text">
        <xsl:apply-templates mode="verbatim"/>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  <!-- safest to drop comments entirely, I think -->
  <xsl:template match="comment()"/>
  <xsl:template match="tei:head" mode="show">
    <xsl:apply-templates/>
  </xsl:template>
  <xsl:template name="lineBreak">
	<text:line-break/>
  </xsl:template>
  <xsl:template name="lineBreakAsPara">
	<text:line-break/>
  </xsl:template>
  <xsl:template match="tei:biblStruct">
    <text:list-item>
      <text:p text:style-name="P2">
        <xsl:apply-templates/>
      </text:p>
    </text:list-item>
  </xsl:template>
  <xsl:template match="tei:bibl|tei:signed|tei:byline|tei:titlePart|tei:docImprint">
    <text:p text:style-name="tei_{local-name(.)}">
      <xsl:apply-templates/>
    </text:p>
  </xsl:template>
  <xsl:template match="tei:lg">
    <text:p text:style-name="lg">
      <xsl:apply-templates/>
    </text:p>
  </xsl:template>
  <xsl:template match="tei:l">
    <xsl:choose>
      <xsl:when test="parent::tei:lg">
        <xsl:apply-templates/>
        <text:line-break/>
      </xsl:when>
      <xsl:otherwise>
        <text:p text:style-name="lg">
          <xsl:apply-templates/>
        </text:p>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template name="Literal">
    <xsl:param name="Text"/>
    <xsl:choose>
      <xsl:when test="contains($Text,'&#10;')">
        <text:p text:style-name="Preformatted_20_Text">
          <xsl:value-of select="translate(substring-before($Text,'&#10;'),' ',' ')"/>
        </text:p>
        <xsl:call-template name="Literal">
          <xsl:with-param name="Text">
            <xsl:value-of select="substring-after($Text,'&#10;')"/>
          </xsl:with-param>
        </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
        <text:p text:style-name="Preformatted_20_Text">
          <xsl:value-of select="translate($Text,' ',' ')"/>
        </text:p>
      </xsl:otherwise>
    </xsl:choose>
    <!-- text:s c="6" to ident 6 spaces -->
  </xsl:template>
  <xsl:template match="tei:sp">
    <xsl:apply-templates/>
  </xsl:template>
  <xsl:template match="tei:stage">
    <xsl:choose>
      <xsl:when test="parent::tei:sp or parent::tei:div">
        <text:p text:style-name="Stage">
          <xsl:apply-templates/>
        </text:p>
      </xsl:when>
      <xsl:otherwise>
        <text:span text:style-name="Stage">
          <xsl:apply-templates/>
        </text:span>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="tei:speaker">
    <text:p text:style-name="Speaker">
      <xsl:apply-templates/>
    </text:p>
  </xsl:template>
  <xsl:template match="tei:quote">
    <xsl:choose>
      <xsl:when test="tei:p">
	  <xsl:apply-templates/>
      </xsl:when>
      <xsl:when test="not(tei:isInline(.))">
	<text:p text:style-name="Quote">
	  <xsl:apply-templates/>
	</text:p>
      </xsl:when>
      <xsl:otherwise>
	<xsl:call-template name="makeQuote"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="tei:ab">
    <xsl:choose>
      <xsl:when test="tei:ab">
	  <xsl:apply-templates/>
      </xsl:when>
      <xsl:otherwise>
	<text:p>
	  <xsl:apply-templates/>
	</text:p>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <!-- tables-->
  <xsl:template match="tei:table">
    <xsl:variable name="tablenum">
      <xsl:choose>
        <xsl:when test="@xml:id">
          <xsl:value-of select="@xml:id"/>
        </xsl:when>
        <xsl:otherwise>table<xsl:number level="any"/></xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <table:table table:name="{$tablenum}" table:style-name="Table1">
      <xsl:for-each select="1 to max(for $i in tei:row return count($i/tei:cell))">
        <table:table-column table:style-name="Table{.}.col{.}">
	</table:table-column>
      </xsl:for-each>
      <xsl:apply-templates/>
    </table:table>
    <xsl:if test="tei:head">
      <text:p text:style-name="Caption">
        <xsl:apply-templates select="tei:head" mode="show"/>
      </text:p>
    </xsl:if>
  </xsl:template>
  <xsl:template match="tei:row[@role='label']">
    <table:table-header-rows>
      <table:table-row>
        <xsl:apply-templates/>
      </table:table-row>
    </table:table-header-rows>
  </xsl:template>
  <xsl:template match="tei:row">
    <table:table-row>
      <xsl:apply-templates/>
    </table:table-row>
  </xsl:template>
  <xsl:template match="tei:seg">
    <text:span>
      <xsl:apply-templates/>
    </text:span>
  </xsl:template>
  <xsl:template match="tei:seg[tei:match(@rend,'parent')]">
    <xsl:apply-templates/>
  </xsl:template>
  <xsl:template match="tei:ident">
    <text:span>
      <xsl:apply-templates/>
    </text:span>
  </xsl:template>
  <xsl:template match="tei:cit">
    <xsl:choose>
      <xsl:when test="tei:match(@rend,'display')">
	<text:p text:style-name="tei_cit">
	  <xsl:apply-templates/>
	</text:p>
      </xsl:when>
      <xsl:when test="tei:quote/tei:l or tei:quote/tei:p">	
	<xsl:apply-templates/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:apply-templates/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="tei:code">
    <text:span text:style-name="User_20_Entry">
      <xsl:apply-templates/>
    </text:span>
  </xsl:template>
  <xsl:template match="tei:cell">
    <table:table-cell>
      <xsl:if test="@cols">
        <xsl:attribute name="table:number-columns-spanned" select="@cols"/>
      </xsl:if>
      <xsl:if test="@rows">
        <xsl:attribute name="table:number-rows-spanned" select="@rows"/>
      </xsl:if>
      <xsl:variable name="cellContents">
        <xsl:apply-templates/>
      </xsl:variable>
      <xsl:for-each-group select="$cellContents/node()" group-adjacent="if (self::draw:frame or        self::text:note or self::text:span or self::text() or self::text:a)        then 1        else 2">
        <xsl:choose>
          <xsl:when test="current-grouping-key()=1">
            <text:p>
              <xsl:copy-of select="current-group()"/>
            </text:p>
          </xsl:when>
          <xsl:otherwise>
            <xsl:copy-of select="current-group()"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:for-each-group>
    </table:table-cell>
    <xsl:if test="@cols">
      <xsl:for-each select="2 to @cols">
        <table:covered-table-cell/>
      </xsl:for-each>
    </xsl:if>
  </xsl:template>
  <xsl:param name="spaceCharacter"> </xsl:param>
  <xsl:param name="showNamespaceDecls">true</xsl:param>
  <xsl:param name="wrapLength">65</xsl:param>
  <xsl:key name="Namespaces" match="*[ancestor::teix:egXML]" use="namespace-uri()"/>
  <xsl:key name="Namespaces" match="*[not(ancestor::*)]" use="namespace-uri()"/>
  <xsl:template name="newLine">
</xsl:template>
  <xsl:template name="makeIndent">
    <xsl:variable name="depth" select="count(ancestor::*[not(namespace-uri()='http://www.tei-c.org/ns/1.0')])"/>
    <xsl:call-template name="makeSpace">
      <xsl:with-param name="d">
        <xsl:value-of select="$depth"/>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  <xsl:template name="makeSpace">
    <xsl:param name="d"/>
    <xsl:if test="number($d)&gt;1">
      <xsl:value-of select="$spaceCharacter"/>
      <xsl:call-template name="makeSpace">
        <xsl:with-param name="d">
          <xsl:value-of select="$d -1"/>
        </xsl:with-param>
      </xsl:call-template>
    </xsl:if>
  </xsl:template>
  <xsl:template match="text()|comment()|processing-instruction()" mode="ns"/>
  <xsl:template match="*" mode="ns">
    <xsl:param name="list"/>
    <xsl:variable name="used">
      <xsl:for-each select="namespace::*">
        <xsl:variable name="ns" select="."/>
        <xsl:choose>
          <xsl:when test="contains($list,$ns)"/>
          <xsl:when test=".='http://relaxng.org/ns/structure/1.0'"/>
          <xsl:when test=".='http://www.w3.org/2001/XInclude'"/>
          <xsl:when test=".='http://www.tei-c.org/ns/Examples'"/>
          <xsl:when test=".='http://relaxng.org/ns/compatibility/annotations/1.0'"/>
          <xsl:when test="name(.)=''"/>
          <xsl:when test=".='http://www.w3.org/XML/1998/namespace'"/>
          <xsl:otherwise>
	    <xsl:text>
	    </xsl:text>
            <xsl:text>   </xsl:text>
            <xsl:text>xmlns:</xsl:text>
            <xsl:value-of select="name(.)"/>
            <xsl:text>="</xsl:text>
            <xsl:value-of select="."/>
            <xsl:text>"</xsl:text>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:for-each>
    </xsl:variable>
    <xsl:copy-of select="$used"/>
    <xsl:apply-templates mode="ns">
      <xsl:with-param name="list">
        <xsl:value-of select="$list"/>
        <xsl:value-of select="$used"/>
      </xsl:with-param>
    </xsl:apply-templates>
  </xsl:template>
  <xsl:template match="tei:soCalled">
    <text:span>
      <xsl:value-of select="$preQuote"/>
      <xsl:apply-templates/>
      <xsl:value-of select="$postQuote"/>
    </text:span>
  </xsl:template>
  <xsl:function name="tei:convert-dim-pt" as="xs:integer">
    <xsl:param name="dim"/>
    <xsl:choose>
      <xsl:when test="ends-with($dim,'cm')">
        <xsl:value-of select="number(number(substring($dim,0,string-length($dim)-1))*28.3464567) cast as xs:integer"/>
      </xsl:when>
      <xsl:when test="ends-with($dim,'in')">
        <xsl:value-of select="number(number(substring($dim,0,string-length($dim)-1))*72) cast as xs:integer"/>
      </xsl:when>
      <xsl:when test="ends-with($dim,'mm')">
        <xsl:value-of select="number(number(substring($dim,0,string-length($dim)-1))*2.83464567) cast as xs:integer"/>
      </xsl:when>
      <xsl:when test="ends-with($dim,'pt')">
        <xsl:value-of select="number(substring($dim,0,string-length($dim)-1)) cast as xs:integer"/>
      </xsl:when>
      <xsl:when test="ends-with($dim,'px')">
        <xsl:value-of select="number(number(substring($dim,0,string-length($dim)-1))*0.75) cast as xs:integer"/>
      </xsl:when>
      <xsl:otherwise>
	-1
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <xsl:template name="makeExternalLink">
    <xsl:param name="ptr" as="xs:boolean" select="false()"/>
    <xsl:param name="dest"/>
    <xsl:param name="title"/>
    <xsl:param name="class">link_<xsl:value-of select="local-name(.)"/></xsl:param>
    <text:a xlink:type="simple" xlink:href="{$dest}">
      <xsl:choose>
        <xsl:when test="$ptr">
          <xsl:value-of select="$dest"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:apply-templates/>
        </xsl:otherwise>
      </xsl:choose>
    </text:a>
  </xsl:template>
  <xsl:template name="makeSpan">
    <xsl:apply-templates/>
  </xsl:template>
  <xsl:template name="makeText">
    <xsl:param name="letters"/>
    <xsl:value-of select="$letters"/>
  </xsl:template>
  <xsl:template name="emphasize">
    <xsl:param name="class"/>
    <xsl:param name="content"/>
    <text:span>
      <xsl:choose>
        <xsl:when test="$class='titlem'">
          <xsl:attribute name="text:style-name">Emphasis</xsl:attribute>
        </xsl:when>
        <xsl:when test="$class='titlej'">
          <xsl:attribute name="text:style-name">Emphasis</xsl:attribute>
        </xsl:when>
      </xsl:choose>
      <xsl:choose>
        <xsl:when test="$class='titles'">
          <xsl:text>, </xsl:text>
        </xsl:when>
        <xsl:when test="$class='titleu'">
          <xsl:text>‘</xsl:text>
        </xsl:when>
        <xsl:when test="$class='titlea'">
          <xsl:text>‘</xsl:text>
        </xsl:when>
      </xsl:choose>
      <xsl:value-of select="$content"/>
      <xsl:choose>
        <xsl:when test="$class='titleu'">
          <xsl:text>’</xsl:text>
        </xsl:when>
        <xsl:when test="$class='titlea'">
          <xsl:text>’</xsl:text>
        </xsl:when>
      </xsl:choose>
    </text:span>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>how to make a horizontal rule</desc>
  </doc>
  <xsl:template name="horizontalRule">
  </xsl:template>

  <xsl:template name="makeBlock">
    <xsl:param name="style"/>
    <xsl:apply-templates/>
  </xsl:template>
  <xsl:template name="makeInline">
    <xsl:param name="before"/>
    <xsl:param name="after"/>
    <xsl:param name="style"/>
    <text:span>
      <xsl:choose>
	<xsl:when test="$style=('bibl','docAuthor','titlem','italic','mentioned','term','foreign')">
	  <xsl:attribute
	      name="text:style-name">Emphasis</xsl:attribute>
	</xsl:when>
	<xsl:when test="$style='bold'">
	  <xsl:attribute
	      name="text:style-name">Highlight</xsl:attribute>
	</xsl:when>
	<xsl:when test="$style='strikethrough'">
	  <xsl:attribute
	      name="text:style-name">StrikeThrough</xsl:attribute>
	</xsl:when>
      </xsl:choose>
      <xsl:value-of select="$before"/>
      <xsl:apply-templates/>
      <xsl:value-of select="$after"/>
    </text:span>
  </xsl:template>

  <xsl:template name="generateEndLink">
    <xsl:param name="where"/>
    <xsl:value-of select="$where"/>
  </xsl:template>
  <xsl:template name="makeSection">
    <xsl:param name="level"/>
    <xsl:param name="implicitBlock"/>
    <xsl:param name="heading"/>
    <text:p text:style-name="tei{local-name()}">
      <xsl:call-template name="test.id"/>
      <text:span>
        <xsl:value-of select="$heading"/>
      </text:span>
    </text:p>
    <text:p>
      <xsl:apply-templates/>
    </text:p>
  </xsl:template>
  <xsl:template name="makeWithLabel">
    <xsl:param name="before"/>
    <text:span text:style-name="Emphasis">
      <xsl:value-of select="$before"/>
      <xsl:text>: </xsl:text>
    </text:span>
    <text:span text:style-name="tei{local-name()}"/>
    <xsl:value-of select="."/>
  </xsl:template>



  <xsl:template name="appReading">
     <xsl:param name="lemma"/>
     <xsl:param name="lemmawitness"/>
     <xsl:param name="readings"/>
     <xsl:value-of select="$lemma"/>
  </xsl:template>


</xsl:stylesheet>
