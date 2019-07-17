<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:iso="http://www.iso.org/ns/1.0" xmlns="http://www.w3.org/1999/xhtml" xmlns:html="http://www.w3.org/1999/xhtml" xmlns:tei="http://www.tei-c.org/ns/1.0" xmlns:teix="http://www.tei-c.org/ns/Examples" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:ncx="http://www.daisy.org/z3986/2005/ncx/" version="2.0" exclude-result-prefixes="iso tei teix dc html ncx">
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
    <desc>
      <p>
	TEI stylesheet for making ePub output. 
      </p>
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
  <xsl:key match="tei:graphic[not(ancestor::teix:egXML or starts-with(@url,'film:'))]" use="1" name="G"/>
  <xsl:key match="tei:media[not(ancestor::teix:egXML)]" use="1" name="G"/>
  <xsl:key name="GRAPHICS" use="1" match="tei:graphic|tei:media"/>
  <xsl:key name="PBGRAPHICS" use="1" match="tei:pb[@facs]"/>
  <xsl:key name="Timeline" match="tei:timeline" use="1"/>
  <xsl:key name="Object" match="tei:when" use="substring(@corresp,2)"/>
  <xsl:key name="objectOnPage" match="tei:*[@xml:id]" use="generate-id(preceding::tei:pb[1])"/>
  <xsl:key name="PB"
	   match="tei:pb[not(@facs='') and not(starts-with(@facs,'tcp:')) and not(starts-with(@facs,'unknown:')) and not(tei:match(@rend,'none'))]" use="1"/>
  <xsl:key name="Timeline" match="tei:timeline" use="1"/>
  <xsl:param name="mediaoverlay">false</xsl:param>
  <xsl:param name="coverimage"/>
  <xsl:param name="coverDir"/>
  <xsl:param name="filePerPage">false</xsl:param>
  <xsl:param name="mediaDir">media</xsl:param>
  <xsl:param name="javascriptFiles"/>
  <xsl:param name="extraGraphicsFiles"/>
  <xsl:param name="pagebreakStyle">simple</xsl:param>
  <xsl:param name="epubMimetype">application/epub+zip</xsl:param>

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
    <xsl:if test="not($subject='')">
      <dc:subject>
        <xsl:value-of select="$subject"/>
      </dc:subject>
    </xsl:if>
    <xsl:call-template name="generateSubjectHook"/>
    <xsl:for-each select="tei:teiHeader/tei:profileDesc/tei:textClass/tei:keywords/tei:term">
      <dc:subject>
        <xsl:value-of select="."/>
      </dc:subject>
    </xsl:for-each>
    <xsl:for-each select="tei:teiHeader/tei:profileDesc/tei:textClass/tei:keywords/tei:list/tei:item">
      <dc:subject>
        <xsl:value-of select="."/>
      </dc:subject>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="generateSubjectHook"/>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>[epub] Set unique identifier for output</desc>
  </doc>
  <xsl:template name="generateID">
    <xsl:choose>
      <xsl:when test="not($uid='')">
        <xsl:value-of select="$uid"/>
      </xsl:when>
      <xsl:when test="tei:teiHeader/tei:fileDesc/tei:publicationStmt/tei:idno">
        <xsl:value-of select="tei:teiHeader/tei:fileDesc/tei:publicationStmt/tei:idno[1]"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>http://www.example.com/TEIEPUB/</xsl:text>
        <xsl:value-of select="format-dateTime(current-dateTime(),'[Y][M02][D02][H02][m02][s02]')"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>[epub] Add specific linebreak in verbatim output, as
      readers do not seem to grok the CSS</desc>
  </doc>
  <xsl:template name="verbatim-lineBreak">
    <xsl:param name="id"/>
    <br/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>[epub] Remove unwanted things from CSS</desc>
  </doc>
  <xsl:template name="purgeCSS">
    <xsl:choose>
      <xsl:when test="starts-with(.,'@import')"/>
      <!--
      <xsl:when test="contains(.,'max-width:')"/>
      <xsl:when test="contains(.,'height:')"/>
      <xsl:when test="contains(.,'line-height:')"/>
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
  <xsl:template name="addLangAtt"/>

  <xsl:template match="tei:lb[tei:match(@rend,'space')]">
    <xsl:text> </xsl:text>
  </xsl:template>

  <xsl:template match="tei:titleStmt" mode="metadata">
    <h3>Title statement</h3>
    <xsl:apply-templates mode="metadata"/>
  </xsl:template>
  <xsl:template match="tei:editionStmt" mode="metadata">
    <h3>Edition statement</h3>
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

  <xsl:template match="tei:encodingDesc" mode="metadata">
    <h3>Encoding</h3>
    <xsl:apply-templates mode="metadata"/>
  </xsl:template>

  <xsl:template match="tei:listPrefixDef" mode="metadata"/>
  <xsl:template match="tei:tagsDecl" mode="metadata"/>

  <xsl:template match="tei:projectDesc" mode="metadata">
    <h3>Creation</h3>
    <xsl:apply-templates mode="metadata"/>
  </xsl:template>

  <xsl:template match="tei:editorialDecl" mode="metadata">
    <h3>Editorial practices</h3>
    <xsl:apply-templates mode="metadata"/>
  </xsl:template>

  <xsl:template match="tei:classDecl" mode="metadata"/>

  <xsl:template match="tei:sourceDesc/tei:bibl" mode="metadata">
    <p> — <xsl:apply-templates mode="metadata"/></p>
  </xsl:template>
  <xsl:template match="tei:sourceDesc/tei:biblFull" mode="metadata">
    <div> — <xsl:apply-templates/></div>
  </xsl:template>
  <xsl:template match="tei:respStmt" mode="metadata">
    <p><i><xsl:value-of select="tei:resp"/></i>:
    <xsl:for-each select="tei:name">
      <xsl:apply-templates select="."/>
      <xsl:choose>
	<xsl:when test="following-sibling::tei:name">, </xsl:when>
	<xsl:otherwise>.</xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
    </p>
  </xsl:template>

  <xsl:template match="tei:list" mode="metadata">
    <xsl:choose>
      <xsl:when test="ancestor::tei:availability">
	<xsl:apply-templates mode="metadata"/>
      </xsl:when>
      <xsl:otherwise>
	<ul>
	  <xsl:apply-templates mode="metadata"/>
	</ul>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="tei:item" mode="metadata">
    <xsl:choose>
      <xsl:when test="ancestor::tei:availability">
	*  <xsl:apply-templates mode="metadata"/>
      </xsl:when>
      <xsl:otherwise>
	<li>
	  <xsl:apply-templates mode="metadata"/>
	</li>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="tei:relatedItem[@target]" mode="metadata" priority="10">
    <a href="{@target}">
      <xsl:value-of select="@target"/>
    </a>
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
  <xsl:template match="tei:publicationStmt/tei:pubPlace" mode="metadata">
    <dt>Place of publication</dt>
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
  <xsl:template match="tei:editor" mode="metadata">
    <p><i>Editor</i>: 
    <xsl:apply-templates/></p>
  </xsl:template>
  <xsl:template match="tei:funder" mode="metadata">
    <p><i>Funder</i>: 
    <xsl:apply-templates/></p>
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
      <xsl:apply-templates mode="metadata"/>
    </dd>
  </xsl:template>

  <xsl:template match="tei:licence" mode="metadata">
    <div>
      <xsl:apply-templates/>
    </div>
  </xsl:template>

  <xsl:template match="tei:title" mode="metadata" priority="99">
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
  <xsl:template match="tei:bibl/tei:date" mode="metadata"
		priority="99">
    <xsl:text> (</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>)</xsl:text>
  </xsl:template>
  <xsl:template match="tei:note" mode="metadata">
    <xsl:text> [</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>] </xsl:text>
  </xsl:template>
  <xsl:template match="tei:notesStmt/tei:note" mode="metadata" priority="99">
    <xsl:choose>
      <xsl:when test="tei:p">
        <xsl:apply-templates/>
      </xsl:when>
      <xsl:otherwise>
        <p>
          <xsl:apply-templates/>
        </p>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="tei:listPerson" mode="metadata">
    <ul>
      <xsl:apply-templates/>
    </ul>
  </xsl:template>
  <!-- fallbacks -->
  <xsl:template match="tei:sourceDesc/tei:bibl/*" mode="metadata">
    <xsl:value-of select="."/>
  </xsl:template>
  <xsl:template match="*" mode="metadata">
    <xsl:choose>
      <xsl:when test="tei:p">
	  <xsl:apply-templates/>
      </xsl:when>
      <xsl:otherwise>
	<p>
	  <xsl:apply-templates/>
	</p>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="tei:seriesStmt/tei:p">
    <xsl:apply-templates/>
  </xsl:template>
  <xsl:template match="tei:distributor/tei:name">
    <xsl:apply-templates/>
    <br/>
  </xsl:template>
  <xsl:template match="tei:distributor/tei:address">
    <xsl:apply-templates/>
  </xsl:template>
  <xsl:template match="tei:authority/tei:address">
    <br/>
    <xsl:apply-templates/>
  </xsl:template>
  <xsl:template match="tei:authority/tei:addrLine">
    <xsl:apply-templates/>
    <br/>
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

  <xsl:template name="generateDate">
    <xsl:choose>
      <xsl:when test="$useHeaderFrontMatter='true' and ancestor-or-self::tei:TEI/tei:text/tei:front//tei:docDate[@when]">
        <xsl:apply-templates mode="date" select="ancestor-or-self::tei:TEI/tei:text/tei:front//tei:docDate/@when"/>
      </xsl:when>
      <xsl:when test="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:editionStmt/descendant::tei:date[@when]">
        <xsl:apply-templates select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:editionStmt/descendant::tei:date[1]/@when"/>
      </xsl:when>
      <xsl:when test="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:publicationStmt/tei:date[@when]">
        <xsl:apply-templates select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:publicationStmt/tei:date/@when"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:value-of select="format-dateTime(current-dateTime(),'[Y]-[M02]-[D02]')"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <!-- its inserted explicitly -->
  <xsl:template match="tei:front/tei:titlePage"/>

  <xsl:template match="html:li">
    <xsl:choose>
      <xsl:when test="not(html:a)"/>
      <xsl:when test="starts-with(html:a/@href,'#')"/>
      <xsl:when test="contains(@class,'headless')"/>
      <xsl:when test="html:a/@href=preceding-sibling::html:li/html:a/@href"/>
      <xsl:otherwise>
        <navPoint xmlns="http://www.daisy.org/z3986/2005/ncx/">
          <navLabel>
            <text>
              <xsl:value-of select="html:span[@class='headingNumber']"/>
              <xsl:value-of select="normalize-space(html:a[1])"/>
            </text>
          </navLabel>
          <content src="{html:a/@href}"/>
        </navPoint>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="javascriptHook">
    <xsl:for-each select="tokenize($javascriptFiles,',')">
      <xsl:variable name="name" select="tokenize(normalize-space(.),'/')[last()]"/>
      <script type="text/javascript" src="{$name}">
        <xsl:comment>JS library</xsl:comment>
      </script>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="epubSpineHook"/>
  <xsl:template name="epubManifestHook"/>
  <xsl:template name="processTEIHook"/>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>[epub] Override addition of CSS links. We force a simple
      name of "stylesheet.css"
      </desc>
  </doc>
  <xsl:template name="includeCSS">
    <xsl:call-template name="linkCSS">
      <xsl:with-param name="file">stylesheet.css</xsl:with-param>
    </xsl:call-template>
    <xsl:if test="not($cssPrintFile='')">
      <xsl:call-template name="linkCSS">
	<xsl:with-param name="file">print.css</xsl:with-param>
	<xsl:with-param name="media">print</xsl:with-param>
      </xsl:call-template>
    </xsl:if>
    <xsl:call-template name="generateLocalCSS"/>
  </xsl:template>

  <xsl:template name="linkCSS">
    <xsl:param name="file"/>
    <xsl:param name="media"/>
    <link xmlns="http://www.w3.org/1999/xhtml" href="{$file}"
	  rel="stylesheet" type="text/css">
      <xsl:if test="not($media='')">
	<xsl:attribute name="media" select="$media"/>
      </xsl:if>
    </link>
  </xsl:template>

  <xsl:template name="hdr3"/>


   <xsl:template name="getgraphics">
     <xsl:result-document href="{concat($directory,'/copy.xml')}">
     <project xmlns="" basedir="." default="dist" name="imagecopy">
       <target name="dist">
	 <xsl:variable name="contents">
	 <xsl:if test="not($coverimage='')">
	     <copy toFile="{$coverDir}/{tokenize($coverimage,'/')[last()]}" file="{$coverimage}"/>
	 </xsl:if>
	 <xsl:if test="$mediaoverlay='true' and key('Timeline',1)">
	   <xsl:for-each select="key('Timeline',1)">
	     <xsl:variable name="target">
	       <xsl:value-of select="replace($outputDir,'file:///','')"/>
	       <xsl:text>/</xsl:text>
	       <xsl:value-of select="$mediaDir"/>
	       <xsl:text>/audio</xsl:text>
	       <xsl:number level="any"/>
	       <xsl:text>.</xsl:text>
	       <xsl:value-of select="tokenize(@corresp,'\.')[last()]"/>
	     </xsl:variable>
	     <copy toFile="{$target}" file="{$inputDir}/{@corresp}"/>
	   </xsl:for-each>
	 </xsl:if>

	 <xsl:for-each select="key('PB',1)">
	   <xsl:choose>
	     <xsl:when test="tei:match(@rend,'none')"/>
	     <xsl:when test="not(@facs)"/>
	     <xsl:when test="starts-with(@facs,'tcp:')"/>
	     <xsl:when test="starts-with(@facs,'unknown:')"/>
	     <xsl:otherwise>
	     <xsl:variable name="F">
	     <xsl:choose>
	       <xsl:when test="starts-with(@facs,'#')">
		 <xsl:for-each
		     select="id(substring(@facs,2))">
		   <xsl:value-of select="tei:resolveURI(.,descendant-or-self::*[@url][1]/@url)"/>
		 </xsl:for-each>
	       </xsl:when>
	       <xsl:otherwise>
		 <xsl:value-of select="tei:resolveURI(.,@facs)"/>
	       </xsl:otherwise>
	     </xsl:choose>
	   </xsl:variable>
	   <xsl:variable name="target">
	     <xsl:value-of select="replace($outputDir,'file:///','')"/>
	     <xsl:text>/</xsl:text>
	     <xsl:value-of select="$mediaDir"/>
	     <xsl:text>/pageimage</xsl:text>
	     <xsl:number level="any"/>
	     <xsl:text>.</xsl:text>
	     <xsl:value-of select="tokenize($F,'\.')[last()]"/>
	   </xsl:variable>
	   <xsl:choose>
	     <xsl:when test="contains($F,':')">
	       <get src="{$F}" dest="{$target}"/>
	     </xsl:when>
	     <xsl:when test="starts-with($F,'/')">
	       <copy toFile="{$target}" file="{@url}"/>
	     </xsl:when>
	     <xsl:otherwise>
	       <copy toFile="{$target}" file="{$inputDir}/{$F}"/>
	     </xsl:otherwise>
	   </xsl:choose>
	     </xsl:otherwise>
	   </xsl:choose>
	 </xsl:for-each>

	 <xsl:for-each select="tokenize($extraGraphicsFiles,',')">
	   <xsl:variable name="target">
	     <xsl:value-of select="replace($outputDir,'file:///','')"/>
	     <xsl:text>/</xsl:text>
	     <xsl:value-of select="tokenize(.,'/')[last()]"/>
	   </xsl:variable>
	   <xsl:choose>
	     <xsl:when test="contains(.,':')">
	       <get src="{.}" dest="{$target}"/>
	     </xsl:when>
	     <xsl:when test="starts-with(.,'/')">
	       <copy toFile="{$target}" file="{.}"/>
	     </xsl:when>
	     <xsl:otherwise>
	       <copy toFile="{$target}" file="{$inputDir}/{.}"/>
	     </xsl:otherwise>
	   </xsl:choose>
	 </xsl:for-each>

	 <xsl:for-each select="key('G',1)">
	   <xsl:variable name="F">
	     <xsl:value-of select="@url"/>
	   </xsl:variable>
	   <xsl:variable name="target">
	     <xsl:value-of select="replace($outputDir,'file:///','')"/>
	     <xsl:text>/</xsl:text>
	     <xsl:value-of select="$mediaDir"/>
	     <xsl:text>/resource</xsl:text>
	     <xsl:number level="any"/>
	     <xsl:text>.</xsl:text>
	     <xsl:value-of select="tokenize($F,'\.')[last()]"/>
	   </xsl:variable>
	   <xsl:choose>
	     <xsl:when test="contains($F,':')">
	       <get src="{$F}" dest="{$target}"/>
	     </xsl:when>
	     <xsl:when test="starts-with($F,'/')">
	       <copy toFile="{$target}" file="{@url}"/>
	     </xsl:when>
	     <xsl:otherwise>
	       <copy toFile="{$target}" file="{$inputDir}/{@url}"/>
	     </xsl:otherwise>
	   </xsl:choose>
	 </xsl:for-each>
	 </xsl:variable>
	 <xsl:if test="not($contents='')">
	   <mkdir>
	     <xsl:attribute name="dir">
	       <xsl:value-of select="replace($outputDir,'file:///','')"/>
	       <xsl:text>/</xsl:text>
	       <xsl:value-of select="$mediaDir"/>
	     </xsl:attribute>
	   </mkdir>
	 </xsl:if>
	 <xsl:copy-of select="$contents"/>
       </target>
     </project>
     </xsl:result-document>
 </xsl:template>

</xsl:stylesheet>
