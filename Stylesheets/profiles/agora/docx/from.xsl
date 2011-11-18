<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns="http://www.tei-c.org/ns/1.0" xmlns:tei="http://www.tei-c.org/ns/1.0" version="2.0" exclude-result-prefixes="tei">
  <xsl:import href="../../../docx/from/docxtotei.xsl"/>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>set up a pass3 </desc>
  </doc>
  <xsl:template match="tei:TEI" mode="pass2">
    <xsl:variable name="Doctext">
      <xsl:copy>
        <xsl:apply-templates mode="pass2"/>
      </xsl:copy>
    </xsl:variable>
    <xsl:apply-templates select="$Doctext" mode="pass3"/>
  </xsl:template>
  <!-- fix up the default header -->
  <xsl:template match="tei:encodingDesc" mode="pass3"/>
  <xsl:template match="tei:titleStmt/tei:author" mode="pass3">
    <xsl:choose>
      <xsl:when test="tei:surname and tei:name">
        <xsl:apply-templates/>
      </xsl:when>
      <xsl:otherwise>
        <author>
          <name>
            <xsl:value-of select="substring-before(.,' ')"/>
          </name>
          <surname>
            <xsl:value-of select="substring-after(.,' ')"/>
          </surname>
        </author>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <!-- jiggle around the paragraphs which should be in front -->
  <xsl:template match="tei:body" mode="pass3">
    <front>
      <titlePage>
        <docTitle>
          <titlePart type="main">
            <xsl:value-of select="//tei:p[@rend='Title']"/>
          </titlePart>
          <titlePart type="sub">
            <xsl:value-of select="//tei:p[@rend='Subtitle']"/>
          </titlePart>
        </docTitle>
        <docAuthor>
          <xsl:value-of select="//tei:p[@rend='author']"/>
        </docAuthor>
      </titlePage>
      <div type="abstract">
        <xsl:for-each select="//tei:p[@rend='abstract']">
          <p>
            <xsl:apply-templates mode="pass3"/>
          </p>
        </xsl:for-each>
      </div>
    </front>
    <body>
      <xsl:apply-templates mode="pass3"/>
    </body>
  </xsl:template>
  <xsl:template match="tei:body/tei:p[@rend='Title']" mode="pass3"/>
  <xsl:template match="tei:body/tei:p[@rend='author']" mode="pass3"/>
  <xsl:template match="tei:body/tei:p[@rend='Subtitle']" mode="pass3"/>
  <xsl:template match="tei:body/tei:p[@rend='abstract']" mode="pass3"/>
  <!-- fix paragraph styles which should be TEI elements -->
  <xsl:template match="tei:p[@rend='epigraph']" mode="pass3">
    <epigraph>
      <p>
        <xsl:value-of select="."/>
      </p>
    </epigraph>
  </xsl:template>
  <xsl:template match="tei:p[@rend='Quote']" mode="pass3">
    <quote rend="block">
      <xsl:apply-templates mode="pass3"/>
    </quote>
  </xsl:template>
  <xsl:template match="tei:hi[@rend='Quote']" mode="pass3">
    <quote>
      <xsl:apply-templates mode="pass3"/>
    </quote>
  </xsl:template>
  <xsl:template match="tei:hi[@rend='foreign']" mode="pass3">
    <foreign>
      <xsl:apply-templates mode="pass3"/>
    </foreign>
  </xsl:template>
  <!-- now some word artefacts we want to suppress -->
  <xsl:template match="tei:hi[@rend='footnote_reference']" mode="pass3">
    <xsl:apply-templates mode="pass3"/>
  </xsl:template>
  <xsl:template match="tei:seg" mode="pass3">
    <!-- <xsl:if test="matches(.,'[a-zA-Z0-9]')">
<xsl:apply-templates mode="pass3"/>
</xsl:if-->
    <xsl:value-of select="."/>
  </xsl:template>
  <xsl:template match="tei:hi[matches(@rend,'color')]" mode="pass3"/>
  <!-- contexta magic references -->
  <xsl:template match="tei:hi[@rend='reference']" mode="pass3">
    <xsl:variable name="magicString">
      <xsl:value-of select="substring-before(substring-after(., '&lt;'),'&gt;')"/>
    </xsl:variable>
    <xsl:element name="ref">
      <xsl:attribute name="cRef">
        <xsl:value-of select="$magicString"/>
      </xsl:attribute>
      <xsl:value-of select="substring-before(.,'&lt;')"/>
    </xsl:element>
  </xsl:template>
  <!-- now some attribute values we want to kill -->
  <xsl:template match="tei:p[@rend='Body Text First Indent']" mode="pass3">
    <p>
      <xsl:apply-templates mode="pass3"/>
    </p>
  </xsl:template>
  <xsl:template match="tei:p[@rend='FootnoteText']" mode="pass3">
    <xsl:apply-templates mode="pass3"/>
  </xsl:template>
  <!-- and copy everything else -->
  <xsl:template match="@*|comment()|processing-instruction()|text()" mode="pass3">
    <xsl:copy-of select="."/>
  </xsl:template>
  <xsl:template match="*" mode="pass3">
    <xsl:copy>
      <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()" mode="pass3"/>
    </xsl:copy>
  </xsl:template>
  <!-- <xsl:template match="/">

     <xsl:variable name="pass0">
       <xsl:apply-templates mode="pass0"/>
     </xsl:variable>

     <xsl:variable name="pass1">
       <xsl:for-each select="$pass0">
	 <xsl:apply-templates mode="pass3"/>
       </xsl:for-each>
     </xsl:variable>		  

     <xsl:apply-templates select="$pass1" mode="pass2"/>
     
     <xsl:call-template name="fromDocxFinalHook"/>
   </xsl:template>

-->
</xsl:stylesheet>
