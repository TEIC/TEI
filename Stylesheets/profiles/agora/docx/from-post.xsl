<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns="http://www.tei-c.org/ns/1.0"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                version="2.0"
                exclude-result-prefixes="tei">
    
<xsl:output indent="yes"/>

<!-- fix up the default header -->

<xsl:template match="tei:encodingDesc"/>

<xsl:template match="tei:titleStmt/tei:author">
<xsl:choose>
<xsl:when test="tei:surname and tei:name">
  <xsl:apply-templates/>
</xsl:when>
<xsl:otherwise>
<author>
<name><xsl:value-of select="substring-before(.,' ')"/></name>
<surname><xsl:value-of select="substring-after(.,' ')"/></surname>
</author>
</xsl:otherwise>
</xsl:choose>
</xsl:template>
            
<!-- jiggle around the paragraphs which should be in front -->

<xsl:template match="tei:body">
<front>
<titlePage>
<docTitle>
<titlePart type="main"><xsl:value-of select="//tei:p[@rend='Title']"/></titlePart>
<titlePart type="sub"><xsl:value-of select="//tei:p[@rend='Subtitle']"/></titlePart>
</docTitle>
<docAuthor><xsl:value-of select="//tei:p[@rend='author']"/></docAuthor>
</titlePage>
<div type="abstract">
<xsl:for-each select="//tei:p[@rend='abstract']">
<p><xsl:apply-templates/></p>
</xsl:for-each>
</div>
</front>
<body>
<xsl:apply-templates/>
</body>
</xsl:template>

<xsl:template match="tei:body/tei:p[@rend='Title']"/>
<xsl:template match="tei:body/tei:p[@rend='author']"/>
<xsl:template match="tei:body/tei:p[@rend='Subtitle']"/>
<xsl:template match="tei:body/tei:p[@rend='abstract']"/>

<!-- fix paragraph styles which should be TEI elements -->

<xsl:template match="tei:p[@rend='epigraph']">
<epigraph>
<p><xsl:value-of select="." /></p>
</epigraph></xsl:template>

<xsl:template match="tei:p[@rend='Quote']">
<quote rend='block'>
<xsl:apply-templates/>
</quote></xsl:template>

<xsl:template match="tei:hi[@rend='Quote']">
<quote>
<xsl:apply-templates/>
</quote></xsl:template>

<xsl:template match="tei:hi[@rend='foreign']">
<foreign>
<xsl:apply-templates/>
</foreign>
</xsl:template>




<!-- now some word artefacts we want to suppress -->
<xsl:template match="tei:hi[@rend='footnote_reference']">
<xsl:apply-templates/>
</xsl:template>

<xsl:template match="tei:seg">
<!-- <xsl:if test="matches(.,'[a-zA-Z0-9]')">
<xsl:apply-templates/>
</xsl:if-->
<xsl:value-of select="."/>
</xsl:template>

<xsl:template match="tei:hi[matches(@rend,'color')]"/>


<!-- contexta magic references -->
<xsl:template match="tei:hi[@rend='reference']">
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
<xsl:template match="tei:p[@rend='Body Text First Indent']">
<p>
<xsl:apply-templates/>
</p>
</xsl:template>

<xsl:template match="tei:p[@rend='FootnoteText']">
<xsl:apply-templates/>
</xsl:template>

<!-- and copy everything else -->

<xsl:template match="*|@*|processing-instruction()">
 <xsl:copy>
  <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
 </xsl:copy>
</xsl:template>




<!-- <xsl:template match="/">

     <xsl:variable name="pass0">
       <xsl:apply-templates mode="pass0"/>
     </xsl:variable>

     <xsl:variable name="pass1">
       <xsl:for-each select="$pass0">
	 <xsl:apply-templates/>
       </xsl:for-each>
     </xsl:variable>		  

     <xsl:apply-templates select="$pass1" mode="pass2"/>
     
     <xsl:call-template name="fromDocxFinalHook"/>
   </xsl:template>

-->

</xsl:stylesheet>
