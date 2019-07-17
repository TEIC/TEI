<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns="http://www.tei-c.org/ns/1.0"
 xmlns:tei="http://www.tei-c.org/ns/1.0" version="2.0" exclude-result-prefixes="tei">

 <xsl:import href="../../../docx/from/docxtotei.xsl"/>

<!-- 1 : here's what we do in pass2 -->

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

 <!-- jiggle around the paragraphs which should be in front -->
 <xsl:template match="tei:text" mode="pass2">
  <text>
   <front>
    <titlePage>
     <docTitle>
      <titlePart type="main">
       <xsl:value-of select="//tei:p[@rend='Title']/text()"/>
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
       <xsl:apply-templates mode="pass2"/>
      </p>
     </xsl:for-each>
    </div>
   </front>
   <body>
    <xsl:apply-templates mode="pass2" select="tei:body/*"/>
   </body>
   <back>
    <listBibl>
     <xsl:for-each select="//tei:p[@rend='bibliography']">
      <bibl>
       <xsl:apply-templates mode="pass2"/>
      </bibl>
     </xsl:for-each>
    </listBibl>
   </back>
  </text>
 </xsl:template>
 <!-- suppress paragraphs which have been jiggled into front/back -->
 <xsl:template match="tei:p[@rend='Title']" mode="pass2"/>
 <xsl:template match="tei:p[@rend='author']" mode="pass2"/>
 <xsl:template match="tei:p[@rend='Subtitle']" mode="pass2"/>
 <xsl:template match="tei:p[@rend='abstract']" mode="pass2"/>
 <xsl:template match="tei:p[@rend='bibliography']" mode="pass2"/>
 <!-- suppress empty head elements -->
 <xsl:template match="tei:head" mode="pass3">
  <xsl:if test="string-length(.)!=0">
   <head>
    <xsl:value-of select="."/>
   </head>
  </xsl:if>
 </xsl:template>
 <!-- fix paragraph styles which should be TEI elements -->
 <xsl:template match="tei:p[@rend='epigraph']" mode="pass2">
  <epigraph>
    <xsl:value-of select="."/>
  </epigraph>
 </xsl:template>

 <xsl:template match="tei:p[@rend='Quote']" mode="pass2">
  <quote rend="block">
   <p>
    <xsl:apply-templates mode="pass2"/>
   </p>
  </quote>
 </xsl:template>

 <xsl:template match="tei:note" mode="pass2">
  <xsl:element name="note">
   <xsl:attribute name="xml:id">
    <xsl:text>N</xsl:text>
    <xsl:number level="any"/>
   </xsl:attribute>
   <xsl:apply-templates mode="pass2"/>
  </xsl:element>
 </xsl:template>

<!-- (2) and here's what do in pass 3 -->

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


 <!-- fix other styles which should be TEI elements -->
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

<xsl:template match="tei:hi[@rend='Article_Title_Char']" mode="pass3">
<title level="a">
   <xsl:apply-templates mode="pass3"/>
</title>
</xsl:template>

<xsl:template match="tei:hi[@rend='Date_Pub']" mode="pass3">
<date>
   <xsl:apply-templates mode="pass3"/>
</date>
</xsl:template>

<xsl:template match="tei:bibl/tei:hi[@rend='italic']" mode="pass3">
<title>
   <xsl:apply-templates mode="pass3"/>
</title>
</xsl:template>


 <!-- now some word artefacts we want to suppress -->

 <xsl:template match="tei:hi[@rend='footnote_reference']" mode="pass3">
  <xsl:apply-templates mode="pass3"/>
 </xsl:template>

 <xsl:template match="tei:hi[@rend='FootnoteReference']" mode="pass3">
  <xsl:apply-templates mode="pass3"/>
 </xsl:template>

<!--
 <xsl:template match="tei:seg" mode="pass3">
  <xsl:value-of select="."/>
 </xsl:template>
-->
 

 <xsl:template match="tei:p" mode="pass3">
  <xsl:if test="ancestor::tei:body">
   <xsl:element name="p">
    <xsl:attribute name="xml:id">
     <xsl:text>P</xsl:text>
     <xsl:number from="tei:body" count="tei:p[not(ancestor::tei:note)]" level="any"/>
    </xsl:attribute>
    <xsl:apply-templates mode="pass3"/>
   </xsl:element>
  </xsl:if>
 </xsl:template>
 <xsl:template match="tei:hi[matches(@rend,'color')]" mode="pass3"/>

 <!-- contexta magic references -->

 <xsl:template match="tei:hi[@rend='reference']" mode="pass3">
  <xsl:variable name="magicString">
   <xsl:value-of select="substring-before(substring-after(., '&lt;'),'&gt;')"/>
  </xsl:variable>

<xsl:variable name="parentN">
<xsl:choose>
  <xsl:when test="ancestor::tei:note">
     <xsl:text>#N</xsl:text>
     <xsl:number from="tei:body" count="tei:note" level="any"/>
</xsl:when>
<xsl:otherwise>
     <xsl:text>#P</xsl:text>
     <xsl:number from="tei:body" count="tei:p[not(ancestor::tei:note)]" level="any"/>
</xsl:otherwise></xsl:choose></xsl:variable>

  <xsl:element name="ref">
   <xsl:attribute name="cRef">
    <xsl:value-of select="$magicString"/>
   </xsl:attribute>
   <xsl:attribute name="corresp">
<xsl:value-of select="$parentN"/>
 	        </xsl:attribute>
<xsl:apply-templates mode="pass3"/>
  </xsl:element>
 </xsl:template>

 <xsl:template match="tei:hi[@rend='reference']/tei:seg" mode="pass3">
<hi rend="{@rend}">
<xsl:value-of select="."/>
</hi>
</xsl:template>


 <xsl:template match="tei:hi[@rend='reference']/text()" mode="pass3">
<xsl:value-of select='substring-before(.,"&lt;")'/>
<xsl:if test="not(contains(.,'&lt;'))">
<xsl:value-of select="."/>
</xsl:if>
<xsl:value-of select='substring-after(.,"&gt;")'/>
</xsl:template>


 <!-- now some attribute values we want to kill -->
 <xsl:template match="@rend[.='Body Text First Indent']" mode="pass3"/>
 <xsl:template match="@rend[.='Body Text']" mode="pass3"/>
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
 <!-- <xsl:template match="/">        <xsl:variable name="pass0">         <xsl:apply-templates mode="pass0"/>       </xsl:variable>        <xsl:variable name="pass1">         <xsl:for-each select="$pass0"> 	 <xsl:apply-templates mode="pass3"/>         </xsl:for-each>       </xsl:variable>		        <xsl:apply-templates select="$pass1" mode="pass2"/>              <xsl:call-template name="fromDocxFinalHook"/>     </xsl:template>  -->
</xsl:stylesheet>
