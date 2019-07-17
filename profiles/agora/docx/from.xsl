<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns="http://www.tei-c.org/ns/1.0"
 xmlns:tei="http://www.tei-c.org/ns/1.0" version="2.0" exclude-result-prefixes="tei">

 <xsl:import href="../../../docx/from/docxtotei.xsl"/>

<!-- 1 : here's what we do in pass2 -->

 <xsl:template match="tei:TEI" mode="pass2">
  <xsl:variable name="Doctext">
   <xsl:copy>
    <xsl:apply-templates mode="pass2"/>
   </xsl:copy>
  </xsl:variable>
  <xsl:apply-templates select="$Doctext" mode="pass3"/>
 </xsl:template>

 <xsl:template match="tei:TEI" mode="pass3">
  <xsl:variable name="Doctext2">
   <xsl:copy>
    <xsl:apply-templates mode="pass3"/>
   </xsl:copy>
  </xsl:variable>
  <xsl:apply-templates select="$Doctext2" mode="pass4"/>
 </xsl:template>

 
 <!-- paragraph styles which should be TEI elements -->
 <!--xsl:template match="tei:p[tei:match(@rend,'epigraph')]" mode="pass2">
  <epigraph>
   <ab>
    <xsl:apply-templates mode="pass2"/>
   </ab>
  </epigraph>
 </xsl:template-->
 
 <xsl:template match="tei:p[tei:match(@rend,'epigraph')][1]" mode="pass2">
  <epigraph>
   <p>
    <xsl:apply-templates mode="pass2"/>
   </p>
   <xsl:for-each select="following::tei:p[tei:match(@rend,'epigraph')]">
    <p> <xsl:apply-templates mode="pass2"/></p>
   </xsl:for-each>
  </epigraph>
 </xsl:template>
 
 <xsl:template match="tei:p[tei:match(@rend,'epigraph')][position() > 1]" mode="pass2"/>
 

 <xsl:template match="tei:p[tei:match(@rend,'Quote')]" mode="pass2">
  <quote>
   <p>
    <xsl:apply-templates mode="pass2"/>
   </p>
  </quote>
 </xsl:template>

 <xsl:template match="tei:p[tei:match(@rend,'CentredQuote')]" mode="pass2">
  <quote rend="center">
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


<!-- templates for pass3 start here -->

 <!-- jiggle around the paragraphs which should be in front -->

 <xsl:template match="tei:text" mode="pass3">
  <text>  
   <front>
    <titlePage>
     <docTitle>
         <titlePart type="main">
        <xsl:for-each select="//tei:p[tei:match(@rend,'Title')]/node()">
        
           <xsl:copy-of select="."/>        
        </xsl:for-each> 
      </titlePart>
      <titlePart type="sub">
       <xsl:for-each select="//tei:p[tei:match(@rend,'Subtitle')]/node()">
        
          <xsl:copy-of select="."/>        
       </xsl:for-each> </titlePart>

     </docTitle>
     
     <docAuthor>
      <xsl:for-each select="//tei:p[tei:match(@rend,'author')]/node()">
       <xsl:copy-of select="."/>    
       </xsl:for-each>      
     </docAuthor>
     
     <xsl:if test="//tei:p[tei:match(@rend,'translator')]">
     <titlePart type="translator">
      <xsl:for-each select="//tei:p[tei:match(@rend,'translator')]">
       <xsl:apply-templates mode="pass3"/>
      </xsl:for-each>
     </titlePart>
     </xsl:if>
    
    </titlePage>
    
    <xsl:if test="//tei:p[tei:match(@rend,'abstract')]">
     <div type="abstract">
     <xsl:for-each select="//tei:p[tei:match(@rend,'abstract')]">
    <xsl:copy-of select="."/>
     </xsl:for-each>
    </div></xsl:if>
   </front>
   
   <body>
    <xsl:apply-templates mode="pass3" select="tei:body/*"/>
   </body>
   
    <xsl:if test="//tei:p[tei:match(@rend,'bibliography')]">
     <back>
      <div type="bibliography"><head>References</head>
      <listBibl>
     <xsl:for-each select="//tei:p[tei:match(@rend,'bibliography')]">
      <bibl>
       <xsl:apply-templates mode="pass3"/>
      </bibl>
     </xsl:for-each>
    </listBibl></div>
     </back>
   </xsl:if>
  </text>
 </xsl:template>

 <!-- suppress paragraphs which have been jiggled into front/back -->

 <xsl:template match="tei:p[tei:match(@rend,'Title')]" mode="pass3"/>
 <xsl:template match="tei:p[tei:match(@rend,'author')]" mode="pass3"/>
 <xsl:template match="tei:p[tei:match(@rend,'translator')]" mode="pass3"/>
 <xsl:template match="tei:p[tei:match(@rend,'Subtitle')]" mode="pass3"/>
 <xsl:template match="tei:p[tei:match(@rend,'abstract')]" mode="pass3"/>
 <xsl:template match="tei:p[tei:match(@rend,'bibliography')]" mode="pass3"/>
 <xsl:template match="tei:p[tei:match(@rend,'Bibliography')]" mode="pass3"/>
 <xsl:template match="tei:div/tei:p[tei:match(@rend,'bibliography')]" mode="pass3"/>
 
 <!-- suppress empty head elements -->

 <xsl:template match="tei:head" mode="pass3">
  <xsl:if test="string-length(.)!=0">
   <head>
    <xsl:value-of select="."/>
   </head>
  </xsl:if>
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

 <xsl:template match="tei:fileDesc/tei:sourceDesc" mode="pass3">
  <sourceDesc>   <p>A Source description is required</p>
  </sourceDesc>  
 </xsl:template>
 
<xsl:template match="tei:publicationStmt" mode="pass3">
   <publicationStmt> <p>A  Publication Statement is required</p></publicationStmt>
 </xsl:template>
 
 <!-- fix other styles which should be TEI elements -->
 <xsl:template match="tei:hi[tei:match(@rend,'Quote')]" mode="pass3">
  <quote>
   <xsl:apply-templates mode="pass3"/>
  </quote>
 </xsl:template>

 <xsl:template match="tei:hi[tei:match(@rend,'foreign')]" mode="pass3">
  <foreign>
   <xsl:apply-templates mode="pass3"/>
  </foreign>
 </xsl:template>

<xsl:template match="tei:hi[tei:match(@rend,'Article_Title_Char')]" mode="pass3">
<title level="a">
   <xsl:apply-templates mode="pass3"/>
</title>
</xsl:template>

<xsl:template match="tei:hi[tei:match(@rend,'Date_Pub')]" mode="pass3">
<date>
   <xsl:apply-templates mode="pass3"/>
</date>
</xsl:template>

<xsl:template match="tei:bibl/tei:hi[tei:match(@rend,'italic')]" mode="pass3">
<title>
   <xsl:apply-templates mode="pass3"/>
</title>
</xsl:template>


 <!-- now some word artefacts we want to suppress -->

 <xsl:template match="tei:hi[tei:match(@rend,'footnote_reference')]" mode="pass3">
  <xsl:apply-templates mode="pass3"/>
 </xsl:template>

 <xsl:template match="tei:hi[tei:match(@rend,'FootnoteReference')]" mode="pass3">
  <xsl:apply-templates mode="pass3"/>
 </xsl:template>

<!-- pass 4 final tweaks -->

<!-- add level indicator to divs -->

 <xsl:template match="//tei:body/tei:div/tei:div/tei:div/tei:div[child::tei:head]" mode="pass4">
  <xsl:element name="div">
   <xsl:attribute  name="type">level4</xsl:attribute>
   <xsl:apply-templates mode="pass4"/>
  </xsl:element>
 </xsl:template>
 
 <xsl:template match="//tei:body/tei:div/tei:div/tei:div[child::tei:head]" mode="pass4">
  <xsl:element name="div">
   <xsl:attribute  name="type">level3</xsl:attribute>
     <xsl:apply-templates mode="pass4"/>
  </xsl:element>
 </xsl:template>

 <xsl:template match="//tei:body/tei:div/tei:div[child::tei:head]" mode="pass4">
  <xsl:element name="div">
   <xsl:attribute  name="type">level2</xsl:attribute>
   <xsl:apply-templates mode="pass4"/>
  </xsl:element>
 </xsl:template>
 
 <xsl:template match="//tei:body/tei:div[child::tei:head]" mode="pass4">
  <xsl:element name="div">
   <xsl:attribute  name="type">level1</xsl:attribute>
   <xsl:apply-templates mode="pass4"/>
  </xsl:element>
 </xsl:template>
 
 <xsl:template match="//tei:body/tei:div[child::tei:epigraph]" mode="pass4">
  <xsl:apply-templates mode="pass4"/>
  </xsl:template>
 
<!-- kill paragraphs inside notes -->
<xsl:template match="tei:note/tei:p" mode="pass3">
<xsl:apply-templates mode="pass4"/>
</xsl:template>

<!-- number paragraphs within body -->
 
 <xsl:template match="tei:p" mode="pass4">
 
   <xsl:element name="p">
    <xsl:if test="ancestor::tei:body">
      <xsl:attribute name="xml:id">
     <xsl:text>P</xsl:text>
     <xsl:number from="tei:body" count="tei:p[not(ancestor::tei:note)]" level="any"/>
    </xsl:attribute>
</xsl:if>
    <xsl:apply-templates mode="pass4"/>
   </xsl:element>
   </xsl:template>
 
 <xsl:template match="tei:hi[matches(@rend,'color')]" mode="pass3"/>

 <!-- add contexta magic references -->

 <xsl:template match="tei:hi[tei:match(@rend,'reference')]" mode="pass4">
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
<xsl:apply-templates mode="pass4"/>
  </xsl:element>
 </xsl:template>

 <xsl:template match="tei:hi[tei:match(@rend,'reference')]/tei:seg" mode="pass4">
<hi rend="{@rend}">
<xsl:value-of select="."/>
</hi>
</xsl:template>

 <xsl:template match="tei:hi[tei:match(@rend,'reference')]/text()" mode="pass4">
<xsl:value-of select='substring-before(.,"&lt;")'/>
<xsl:if test="not(contains(.,'&lt;'))">
<xsl:value-of select="."/>
</xsl:if>
<xsl:value-of select='substring-after(.,"&gt;")'/>
</xsl:template>


 <!-- now some attribute values we want to kill -->
 <xsl:template match="@rend[.='Body Text First Indent']" mode="pass3"/>
 <xsl:template match="@rend[.='Body Text']" mode="pass3"/>
 <xsl:template match="tei:p[tei:match(@rend,'FootnoteText')]" mode="pass3">
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


 <xsl:template match="@*|comment()|processing-instruction()|text()" mode="pass4">
  <xsl:copy-of select="."/>
 </xsl:template>
 <xsl:template match="*" mode="pass4">
  <xsl:copy>
   <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()" mode="pass4"/>
  </xsl:copy>
 </xsl:template>

</xsl:stylesheet>
