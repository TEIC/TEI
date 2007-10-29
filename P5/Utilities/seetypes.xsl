<xsl:stylesheet 
xmlns:rng="http://relaxng.org/ns/structure/1.0"
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
  version="1.0"
>
<xsl:output method="xml" indent="yes"/>

<xsl:template match="/">
<TEI.2>
  <teiHeader>
    <fileDesc>
      <titleStmt>
        <title>Attributes values</title>
        <author>Sebastian Rahtz</author>
      </titleStmt>
      <editionStmt>
        <edition>
          <date>March 2004</date>
        </edition>
      </editionStmt>
      <publicationStmt>
        <authority/>
      </publicationStmt>
      <sourceDesc>
        <p/>
      </sourceDesc>
    </fileDesc>
  </teiHeader>
  <text>
<body>
<div>
<head>CDATA attributes</head>
<p>
  <table rend="rules">
  <xsl:for-each select=".//tei:attDef">
    <xsl:sort select="ancestor::tei:classSpec/@ident|ancestor::tei:elementSpec/@ident"/>
    <xsl:sort select="@ident"/>
<xsl:if test="tei:datatype/rng:text">
    <row>
    <cell><xsl:value-of select="ancestor::tei:classSpec/@ident|ancestor::tei:elementSpec/@ident"/></cell>
    <cell><xsl:value-of select="@ident"/></cell>
    <cell><xsl:value-of select="tei:valDesc"/></cell>
  </row>
</xsl:if>
  </xsl:for-each>
</table>
</p>

</div>
<!--
<div>
<head>All attributes</head>
<p>
  <table rend="rules">
  <xsl:for-each select=".//tei:attDef">
    <xsl:sort select="../../@ident"/>
    <xsl:sort select="@ident"/>
    <row>
    <cell><xsl:value-of select="name(../..)"/></cell>
    <cell><xsl:value-of select="../../@ident"/></cell>
    <cell><xsl:value-of select="@ident"/></cell>
    <cell><xsl:value-of select="tei:valDesc"/></cell>
    <cell>
<xsl:choose>
 <xsl:when test="tei:datatype/rng:data">
    XSD Datatype <xsl:value-of select="tei:datatype/rng:data/@type"/>
 </xsl:when>
 <xsl:when test="tei:datatype/@key">
    <xsl:value-of select="tei:datatype/@key"/>
 </xsl:when>
 <xsl:otherwise>
   <xsl:for-each select="tei:valList/tei:valItem">
     <xsl:value-of select="@ident"/>
     <xsl:if test="following-sibling::tei:valItem"> | </xsl:if>
   </xsl:for-each>
 </xsl:otherwise>
</xsl:choose>
    </cell>
  </row>
  </xsl:for-each>
</table>
</p>
</div>
-->
</body>
  </text>
</TEI.2>
</xsl:template>
</xsl:stylesheet>
