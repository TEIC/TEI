<!-- 
Text Encoding Initiative Consortium XSLT stylesheet family
$Date$, $Revision$, $Author$

XSL stylesheet to format TEI XML documents to HTML or XSL FO

 
##LICENSE
--> 
<xsl:stylesheet
  xmlns:tei="http://www.tei-c.org/ns/1.0"

  xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">



<!-- top-level stuff -->

<xsl:template match="tei:docImprint"/>


<xsl:template match="tei:front|titlePart">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="tei:titlePage">
    <hr/>
    <p>
      <!-- first, the complete <docTitle> in bold -->
      <span class="docTitle">
        <xsl:value-of select="normalize-space(tei:docTitle)"/>
      </span>
    </p>
    <p>
      <xsl:text>by </xsl:text>
      <xsl:for-each select="tei:docAuthor">
	<xsl:if test="preceding-sibling::tei:docAuthor"> 
	  <xsl:choose>
	    <xsl:when test="not(following-sibling::tei:docAuthor">
	      <xsl:text> and </xsl:text>
	    </xsl:when>
	    <xsl:otherwise>
	       <xsl:text>, </xsl:text>
	    </xsl:otherwise>
	  </xsl:choose>
	</xsl:if>
	<span class="docAuthor">
	  <xsl:apply-templates select="." mode="print"/>
	</span>
      </xsl:for-each>
    </p>
    <xsl:if test="tei:docDate">
      <p class="docDate">
	<xsl:text>on </xsl:text>
	<xsl:apply-templates mode="print" select="tei:docDate"/>
      </p>
    </xsl:if>
    <hr/>
</xsl:template>

<xsl:template match="tei:body|tei:back" mode="split">
  <xsl:for-each select="*">
   <xsl:choose>
    <xsl:when test="starts-with(local-name(.),'div')">
       <xsl:apply-templates select="." mode="split"/>
    </xsl:when>
    <xsl:otherwise>
       <xsl:apply-templates select="."/>
    </xsl:otherwise>
   </xsl:choose>
  </xsl:for-each>
</xsl:template>

<xsl:template match="tei:teiHeader"/>

<!-- author and title -->
<xsl:template match="tei:docTitle"/>
<xsl:template match="tei:docAuthor"/>
<xsl:template match="tei:docDate"/>

<xsl:template match="tei:docDate" mode="print">
    <xsl:apply-templates/>
</xsl:template>

<xsl:template match="tei:docAuthor" mode="author">
     <xsl:if test="preceding-sibling::tei:docAuthor">
	<xsl:text>, </xsl:text>
     </xsl:if>
    <xsl:apply-templates/>
</xsl:template>



</xsl:stylesheet>
