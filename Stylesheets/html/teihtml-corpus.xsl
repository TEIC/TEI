<!-- 
Text Encoding Initiative Consortium XSLT stylesheet family
$Date$, $Revision$, $Author$

XSL stylesheet to format TEI XML documents to HTML or XSL FO

 
##LICENSE
--> 
<xsl:stylesheet
  xmlns:tei="http://www.tei-c.org/ns/1.0"

  xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<xsl:template match="tei:teiCorpus">
 <xsl:for-each select="tei:TEI">
 <xsl:if test="$verbose">
   <xsl:message>Process <xsl:value-of select="tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:title"/></xsl:message>
 </xsl:if>
   <xsl:apply-templates select="." mode="split"/>
 </xsl:for-each>
 <html><xsl:call-template name="addLangAtt"/> 
 <head>
 <title><xsl:apply-templates select="tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:title/text()"/></title>
 <xsl:call-template name="includeCSS"/>
 </head>
 <body>
 <xsl:call-template name="bodyHook"/>
 <xsl:call-template name="bodyJavaScript"/>
 <xsl:call-template name="stdheader">
  <xsl:with-param name="title">
   <xsl:apply-templates select="tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:title"/>
  </xsl:with-param>
 </xsl:call-template>

 <xsl:call-template name="corpusBody"/>

 <xsl:call-template name="stdfooter">
       <xsl:with-param name="date">
         <xsl:choose>
          <xsl:when test="tei:teiHeader/tei:revisionDesc//tei:date[1]">
            <xsl:value-of select="tei:teiHeader/tei:revisionDesc//tei:date[1]"/>
          </xsl:when>
          <xsl:otherwise>
    	   <xsl:text>(undated)</xsl:text>
          </xsl:otherwise>    
         </xsl:choose>
       </xsl:with-param>
       <xsl:with-param name="author"/>
   </xsl:call-template>
 </body>
 </html>
</xsl:template>


<xsl:template name="corpusBody">
<ul>
 <xsl:for-each select="tei:TEI">
 <li>
    <a> <xsl:attribute name="href">
     <xsl:apply-templates mode="generateLink" select="."/>
     </xsl:attribute>
     <xsl:call-template name="header">
     <xsl:with-param name="minimal"/>
     </xsl:call-template>
     </a>  
 </li>
 </xsl:for-each>
</ul>
</xsl:template>

<xsl:template match="tei:catRef">
  <xsl:if test="preceding-sibling::tei:catRef"><xsl:text> </xsl:text></xsl:if>
  <em><xsl:value-of select="@scheme"/></em>:
  <xsl:apply-templates select="key('IDS',@target)/catDesc"/>
</xsl:template>

</xsl:stylesheet>
