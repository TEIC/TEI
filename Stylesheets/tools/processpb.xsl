<xsl:stylesheet 
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns:exsl="http://exslt.org/common"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
    exclude-result-prefixes="exsl tei"
    version="1.0"
>
<xsl:output method="html" indent="yes"/>

<!-- all the elements which follow a page break -->
<xsl:key name="PageContents" 
	 match="*[not(local-name()='pb')]" 
	 use="preceding-sibling::tei:pb[1]/@xml:id"/>

<!-- all the page-break elements -->
<xsl:key name="PBs" match="tei:pb" use="1"/>

<xsl:template match="/"> 
  <xsl:variable name="flat">
    <xsl:apply-templates mode="flat"/>
  </xsl:variable>
  <html>
    <body>
      <xsl:for-each select="exsl:node-set($flat)/tei:TEI">
	<xsl:for-each select="key('PBs',1)">
	  <div>
	    <h1>Page <xsl:value-of select="@n"/></h1>
	    <xsl:apply-templates select="key('PageContents',@xml:id)"/>
	  </div>
	</xsl:for-each>
      </xsl:for-each>
    </body>
  </html>

</xsl:template>


<xsl:template match="tei:milestone">
  <xsl:choose>
    <xsl:when test="@unit='div-start'">
      <p>-------div <xsl:value-of select="@xml:id"/> start ---------</p>
    </xsl:when>
    <xsl:when test="@unit='div-end'">
      <p>-------div <xsl:value-of select="@corresp"/> end ---------</p>
    </xsl:when>
    <xsl:when test="@unit='hi-start'">
      <xsl:text disable-output-escaping="yes">&lt;b&gt;</xsl:text>
    </xsl:when>
    <xsl:when test="@unit='hi-end'">
      <xsl:text disable-output-escaping="yes">&lt;/b&gt;</xsl:text>
    </xsl:when>
  </xsl:choose>
</xsl:template>

<xsl:template match="tei:p">
  <p>
    <xsl:apply-templates/>
  </p>
</xsl:template>

<!-- flattening -->
<xsl:template match="comment()|@*|processing-instruction()" mode="flat">
  <xsl:copy-of select="."/>
</xsl:template>

<xsl:template match="text()" mode="flat">
  <seg xmlns="http://www.tei-c.org/ns/1.0" type="container">
    <xsl:copy-of select="."/>
  </seg>
</xsl:template>

<xsl:template match="tei:TEI" mode="flat">
  <xsl:copy>
    <xsl:apply-templates mode="flat"/>
  </xsl:copy>
</xsl:template>

<xsl:template match="tei:pb" mode="flat">
  <xsl:copy-of select="."/>
</xsl:template>

<xsl:template match="*" mode="flat">
  <xsl:choose>
    <xsl:when test="not(*)">
      <xsl:copy-of select="."/>
    </xsl:when>
    <xsl:when test=".//tei:pb">
      <milestone unit="{local-name(.)}-start"
		 xmlns="http://www.tei-c.org/ns/1.0">
	<xsl:copy-of select="@*"/>
      </milestone>
      <xsl:apply-templates
	  select="*|processing-instruction()|comment()|text()" mode="flat"/>
      <milestone unit="{local-name(.)}-end"
		 xmlns="http://www.tei-c.org/ns/1.0">
	<xsl:if test="@xml:id">
	  <xsl:attribute name="corresp">
	    <xsl:value-of select="@xml:id"/>
	  </xsl:attribute>
	</xsl:if>
      </milestone>
    </xsl:when>
    <xsl:otherwise>
      <xsl:copy-of select="."/>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>


</xsl:stylesheet>
