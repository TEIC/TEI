<xsl:stylesheet version="1.0"
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:teix="http://www.tei-c.org/ns/Examples"
  xmlns:rng="http://relaxng.org/ns/structure/1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:XSL="http://www.w3.org/1999/xsl/TransformAlias" >

  
<xsl:namespace-alias stylesheet-prefix="XSL" result-prefix="xsl"/>
<xsl:output indent="yes"/>
<xsl:key name="MEMBERS" 
	 match="tei:elementSpec"
	 use="tei:classes/tei:memberOf/@key"/>
<xsl:key name="POINTERLIST" 
	 match="tei:datatype/rng:ref[@name='data.pointer']"
	 use="'1'"/>
<xsl:template match="tei:TEI">
  <XSL:stylesheet 
      version="1.0"
      xmlns:teix="http://www.tei-c.org/ns/Examples"
      xmlns:tei="http://www.tei-c.org/ns/1.0">
    <xsl:for-each select="key('POINTERLIST',1)">
      <xsl:variable name="a" select="ancestor::tei:attDef/@ident"/>
      <xsl:variable name="e"
		      select="ancestor::tei:elementSpec/@ident"/>
      <xsl:variable name="c"
		      select="ancestor::tei:classSpec/@ident"/>
      <xsl:if test="not(starts-with($a,'xml'))">
	  <xsl:choose>
	    <xsl:when test="$e">
	      <XSL:template match="tei:{$e}/@{$a}">
		<XSL:call-template name="checklinks">
		  <XSL:with-param name="stuff" select="normalize-space(.)"/>
		</XSL:call-template>
	      </XSL:template>
	      <XSL:template match="teix:{$e}/@{$a}">
		<XSL:call-template name="checkexamplelinks">
		  <XSL:with-param name="stuff" select="normalize-space(.)"/>
		</XSL:call-template>
	      </XSL:template>
	    </xsl:when>
	    <xsl:otherwise>
	      <xsl:for-each select="key('MEMBERS',$c)">
		<XSL:template match="tei:{@ident}/@{$a}">
		  <XSL:call-template name="checklinks">
		    <XSL:with-param name="stuff" select="normalize-space(.)"/>
		  </XSL:call-template>
		</XSL:template>
		<XSL:template match="teix:{@ident}/@{$a}">
		  <XSL:call-template name="checkexamplelinks">
		    <XSL:with-param name="stuff" select="normalize-space(.)"/>
		  </XSL:call-template>
		</XSL:template>
	      </xsl:for-each>
	    </xsl:otherwise>
	  </xsl:choose>
	</xsl:if>

    </xsl:for-each>

  </XSL:stylesheet>
</xsl:template>

</xsl:stylesheet>

