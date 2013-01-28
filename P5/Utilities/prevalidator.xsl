<xsl:stylesheet version="2.0" xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:teix="http://www.tei-c.org/ns/Examples" xmlns:rng="http://relaxng.org/ns/structure/1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:XSL="http://www.w3.org/1999/xsl/TransformAlias">

  <!-- make list  of attributes which refer to data.pointer,
so that we can check them -->
  <xsl:namespace-alias stylesheet-prefix="XSL" result-prefix="xsl"/>
  <xsl:output indent="yes"/>
  <xsl:key name="MEMBERS" match="tei:elementSpec" use="tei:classes/tei:memberOf/@key"/>
  <xsl:template match="tei:TEI">
    <XSL:stylesheet version="2.0" xmlns:teix="http://www.tei-c.org/ns/Examples"
      xmlns:tei="http://www.tei-c.org/ns/1.0">
      <xsl:for-each select="//tei:datatype/rng:ref[@name='data.pointer']">
        <xsl:variable name="a" select="ancestor::tei:attDef/@ident"/>
        <xsl:variable name="e" select="ancestor::tei:elementSpec/@ident"/>
        <xsl:variable name="c" select="ancestor::tei:classSpec/@ident"/>
        <xsl:if test="not(starts-with($a,'xml'))">
          <xsl:choose>
            <xsl:when test="$e">
              <xsl:choose>
                <xsl:when test="parent::tei:datatype[not(@maxOccurs)]">
                  <!--	This is a single data.pointer value, not a list of them.         -->
                  <XSL:template match="tei:{$e}/@{$a}">
                    <XSL:call-template name="checkThisLink">
                      <XSL:with-param name="What" select="normalize-space(.)"/>
                    </XSL:call-template>
                  </XSL:template>
                  <XSL:template match="teix:{$e}/@{$a}">
                    <XSL:call-template name="checkThisExampleLink">
                      <XSL:with-param name="What" select="normalize-space(.)"/>
                    </XSL:call-template>
                  </XSL:template>
                </xsl:when>
                <xsl:otherwise>
<!-- This is a list of data.pointer values. -->
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
                </xsl:otherwise>
              </xsl:choose>
            </xsl:when>
            <xsl:otherwise>
              <xsl:for-each select="key('MEMBERS',$c)">
                <xsl:if test="not(.//tei:attDef[@ident=$a and @mode])">
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
                </xsl:if>
              </xsl:for-each>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:if>

      </xsl:for-each>

    </XSL:stylesheet>
  </xsl:template>

</xsl:stylesheet>
