<?xml version="1.0"?>
<xsl:stylesheet xmlns:rng="http://relaxng.org/ns/structure/1.0" xmlns:teix="http://www.tei-c.org/ns/Examples" xmlns:tei="http://www.tei-c.org/ns/1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:template match="text()" mode="verbatim">
    <xsl:choose>
      <xsl:when test="normalize-space(.)=''">
        <xsl:for-each select="following-sibling::*[1]">
          <xsl:call-template name="lineBreak">
            <xsl:with-param name="id">7</xsl:with-param>
          </xsl:call-template>
          <xsl:call-template name="makeIndent"/>
        </xsl:for-each>
      </xsl:when>
      <xsl:otherwise>
        <xsl:call-template name="wraptext">
          <xsl:with-param name="indent">
            <xsl:for-each select="parent::*">
              <xsl:call-template name="makeIndent"/>
            </xsl:for-each>
          </xsl:with-param>
          <xsl:with-param name="text">
            <xsl:value-of select="."/>
          </xsl:with-param>
        </xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template name="wraptext">
    <xsl:param name="indent"/>
    <xsl:param name="text"/>
    <xsl:choose>
      <xsl:when test="$text='&#10;'"/>
      <xsl:when test="contains($text,'&#10;')">
        <xsl:value-of select="substring-before($text,'&#10;')"/>
        <xsl:call-template name="lineBreak">
          <xsl:with-param name="id">6</xsl:with-param>
        </xsl:call-template>
        <xsl:value-of select="$indent"/>
        <xsl:call-template name="wraptext">
          <xsl:with-param name="indent">
            <xsl:value-of select="$indent"/>
          </xsl:with-param>
          <xsl:with-param name="text">
            <xsl:value-of select="substring-after($text,'&#10;')"/>
          </xsl:with-param>
        </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$text"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="*" mode="verbatim">
    <xsl:choose>
      <xsl:when test="not(preceding-sibling::node())">
        <xsl:call-template name="lineBreak">
          <xsl:with-param name="id">2</xsl:with-param>
        </xsl:call-template>
        <xsl:call-template name="makeIndent"/>
      </xsl:when>
      <xsl:when test="preceding-sibling::node()[1]/self::*">
        <xsl:call-template name="lineBreak">
          <xsl:with-param name="id">1</xsl:with-param>
        </xsl:call-template>
        <xsl:call-template name="makeIndent"/>
      </xsl:when>
      <xsl:when test="preceding-sibling::node()[1]/self::text()">
    </xsl:when>
      <xsl:otherwise>
        <xsl:call-template name="lineBreak">
          <xsl:with-param name="id">9</xsl:with-param>
        </xsl:call-template>
        <xsl:call-template name="makeIndent"/>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:text>&lt;</xsl:text>
    <xsl:value-of disable-output-escaping="yes" select="$startElement"/>
    <xsl:choose>
      <xsl:when test="namespace-uri()='http://relaxng.org/ns/structure/1.0'">
        <xsl:text>rng:</xsl:text>
        <xsl:value-of select="local-name(.)"/>
      </xsl:when>
      <xsl:when test="namespace-uri()='http://www.w3.org/1999/XSL/Transform'">
        <xsl:value-of disable-output-escaping="yes" select="$startNamespace"/>
        <xsl:text>xsl:</xsl:text>
        <xsl:value-of select="local-name(.)"/>
        <xsl:value-of disable-output-escaping="yes" select="$endNamespace"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="local-name(.)"/>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:value-of disable-output-escaping="yes" select="$endElement"/>
    <xsl:for-each select="@*">
      <xsl:text>&#xA0;</xsl:text>
      <xsl:value-of disable-output-escaping="yes" select="$startAttribute"/>
      <xsl:if test="namespace-uri()='http://relaxng.org/ns/structure/1.0'">
        <xsl:text>xml:</xsl:text>
      </xsl:if>
      <xsl:value-of select="local-name(.)"/>
      <xsl:value-of disable-output-escaping="yes" select="$endAttribute"/>
      <xsl:text>="</xsl:text>
      <xsl:value-of disable-output-escaping="yes" select="$startAttributeValue"/>
      <xsl:value-of select="."/>
      <xsl:value-of disable-output-escaping="yes" select="$endAttributeValue"/>
      <xsl:text>"</xsl:text>
    </xsl:for-each>
    <xsl:choose>
      <xsl:when test="child::node()">
        <xsl:text>&gt;</xsl:text>
        <xsl:apply-templates mode="verbatim"/>
        <xsl:choose>
          <xsl:when test="child::node()[last()]/self::text()[normalize-space(.)='']">
            <xsl:call-template name="lineBreak">
              <xsl:with-param name="id">3</xsl:with-param>
            </xsl:call-template>
            <xsl:call-template name="makeIndent"/>
          </xsl:when>
          <xsl:when test="child::node()[last()]/self::comment()">
            <xsl:call-template name="lineBreak">
              <xsl:with-param name="id">4</xsl:with-param>
            </xsl:call-template>
            <xsl:call-template name="makeIndent"/>
          </xsl:when>
          <xsl:when test="child::node()[last()]/self::*">
            <xsl:call-template name="lineBreak">
              <xsl:with-param name="id">5</xsl:with-param>
            </xsl:call-template>
            <xsl:call-template name="makeIndent"/>
          </xsl:when>
        </xsl:choose>
        <xsl:text>&lt;/</xsl:text>
        <xsl:value-of disable-output-escaping="yes" select="$startElement"/>
        <xsl:choose>
          <xsl:when test="namespace-uri()='http://relaxng.org/ns/structure/1.0'">
            <xsl:text>rng:</xsl:text>
            <xsl:value-of select="local-name(.)"/>
          </xsl:when>
          <xsl:when test="namespace-uri()='http://www.w3.org/1999/XSL/Transform'">
            <xsl:value-of disable-output-escaping="yes" select="$startNamespace"/>
            <xsl:text>xsl:</xsl:text>
            <xsl:value-of select="local-name(.)"/>
            <xsl:value-of disable-output-escaping="yes" select="$endNamesapce"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="local-name(.)"/>
          </xsl:otherwise>
        </xsl:choose>
        <xsl:value-of disable-output-escaping="yes" select="$endElement"/>
        <xsl:text>&gt;</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>/&gt;</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template name="makeIndent">
    <xsl:for-each select="ancestor::*[not(namespace-uri()='http://www.tei-c.org/ns/1.0')]">
      <xsl:value-of select="$spaceCharacter"/>
    </xsl:for-each>
  </xsl:template>
</xsl:stylesheet>
