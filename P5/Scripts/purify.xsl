<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xpath-default-namespace="http://www.tei-c.org/ns/1.0" xmlns="http://www.tei-c.org/ns/1.0"
  xmlns:rng="http://relaxng.org/ns/structure/1.0" exclude-result-prefixes="#all" version="2.0">

  <xsl:output method="xml" indent="yes"/>
  
  <!-- Overall, this is an identity transform: anything that is not an ODD element -->
  <!-- matched below just gets copied over. -->
  <xsl:template match="node()">
    <xsl:if test="not(ancestor::*)">
      <xsl:text>&#x0A;</xsl:text>
    </xsl:if>
    <xsl:copy>
      <xsl:apply-templates select="@* | node()"/>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="@*">
    <xsl:copy/>
  </xsl:template>
    
  <!-- process TEI ODD elements that need to change -->
  <xsl:template match="datatype">
    <xsl:copy>
      <xsl:apply-templates select="@*"/>
      <xsl:choose>
        <xsl:when test="rng:ref">
          <dataRef key="{concat('tei',rng:ref/@name)}"/>
        </xsl:when>
        <xsl:when test="rng:data">
          <dataRef name="{rng:data/@type}"/>
        </xsl:when>
        <xsl:when test="rng:text">
          <textNode/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:apply-templates select="@*"/>
          <xsl:message>Cannot cope with datatype child of <xsl:value-of select="ancestor::attDef/@ident"/> attDef</xsl:message>
          <xsl:text>&#x0A;</xsl:text>
          <xsl:processing-instruction name="tei-purify">Cannot cope with datatype child of <xsl:value-of select="ancestor::attDef/@ident"/> attDef</xsl:processing-instruction>
          <xsl:apply-templates select="node()"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="content">
    <xsl:choose>
      <xsl:when test=".//rng:anyName">
        <xsl:copy-of select="."/>
      </xsl:when>
      <xsl:when test=".//rng:attribute">
        <xsl:copy-of select="."/>
      </xsl:when>
      <xsl:when test=".//rng:data">
        <xsl:copy-of select="."/>
      </xsl:when>
      <xsl:when test=".//rng:element">
        <xsl:copy-of select="."/>
      </xsl:when>
      <xsl:when test=".//rng:except">
        <xsl:copy-of select="."/>
      </xsl:when>
      <xsl:when test=".//rng:name">
        <xsl:copy-of select="."/>
      </xsl:when>
      <xsl:when test=".//rng:nsName">
        <xsl:copy-of select="."/>
      </xsl:when>
      <xsl:when test=".//rng:param">
        <xsl:copy-of select="."/>
      </xsl:when>
      <xsl:when test=".//rng:value">
        <xsl:copy-of select="."/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:copy>
          <xsl:apply-templates/>
        </xsl:copy>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="exemplum">
    <xsl:copy-of select="."/>
  </xsl:template>
  
  <!-- process the RELAX NG itself -->
  <xsl:template match="rng:ref">
    <xsl:choose>
      <xsl:when test="starts-with(@name, 'model.')">
        <xsl:choose>
          <xsl:when test="contains(@name, '_')">
            <classRef key="{substring-before(@name,'_')}">
              <xsl:attribute name="expand">
                <xsl:value-of select="substring-after(@name, '_')"/>
              </xsl:attribute>
              <xsl:call-template name="maxmin"/>
            </classRef>
          </xsl:when>
          <xsl:otherwise>
            <classRef key="{@name}">
              <xsl:call-template name="maxmin"/>
            </classRef>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:when>
      <xsl:when test="starts-with(@name, 'att.')">
        <classRef key="{@name}">
          <xsl:call-template name="maxmin"/>
        </classRef>
      </xsl:when>
      <xsl:when test="starts-with(@name, 'macro.')">
        <macroRef key="{@name}">
          <xsl:call-template name="maxmin"/>
        </macroRef>
      </xsl:when>
      <xsl:when test="starts-with(@name, 'data.')">
        <macroRef key="{@name}">
          <xsl:call-template name="maxmin"/>
        </macroRef>
      </xsl:when>
      <xsl:otherwise>
        <elementRef key="{@name}">
          <xsl:call-template name="maxmin"/>
        </elementRef>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="rng:group">
    <xsl:choose>
      <xsl:when test="count(*) gt 1">
        <sequence>
          <xsl:call-template name="maxmin"/>
          <xsl:apply-templates select="node() except text()[ normalize-space(.) eq '']"/>
        </sequence>
      </xsl:when>
      <xsl:otherwise>
        <xsl:apply-templates select="node() except text()[ normalize-space(.) eq '']"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="rng:text">
    <textNode/>
  </xsl:template>

  <xsl:template match="rng:choice">
    <alternate>
      <xsl:call-template name="maxmin"/>
      <xsl:apply-templates select="node() except text()[ normalize-space(.) eq '']"/>
    </alternate>
  </xsl:template>

  <xsl:template match="rng:zeroOrMore | rng:oneOrMore | rng:optional">
    <xsl:choose>
      <xsl:when test="count(*) eq 1">
        <xsl:apply-templates select="node() except text()[ normalize-space(.) eq '']"/>
      </xsl:when>
      <xsl:otherwise>
        <sequence>
          <xsl:choose>
            <xsl:when test="self::rng:zeroOrMore">
              <xsl:attribute name="minOccurs">0</xsl:attribute>
              <xsl:attribute name="maxOccurs">unbounded</xsl:attribute>
            </xsl:when>
            <xsl:when test="self::rng:oneOrMore">
              <xsl:attribute name="minOccurs">1</xsl:attribute>
              <xsl:attribute name="maxOccurs">unbounded</xsl:attribute>
            </xsl:when>
            <xsl:when test="self::rng:optional">
              <xsl:attribute name="minOccurs">0</xsl:attribute>
            </xsl:when>
          </xsl:choose>
          <xsl:apply-templates select="node() except text()[ normalize-space(.) eq '']"/>
        </sequence>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="rng:empty"/>

  <xsl:template match="rng:anyName | rng:attribute | rng:data | rng:element | rng:except | rng:name | rng:nsName | rng:param | rng:data | rng:value">
    <xsl:message><xsl:value-of select="name(.)"/>/@<xsl:value-of select="@name"/> TODO</xsl:message>
    <junk was="{name(.)}">
      <xsl:apply-templates/>
    </junk>
  </xsl:template>

  <xsl:template match="rng:*">
    <xsl:message><xsl:value-of select="name(.)"/> unprocessed</xsl:message>
    <xsl:processing-instruction name="tei-purify"><xsl:value-of select="name(.)"/> unprocessed</xsl:processing-instruction>
    <xsl:apply-templates/>
    <xsl:processing-instruction name="tei-purify">end-<xsl:value-of select="name(.)"/></xsl:processing-instruction>
  </xsl:template>
  
  <!-- subroutines -->
  <xsl:template name="maxmin">
    <xsl:choose>
      <xsl:when test="parent::rng:zeroOrMore and count(../*) eq 1">
        <xsl:attribute name="minOccurs">0</xsl:attribute>
        <xsl:attribute name="maxOccurs">unbounded</xsl:attribute>
      </xsl:when>
      <xsl:when test="parent::rng:oneOrMore and count(../*) eq 1">
        <xsl:attribute name="minOccurs">1</xsl:attribute>
        <xsl:attribute name="maxOccurs">unbounded</xsl:attribute>
      </xsl:when>
      <xsl:when test="parent::rng:optional and count(../*) eq 1">
        <xsl:attribute name="minOccurs">0</xsl:attribute>
      </xsl:when>
    </xsl:choose>
  </xsl:template>

</xsl:stylesheet>
