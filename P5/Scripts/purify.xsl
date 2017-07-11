<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xpath-default-namespace="http://www.tei-c.org/ns/1.0" xmlns="http://www.tei-c.org/ns/1.0"
  xmlns:rng="http://relaxng.org/ns/structure/1.0" exclude-result-prefixes="#all" version="2.0">

  <xsl:output method="xml" indent="yes"/>
  
  <!-- ****************************************************************** -->
  <!-- Overall, this is an identity transform: here we copy over anything -->
  <!-- and everything that is not an ODD element matched below.           -->
  <!-- ****************************************************************** -->
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
    
  <!-- ********************************************* -->
  <!-- Process TEI ODD elements that need to change. -->
  <!-- ********************************************* -->

  <!-- If the <content> has unusual or complicated stuff, -->
  <!-- we just copy it for manual treatment later         -->
  <xsl:template match="content[
      descendant::rng:anyName
    | descendant::rng:attribute
    | descendant::rng:data
    | descendant::rng:element
    | descendant::rng:except
    | descendant::rng:name
    | descendant::rng:nsName
    | descendant::rng:param
    | descendant::rng:value
    ]">
    <xsl:copy-of select="."/>
  </xsl:template>

  <xsl:template match="exemplum">
    <xsl:copy-of select="."/>
  </xsl:template>
 
  <!-- ********************************************* -->
  <!-- Process RNG elements that need to change. -->
  <!-- ********************************************* -->
  
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
  
  <!-- ********************************************* -->
  <!-- Convert datatypes .                           -->
  <!-- ********************************************* -->
   
  <xsl:template match="datatype/rng:ref">
    <dataRef key="{concat('tei',./@name)}"/>
  </xsl:template>
  
  <xsl:template match="datatype/rng:data">
    <dataRef name="{./@type}"/>
  </xsl:template>
  
  <!-- ********************************************* -->
  <!-- Warn about the stuff we couldn't handle.      -->
  <!-- ********************************************* -->

  
  <xsl:template match="rng:anyName | rng:attribute | rng:data | rng:element | rng:except
                     | rng:name | rng:nsName | rng:param | rng:value">
    <xsl:message><xsl:value-of select="name(.)"/>/@<xsl:value-of select="@name"/> TODO</xsl:message>
    <junk was="{name(.)}">
      <xsl:apply-templates/>
    </junk>
  </xsl:template>

  <xsl:template match="rng:*">
    <xsl:message><xsl:value-of select="name(.)"/> unprocessed</xsl:message>
    <xsl:processing-instruction name="tei-purify">an unprocessed <xsl:value-of select="name(.)"/> started here</xsl:processing-instruction>
    <xsl:apply-templates/>
    <xsl:processing-instruction name="tei-purify">an unprocessed <xsl:value-of select="name(.)"/> ended here</xsl:processing-instruction>
  </xsl:template>

  <!-- *********************************************** -->
  <!-- subroutine to handle repetition and optionality -->
  <!-- *********************************************** -->
  <xsl:template name="maxmin">
    <xsl:variable name="num_siblings" select="count(../*) -1"/>
    <xsl:choose>
      <xsl:when test="parent::rng:zeroOrMore  and  $num_siblings eq 0">
        <xsl:attribute name="minOccurs">0</xsl:attribute>
        <xsl:attribute name="maxOccurs">unbounded</xsl:attribute>
      </xsl:when>
      <xsl:when test="parent::rng:oneOrMore  and  $num_siblings eq 0">
        <xsl:attribute name="minOccurs">1</xsl:attribute>
        <xsl:attribute name="maxOccurs">unbounded</xsl:attribute>
      </xsl:when>
      <xsl:when test="parent::rng:optional  and  $num_siblings eq 0">
        <xsl:attribute name="minOccurs">0</xsl:attribute>
      </xsl:when>
    </xsl:choose>
  </xsl:template>

</xsl:stylesheet>
