<xsl:transform version="1.0"
               xmlns="http://www.w3.org/1999/XSL/TransformAlias"
               xmlns:sch="http://purl.oclc.org/dsdl/schematron"
               xmlns:schxslt="https://doi.org/10.5281/zenodo.1495494"
               xmlns:schxslt-api="https://doi.org/10.5281/zenodo.1495494#api"
               xmlns:svrl="http://purl.oclc.org/dsdl/svrl"
               xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:import href="compile/compile-1.0.xsl"/>

  <xsl:template name="schxslt-api:report">
    <xsl:param name="schema"/>
    <xsl:param name="phase"/>

    <svrl:schematron-output>
      <xsl:copy-of select="$schema/@xml:*"/>
      <xsl:copy-of select="$schema/@schemaVersion"/>
      <xsl:if test="$phase != '#ALL'">
        <xsl:attribute name="phase"><xsl:value-of select="$phase"/></xsl:attribute>
      </xsl:if>
      <xsl:if test="$schema/sch:title">
        <xsl:attribute name="title"><xsl:value-of select="$schema/sch:title"/></xsl:attribute>
      </xsl:if>
      <xsl:for-each select="$schema/sch:p">
        <svrl:text>
          <xsl:copy-of select="@id | @class | @icon | @xml:*"/>
          <xsl:apply-templates select="node()" mode="schxslt:message-template"/>
        </svrl:text>
      </xsl:for-each>

      <xsl:for-each select="$schema/sch:ns">
        <svrl:ns-prefix-in-attribute-values>
          <xsl:copy-of select="@prefix | @uri"/>
        </svrl:ns-prefix-in-attribute-values>
      </xsl:for-each>

      <copy-of select="$schxslt:report"/>

    </svrl:schematron-output>

  </xsl:template>

  <xsl:template name="schxslt-api:active-pattern">
    <xsl:param name="pattern"/>

    <svrl:active-pattern>
      <xsl:copy-of select="$pattern/@id | $pattern/@role | $pattern/@xml:*"/>
      <xsl:if test="$pattern/@documents">
        <attribute name="documents"><value-of select="normalize-space()"/></attribute>
      </xsl:if>
      <xsl:choose>
        <xsl:when test="$pattern/sch:title">
          <xsl:attribute name="name">
            <xsl:value-of select="$pattern/sch:title"/>
          </xsl:attribute>
        </xsl:when>
        <xsl:when test="$pattern/@id">
          <xsl:attribute name="name">
            <xsl:value-of select="$pattern/@id"/>
          </xsl:attribute>
        </xsl:when>
      </xsl:choose>
    </svrl:active-pattern>

  </xsl:template>

  <xsl:template name="schxslt-api:fired-rule">
    <xsl:param name="rule"/>

    <svrl:fired-rule>
      <xsl:copy-of select="$rule/@id | $rule/@role | $rule/@flag | $rule/@see | $rule/@icon | $rule/@fpi | $rule/@xml:*"/>
      <attribute name="context">
        <xsl:value-of select="$rule/@context"/>
      </attribute>
    </svrl:fired-rule>
  </xsl:template>

  <xsl:template name="schxslt-api:failed-assert">
    <xsl:param name="assert"/>

    <variable name="location">
      <call-template name="schxslt:location">
        <xsl:choose>
          <xsl:when test="$assert/@subject">
            <with-param name="node" select="{$assert/@subject}"/>
          </xsl:when>
          <xsl:when test="$assert/../@subject">
            <with-param name="node" select="{$assert/../@subject}"/>
          </xsl:when>
          <xsl:otherwise>
            <with-param name="node" select="."/>
          </xsl:otherwise>
        </xsl:choose>
      </call-template>
    </variable>
    <svrl:failed-assert location="{{normalize-space($location)}}">
      <xsl:copy-of select="$assert/@role | $assert/@flag | $assert/@id | $assert/@see | $assert/@icon | $assert/@fpi"/>
      <xsl:copy-of select="$assert/@xml:id | $assert/ancestor-or-self::*[@xml:lang]/@xml:lang"/>
      <attribute name="test">
        <xsl:value-of select="$assert/@test"/>
      </attribute>
      <xsl:call-template name="schxslt:detailed-report">
        <xsl:with-param name="pattern" select="$assert/../.."/>
      </xsl:call-template>
    </svrl:failed-assert>
  </xsl:template>

  <xsl:template name="schxslt-api:successful-report">
    <xsl:param name="report"/>

    <variable name="location">
      <call-template name="schxslt:location">
        <xsl:choose>
          <xsl:when test="$report/@subject">
            <with-param name="node" select="{$report/@subject}"/>
          </xsl:when>
          <xsl:when test="$report/../@subject">
            <with-param name="node" select="{$report/../@subject}"/>
          </xsl:when>
          <xsl:otherwise>
            <with-param name="node" select="."/>
          </xsl:otherwise>
        </xsl:choose>
      </call-template>
    </variable>
    <svrl:successful-report location="{{normalize-space($location)}}">
      <xsl:copy-of select="$report/@role | $report/@flag | $report/@id | $report/@see | $report/@icon | $report/@fpi"/>
      <xsl:copy-of select="$report/@xml:id | $report/ancestor-or-self::*[@xml:lang]/@xml:lang"/>
      <attribute name="test">
        <xsl:value-of select="$report/@test"/>
      </attribute>
      <xsl:call-template name="schxslt:detailed-report">
        <xsl:with-param name="pattern" select="$report/../.."/>
      </xsl:call-template>
    </svrl:successful-report>
  </xsl:template>

  <xsl:template name="schxslt-api:validation-stylesheet-body-bottom-hook">
    <xsl:param name="schema"/>
    <xsl:copy-of select="document('')/xsl:transform/xsl:template[@name = 'schxslt:location']"/>
  </xsl:template>

  <xsl:template name="schxslt-api:metadata">
    <xsl:param name="schema"/>
    <xsl:param name="source"/>
    <svrl:metadata xmlns:dct="http://purl.org/dc/terms/">
      <dct:source>
        <xsl:copy-of select="$source"/>
      </dct:source>
    </svrl:metadata>
  </xsl:template>

  <xsl:template name="schxslt:detailed-report">
    <xsl:param name="pattern"/>

    <xsl:if test="@diagnostics">
      <xsl:call-template name="schxslt:copy-diagnostics">
        <xsl:with-param name="pattern" select="$pattern"/>
      </xsl:call-template>
    </xsl:if>
    <xsl:if test="@properties">
      <xsl:call-template name="schxslt:copy-properties">
        <xsl:with-param name="pattern" select="$pattern"/>
      </xsl:call-template>
    </xsl:if>
    <xsl:if test="text() | *">
      <svrl:text>
        <xsl:copy-of select="@xml:*"/>
        <xsl:apply-templates select="node()" mode="schxslt:message-template"/>
      </svrl:text>
    </xsl:if>
  </xsl:template>

  <xsl:template name="schxslt:copy-diagnostics">
    <xsl:param name="sequence" select="normalize-space(@diagnostics)"/>
    <xsl:param name="pattern"/>

    <xsl:variable name="head">
      <xsl:choose>
        <xsl:when test="contains($sequence, ' ')">
          <xsl:value-of select="substring-before($sequence, ' ')"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="$sequence"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>

    <svrl:diagnostic-reference diagnostic="{$head}">
      <svrl:text>
        <xsl:choose>
          <xsl:when test="$pattern/sch:diagnostics/sch:diagnostic[@id = $head]">
            <xsl:copy-of select="$pattern/sch:diagnostics/sch:diagnostic[@id = $head]/@*"/>
            <xsl:apply-templates select="$pattern/sch:diagnostics/sch:diagnostic[@id = $head]/node()" mode="schxslt:message-template"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:copy-of select="key('schxslt:diagnostics', $head)/@*"/>
            <xsl:apply-templates select="key('schxslt:diagnostics', $head)/node()" mode="schxslt:message-template"/>
          </xsl:otherwise>
        </xsl:choose>
      </svrl:text>
    </svrl:diagnostic-reference>

    <xsl:choose>
      <xsl:when test="contains($sequence, ' ')">
        <xsl:call-template name="schxslt:copy-diagnostics">
          <xsl:with-param name="sequence" select="substring-after($sequence, ' ')"/>
          <xsl:with-param name="pattern" select="$pattern"/>
        </xsl:call-template>
      </xsl:when>
      <xsl:otherwise/>
    </xsl:choose>

  </xsl:template>

  <xsl:template name="schxslt:copy-properties">
    <xsl:param name="sequence" select="normalize-space(@properties)"/>
    <xsl:param name="pattern"/>

    <xsl:variable name="head">
      <xsl:choose>
        <xsl:when test="contains($sequence, ' ')">
          <xsl:value-of select="substring-before($sequence, ' ')"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="$sequence"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>

    <svrl:property-reference property="{$head}">
      <xsl:copy-of select="key('schxslt:properties', $head)/@role"/>
      <xsl:copy-of select="key('schxslt:properties', $head)/@scheme"/>

      <svrl:text>
        <xsl:choose>
          <xsl:when test="$pattern/sch:properties/sch:property[@id = $head]">
            <xsl:copy-of select="$pattern/sch:properties/sch:property[@id = $head]/@*"/>
            <xsl:apply-templates select="$pattern/sch:properties/sch:property[@id = $head]/node()" mode="schxslt:message-template"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:copy-of select="key('schxslt:properties', $head)/@*"/>
            <xsl:apply-templates select="key('schxslt:properties', $head)/node()" mode="schxslt:message-template"/>
          </xsl:otherwise>
        </xsl:choose>
      </svrl:text>
    </svrl:property-reference>

    <xsl:choose>
      <xsl:when test="contains($sequence, ' ')">
        <xsl:call-template name="schxslt:copy-properties">
          <xsl:with-param name="sequence" select="substring-after($sequence, ' ')"/>
          <xsl:with-param name="pattern" select="$pattern"/>
        </xsl:call-template>
      </xsl:when>
      <xsl:otherwise/>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="schxslt:location">
    <xsl:param name="node"/>

    <xsl:variable name="path">
      <xsl:for-each select="$node/ancestor::*">
        <xsl:variable name="position">
          <xsl:number level="single"/>
        </xsl:variable>
        <xsl:text>/</xsl:text>
        <xsl:value-of select="concat('Q{', namespace-uri(.), '}', local-name(.), '[', $position, ']')"/>
      </xsl:for-each>
      <xsl:text>/</xsl:text>
      <xsl:variable name="position">
        <xsl:number level="single"/>
      </xsl:variable>
      <xsl:choose>
        <xsl:when test="$node/self::*">
          <xsl:value-of select="concat('Q{', namespace-uri($node), '}', local-name($node), '[', $position, ']')"/>
        </xsl:when>
        <xsl:when test="count($node/../@*) = count($node|$node/../@*)">
          <xsl:value-of select="concat('@Q{', namespace-uri($node), '}', local-name($node))"/>
        </xsl:when>
        <xsl:when test="$node/self::processing-instruction()">
          <xsl:value-of select="concat('processing-instruction(&quot;', name(.), '&quot;)', '[', $position, ']')"/>
        </xsl:when>
        <xsl:when test="$node/self::comment()">
          <xsl:value-of select="concat('comment()', '[', $position, ']')"/>
        </xsl:when>
        <xsl:when test="$node/self::text()">
          <xsl:value-of select="concat('text()', '[', $position, ']')"/>
        </xsl:when>
      </xsl:choose>
    </xsl:variable>

    <xsl:value-of select="$path"/>
  </xsl:template>

  <xsl:template match="sch:dir | sch:emph | sch:span" mode="schxslt:message-template">
    <xsl:element name="svrl:{local-name()}">
      <xsl:copy-of select="@xml:*"/>
      <xsl:apply-templates select="node() | @*" mode="schxslt:message-template"/>
    </xsl:element>
  </xsl:template>

</xsl:transform>
