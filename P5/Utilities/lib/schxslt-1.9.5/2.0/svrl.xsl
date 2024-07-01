<xsl:transform version="2.0"
               xmlns="http://www.w3.org/1999/XSL/TransformAlias"
               xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
               xmlns:sch="http://purl.oclc.org/dsdl/schematron"
               xmlns:error="https://doi.org/10.5281/zenodo.1495494#error"
               xmlns:schxslt="https://doi.org/10.5281/zenodo.1495494"
               xmlns:schxslt-api="https://doi.org/10.5281/zenodo.1495494#api"
               xmlns:svrl="http://purl.oclc.org/dsdl/svrl"
               xmlns:xs="http://www.w3.org/2001/XMLSchema"
               xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:param name="schxslt.svrl.compact" as="xs:boolean" select="false()"/>

  <xsl:template name="schxslt-api:report">
    <xsl:param name="schema" as="element(sch:schema)" required="yes"/>
    <xsl:param name="phase" as="xs:string" required="yes"/>

    <svrl:schematron-output>
      <xsl:sequence select="$schema/@schemaVersion"/>
      <xsl:if test="$phase ne '#ALL'">
        <xsl:attribute name="phase" select="$phase"/>
      </xsl:if>

      <xsl:if test="$schxslt.svrl.compact eq false()">
        <xsl:if test="$schema/sch:title">
          <xsl:attribute name="title" select="$schema/sch:title"/>
        </xsl:if>
        <xsl:for-each select="$schema/sch:p">
          <svrl:text>
            <xsl:sequence select="(@id, @class, @icon, @xml:*)"/>
            <xsl:apply-templates select="node()" mode="schxslt:message-template"/>
          </svrl:text>
        </xsl:for-each>

        <xsl:for-each select="$schema/sch:ns">
          <svrl:ns-prefix-in-attribute-values>
            <xsl:sequence select="(@prefix, @uri)"/>
          </svrl:ns-prefix-in-attribute-values>
        </xsl:for-each>
      </xsl:if>

      <sequence select="$schxslt:report"/>

    </svrl:schematron-output>
  </xsl:template>

  <xsl:template name="schxslt-api:active-pattern">
    <xsl:param name="pattern" as="element(sch:pattern)" required="yes"/>
    <xsl:if test="$schxslt.svrl.compact eq false()">
      <svrl:active-pattern>
        <xsl:if test="exists($pattern/sch:title | $pattern/@id)">
          <xsl:attribute name="name" select="($pattern/sch:title, $pattern/@id)[1]"/>
        </xsl:if>
        <xsl:sequence select="($pattern/@id, $pattern/@role, $pattern/@xml:*)"/>
        <attribute name="documents" select="base-uri(.)"/>
      </svrl:active-pattern>
    </xsl:if>
  </xsl:template>

  <xsl:template name="schxslt-api:fired-rule">
    <xsl:param name="rule" as="element(sch:rule)" required="yes"/>
    <xsl:if test="$schxslt.svrl.compact eq false()">
      <svrl:fired-rule>
        <xsl:sequence select="($rule/@id, $rule/@role, $rule/@flag, $rule/@see, $rule/@icon, $rule/@fpi, $rule/@xml:*)"/>
        <attribute name="context">
          <xsl:value-of select="$rule/@context"/>
        </attribute>
      </svrl:fired-rule>
    </xsl:if>
  </xsl:template>

  <xsl:template name="schxslt-api:suppressed-rule">
    <xsl:param name="rule" as="element(sch:rule)" required="yes"/>
    <xsl:if test="$schxslt.svrl.compact eq false()">
      <xsl:variable name="message">
        WARNING: Rule <xsl:value-of select="normalize-space(@id)"/> for context "<xsl:value-of select="@context"/>" shadowed by preceding rule
      </xsl:variable>
      <comment> <xsl:sequence select="normalize-space($message)"/> </comment>
      <svrl:suppressed-rule>
        <xsl:sequence select="($rule/@id, $rule/@role, $rule/@flag, $rule/@see, $rule/@icon, $rule/@fpi)"/>
        <attribute name="context">
          <xsl:value-of select="$rule/@context"/>
        </attribute>
      </svrl:suppressed-rule>
    </xsl:if>
  </xsl:template>

  <xsl:template name="schxslt-api:failed-assert">
    <xsl:param name="assert" as="element(sch:assert)" required="yes"/>
    <xsl:param name="location-function" as="xs:string" required="yes" tunnel="yes"/>
    <svrl:failed-assert location="{{{$location-function}({($assert/@subject, $assert/../@subject, '.')[1]})}}">
      <xsl:sequence select="($assert/@role, $assert/@flag, $assert/@id, $assert/@see, $assert/@icon, $assert/@fpi, $assert/@xml:*)"/>
      <attribute name="test">
        <xsl:value-of select="$assert/@test"/>
      </attribute>
      <xsl:call-template name="schxslt:handle-detailed-report">
        <xsl:with-param name="pattern" as="element(sch:pattern)" tunnel="yes" select="$assert/../.."/>
      </xsl:call-template>
    </svrl:failed-assert>
  </xsl:template>

  <xsl:template name="schxslt-api:successful-report">
    <xsl:param name="report" as="element(sch:report)" required="yes"/>
    <xsl:param name="location-function" as="xs:string" required="yes" tunnel="yes"/>
    <svrl:successful-report location="{{{$location-function}({($report/@subject, $report/../@subject, '.')[1]})}}">
      <xsl:sequence select="($report/@role, $report/@flag, $report/@id, $report/@see, $report/@icon, $report/@fpi, $report/@xml:*)"/>
      <attribute name="test">
        <xsl:value-of select="$report/@test"/>
      </attribute>
      <xsl:call-template name="schxslt:handle-detailed-report">
        <xsl:with-param name="pattern" as="element(sch:pattern)" tunnel="yes" select="$report/../.."/>
      </xsl:call-template>
    </svrl:successful-report>
  </xsl:template>

  <xsl:template name="schxslt-api:metadata">
    <xsl:param name="schema" as="element(sch:schema)" required="yes"/>
    <xsl:param name="source" as="element(rdf:Description)" required="yes"/>
    <xsl:param name="xslt-version" as="xs:string" required="yes" tunnel="yes"/>
    <xsl:if test="$schxslt.svrl.compact eq false()">
      <svrl:metadata xmlns:dct="http://purl.org/dc/terms/" xmlns:skos="http://www.w3.org/2004/02/skos/core#">
        <dct:creator>
          <dct:Agent>
            <skos:prefLabel>
              <value-of separator="/" select="(system-property('xsl:product-name'), system-property('xsl:product-version'))"/>
            </skos:prefLabel>
          </dct:Agent>
        </dct:creator>
        <dct:created><value-of select="current-dateTime()"/></dct:created>
        <dct:source>
          <xsl:sequence select="$source"/>
        </dct:source>
      </svrl:metadata>
    </xsl:if>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
      <p>Create detailed report about failed assert or successful report</p>
    </desc>
  </doc>
  <xsl:template name="schxslt:handle-detailed-report">
    <xsl:call-template name="schxslt:copy-diagnostics"/>
    <xsl:call-template name="schxslt:copy-properties"/>
    <xsl:if test="text() | *">
      <svrl:text>
        <xsl:sequence select="@xml:*"/>
        <xsl:apply-templates select="node()" mode="schxslt:message-template"/>
      </svrl:text>
    </xsl:if>
  </xsl:template>

  <xsl:template name="schxslt:handle-property">
    <xsl:param name="property" as="element(sch:property)"/>
    <svrl:property-reference property="{.}">
      <xsl:sequence select="($property/@role, $property/@scheme, $property/@xml:*)"/>
      <svrl:text>
        <xsl:apply-templates select="$property/node()" mode="schxslt:message-template">
          <xsl:with-param name="allow-xsl-copy-of" tunnel="yes" as="xs:boolean" select="true()"/>
        </xsl:apply-templates>
      </svrl:text>
    </svrl:property-reference>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
      <p>Copy Schematron properties to SVRL</p>
    </desc>
    <param name="schema">Schematron</param>
  </doc>
  <xsl:template name="schxslt:copy-properties">
    <xsl:param name="pattern" as="element(sch:pattern)" tunnel="yes"/>

    <xsl:for-each select="tokenize(@properties, ' ')">
      <xsl:call-template name="schxslt:handle-property">
        <xsl:with-param name="property" select="($pattern/sch:properties/sch:property[@id eq current()], $pattern/../sch:properties/sch:property[@id eq current()])[1]"/>
      </xsl:call-template>
    </xsl:for-each>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
      <p>Copy Schematron diagnostics to SVRL</p>
    </desc>
    <param name="schema">Schematron</param>
  </doc>
  <xsl:template name="schxslt:copy-diagnostics">
    <xsl:param name="pattern" as="element(sch:pattern)" tunnel="yes"/>

    <xsl:for-each select="tokenize(@diagnostics, ' ')">
      <xsl:variable name="diagnostic" select="($pattern/sch:diagnostics/sch:diagnostic[@id eq current()] | $pattern/../sch:diagnostics/sch:diagnostic[@id eq current()])[1]"/>
      <svrl:diagnostic-reference diagnostic="{.}">
        <xsl:sequence select="$diagnostic/@xml:*"/>
        <svrl:text>
          <xsl:sequence select="$diagnostic/@*"/>
          <xsl:apply-templates select="$diagnostic/node()" mode="schxslt:message-template"/>
        </svrl:text>
      </svrl:diagnostic-reference>
    </xsl:for-each>
  </xsl:template>

  <xsl:template match="sch:dir | sch:emph | sch:span" mode="schxslt:message-template">
    <xsl:element name="svrl:{local-name()}">
      <xsl:sequence select="@xml:*"/>
      <xsl:apply-templates select="node() | @*" mode="#current"/>
    </xsl:element>
  </xsl:template>

</xsl:transform>
