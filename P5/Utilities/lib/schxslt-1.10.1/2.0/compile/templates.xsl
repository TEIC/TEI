<!-- Compiler templates -->
<xsl:transform version="2.0"
               xmlns="http://www.w3.org/1999/XSL/TransformAlias"
               xmlns:error="https://doi.org/10.5281/zenodo.1495494#error"
               xmlns:sch="http://purl.oclc.org/dsdl/schematron"
               xmlns:schxslt="https://doi.org/10.5281/zenodo.1495494"
               xmlns:schxslt-api="https://doi.org/10.5281/zenodo.1495494#api"
               xmlns:xs="http://www.w3.org/2001/XMLSchema"
               xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <!-- Match templates -->
  <xsl:template match="sch:assert" mode="schxslt:compile">
    <if test="not({@test})">
      <xsl:sequence select="@xml:base"/>
      <xsl:call-template name="schxslt-api:failed-assert">
        <xsl:with-param name="assert" as="element(sch:assert)" select="."/>
      </xsl:call-template>
    </if>
  </xsl:template>

  <xsl:template match="sch:report" mode="schxslt:compile">
    <if test="{@test}">
      <xsl:sequence select="@xml:base"/>
      <xsl:call-template name="schxslt-api:successful-report">
        <xsl:with-param name="report" as="element(sch:report)" select="."/>
      </xsl:call-template>
    </if>
  </xsl:template>

  <!-- Message templates -->
  <xsl:template match="node() | @*" mode="schxslt:message-template" priority="-10">
    <xsl:copy>
      <xsl:apply-templates mode="#current"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="sch:name" mode="schxslt:message-template">
    <value-of select="{if (@path) then @path else 'name()'}">
      <xsl:sequence select="@xml:base"/>
    </value-of>
  </xsl:template>

  <xsl:template match="sch:value-of" mode="schxslt:message-template">
    <value-of select="{@select}">
      <xsl:sequence select="@xml:base"/>
    </value-of>
  </xsl:template>

  <!-- Copy variable content -->
  <xsl:template match="comment() | processing-instruction()" mode="schxslt:variable-content schxslt:message-template">
    <xsl:sequence select="."/>
  </xsl:template>

  <xsl:template match="*" mode="schxslt:variable-content schxslt:message-template">
    <element namespace="{namespace-uri(.)}" name="{local-name(.)}">
      <xsl:apply-templates select="node() | @*" mode="#current"/>
    </element>
  </xsl:template>

  <xsl:template match="xsl:copy-of" mode="schxslt:message-template">
    <xsl:param name="allow-xsl-copy-of" tunnel="yes" as="xs:boolean" select="false()"/>
    <xsl:choose>
      <xsl:when test="$allow-xsl-copy-of">
        <xsl:sequence select="."/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:next-match/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="@*" mode="schxslt:variable-content schxslt:message-template">
    <attribute namespace="{namespace-uri(.)}" name="{local-name(.)}">
      <xsl:value-of select="."/>
    </attribute>
  </xsl:template>

  <!-- Named templates -->
  <xsl:template name="schxslt:let-variable">
    <xsl:param name="bindings" as="element(sch:let)*"/>
    <xsl:param name="typed-variables" as="xs:boolean" required="yes"/>

    <xsl:for-each select="$bindings">
      <xsl:choose>
        <xsl:when test="@value">
          <variable name="{@name}" select="{@value}">
            <xsl:if test="$typed-variables">
              <xsl:sequence select="@as"/>
            </xsl:if>
            <xsl:sequence select="(@xml:base, ../@xml:base)[1]"/>
          </variable>
        </xsl:when>
        <xsl:otherwise>
          <variable name="{@name}">
            <xsl:if test="$typed-variables">
              <xsl:sequence select="@as"/>
            </xsl:if>
            <xsl:sequence select="(@xml:base, ../@xml:base)[1]"/>
            <xsl:apply-templates select="node()" mode="schxslt:variable-content"/>
          </variable>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="schxslt:let-param">
    <xsl:param name="bindings" as="element(sch:let)*"/>
    <xsl:param name="typed-variables" as="xs:boolean" required="yes"/>

    <xsl:for-each select="$bindings">
      <xsl:choose>
        <xsl:when test="@value">
          <param name="{@name}" select="{@value}">
            <xsl:if test="$typed-variables">
              <xsl:sequence select="@as"/>
            </xsl:if>
            <xsl:sequence select="(@xml:base, ../@xml:base)[1]"/>
          </param>
        </xsl:when>
        <xsl:otherwise>
          <param name="{@name}">
            <xsl:if test="$typed-variables">
              <xsl:sequence select="@as"/>
            </xsl:if>
            <xsl:sequence select="(@xml:base, ../@xml:base)[1]"/>
            <xsl:apply-templates select="node()" mode="schxslt:variable-content"/>
          </param>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="schxslt:check-multiply-defined">
    <xsl:param name="bindings" as="element(sch:let)*"/>
    <xsl:if test="count($bindings) ne count(distinct-values($bindings/@name))">
      <xsl:variable name="message" as="xs:string+">
        Compilation aborted: It is an error for a variable to be multiply defined
        <xsl:for-each-group select="$bindings" group-by="@name">
          <xsl:sort select="current-grouping-key()"/>
          <xsl:if test="count(current-group()) gt 1">
            <xsl:value-of select="current-grouping-key()"/>
          </xsl:if>
        </xsl:for-each-group>
      </xsl:variable>
      <xsl:message terminate="yes" select="error(xs:QName('error:E0003'), normalize-space(string-join($message, ' ')))">
        <xsl:value-of select="$message"/>
      </xsl:message>
    </xsl:if>
  </xsl:template>

</xsl:transform>
