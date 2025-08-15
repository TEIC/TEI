<xsl:transform version="2.0"
               xmlns:sch="http://purl.oclc.org/dsdl/schematron"
               xmlns:xs="http://www.w3.org/2001/XMLSchema"
               xmlns:schxslt="https://doi.org/10.5281/zenodo.1495494"
               xmlns:error="https://doi.org/10.5281/zenodo.1495494#error"
               xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:template match="sch:schema">
    <xsl:call-template name="schxslt:expand">
      <xsl:with-param name="schema" as="element(sch:schema)" select="."/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template name="schxslt:expand">
    <xsl:param name="schema" as="element(sch:schema)" required="yes"/>
    <xsl:apply-templates select="$schema" mode="schxslt:expand">
      <xsl:with-param name="abstract-patterns" as="element(sch:pattern)*" tunnel="yes" select="$schema/sch:pattern[@abstract = 'true']"/>
    </xsl:apply-templates>
  </xsl:template>

  <!-- Copy all other elements -->
  <xsl:template match="*" mode="schxslt:expand">
    <xsl:param name="lang" as="xs:string?"/>
    <xsl:copy>
      <xsl:if test="empty(@xml:lang) and schxslt:in-scope-language(.) != $lang">
        <xsl:attribute name="xml:lang" select="schxslt:in-scope-language(.)"/>
      </xsl:if>
      <xsl:apply-templates select="@*" mode="schxslt:expand"/>
      <xsl:apply-templates select="node()" mode="schxslt:expand"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="comment() | processing-instruction() | @*" mode="schxslt:expand">
    <xsl:copy>
      <xsl:apply-templates select="node() | @*" mode="schxslt:expand"/>
    </xsl:copy>
  </xsl:template>

  <!-- Remove abstract patterns from output -->
  <xsl:template match="sch:pattern[@abstract = 'true']"  mode="schxslt:expand"/>

  <!-- Remove abstract rules from output -->
  <xsl:template match="sch:rule[@abstract = 'true']"     mode="schxslt:expand"/>

  <!-- Instantiate an abstract rule -->
  <xsl:template match="sch:extends[@rule]" mode="schxslt:expand">
    <xsl:if test="empty(ancestor::sch:schema/(sch:pattern | sch:rules)/sch:rule[@abstract = 'true'][@id = current()/@rule])">
      <xsl:message terminate="yes">
        The current pattern defines no abstract rule named '<xsl:value-of select="@rule"/>'.
      </xsl:message>
    </xsl:if>
    <xsl:variable name="sourceLang" as="xs:string" select="schxslt:in-scope-language(.)"/>
    <xsl:variable name="targetLang" as="xs:string" select="schxslt:in-scope-language(ancestor::sch:schema/(sch:pattern | sch:rules)/sch:rule[@abstract = 'true'][@id = current()/@rule])"/>
    <xsl:choose>
      <xsl:when test="$sourceLang = $targetLang">
        <xsl:sequence select="ancestor::sch:schema/(sch:pattern | sch:rules)/sch:rule[@abstract = 'true'][@id = current()/@rule]/node()"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:for-each select="ancestor::sch:schema/(sch:pattern | sch:rules)/sch:rule[@abstract = 'true'][@id = current()/@rule]/node()">
          <xsl:choose>
            <xsl:when test="self::* and not(@xml:lang)">
              <xsl:copy>
                <xsl:attribute name="xml:lang" select="$targetLang"/>
                <xsl:sequence select="@*"/>
                <xsl:sequence select="node()"/>
              </xsl:copy>
            </xsl:when>
            <xsl:otherwise>
              <xsl:sequence select="."/>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:for-each>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:apply-templates select="ancestor::sch:schema/(sch:pattern | sch:rules)/sch:rule[@abstract = 'true'][@id = current()/@rule]/sch:extends" mode="#current"/>
  </xsl:template>

  <!-- Instantiate an abstract pattern -->
  <xsl:template match="sch:pattern[@is-a]" mode="schxslt:expand">
    <xsl:param name="abstract-patterns" tunnel="yes" as="element(sch:pattern)*"/>
    <xsl:variable name="is-a" select="$abstract-patterns[@id = current()/@is-a]"/>
    <xsl:variable name="sourceLang" as="xs:string" select="schxslt:in-scope-language(.)"/>
    <xsl:variable name="targetLang" as="xs:string" select="schxslt:in-scope-language($is-a)"/>
    <xsl:copy>
      <xsl:sequence select="@* except @is-a"/>
      <xsl:if test="$sourceLang != $targetLang and not(@xml:lang)">
        <xsl:attribute name="xml:lang" select="$targetLang"/>
      </xsl:if>
      <xsl:apply-templates select="(if (not(@documents)) then $is-a/@documents else (), $is-a/node())" mode="schxslt:expand">
        <xsl:with-param name="schxslt:params" select="sch:param" tunnel="yes"/>
      </xsl:apply-templates>
      <xsl:if test="$is-a/sch:rule/sch:*/@properties">
        <xsl:variable name="ids" as="xs:string*"
                      select="for $prop in $is-a/sch:rule/sch:*/@properties return tokenize($prop, '\s+')"/>
        <sch:properties>
          <xsl:apply-templates select="../sch:properties/sch:property[@id = $ids]" mode="schxslt:expand">
            <xsl:with-param name="schxslt:params" select="sch:param" tunnel="yes"/>
            <xsl:with-param name="lang" as="xs:string" select="schxslt:in-scope-language(.)"/>
          </xsl:apply-templates>
        </sch:properties>
      </xsl:if>
      <xsl:if test="$is-a/sch:rule/sch:*/@diagnostics">
        <xsl:variable name="ids" as="xs:string*"
                      select="for $diag in $is-a/sch:rule/sch:*/@diagnostics return tokenize($diag, '\s+')"/>
        <sch:diagnostics>
          <xsl:apply-templates select="../sch:diagnostics/sch:diagnostic[@id = $ids]" mode="schxslt:expand">
            <xsl:with-param name="schxslt:params" select="sch:param" tunnel="yes"/>
            <xsl:with-param name="lang" as="xs:string" select="schxslt:in-scope-language(.)"/>
          </xsl:apply-templates>
        </sch:diagnostics>
      </xsl:if>
    </xsl:copy>
  </xsl:template>

  <!-- Replace placeholders in abstract pattern instance -->
  <xsl:template match="sch:assert/@test | sch:report/@test | sch:rule/@context | sch:value-of/@select | sch:pattern/@documents | sch:name/@path | sch:let/@value | xsl:copy-of[ancestor::sch:property]/@select" mode="schxslt:expand">
    <xsl:param name="schxslt:params" as="element(sch:param)*" tunnel="yes"/>
    <xsl:attribute name="{name()}" select="schxslt:replace-params(., $schxslt:params)"/>
  </xsl:template>

  <!-- Replace placeholders in property value -->
  <xsl:function name="schxslt:replace-params" as="xs:string?">
    <xsl:param name="src" as="xs:string"/>
    <xsl:param name="params" as="element(sch:param)*"/>
    <xsl:choose>
      <xsl:when test="empty($params)">
        <xsl:value-of select="$src"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:variable name="paramsSorted" as="element(sch:param)*">
          <xsl:for-each select="$params">
            <xsl:sort select="string-length(@name)" order="descending"/>
            <xsl:sequence select="."/>
          </xsl:for-each>
        </xsl:variable>

        <xsl:variable name="value" select="replace(replace($paramsSorted[1]/@value, '\\', '\\\\'), '\$', '\\\$')"/>
        <xsl:variable name="src" select="replace($src, concat('(\W*)\$', $paramsSorted[1]/@name, '(\W*)'), concat('$1', $value, '$2'))"/>
        <xsl:value-of select="schxslt:replace-params($src, $paramsSorted[position() > 1])"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>

  <xsl:function name="schxslt:in-scope-language" as="xs:string">
    <xsl:param name="context" as="node()"/>
    <xsl:value-of select="$context/ancestor-or-self::*[@xml:lang][1]/@xml:lang"/>
  </xsl:function>

</xsl:transform>
