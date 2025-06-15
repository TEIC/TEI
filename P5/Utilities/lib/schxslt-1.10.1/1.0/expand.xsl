<xsl:transform version="1.0"
               xmlns:schxslt="https://doi.org/10.5281/zenodo.1495494"
               xmlns:sch="http://purl.oclc.org/dsdl/schematron"
               xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:key name="schxslt:properties"        match="sch:property"                    use="@id"/>
  <xsl:key name="schxslt:diagnostics"       match="sch:diagnostic"                  use="@id"/>
  <xsl:key name="schxslt:abstract-patterns" match="sch:pattern[@abstract = 'true']" use="@id"/>
  <xsl:key name="schxslt:params"            match="sch:pattern/sch:param"           use="generate-id(..)"/>

  <xsl:template match="node() | @*">
    <xsl:copy>
      <xsl:apply-templates select="node() | @*"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="sch:pattern[@abstract = 'true']"/>
  <xsl:template match="sch:rule[@abstract = 'true']"/>

  <xsl:template match="sch:extends[@rule]">
    <xsl:if test="not((ancestor::sch:pattern|ancestor::sch:schema/sch:rules)/sch:rule[@abstract = 'true'][@id = current()/@rule])">
      <xsl:message terminate="yes">
        The current pattern defines no abstract rule named '<xsl:value-of select="@rule"/>'.
      </xsl:message>
    </xsl:if>

    <xsl:variable name="sourceLang">
      <xsl:call-template name="schxslt:in-scope-language"/>
    </xsl:variable>
    <xsl:variable name="targetLang">
      <xsl:call-template name="schxslt:in-scope-language">
        <xsl:with-param name="context" select="(ancestor::sch:pattern|ancestor::sch:schema/sch:rules)/sch:rule[@abstract = 'true'][@id = current()/@rule]"/>
      </xsl:call-template>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="$sourceLang = $targetLang">
        <xsl:copy-of select="(ancestor::sch:pattern|ancestor::sch:schema/sch:rules)/sch:rule[@abstract = 'true'][@id = current()/@rule]/node()"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:for-each select="(ancestor::sch:pattern|ancestor::sch:schema/sch:rules)/sch:rule[@abstract = 'true'][@id = current()/@rule]/node()">
          <xsl:choose>
            <xsl:when test="self::* and not(@xml:lang)">
              <xsl:copy>
                <xsl:attribute name="xml:lang">
                  <xsl:value-of select="$targetLang"/>
                </xsl:attribute>
                <xsl:copy-of select="@*"/>
                <xsl:copy-of select="node()"/>
              </xsl:copy>
            </xsl:when>
            <xsl:otherwise>
              <xsl:copy-of select="."/>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:for-each>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:apply-templates select="(ancestor::sch:pattern|ancestor::sch:schema/sch:rules)/sch:rule[@abstract = 'true'][@id = current()/@rule]/sch:extends"/>
  </xsl:template>

  <xsl:template match="sch:pattern[@is-a]">
    <xsl:variable name="instanceId" select="generate-id()"/>
    <xsl:variable name="sourceLang">
      <xsl:call-template name="schxslt:in-scope-language"/>
    </xsl:variable>
    <xsl:variable name="targetLang">
      <xsl:call-template name="schxslt:in-scope-language">
        <xsl:with-param name="context" select="key('schxslt:abstract-patterns', @is-a)"/>
      </xsl:call-template>
    </xsl:variable>
    <xsl:copy>
      <xsl:if test="$sourceLang != $targetLang and not(@xml:lang)">
        <xsl:attribute name="xml:lang">
          <xsl:value-of select="$targetLang"/>
        </xsl:attribute>
      </xsl:if>
      <xsl:apply-templates select="@* | key('schxslt:abstract-patterns', @is-a)/@documents" mode="schxslt:pattern-instance">
        <xsl:with-param name="instanceId" select="$instanceId"/>
      </xsl:apply-templates>
      <xsl:apply-templates select="key('schxslt:abstract-patterns', @is-a)/node()" mode="schxslt:pattern-instance">
        <xsl:with-param name="instanceId" select="$instanceId"/>
      </xsl:apply-templates>
      <xsl:if test="key('schxslt:abstract-patterns', @is-a)/sch:rule/sch:*/@properties">
        <sch:properties>
          <xsl:for-each select="key('schxslt:abstract-patterns', @is-a)/sch:rule/sch:*[@properties]">
            <xsl:call-template name="copy">
              <xsl:with-param name="ids" select="string(@properties)"/>
              <xsl:with-param name="index" select="'schxslt:properties'"/>
              <xsl:with-param name="instanceId" select="$instanceId"/>
              <xsl:with-param name="lang">
                <xsl:call-template name="schxslt:in-scope-language"/>
              </xsl:with-param>
            </xsl:call-template>
          </xsl:for-each>
        </sch:properties>
      </xsl:if>
      <xsl:if test="key('schxslt:abstract-patterns', @is-a)/sch:rule/sch:*/@diagnostics">
        <sch:diagnostics>
          <xsl:for-each select="key('schxslt:abstract-patterns', @is-a)/sch:rule/sch:*[@diagnostics]">
            <xsl:call-template name="copy">
              <xsl:with-param name="ids" select="string(@diagnostics)"/>
              <xsl:with-param name="index" select="'schxslt:diagnostics'"/>
              <xsl:with-param name="instanceId" select="$instanceId"/>
              <xsl:with-param name="lang">
                <xsl:call-template name="schxslt:in-scope-language"/>
              </xsl:with-param>
            </xsl:call-template>
          </xsl:for-each>
        </sch:diagnostics>
      </xsl:if>
    </xsl:copy>
  </xsl:template>

  <xsl:template name="copy">
    <xsl:param name="ids"/>
    <xsl:param name="index"/>
    <xsl:param name="instanceId"/>
    <xsl:param name="lang"/>

    <xsl:if test="normalize-space($ids) != ''">
      <xsl:variable name="head">
        <xsl:choose>
          <xsl:when test="contains($ids, ' ')">
            <xsl:value-of select="substring-before($ids, ' ')"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="$ids"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:variable>

      <xsl:apply-templates select="key($index, $head)" mode="schxslt:pattern-instance">
        <xsl:with-param name="instanceId" select="$instanceId"/>
        <xsl:with-param name="lang" select="$lang"/>
      </xsl:apply-templates>

      <xsl:if test="contains($ids, ' ')">
        <xsl:call-template name="copy">
          <xsl:with-param name="ids" select="substring-after($ids, ' ')"/>
          <xsl:with-param name="index" select="$index"/>
          <xsl:with-param name="instanceId" select="$instanceId"/>
          <xsl:with-param name="lang" select="$lang"/>
        </xsl:call-template>
      </xsl:if>

    </xsl:if>

  </xsl:template>

  <xsl:template match="*" mode="schxslt:pattern-instance">
    <xsl:param name="instanceId"/>
    <xsl:param name="lang"/>
    <xsl:variable name="thisLang">
      <xsl:call-template name="schxslt:in-scope-language"/>
    </xsl:variable>
    <xsl:copy>
      <xsl:if test="not(@xml:lang) and $lang != $thisLang">
        <xsl:attribute name="xml:lang">
          <xsl:value-of select="$thisLang"/>
        </xsl:attribute>
      </xsl:if>
      <xsl:apply-templates select="@*" mode="schxslt:pattern-instance">
        <xsl:with-param name="instanceId" select="$instanceId"/>
      </xsl:apply-templates>
      <xsl:apply-templates select="node()" mode="schxslt:pattern-instance">
        <xsl:with-param name="instanceId" select="$instanceId"/>
      </xsl:apply-templates>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="comment() | processing-instruction() | @*" mode="schxslt:pattern-instance">
    <xsl:param name="instanceId"/>
    <xsl:copy/>
  </xsl:template>

  <xsl:template match="sch:pattern/@is-a" mode="schxslt:pattern-instance"/>

  <xsl:template match="sch:assert/@test | sch:report/@test | sch:rule/@context | sch:value-of/@select | sch:pattern/@documents | sch:name/@path | sch:let/@value | xsl:copy-of[ancestor::sch:property]/@select" mode="schxslt:pattern-instance">
    <xsl:param name="instanceId"/>
    <xsl:variable name="params">
      <xsl:for-each select="key('schxslt:params', $instanceId)">
        <xsl:sort select="string-length(@name)" order="descending"/>
        <xsl:value-of select="concat('$', @name, ' ')"/>
      </xsl:for-each>
    </xsl:variable>
    <xsl:attribute name="{local-name()}">
      <xsl:call-template name="schxslt:replace-params">
        <xsl:with-param name="instanceId" select="$instanceId"/>
        <xsl:with-param name="source" select="."/>
        <xsl:with-param name="params" select="normalize-space($params)"/>
      </xsl:call-template>
    </xsl:attribute>
  </xsl:template>

  <xsl:template name="schxslt:replace-params">
    <xsl:param name="instanceId"/>
    <xsl:param name="source"/>
    <xsl:param name="params"/>

    <xsl:choose>
      <xsl:when test="normalize-space($params) != ''">
        <xsl:variable name="param">
          <xsl:choose>
            <xsl:when test="contains($params, ' ')">
              <xsl:value-of select="substring-before($params, ' ')"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:value-of select="$params"/>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:variable>
        <xsl:variable name="replacement" select="key('schxslt:params', $instanceId)[@name = substring($param, 2)]/@value"/>

        <xsl:variable name="source-replaced">
          <xsl:call-template name="schxslt:replace-single-param">
            <xsl:with-param name="source" select="$source"/>
            <xsl:with-param name="param" select="$param"/>
            <xsl:with-param name="replacement" select="$replacement"/>
          </xsl:call-template>
        </xsl:variable>

        <xsl:choose>
          <xsl:when test="contains($params, ' ')">
            <xsl:call-template name="schxslt:replace-params">
              <xsl:with-param name="instanceId" select="$instanceId"/>
              <xsl:with-param name="source" select="$source-replaced"/>
              <xsl:with-param name="params" select="substring-after($params, ' ')"/>
            </xsl:call-template>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="$source-replaced"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$source"/>
      </xsl:otherwise>
    </xsl:choose>

  </xsl:template>

  <xsl:template name="schxslt:replace-single-param">
    <xsl:param name="source"/>
    <xsl:param name="param"/>
    <xsl:param name="replacement"/>

    <xsl:choose>
      <xsl:when test="contains($source, $param)">
        <xsl:value-of select="substring-before($source, $param)"/>
        <xsl:value-of select="$replacement"/>
        <xsl:call-template name="schxslt:replace-single-param">
          <xsl:with-param name="source" select="substring-after($source, $param)"/>
          <xsl:with-param name="param" select="$param"/>
          <xsl:with-param name="replacement" select="$replacement"/>
        </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$source"/>
      </xsl:otherwise>
    </xsl:choose>

  </xsl:template>

  <xsl:template name="schxslt:in-scope-language">
    <xsl:param name="context" select="."/>
    <xsl:value-of select="$context/ancestor-or-self::*[@xml:lang][1]/@xml:lang"/>
  </xsl:template>

</xsl:transform>
