<xsl:transform xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:sch="http://purl.oclc.org/dsdl/schematron" xmlns:schxslt="https://doi.org/10.5281/zenodo.1495494" xmlns:xs="http://www.w3.org/2001/XMLSchema" version="2.0" xmlns:error="https://doi.org/10.5281/zenodo.1495494#error">

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
      <p>Return the effective phase</p>
      <p>
        The effective phase is #ALL if the selected phase is #DEFAULT or no phase was selected.  Terminates if the schema does not contain the selected phase.
      </p>
    </desc>
    <param name="schema">Schematron schema</param>
    <param name="phase">Requested phase</param>
    <return>Effective phase</return>
  </doc>
  <xsl:function name="schxslt:effective-phase" as="xs:string">
    <xsl:param name="schema" as="element(sch:schema)"/>
    <xsl:param name="phase" as="xs:string"/>

    <xsl:variable name="phase">
      <xsl:choose>
        <xsl:when test="$phase = ('#DEFAULT', '')">
          <xsl:value-of select="($schema/@defaultPhase, '#ALL')[1]"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="$phase"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>

    <xsl:if test="$phase ne '#ALL' and not($schema/sch:phase[@id = $phase])">
      <xsl:variable name="message">
        The phase '<xsl:value-of select="$phase"/>' is not defined.
      </xsl:variable>
      <xsl:message terminate="yes" select="error(xs:QName('error:E0001'), normalize-space($message))"/>
    </xsl:if>

    <xsl:value-of select="$phase"/>

  </xsl:function>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
      <p>Return sequence of active patterns</p>
    </desc>
    <param name="schema">Schematron schema</param>
    <param name="phase">Phase</param>
    <return>Sequence of patterns active in selected phase</return>
  </doc>
  <xsl:function name="schxslt:active-patterns" as="element(sch:pattern)+">
    <xsl:param name="schema" as="element(sch:schema)"/>
    <xsl:param name="phase" as="xs:string"/>

    <xsl:choose>
      <xsl:when test="$phase eq '#ALL'">
        <xsl:sequence select="$schema/sch:pattern[sch:rule]"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:sequence select="$schema/sch:pattern[@id = $schema/sch:phase[@id eq $phase]/sch:active/@pattern][sch:rule]"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>

  <xsl:function name="schxslt:xslt-version" as="xs:string">
    <xsl:param name="schema" as="element(sch:schema)"/>

    <xsl:if test="not(lower-case($schema/@queryBinding) = ('xslt2', 'xslt3'))">
      <xsl:variable name="message">
        The query language '<xsl:value-of select="($schema/@queryBinding, 'xslt')[1]"/>' is not supported.
      </xsl:variable>
      <xsl:message terminate="yes" select="error(xs:QName('error:E0002'), normalize-space($message))"/>
    </xsl:if>

    <xsl:value-of select="if (lower-case($schema/@queryBinding) eq 'xslt2') then '2.0' else '3.0'"/>

  </xsl:function>

  <xsl:function name="schxslt:is-location-function" as="xs:boolean">
    <xsl:param name="function" as="element(xsl:function)"/>

    <xsl:variable name="nsUri" select="namespace-uri-for-prefix(substring-before($function/@name, ':'), $function)"/>
    <xsl:variable name="localname" select="substring-after($function/@name, ':')"/>

    <xsl:sequence select="boolean($nsUri eq 'https://doi.org/10.5281/zenodo.1495494' and $localname eq 'location')"/>
  </xsl:function>

</xsl:transform>
