<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns="http://www.tei-c.org/ns/1.0"
                xpath-default-namespace="http://www.tei-c.org/ns/1.0"
                version="3.0">
  
  <!--
      Read in a TEIC/TEI/P5/Exemplars.template file, write out both a version
      of thte same file that has proper <?xml-model?> PIs, and a .properties file
      (which is used by the oXygen framework for locating icons).
      
      Written long ago, almost assuredly by Sebastian Rahtz.
      
      Moderate overhaul 2022-04-22 by Syd Bauman
  -->

  <xsl:param name="file" select="document-uri(/)!tokenize( .,'/')[last()]"/>
  <xsl:param name="fn"   select="replace( $file, '\.template$','')"/>
  <xsl:param name="RNGsite" select="'http://www.tei-c.org/release/xml/tei/custom/schema/relaxng/'"/>

  <xsl:mode on-no-match="shallow-copy"/>
  
  <xsl:template match="/">
    <xsl:variable name="templateName" select="//processing-instruction('tei-template-name')"/>
    <xsl:if test="not( exists( $templateName ) )">
      <xsl:message expand-text="yes">WARNING: Unable to find a full name in the template file
        {$file} (i.e., no "//processing-instruction('tei-template-name')" found).
        Using "{$fn}" instead.</xsl:message>
    </xsl:if>
    
    <xsl:message select="'Create template &quot;'||$templateName||'&quot; (file '||$fn||'.xml) from '||$file"/>
    <xsl:result-document href="{$fn}.xml" indent="no">
      <xsl:text>&#x0A;</xsl:text>
      <xsl:processing-instruction name="xml-model" select="
          'href=&quot;'||$RNGsite||$fn||'.rng&quot; '
        ||'type=&quot;application/xml&quot; '
        ||'schematypens=&quot;http://relaxng.org/ns/structure/1.0&quot;'
        "/>
      <xsl:text>&#x0A;</xsl:text>
      <xsl:processing-instruction name="xml-model" select="
          'href=&quot;'||$RNGsite||$fn||'.rng&quot; '
        ||'type=&quot;application/xml&quot; '
        ||'schematypens=&quot;http://purl.oclc.org/dsdl/schematron&quot;'
        "/>
      <xsl:text>&#x0A;</xsl:text>
      <xsl:apply-templates select="@*|node()"/>
    </xsl:result-document>

    <xsl:message select="'Create property file for &quot;'||$templateName||'&quot; (file '||$fn||'.properties) from '||$file"/>
    <xsl:result-document href="{$fn}.properties" method="text">
      <xsl:text>smallIcon=../icons/TEI_16.gif</xsl:text>
      <xsl:text>&#x0A;</xsl:text>
      <xsl:text>bigIcon=../icons/TEI_48.png</xsl:text>
      <xsl:text>&#x0A;</xsl:text>
    </xsl:result-document>

  </xsl:template>

  <xsl:template match="//processing-instruction('tei-template-name')"/>

</xsl:stylesheet>
