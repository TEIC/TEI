<?xml version="1.0" encoding="UTF-8"?>
<!--
 %% Input:
 %%     <egXML wrapMe="p">Is this <term>well-formed</term>?</egXML>
 %%     <egXML>Maybe, <emph>but</emph> maybe not.</egXML>
 %% Output:
 %%     <egXML><p>Is this <term>well-formed</term>?</p></egXML>
 %%     <egXML><![CDATA[Maybe, ]]><emph>but</emph><![CDATA[ maybe not.]]></egXML>

 %% I don't know why it does that CDATA stuff, nor do I understand the 
 %% use of the cdata-section-elments= attribute. Wanna bet they're
 %% related? :-)
 %% Known bug: doesn't reprodcue the xmlns= of <egXML> (nor any
 %% others, I suppose).
-->

<!-- prolog, copied from some of Sebastian's stuff -->
<xsl:stylesheet 
 xmlns:teix="http://www.tei-c.org/ns/Examples"
 xmlns:tei="http://www.tei-c.org/ns/1.0"
 xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
 xmlns:rng="http://relaxng.org/ns/structure/1.0"
 extension-element-prefixes="exsl"
 exclude-result-prefixes="exsl teix rng tei"
 xmlns:exsl="http://exslt.org/common"
 version="1.0">

  <xsl:output 
    method="xml"
    indent="yes"
    cdata-section-elements="tei:eg teix:egXML"
    omit-xml-declaration="yes"/>

  <!-- identity transform for most everything -->
  <xsl:template match="*|@*|processing-instruction()|comment()">
    <xsl:copy>
      <xsl:apply-templates 
        select="*|@*|text()|processing-instruction()|comment()"/>
    </xsl:copy>
  </xsl:template>

  <!-- handle <egXML wrapMe=> separately -->
  <xsl:template match="teix:egXML[@wrapMe]">
    <!-- remember value of wrapMe= for later use -->
    <xsl:variable name="wrapper">
      <xsl:value-of select="@wrapMe"/>
    </xsl:variable>
    <!-- output an equivalent element -->
    <xsl:element name="egXML">
      <!-- that has the same xmlns= --><!-- NOT! -->
      <xsl:attribute name="xmlns">
        <xsl:value-of select="@xmlns"/>
      </xsl:attribute>
      <!-- and has one child element, <@wrapMe> -->
      <xsl:element name="{$wrapper}">
	<!-- whose content is the original content of <egXML> -->
        <xsl:apply-templates select="child::*|child::text()|comment()|processing-instruction()"/>
      </xsl:element>
    </xsl:element>
  </xsl:template>

</xsl:stylesheet>
