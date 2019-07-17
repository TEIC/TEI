<xsl:stylesheet
    xpath-default-namespace="http://www.tei-c.org/ns/1.0"
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns="http://www.tei-c.org/ns/1.0"
    xmlns:xs="http://www.w3.org/2001/XMLSchema"                
    exclude-result-prefixes="tei xs"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    version="2.0"
    >
<xsl:strip-space elements="*"/>
<xsl:output method="text" encoding="utf-8" />
  
<xsl:function name="tei:jsonString" as="xs:string">
    <xsl:param name="content"/>
    <xsl:variable name="inq">"</xsl:variable>
    <xsl:variable name="outq">\\"</xsl:variable>
    <xsl:value-of select="replace(replace(normalize-space($content),'\\','\\\\'),$inq,$outq)"/>
</xsl:function>

<!-- JSON name:value pair. Third parameter is boolean to say whether
     or not the value should be enclosed in quotes -->
<xsl:function name="tei:json" as="xs:string">
  <xsl:param name="label"/>
  <xsl:param name="content"/>
  <xsl:param name="string"/>
  <xsl:variable name="dq">"</xsl:variable>
  <xsl:value-of select="concat($dq,$label,$dq,':',tei:jsonValue($content,$string))"/>
</xsl:function>

<!-- JSON value. Second parameter is boolean to say whether
     or not the value should be enclosed in quotes -->
<xsl:function name="tei:jsonValue" as="xs:string">
  <xsl:param name="content"/>
  <xsl:param name="string"/>
  <xsl:variable name="dq">"</xsl:variable>
  <xsl:value-of select="concat(if ($string) then $dq else '',$content,if ($string) then $dq else '')"/>
</xsl:function>

<!-- JSON object, to be enclosed in {}, content separated by , -->
<xsl:function name="tei:jsonObject" as="xs:string">
  <xsl:param name="content" as="xs:string*"/>
  <xsl:variable name="r" >
    <xsl:value-of select="$content" separator=","/>
  </xsl:variable>
  <xsl:value-of select="concat('{',$r,'}')"/>
</xsl:function>

<!-- JSON label:array, to be encloded in []. 
     Second parameter is a sequence of strings.
     Third parameter is boolean to say whether
     or not the value should be enclosed in quotes 
 -->
<xsl:function name="tei:jsonArray" as="xs:string">
  <xsl:param name="label"/>
  <xsl:param name="contents"/>
  <xsl:param name="string"/>
  <xsl:variable name="dq">"</xsl:variable>
  <xsl:variable name="strings" as="xs:string*">
    <xsl:for-each select="$contents">
      <xsl:value-of select="tei:jsonValue(.,$string)"/>
    </xsl:for-each>
  </xsl:variable>
  <xsl:variable name="r" >
    <xsl:value-of select="($strings)" separator=","/>
  </xsl:variable>
  <xsl:value-of select="concat($dq,$label,$dq,':','[',$r,']')"/>
</xsl:function>

<!-- calculate an XPATH to take us from root of doc to where we are
     now -->
<xsl:function name="tei:xpath" as="xs:string">
  <xsl:param name="context"/>
  <xsl:for-each select="$context">
    <xsl:variable name="r">
      <xsl:for-each select="ancestor::*">
	<xsl:value-of select="name()"/>
      <xsl:text>[</xsl:text>
      <xsl:value-of select="position()"/>
      <xsl:text>]</xsl:text>
      <xsl:text>/</xsl:text>
      </xsl:for-each>
      <xsl:value-of select="name()"/>
      <xsl:text>[</xsl:text>
      <xsl:value-of select="position()"/>
      <xsl:text>]</xsl:text>
    </xsl:variable>
    <xsl:value-of select="$r"/>
    </xsl:for-each>
</xsl:function>

</xsl:stylesheet>
