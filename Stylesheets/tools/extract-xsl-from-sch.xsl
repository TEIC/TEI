<xsl:stylesheet
    xmlns:s="http://www.ascc.net/xml/schematron"
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
    xmlns:i="http://www.iso.org/ns/1.0"
    xmlns:XSL="http://www.w3.org/1999/XSL/TransformAlias" 
    exclude-result-prefixes="xsl s" 
    version="2.0"
>

<xsl:namespace-alias stylesheet-prefix="XSL" result-prefix="xsl"/>

<xsl:key name="CONTEXTS" use="." match="@context"/>
<xsl:key name="ALLCONTEXTS" use="1" match="@context"/>
<xsl:output method="xml" indent="yes" encoding="utf-8"/>


<xsl:template match="/">
  <XSL:stylesheet
      xmlns:i="http://www.iso.org/ns/1.0"
      xmlns:tei="http://www.tei-c.org/ns/1.0"
      xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
      version="2.0"
      >
   
   <xsl:for-each select="key('ALLCONTEXTS',1)">
     <xsl:if test="generate-id(.)=generate-id(key('CONTEXTS',.)[1])">
       <XSL:template match="{.}" mode="checkSchematron">
	 <xsl:for-each select="key('CONTEXTS',.)">
	   <xsl:for-each select="parent::s:rule">
	     <xsl:for-each select="s:assert|s:report">
	       <xsl:choose>
		 <xsl:when test="self::s:assert">
		   <XSL:if test="not({@test})">
		     <XSL:call-template name="generateError">
		       <XSL:with-param name="message">
			 <xsl:value-of select="text()"/>
		       </XSL:with-param>
		     </XSL:call-template>
		   </XSL:if>
		 </xsl:when>
		 <xsl:when test="self::s:report">
		   <XSL:if test="{@test}">
		     <XSL:call-template name="generateError">
		       <XSL:with-param name="message">
			 <xsl:value-of select="text()"/>
		       </XSL:with-param>
		     </XSL:call-template>
		   </XSL:if>
		 </xsl:when>
	       </xsl:choose>
	     </xsl:for-each>
	   </xsl:for-each>
	 </xsl:for-each>
	 <XSL:call-template name="copyIt"/>
       </XSL:template>
     </xsl:if>
  </xsl:for-each>

  <XSL:template match="processing-instruction()" mode="checkSchematron">
    <XSL:call-template name="copyMe"/>
  </XSL:template>
  
  <XSL:template match="@*|text()|comment()" mode="checkSchematron">
    <XSL:call-template name="copyMe"/>
  </XSL:template>
  
  <XSL:template match="*" mode="checkSchematron">
    <XSL:call-template name="copyIt"/>
  </XSL:template>


</XSL:stylesheet>


</xsl:template>

</xsl:stylesheet>
