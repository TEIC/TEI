<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet 
    xmlns:s="http://www.ascc.net/xml/schematron" 
    xmlns:sch="http://purl.oclc.org/dsdl/schematron"
    xmlns:teix="http://www.tei-c.org/ns/Examples"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns="http://www.tei-c.org/ns/1.0"
    exclude-result-prefixes="tei teix s sch"
    version="2.0">

<xsl:output 
   method="xml"
   indent="yes"
   encoding="utf-8"
   cdata-section-elements="tei:eg"
   omit-xml-declaration="yes"/>

  <xsl:key name="IDENTS" use="@ident" match="tei:elementSpec"/>
  <xsl:key name="IDENTS" use="@ident" match="tei:classSpec"/>
  <xsl:key name="IDENTS" use="@ident" match="tei:macroSpec"/>

  <xsl:key name="IDENTS"   use="concat(ancestor::tei:elementSpec/@ident,'_',@ident)"
	   match="tei:elementSpec/tei:attList/tei:attDef"/>

  <xsl:key name="IDENTS"   use="concat(ancestor::tei:classSpec/@ident,'_',@ident)"
	   match="tei:classSpec/tei:attList/tei:attDef"/>

  <xsl:key name="IDENTS"   
	   use="concat(ancestor::tei:elementSpec/@ident,'_',ancestor::tei:attDef/@ident,'_',@ident)"
	   match="tei:elementSpec/tei:attList/tei:attDef/tei:valList/tei:valItem"/>

  <xsl:key name="IDENTS"   
	   use="concat(ancestor::tei:classSpec/@ident,'_',ancestor::tei:attDef/@ident,'_',@ident)"
	   match="tei:classSpec/tei:attList/tei:attDef/tei:valList/tei:valItem"/>


  <xsl:param name="verbose"/>
  <xsl:param name="newFile"/>
  <xsl:param name="newLang"/>
  <xsl:param name="source"/>
  <xsl:param name="overwrite">false</xsl:param>

  <xsl:output encoding="utf-8" indent="yes"/>

  <xsl:variable name="Original" select="/"/>

  <xsl:variable name="New" select="document($newFile)"/>

  <xsl:template match="/">
<xsl:if test="$verbose='true'">
<xsl:message>
Language: <xsl:value-of select="$newLang"/>
File: <xsl:value-of select="$newFile"/>
Source: <xsl:value-of select="$source"/>
Overwrite: <xsl:value-of select="$overwrite"/>
</xsl:message>
</xsl:if>
    <xsl:apply-templates/>
  </xsl:template>


<xsl:template match="tei:gloss|tei:desc">

  <xsl:variable name="Name">
    <xsl:value-of select="local-name(.)"/>
  </xsl:variable>

  <xsl:choose>
    <xsl:when test="@xml:lang and not(@xml:lang=$newLang)">
      <xsl:element name="{local-name()}">
	<xsl:apply-templates select="@*|text()|*|comment()"/>
      </xsl:element>
    </xsl:when>
    <xsl:when test="@xml:lang=$newLang and $overwrite='false'">
      <xsl:element name="{local-name()}">
	<xsl:apply-templates select="@*|text()|*|comment()"/>
      </xsl:element>
    </xsl:when>
    <xsl:when test="@xml:lang=$newLang and $overwrite='true'"/>
    <xsl:otherwise>
      <xsl:element name="{local-name()}">
	<xsl:apply-templates select="@*|text()|*|comment()"/>
      </xsl:element>
      <xsl:variable name="this">
	<xsl:value-of select="normalize-space(.)"/>
      </xsl:variable>

      <xsl:variable name="glossy">
	<xsl:choose>
	  <xsl:when test="tei:gloss">true</xsl:when>
	  <xsl:otherwise>false</xsl:otherwise>
	</xsl:choose>
      </xsl:variable>
      <xsl:variable name="What">
	<xsl:choose>
	  <xsl:when test="parent::tei:attDef">
	    <xsl:value-of
		select="concat(ancestor::tei:elementSpec/@ident|ancestor::tei:classSpec/@ident,'_',../@ident)"/>
	  </xsl:when>
	  <xsl:when test="parent::tei:valItem">
	    <xsl:value-of
		select="concat(ancestor::tei:elementSpec/@ident|ancestor::tei:classSpec/@ident,'_',ancestor::tei:attDef/@ident,'_',../@ident)"/>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:value-of select="../@ident"/>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:variable>

      <xsl:for-each select="$New">
	<xsl:for-each select="key('IDENTS',$What)">
	  <xsl:choose>
	    <xsl:when test="$Name='gloss' and tei:gloss and not(tei:desc)"/>
	    <xsl:when test="$Name='gloss' and tei:gloss">
	      <xsl:for-each select="tei:gloss">
	      <xsl:call-template name="bits">
		<xsl:with-param name="Name">
		  <xsl:value-of select="$Name"/>
		</xsl:with-param>
		<xsl:with-param name="this">
		  <xsl:value-of select="$this"/>
		</xsl:with-param>
	      </xsl:call-template>
	      </xsl:for-each>
	    </xsl:when>
	    <xsl:when test="$Name='desc' and tei:desc">
	      <xsl:for-each select="tei:desc">
	      <xsl:call-template name="bits">
		<xsl:with-param name="Name">
		  <xsl:value-of select="$Name"/>
		</xsl:with-param>
		<xsl:with-param name="this">
		  <xsl:value-of select="$this"/>
		</xsl:with-param>
	      </xsl:call-template>
	      </xsl:for-each>
	    </xsl:when>
	    <xsl:when test="$Name='desc' and $glossy = 'false' and
			    tei:gloss">
	      <xsl:for-each select="tei:gloss">
	      <xsl:call-template name="bits">
		<xsl:with-param name="Name">
		  <xsl:value-of select="$Name"/>		  
		</xsl:with-param>
		<xsl:with-param name="this">
		  <xsl:value-of select="$this"/>
		</xsl:with-param>
	      </xsl:call-template>
	      </xsl:for-each>
	    </xsl:when>
	  </xsl:choose>
	</xsl:for-each>
      </xsl:for-each>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template name="bits">
  <xsl:param name="Name"/>
  <xsl:param name="this"/>
  <xsl:if test="@xml:lang=$newLang">
    <xsl:variable name="that">
      <xsl:choose>
	<xsl:when test="starts-with(.,'(')">
	  <xsl:value-of select="substring-before(substring-after(normalize-space(.),'('),')')"/>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:value-of select="normalize-space(.)"/>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <!--<xsl:message>look for <xsl:value-of select="$What"/> giving <xsl:value-of select="$that"/></xsl:message>-->
    <xsl:if test="not($that=$this)">
      <xsl:variable name="date">
	<xsl:choose>
	  <xsl:when test="@notBefore">
	    <xsl:value-of select="@notBefore"/>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:value-of select="format-dateTime(current-dateTime(),'[Y]-[M02]-[D02]')"/>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:variable>
      <xsl:element name="{local-name()}"
		   xmlns="http://www.tei-c.org/ns/1.0">
	<xsl:attribute name="version">
	  <xsl:value-of select="$date"/>
	</xsl:attribute>
	<xsl:attribute name="xml:lang">
	  <xsl:value-of select="$newLang"/>
	</xsl:attribute>
	<xsl:apply-templates/>
      </xsl:element>
    </xsl:if>
  </xsl:if>
</xsl:template>


<xsl:template match="tei:remarks">
  <xsl:choose>
    <xsl:when test="not(@xml:lang)">
      <xsl:element name="{local-name()}">
	<xsl:apply-templates select="@*|text()|*|comment()"/>
      </xsl:element>
    </xsl:when>
    <xsl:when test="$overwrite='false'">
      <xsl:element name="{local-name()}">
	<xsl:apply-templates select="@*|text()|*|comment()"/>
      </xsl:element>
    </xsl:when>
    <xsl:when test="not(@xml:lang=$newLang)">
      <xsl:element name="{local-name()}">
	<xsl:apply-templates select="@*|text()|*|comment()"/>
      </xsl:element>
    </xsl:when>
  </xsl:choose>
  <xsl:if test="not(preceding-sibling::tei:remarks)">
    <xsl:variable name="this">
      <xsl:value-of select="normalize-space(.)"/>
    </xsl:variable>
    <xsl:variable name="What">
      <xsl:choose>
	<xsl:when test="parent::tei:attDef">
	  <xsl:value-of select="ancestor::tei:elementSpec/@ident|ancestor::tei:classSpec/@ident"/>
	  <xsl:text>_</xsl:text>
	  <xsl:value-of select="../@ident"/>
	</xsl:when>
	<xsl:when test="parent::tei:classSpec">
	  <xsl:value-of select="../@ident"/>
	</xsl:when>
	<xsl:when test="parent::tei:elementSpec">
	  <xsl:value-of select="../@ident"/>
	</xsl:when>
	<xsl:when test="parent::tei:macroSpec">
	  <xsl:value-of select="../@ident"/>
	</xsl:when>
      </xsl:choose>
    </xsl:variable>
    <xsl:for-each select="$New">
      <xsl:for-each select="key('IDENTS',$What)/tei:remarks">
	<xsl:if test="@xml:lang=$newLang">
	<xsl:variable name="that">
	  <xsl:value-of select="normalize-space(.)"/>
	</xsl:variable>
	<xsl:if test="not($that=$this) and not($that='')">
	  <remarks xmlns="http://www.tei-c.org/ns/1.0">
	    <xsl:attribute name="xml:lang">
	      <xsl:value-of select="$newLang"/>
	    </xsl:attribute>
	    <xsl:apply-templates/>
	  </remarks>
	</xsl:if>
	</xsl:if>
      </xsl:for-each>
    </xsl:for-each>
  </xsl:if>
</xsl:template>

<xsl:template match="tei:exemplum">
  <xsl:choose>
    <xsl:when test="not(@xml:lang)">
      <xsl:element name="{local-name()}">
	<xsl:apply-templates select="@*|text()|*|comment()"/>
      </xsl:element>
    </xsl:when>
    <xsl:when test="not(@xml:lang=$newLang)">
      <xsl:element name="{local-name()}">
	<xsl:apply-templates select="@*|text()|*|comment()"/>
      </xsl:element>
    </xsl:when>
  </xsl:choose>
  <xsl:variable name="What">
    <xsl:choose>
      <xsl:when test="parent::tei:attDef">
	<xsl:value-of select="ancestor::tei:elementSpec/@ident|ancestor::tei:classSpec/@ident"/>
	<xsl:text>_</xsl:text>
	<xsl:value-of select="../@ident"/>
      </xsl:when>
      <xsl:when test="parent::tei:classSpec">
	<xsl:value-of select="../@ident"/>
      </xsl:when>
      <xsl:when test="parent::tei:elementSpec">
	<xsl:value-of select="../@ident"/>
      </xsl:when>
      <xsl:when test="parent::tei:macroSpec">
	<xsl:value-of select="../@ident"/>
      </xsl:when>
    </xsl:choose>
  </xsl:variable>
  <xsl:if test="not(preceding-sibling::tei:exemplum)">
    <xsl:message>Look for example in language <xsl:value-of select="$newLang"/> for <xsl:value-of select="$What"/></xsl:message>
    <xsl:for-each select="$New">
      <xsl:for-each select="key('IDENTS',$What)">
	<xsl:for-each select="tei:exemplum[@xml:lang=$newLang]">
	  <xsl:message>FOUND Example in language <xsl:value-of
	  select="$newLang"/> for <xsl:value-of
	  select="$What"/></xsl:message>
	  
	  <exemplum>
	    <xsl:copy-of select="@*"/>
	    <xsl:apply-templates mode="exemplum"/>
	  </exemplum>
	</xsl:for-each>
      </xsl:for-each>
    </xsl:for-each>
  </xsl:if>
</xsl:template>


<xsl:template 
    match="@*|text()|comment()|processing-instruction()"  >
  <xsl:copy-of select="."/>
</xsl:template>

<xsl:template match="tei:*">
  <xsl:element name="{local-name()}">
    <xsl:apply-templates 
	select="*|@*|processing-instruction()|comment()|text()"/>
  </xsl:element>
</xsl:template>

<xsl:template match="teix:*">
  <xsl:element name="{local-name()}" namespace="http://www.tei-c.org/ns/Examples">
    <xsl:apply-templates  select="@*"/>
    <xsl:apply-templates
	    select="*|processing-instruction()|comment()|text()"/>
  </xsl:element>
</xsl:template>

<xsl:template match="rng:*"     xmlns:rng="http://relaxng.org/ns/structure/1.0">
  <xsl:element name="{local-name()}" namespace="http://relaxng.org/ns/structure/1.0">
    <xsl:apply-templates 
	select="*|@*|processing-instruction()|comment()|text()"/>
  </xsl:element>
</xsl:template>

<xsl:template match="s:*">
  <xsl:element name="{local-name()}" namespace="http://www.ascc.net/xml/schematron">
    <xsl:apply-templates 
	select="*|@*|processing-instruction()|comment()|text()"/>
  </xsl:element>
</xsl:template>

<xsl:template match="sch:*">
  <xsl:element name="{local-name()}" namespace="http://purl.oclc.org/dsdl/schematron">
    <xsl:apply-templates 
	select="*|@*|processing-instruction()|comment()|text()"/>
  </xsl:element>
</xsl:template>

<xsl:template match="*">
  <xsl:copy>
    <xsl:apply-templates 
	select="*|@*|processing-instruction()|comment()|text()"/>
  </xsl:copy>
</xsl:template>

<!-- example processing -->

<xsl:template name="mangle">
  <xsl:param name="text"/>
  <xsl:text>#</xsl:text>
  <xsl:value-of select="concat($newLang,'_')"/>
  <xsl:choose>
    <xsl:when test="contains($text,' #')">
      <xsl:value-of select="substring-before($text,'#')"/>
      <xsl:call-template name="mangle">
	<xsl:with-param name="text">
	  <xsl:value-of select="substring-after($text,'#')"/>
	</xsl:with-param>
      </xsl:call-template>
    </xsl:when>
    <xsl:otherwise>
      <xsl:value-of select="$text"/>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>


<xsl:template match="teix:*" mode="exemplum">
  <xsl:element name="{local-name()}" namespace="http://www.tei-c.org/ns/Examples">
    <xsl:apply-templates 
	select="*|@*|processing-instruction()|comment()|text()"  mode="exemplum"/>
  </xsl:element>
</xsl:template>

<xsl:template match="rng:*"  mode="exemplum"     xmlns:rng="http://relaxng.org/ns/structure/1.0">
  <xsl:element name="{local-name()}" namespace="http://relaxng.org/ns/structure/1.0">
    <xsl:apply-templates 
	select="*|@*|processing-instruction()|comment()|text()"  mode="exemplum"/>
  </xsl:element>
</xsl:template>

<xsl:template match="*"  mode="exemplum">
  <xsl:copy>
    <xsl:apply-templates 
	select="*|@*|processing-instruction()|comment()|text()"  mode="exemplum"/>
  </xsl:copy>
</xsl:template>

<xsl:template match="tei:*"  mode="exemplum">
  <xsl:copy>
    <xsl:apply-templates 
	select="*|@*|processing-instruction()|comment()|text()"/>
  </xsl:copy>
</xsl:template>

<xsl:template 
    match="@*|text()|comment()|processing-instruction()"  mode="exemplum" >
  <xsl:copy-of select="."/>
</xsl:template>

<xsl:template match="@xml:id"  mode="exemplum">
  <xsl:attribute name="xml:id">
    <xsl:value-of select="concat($newLang,'_')"/>
    <xsl:value-of select="."/>
  </xsl:attribute>
</xsl:template>

<xsl:template 
    match="@active|@children|@from|@mergedIn|@mutual|@origin|@parent|@ref|@render|@replacementPattern|@resp|@scheme|@since|@spanTo|@target|@targets|@who|@wit"
    mode="exemplum">
  <xsl:choose>
    <xsl:when test="starts-with(.,'#')">
      <xsl:attribute name="{local-name(.)}">
	<xsl:call-template name="mangle">
	  <xsl:with-param name="text">
	    <xsl:value-of select="substring-after(.,'#')"/>
	  </xsl:with-param>
	</xsl:call-template>
      </xsl:attribute>
    </xsl:when>
    <xsl:otherwise>
      <xsl:copy-of select="."/>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>


</xsl:stylesheet>
