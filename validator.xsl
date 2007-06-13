<xsl:stylesheet version="1.0"
  xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
  xmlns:teix="http://www.tei-c.org/ns/Examples"
  xmlns:rng="http://relaxng.org/ns/structure/1.0"
  xmlns:estr="http://exslt.org/strings"
  xmlns:pantor="http://www.pantor.com/ns/local"
  xmlns:exsl="http://exslt.org/common"
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:edate="http://exslt.org/dates-and-times"
  extension-element-prefixes="exsl estr edate"
  exclude-result-prefixes="exsl rng edate estr tei a pantor teix" 
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:include href="pointerattributes.xsl"/>

<xsl:key name="IDENTS"
	 match="tei:moduleSpec|tei:elementSpec|tei:classSpec|tei:macroSpec"   
	 use="@ident"/>

<xsl:key name="IDS"
	 match="tei:*"   
	 use="@xml:id"/>

<xsl:key name="EXIDS"
	 match="teix:*"   
	 use="@xml:id"/>


<xsl:template match="text()"/>

<xsl:template match="teix:*|tei:*|rng:*">
  <xsl:apply-templates select="@*"/>
  <xsl:apply-templates 
      select="teix:*|tei:*|rng:*"/>
</xsl:template>

<xsl:template match="@*"/>

<!-- vallist data.enumerated check removed pro tem -->
<!--
<xsl:template match="rng:ref[@name='data.enumerated']">
  <xsl:if
      test="not(../../tei:valList)">
  <xsl:message>ERROR: 
	No valList in <xsl:value-of
	select="ancestor::tei:attDef/@ident"/>@<xsl:value-of
	select="ancestor::tei:elementSpec/@ident|ancestor::tei:classSpec/@ident"/>
	where datatype is enumerated
  </xsl:message>
  </xsl:if>
</xsl:template>

<xsl:template match="tei:valList">
  <xsl:if
      test="not(../tei:datatype/rng:ref[@name='data.enumerated'])">
  <xsl:message>ERROR: 
	valList in <xsl:value-of
	select="ancestor::tei:attDef/@ident"/>@<xsl:value-of
	select="ancestor::tei:elementSpec/@ident|ancestor::tei:classSpec/@ident"/>
	where datatype is not enumerated
  </xsl:message>

  </xsl:if>
</xsl:template>
-->

<!-- moduleRef must point to something -->
<xsl:template match="tei:memberOf|tei:moduleRef">
     <xsl:if test="not(key('IDENTS',@key))">
       <xsl:call-template name="Error">
	 <xsl:with-param name="value" select="@key"/>
       </xsl:call-template>
     </xsl:if>
</xsl:template>

<!-- specDesc must point to something -->
<xsl:template match="tei:specDesc">
    <xsl:choose>
     <xsl:when test="key('IDENTS',@key)">
       <xsl:if test="@atts">
	 <xsl:call-template name="checkAtts">
	   <xsl:with-param name="a" select="concat(normalize-space(@atts),' ')"/>
	 </xsl:call-template>
       </xsl:if>
     </xsl:when>
     <xsl:otherwise>
       <xsl:call-template name="Error">
	 <xsl:with-param name="value" select="@key"/>
       </xsl:call-template>
     </xsl:otherwise>
    </xsl:choose>
</xsl:template>


<xsl:template name="checkAtts">
  <xsl:param name="a"/>
<xsl:variable name="me"><xsl:value-of select="name(.)"/>: <xsl:call-template name="loc"/></xsl:variable>
<xsl:variable name="k"><xsl:value-of select="@key"/></xsl:variable>

  <xsl:choose>
    <xsl:when test="contains($a,' ')">
      <xsl:variable name="this">
	<xsl:value-of select="substring-before($a,' ')"/>
      </xsl:variable>
      <xsl:if test="not(key('IDENTS',$k)/tei:attList//tei:attDef[@ident=$this])">
  <xsl:message>ERROR: <xsl:value-of select="$me"/> refers to <xsl:value-of select="$this"/> in <xsl:value-of select="$k"/>, which does not exist</xsl:message>
	</xsl:if>
      <xsl:call-template name="checkAtts">
	<xsl:with-param name="a" select="substring-after($a,' ')"/>
      </xsl:call-template>
    </xsl:when>
  </xsl:choose>
</xsl:template>



<xsl:template name="checklinks">
  <xsl:param name="stuff"/>
  <xsl:choose>
    <xsl:when test="contains($stuff,' ')">
      <xsl:variable name="This">
	<xsl:value-of select="substring-before($stuff,' ')"/>
      </xsl:variable>
      <xsl:call-template name="checkThisLink">
	<xsl:with-param name="What" select="$This"/>
      </xsl:call-template>
      <xsl:call-template name="checklinks">
	<xsl:with-param name="stuff">
	  <xsl:value-of select="substring-after($stuff,' ')"/>
	</xsl:with-param>
      </xsl:call-template>
    </xsl:when>
    <xsl:otherwise>
      <xsl:call-template name="checkThisLink">
	<xsl:with-param name="What" select="$stuff"/>
      </xsl:call-template>
  </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template name="checkThisLink">
  <xsl:param name="What"/>
  <xsl:choose>
    <xsl:when test="starts-with($What,'#')">
      <xsl:choose>
	<xsl:when test="key('IDS',substring-after($What,'#'))"/>
	<xsl:otherwise>
	  <xsl:call-template name="Error">
	    <xsl:with-param name="value" select="$What"/>
	  </xsl:call-template>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:when>
    <xsl:when test="starts-with($What,'mailto:')"/>
    <xsl:when test="starts-with($What,'http:')"/>
      <xsl:when test="not(contains($What,'/')) and
		      not(key('@IDS',$What))">
	<xsl:call-template name="Error">
	  <xsl:with-param name="value" select="$What"/>
       </xsl:call-template>
      </xsl:when>
  </xsl:choose>
</xsl:template>


<xsl:template name="loc">
    <xsl:for-each select="ancestor::tei:*|ancestor::teix:*">
      <xsl:value-of select="name(.)"/>
      <xsl:text>[</xsl:text>
      <xsl:choose>
	<xsl:when test="@xml:id">
	  <xsl:text>"</xsl:text>
	  <xsl:value-of select="@xml:id"/>
	  <xsl:text>"</xsl:text>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:value-of select="position()"/>
	</xsl:otherwise>
      </xsl:choose>
      <xsl:text>]/</xsl:text>
    </xsl:for-each>
</xsl:template>

<xsl:template name="Remark">
  <xsl:param name="value"/>
  <xsl:message>Remark: <xsl:value-of 
select="name(.)"/> points to ID not in my namespace: <xsl:value-of select="$value"/> (<xsl:call-template name="loc"/>) </xsl:message>
</xsl:template>

<xsl:template name="Error">
  <xsl:param name="value"/>
  <xsl:message>ERROR: <xsl:value-of 
select="name(.)"/> points to non-existent <xsl:value-of select="$value"/> (<xsl:call-template name="loc"/>) </xsl:message>
</xsl:template>

<xsl:template name="Warning">
  <xsl:param name="value"/>
  <xsl:variable name="where">
    <xsl:value-of select="name(parent::*)"/>
    <xsl:text>@</xsl:text>
    <xsl:value-of select="name(.)"/>
  </xsl:variable>

  <xsl:message>Warning: <xsl:value-of 
select="$where"/> points to something I cannot find: <xsl:value-of
select="$value"/> (<xsl:call-template name="loc"/>) 
</xsl:message>
</xsl:template>


<xsl:template name="checkexamplelinks">
  <xsl:param name="stuff"/>
  <xsl:choose>
    <xsl:when test="contains($stuff,' ')">
      <xsl:variable name="This">
	<xsl:value-of select="substring-before($stuff,' ')"/>
      </xsl:variable>
      <xsl:call-template name="checkThisExampleLink">
	<xsl:with-param name="What" select="$This"/>
      </xsl:call-template>
      <xsl:call-template name="checkexamplelinks">
	<xsl:with-param name="stuff">
	  <xsl:value-of select="substring-after($stuff,' ')"/>
	</xsl:with-param>
      </xsl:call-template>
    </xsl:when>
    <xsl:otherwise>
      <xsl:call-template name="checkThisExampleLink">
	<xsl:with-param name="What" select="$stuff"/>
      </xsl:call-template>
  </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template name="checkThisExampleLink">
  <xsl:param name="What"/>
  <xsl:choose>
    <xsl:when test="starts-with($What,'#')">
      <xsl:variable name="N">
	<xsl:value-of select="substring-after($What,'#')"/>
      </xsl:variable>
      <xsl:choose>
	<xsl:when test="key('EXIDS',$N)"/>
	<xsl:when test="key('IDS',$N)">
	  <xsl:call-template name="Remark">
	    <xsl:with-param name="value" select="$What"/>
	  </xsl:call-template>
	</xsl:when>
	<!--
	    <xsl:when test="not(ancestor::teix:egXML//teix:*[@xml:id=$N])">
	    <xsl:call-template name="Warning">
	    <xsl:with-param name="value" select="$What"/>
	    </xsl:call-template>
	    </xsl:when>
	-->
	<xsl:otherwise>
	    <xsl:call-template name="Warning">
	    <xsl:with-param name="value" select="$What"/>
	    </xsl:call-template>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:when>
    <xsl:when test="starts-with($What,'mailto:')"/>
    <xsl:when test="starts-with($What,'http:')"/>
      <xsl:when test="name(.)='url' and
		      local-name(parent::*)='graphic'"/>
      <xsl:when test="name(.)='url' and
		      local-name(parent::*)='fsdDecl'"/>
      <xsl:when test="name(.)='target' and
		      local-name(parent::*)='ref'"/>
      <xsl:when test="name(.)='target' and
		      local-name(parent::*)='ptr'"/>
      <xsl:when test="name(.)='url' and local-name(parent::*)='graphic'"/>
      <xsl:when test="not(contains($What,'/'))">
       <xsl:call-template name="Warning">
	 <xsl:with-param name="value" select="$What"/>
       </xsl:call-template>
      </xsl:when>
  </xsl:choose>
</xsl:template>

</xsl:stylesheet>

