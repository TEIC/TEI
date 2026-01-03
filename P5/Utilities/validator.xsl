<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:svrl="http://purl.oclc.org/dsdl/svrl"
  xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
  xmlns:teix="http://www.tei-c.org/ns/Examples"
  xmlns:rng="http://relaxng.org/ns/structure/1.0"
  xmlns:tei="http://www.tei-c.org/ns/1.0" 
  version="2.0" exclude-result-prefixes="svrl rng tei a teix">
  
  <!--
    REVISIONS:
    2013-12-10 by Syd Bauman:
    * collapse the 3 (should have been 4) templates that matched
      svrl:failed-assert and svrl:successful-report into one template
    * iterate only over distinct IDs, so we only get 1 error for a
      duplicate ID, rather than 1 error for each duplicate value
  -->

  <xsl:output indent="yes" omit-xml-declaration="yes"/>

  <xsl:include href="pointerattributes.xsl"/>
  <xsl:key name="IDENTS" match="tei:dataSpec|tei:elementSpec|tei:classSpec|tei:macroSpec" use="@ident"/>
  <xsl:key name="MIDENTS" match="tei:moduleSpec" use="@ident"/>
  <xsl:key name="EXIDS" match="teix:*[@xml:id]" use="@xml:id"/>
  <xsl:key name="IDS" match="*[@xml:id]" use="@xml:id"/>
  <xsl:variable name="root" select="/"/>

  <xsl:template match="/">
    <Messages>
      <xsl:apply-templates/>
      <xsl:for-each select="distinct-values( //*[@xml:id]/@xml:id )">
        <xsl:variable name="thisID" select="."/>
        <xsl:for-each select="$root">
          <xsl:if test="count( key('IDS', $thisID ) ) > 1">
            <ERROR>id '<xsl:value-of select="$thisID"/>' used more than once</ERROR>
          </xsl:if>
        </xsl:for-each>
      </xsl:for-each>
      <xsl:apply-templates select="doc('../Schematron1.xml')//svrl:failed-assert"/>
      <xsl:apply-templates select="doc('../Schematron1.xml')//svrl:successful-report"/>
      <xsl:apply-templates select="doc('../Schematron2.xml')//svrl:failed-assert"/>
      <xsl:apply-templates select="doc('../Schematron2.xml')//svrl:successful-report"/>
    </Messages>
  </xsl:template>
  
  <xsl:template match="svrl:failed-assert|svrl:successful-report">
    <xsl:variable name="outGI">
      <xsl:choose>
	<!-- We explicitly test for the 6 values oXygen provides in its
	     “sample values include” list, plus the 1 other value that
	     actually occurs in P5. -->
	<xsl:when test="@role = ('info','information')">INFORMATION</xsl:when>
	<xsl:when test="@role = ('warn','warning')"    >WARNING</xsl:when>
	<xsl:when test="@role eq 'nonfatal'"           >WARNING</xsl:when>
	<xsl:when test="@role = ('error','fatal')"     >ERROR</xsl:when>
        <xsl:otherwise>ERROR</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:element name="{$outGI}">Schematron <xsl:value-of
      select="lower-case($outGI)"/>: <xsl:value-of select="svrl:text"/> [Test: <xsl:value-of select="@test"
      />] Location: <xsl:value-of
        select="replace(replace(@location,'\[namespace-uri\(\)=.http://www.tei-c.org/ns/1.0.\]',''),'/\*:','/')"
      /></xsl:element>
  </xsl:template>

  <xsl:template match="text()"/>

  <xsl:template match="teix:*|tei:*|rng:*">
    <xsl:apply-templates select="@*"/>
    <xsl:apply-templates select="teix:*|tei:*|rng:*"/>
  </xsl:template>
  <xsl:template match="@*"/>
  <!-- vallist data.enumerated check removed pro tem -->
  <!--
<xsl:template match="rng:ref[@name='data.enumerated']">
  <xsl:if
      test="not(../../tei:valList)">
  <ERROR>
	No valList in <xsl:value-of
	select="ancestor::tei:attDef/@ident"/>@<xsl:value-of
	select="ancestor::tei:elementSpec/@ident|ancestor::tei:classSpec/@ident"/>
	where datatype is enumerated
  </
  </xsl:if>
</xsl:template>

<xsl:template match="tei:valList">
  <xsl:if
      test="not(../tei:datatype/rng:ref[@name='data.enumerated'])">
  <ERROR>
	valList in <xsl:value-of
	select="ancestor::tei:attDef/@ident"/>@<xsl:value-of
	select="ancestor::tei:elementSpec/@ident|ancestor::tei:classSpec/@ident"/>
	where datatype is not enumerated
  </

  </xsl:if>
</xsl:template>
-->
  <!-- moduleRef must point to something -->
  <xsl:template match="tei:moduleRef">
    <xsl:if test="not(key('MIDENTS',@key))">
      <xsl:call-template name="Error">
        <xsl:with-param name="value" select="@key"/>
      </xsl:call-template>
    </xsl:if>
  </xsl:template>

  <!-- memberOf must point to something -->
  <xsl:template match="tei:memberOf">
    <xsl:if test="not(key('IDENTS',@key))">
      <xsl:call-template name="Error">
        <xsl:with-param name="value" select="@key"/>
      </xsl:call-template>
    </xsl:if>
  </xsl:template>
  <!-- source on egXML must point to something -->
  <xsl:template match="teix:egXML[@source]">
    <xsl:if test="not(id(substring(@source,2)))">
      <xsl:call-template name="Error">
        <xsl:with-param name="value" select="@source"/>
      </xsl:call-template>
    </xsl:if>
  </xsl:template>
  <xsl:template match="teix:egXML[@corresp]">
    <xsl:call-template name="Error">
      <xsl:with-param name="value" select="@corresp"/>
    </xsl:call-template>
  </xsl:template>
  <!-- content of <ident type="class"> must point to something -->
  <xsl:template match="tei:ident[@type='class']">
    <xsl:if test="not(key('IDENTS',replace(.,'_[A-z]*','')))">
      <xsl:call-template name="Error">
        <xsl:with-param name="value" select="."/>
      </xsl:call-template>
    </xsl:if>
  </xsl:template>


  <!-- specDesc must point to something -->
  <xsl:template match="tei:specDesc">
    <xsl:choose>
      <xsl:when test="key('IDENTS',@key)">
        <xsl:if test="@atts">
	  <xsl:call-template name="checkAtts">
	    <xsl:with-param name="loc">
	      <xsl:value-of select="name(.)"/>
	      <xsl:text>: </xsl:text>
	      <xsl:call-template name="loc"/>
	    </xsl:with-param>
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
    <xsl:param name="loc"/>
    <xsl:variable name="atts" select="normalize-space(@atts)"/>
    <xsl:for-each select="key('IDENTS',@key)">
      <xsl:variable name="here" select="."/>
      <xsl:for-each select="tokenize($atts, ' ')">
	<xsl:variable name="this" select="."/>
	<xsl:for-each select="$here">
	  <xsl:choose>
	    <xsl:when test="$this=''"/>
	    <xsl:when test="$this='-'"/>
	    <xsl:when test="$this='+'"/>
	    <xsl:when test=".//tei:attDef[@ident=$this]"/>
	    <xsl:otherwise>
	      <xsl:variable name="ok">
		<xsl:call-template name="checkClassesForAttribute">
		  <xsl:with-param name="TOKEN" select="$this"/>
		</xsl:call-template>
	      </xsl:variable>
	      <xsl:if test="$ok=''">
		<ERROR>"<xsl:value-of select="@ident"/>" in <xsl:value-of select="$loc"/> refers to attribute "<xsl:value-of select="$this"/>" which does not exist</ERROR>
	      </xsl:if>
	    </xsl:otherwise>
	  </xsl:choose>
	</xsl:for-each>
      </xsl:for-each>
    </xsl:for-each>
  </xsl:template>


  <xsl:template name="checkClassesForAttribute">
    <xsl:param name="TOKEN"/>
      <xsl:for-each select="tei:classes/tei:memberOf/key('IDENTS',@key)">
	<xsl:choose>
	  <xsl:when test="tei:attList//tei:attDef[@ident=$TOKEN]">
	      <xsl:text>y</xsl:text>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:call-template name="checkClassesForAttribute">
	      <xsl:with-param name="TOKEN" select="$TOKEN"/>
	    </xsl:call-template>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:for-each>
  </xsl:template>

  <xsl:template name="checklinks">
    <xsl:param name="stuff"/>
    <xsl:variable name="context" select="."/>
    <xsl:for-each select="tokenize($stuff,' ')">
      <xsl:sequence select="tei:checkThisLink(normalize-space(.),$context)"/>
    </xsl:for-each>
  </xsl:template>
  <xsl:function name="tei:checkThisLink" as="node()*">
    <xsl:param name="What"/>
    <xsl:param name="Where"/>
    <xsl:for-each select="$Where">
      <xsl:choose>
        <xsl:when test="starts-with($What,'#')">
          <xsl:choose>
            <xsl:when test="id(substring($What,2))"/>
            <xsl:otherwise>
              <xsl:call-template name="Error">
                <xsl:with-param name="value" select="$What"/>
              </xsl:call-template>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:when>
        <xsl:when test="starts-with($What,'tei:')"/>
        <xsl:when test="starts-with($What,'mailto:')"/>
        <xsl:when test="starts-with($What,'http:')"/>
        <xsl:when test="contains($What,'.html')"/>
        <xsl:when test="not(contains($What,'/')) and not(id($What))">
          <xsl:call-template name="Error">
            <xsl:with-param name="value" select="$What"/>
          </xsl:call-template>
        </xsl:when>
      </xsl:choose>
    </xsl:for-each>
  </xsl:function>

  <xsl:template name="loc">
    <xsl:for-each select="ancestor::tei:*|ancestor::teix:*">
      <xsl:value-of select="name(.)"/>
      <xsl:text>[</xsl:text>
      <xsl:choose>
        <xsl:when test="@ident">
          <xsl:text>"</xsl:text>
          <xsl:value-of select="@ident"/>
          <xsl:text>"</xsl:text>
        </xsl:when>
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
    <Note><xsl:value-of select="name(.)"/> points to ID not in my namespace: <xsl:value-of
        select="$value"/> (<xsl:call-template name="loc"/>) </Note>
  </xsl:template>
  <xsl:template name="Error">
    <xsl:param name="value"/>
    <ERROR><xsl:value-of select="name(.)"/> points to non-existent <xsl:value-of select="$value"/>
        (<xsl:call-template name="loc"/>) </ERROR>
  </xsl:template>
  <xsl:template name="Warning">
    <xsl:param name="value"/>
    <xsl:variable name="where">
      <xsl:value-of select="name(parent::*)"/>
      <xsl:text>@</xsl:text>
      <xsl:value-of select="name(.)"/>
    </xsl:variable>
    <Note><xsl:value-of select="$where"/> points to something I cannot find: <xsl:value-of
        select="$value"/> (<xsl:call-template name="loc"/>) </Note>
  </xsl:template>
  <xsl:template name="checkexamplelinks">
    <xsl:param name="stuff"/>
    <xsl:variable name="context" select="."/>
    <xsl:for-each select="tokenize($stuff,' ')">
      <xsl:sequence select="tei:checkThisExampleLink(normalize-space(.),$context)"/>
    </xsl:for-each>
  </xsl:template>

  <xsl:function name="tei:checkThisExampleLink" as="node()*">
    <xsl:param name="What"/>
    <xsl:param name="Where"/>
    <xsl:for-each select="$Where">
      <xsl:choose>
        <xsl:when test="starts-with($What,'#')">
          <xsl:variable name="N">
            <xsl:value-of select="substring-after($What,'#')"/>
          </xsl:variable>
          <xsl:choose>
            <xsl:when test="key('EXIDS',$N)"/>
            <xsl:when test="$N='COHQHF'"/>
            <!-- special exception -->
            <xsl:when test="id($N)">
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
              <!--
		  <xsl:call-template name="Warning">
		  <xsl:with-param name="value" select="$What"/>
		  </xsl:call-template>
	      -->
            </xsl:otherwise>
          </xsl:choose>
        </xsl:when>
        <xsl:when test="starts-with($What,'example.xml')"/>
        <xsl:when test="ends-with($What,'.png')"/>
        <xsl:when test="ends-with($What,'.mp4')"/>
        <xsl:when test="ends-with($What,'.wav')"/>
        <xsl:when test="ends-with($What,'.rng')"/>
        <xsl:when test="starts-with($What,'tei:')"/>
        <xsl:when test="starts-with($What,'mailto:')"/>
        <xsl:when test="starts-with($What,'http:')"/>
        <xsl:when test="name(.)='url' and         local-name(parent::*)='graphic'"/>
        <xsl:when test="name(.)='url' and         local-name(parent::*)='fsdDecl'"/>
        <xsl:when test="name(.)='target' and         local-name(parent::*)='ref'"/>
        <xsl:when test="name(.)='target' and         local-name(parent::*)='ptr'"/>
        <xsl:when test="name(.)='url' and local-name(parent::*)='graphic'"/>
        <xsl:when test="not(contains($What,'/'))">
          <xsl:call-template name="Warning">
            <xsl:with-param name="value" select="$What"/>
          </xsl:call-template>
        </xsl:when>
      </xsl:choose>
    </xsl:for-each>
  </xsl:function>

</xsl:stylesheet>
