<?xml version="1.0" encoding="utf-8"?>
<!-- 
Text Encoding Initiative Consortium XSLT stylesheet family
$Date$, $Revision$, $Author$

XSL stylesheet to format TEI XML documents to HTML or XSL FO

 
##LICENSE
--> 
<xsl:stylesheet 
  xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
  xmlns:rng="http://relaxng.org/ns/structure/1.0"
  xmlns:teix="http://www.tei-c.org/ns/Examples"
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:fo="http://www.w3.org/1999/XSL/Format"
  xmlns:edate="http://exslt.org/dates-and-times"
  xmlns:exsl="http://exslt.org/common"
  xmlns:estr="http://exslt.org/strings"
  exclude-result-prefixes="exsl estr edate teix fo a tei rng" 
  extension-element-prefixes="edate exsl estr"
  version="1.0">
<xsl:include href="RngToRnc.xsl"/>
<xsl:param name="verbose"></xsl:param>
<xsl:param name="oddmode">html</xsl:param>
<xsl:variable name="TEITAGS">http://www.tei-c.org.uk/tei-bin/files.pl?name=tags.xml</xsl:variable>
<xsl:param name="ODDROOT">http://www.tei-c.org/P5/Odds/</xsl:param>
<xsl:param name="schemaBaseURL">http://www.tei-c.org/P5/Schema/</xsl:param>
 <xsl:key  name="CLASSMEMBERS" match="tei:elementSpec|tei:classSpec" use="tei:classes/tei:memberOf/@key"/>
 <xsl:key name="TAGS" match="Tag|Pattern|Class" use="ident"/>
 <xsl:key name="FILES"   match="tei:module[@ident]|tei:schema[@ident]"  use="@ident"/>
 <xsl:key name="IDENTS"   match="tei:elementSpec|tei:classSpec|tei:macroSpec"   use="@ident"/>
 <xsl:key name="NAMES"   match="tei:module"   use="@ident"/>
 <xsl:key name="TAGIDS"     match="*[@id]"           use="@id"/>
 <xsl:key name="TAGIDENTS"     match="Table/*[ident]"           use="ident"/>
 <xsl:key name="IDS"     match="tei:*[@id]"  use="@id"/>
 <xsl:key name="PATTERNDOCS" match="tei:macroSpec" use='1'/>
 <xsl:key name="PATTERNS" match="tei:macroSpec" use="@ident"/>
 <xsl:key name="CLASSDOCS" match="tei:classSpec" use='1'/>
 <xsl:key name="TAGDOCS" match="tei:elementSpec" use='1'/>
 <xsl:key name="CHUNKS" match="tei:specGrpRef" use="@target"/>
 <xsl:key name='PUBLICIDS' match="publicID" use="@file"/>
 <xsl:key name='NameToID' match="tei:*" use="@ident"/>
 <!-- build a lookup table of class names and their members -->

 <!-- lookup table of element contents, and templates to access the result -->
 <xsl:key name="ELEMENTPARENTS" match="Contains" use="."/>
 <xsl:key name="ELEMENTS" match="Element" use="@id"/>
 <xsl:param name="wrapLength">65</xsl:param>



<xsl:template match="processing-instruction()">
  <xsl:if test="name(.) = 'odds'">
    <xsl:choose>
      <xsl:when test=".='date'">
This formatted version of the Guidelines was 
created on <xsl:value-of select="edate:date-time()"/>.
     </xsl:when>
    </xsl:choose>
  </xsl:if>
</xsl:template>



<xsl:template match="rng:*" mode="literal">
  <xsl:text>
  </xsl:text>
  <xsl:for-each select="ancestor::rng:*">
    <xsl:text> </xsl:text>
  </xsl:for-each>
  <xsl:text>&lt;</xsl:text>
  <xsl:value-of select="name(.)"/>
  <xsl:for-each select="@*">
    <xsl:text> </xsl:text>
  <xsl:value-of select="name(.)"/>="<xsl:value-of select="."/>"</xsl:for-each>
  <xsl:choose>
    <xsl:when test=".//text()|rng:*">
      <xsl:text>&gt;</xsl:text>
      <xsl:apply-templates mode="literal"/>
      <xsl:if test="node()[last()]/self::rng:*"> 
	<xsl:text>
	</xsl:text>
      </xsl:if>
      <xsl:for-each select="ancestor::rng:*">
	<xsl:text> </xsl:text>
      </xsl:for-each>
      <xsl:text>&lt;/</xsl:text>
      <xsl:value-of select="name(.)"/>
      <xsl:text>&gt;</xsl:text>
    </xsl:when>    
    <xsl:otherwise>
      <xsl:text>/&gt;</xsl:text>
      <xsl:if test="node()[last()]/self::rng:*"> 
	<xsl:text>
	</xsl:text>
      </xsl:if>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>



<xsl:template match="rng:*">
  <xsl:copy>
    <xsl:copy-of select="@*"/>
    <xsl:apply-templates select="rng:*|tei:*|text()|comment()"/>
  </xsl:copy>
</xsl:template>



<xsl:template match="tei:*" mode="tangle"/>



<xsl:template match="tei:att">
  <xsl:call-template name="italicize">
    <xsl:with-param name="text">
      <xsl:apply-templates/>
    </xsl:with-param>
  </xsl:call-template>
</xsl:template>



<xsl:template match="tei:attDef" mode="tangle">
  <xsl:if test="not(@ident='xmlns')">
    <rng:ref name="{ancestor::tei:attList/../@ident}.attributes.{@ident}"/>
  </xsl:if>
</xsl:template>



<xsl:template match="tei:attDef[@mode='change']" mode="tangle"/>



<xsl:template match="tei:attList" mode="tangle">
  <xsl:choose>
    <xsl:when test="@org='choice'">
      <rng:optional>
	<rng:choice>
	  <xsl:apply-templates select="tei:*" mode="tangle"/>
	</rng:choice>
      </rng:optional>
    </xsl:when>
    <xsl:otherwise>
      <xsl:apply-templates select="tei:*" mode="tangle"/>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>



<xsl:template match="tei:author">
  <xsl:apply-templates/>,
</xsl:template>



<xsl:template match="tei:claDecl" mode="tangle">
  <xsl:if test="$verbose='true'">
    <xsl:message>  .. claDecl <xsl:value-of
    select="@target"/>,<xsl:value-of select="@type"/>  </xsl:message>
  </xsl:if>
  <xsl:if test="$verbose='true'">
    <xsl:message>        .. so visit  classSpec <xsl:value-of
    select="key('IDS',@target)/@ident"/></xsl:message>
  </xsl:if>
  
  <xsl:choose>
    <xsl:when test="@type='model'">    
      <xsl:apply-templates select="key('IDS',@target)" mode="processModel"/>
    </xsl:when>
    <xsl:when test="@type='default'">    
      <xsl:apply-templates select="key('IDS',@target)" mode="processDefaultAtts"/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:message terminate="yes"> claDecl for <xsl:value-of
      select="@target"/>,<xsl:value-of select="@type"/> is invalid</xsl:message>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>



<xsl:template match="tei:classSpec" mode="processAtts">
  <xsl:if test="$verbose='true'">
    <xsl:message>    .... class attributes <xsl:value-of select="@ident"/></xsl:message>  
  </xsl:if>
  <xsl:variable name="thisClass">
    <xsl:value-of select="@ident"/>   
  </xsl:variable>
  <xsl:variable name="attclasscontent">
    <rng:x>
      <xsl:for-each select="tei:classes/tei:memberOf">
	<xsl:variable name="any">
	  <xsl:for-each select="key('IDENTS',@key)">
	    <xsl:if test="tei:attList">
	      <xsl:value-of select="@ident"/><xsl:text> </xsl:text>
	    </xsl:if>
	  </xsl:for-each>
	</xsl:variable>
	<xsl:if test="not($any='')">
	  <xsl:if test="$verbose='true'">
	    <xsl:message>          ..... add link to attributes from  class [<xsl:value-of 
	    select="substring-before($any,' ')"/>]</xsl:message>
	  </xsl:if>
	  <rng:ref name="{substring-before($any,' ')}.attributes"/>
	</xsl:if>
      </xsl:for-each>
      <xsl:apply-templates select="tei:attList" mode="tangle" />
    </rng:x>
  </xsl:variable>
  <xsl:call-template name="bitOut">
    <xsl:with-param name="grammar">true</xsl:with-param>
    <xsl:with-param name="content">
      <Wrapper>
	<rng:define name="{$thisClass}.attributes" combine="choice">
	  <xsl:for-each select="exsl:node-set($attclasscontent)/rng:x">
	    <xsl:choose>
	      <xsl:when test="rng:*">
		<xsl:copy-of select="rng:*"/>
	      </xsl:when>
	      <xsl:otherwise>
		<rng:empty/>
	      </xsl:otherwise>
	    </xsl:choose>
	  </xsl:for-each>
	</rng:define>
	<xsl:call-template name="defineRelaxAttributes"/>
      </Wrapper>
    </xsl:with-param>
  </xsl:call-template>
</xsl:template>



<xsl:template match="tei:classSpec" mode="processDefaultAtts">
  <xsl:if test="$verbose='true'">
    <xsl:message>    .. default attribute settings for <xsl:value-of
    select="@ident"/></xsl:message>
  </xsl:if>
  <xsl:call-template name="bitOut">
    <xsl:with-param name="grammar">true</xsl:with-param>
    <xsl:with-param name="content">
      <Wrapper>
	<rng:define name="{@ident}.attributes" combine="choice">
	  <rng:empty/>
	</rng:define>
      </Wrapper>
    </xsl:with-param>
  </xsl:call-template>
</xsl:template>



<xsl:template match="tei:classSpec" mode="processModel">
  <xsl:if test="$verbose='true'">
    <xsl:message>    .... class model <xsl:value-of  select="@ident"/></xsl:message>
  </xsl:if>
  
  <xsl:variable name="thisClass">
    <xsl:value-of select="@ident"/>   
  </xsl:variable>
  <xsl:call-template name="bitOut">
    <xsl:with-param name="grammar">true</xsl:with-param>
    <xsl:with-param name="content">
      <Wrapper>
	<rng:define name="{$thisClass}" combine="choice">
	  <rng:choice><rng:notAllowed/></rng:choice>
	</rng:define>
	<xsl:apply-templates select="tei:classes/tei:memberOf" mode="tangleModel"/>
      </Wrapper>
    </xsl:with-param>
  </xsl:call-template>
</xsl:template>



<xsl:template match="tei:classSpec" mode="tangle">
  <xsl:param name="type"/>
  <xsl:if test="$verbose='true'">
    <xsl:message>      .. process classSpec <xsl:value-of
    select="@ident"/> (type <xsl:value-of select="@type"/>)    </xsl:message>
  </xsl:if>
  <xsl:apply-templates select="." mode="processModel"/>
  <xsl:apply-templates select="." mode="processAtts"/>
</xsl:template>



<xsl:template match="tei:classSpec" mode="tangleadd">
  <xsl:apply-templates mode="tangleadd"/>
</xsl:template>



<xsl:template match="tei:classSpec|tei:elementSpec|tei:macroSpec">     
 <xsl:call-template name="refdoc"/>
</xsl:template>



<xsl:template match="tei:classSpec/@ident"/>



<xsl:template match="tei:classSpec/tei:attList" mode="tangleadd">
  <xsl:for-each select="tei:attDef[@mode='add']">
    <xsl:call-template name="defineAnAttribute"/>
  </xsl:for-each>
</xsl:template>



<xsl:template match="tei:classSpec/tei:content">
<xsl:message>????SCHEMA CONTENT OF CLASS DOC <xsl:value-of select="@id"/></xsl:message>
</xsl:template>



<xsl:template match="tei:classSpec[@mode='change']" mode="tangle">
    <xsl:if test="tei:attList/tei:attDef[@mode='add']">
      <rng:define name="{@ident}.attributes" combine="choice">
	<xsl:apply-templates mode="tangle"/>
      </rng:define>
    </xsl:if>
    <xsl:call-template name="defineRelaxAttributes"/>
</xsl:template>



<xsl:template match="tei:classSpec[@mode='replace']/tei:attList" mode="tangle">
  <rng:define name="{../@ident}.attributes" combine="choice">
    <xsl:choose>
      <xsl:when test="@org='choice'">
	<rng:optional>
	  <rng:choice>
	    <xsl:apply-templates select="tei:*" mode="tangle"/>
	  </rng:choice>
	</rng:optional>
      </xsl:when>
      <xsl:otherwise>
	<xsl:apply-templates select="tei:*" mode="tangle"/>
      </xsl:otherwise>
    </xsl:choose>
  </rng:define>
  <xsl:for-each select="..">
    <xsl:call-template name="defineRelaxAttributes"/>
  </xsl:for-each>
</xsl:template>



<xsl:template match="tei:code">
  <xsl:call-template name="typewriter">
    <xsl:with-param name="text">
      <xsl:apply-templates/>
    </xsl:with-param>
  </xsl:call-template>
</xsl:template>



<xsl:template match="tei:commDecl" mode="tangle">
<xsl:text>
</xsl:text>
<xsl:call-template name="italicize">
<xsl:with-param name="text">
    <xsl:apply-templates/>
</xsl:with-param>
</xsl:call-template>
</xsl:template>



<xsl:template match="tei:desc" mode="show">
 <xsl:apply-templates select="preceding-sibling::tei:gloss"
		      mode="show"/>
 <xsl:apply-templates/>
</xsl:template>



<xsl:template match="tei:desc"/>



<xsl:template match="tei:divGen[@type='classcat']">
  <xsl:apply-templates select="key('CLASSDOCS',1)">
    <xsl:sort select="@ident"/>
  </xsl:apply-templates>
</xsl:template>



<xsl:template match="tei:divGen[@type='patterncat']">
  <xsl:apply-templates select="key('PATTERNDOCS',1)">
    <xsl:sort select="@ident"/>
  </xsl:apply-templates>
</xsl:template>



<xsl:template match="tei:divGen[@type='tagcat']">
  <xsl:apply-templates select="key('TAGDOCS',1)">
    <xsl:sort select="@ident"/>
  </xsl:apply-templates>
</xsl:template>



<xsl:template match="tei:editor">
     <xsl:apply-templates/>:
</xsl:template>



<xsl:template match="tei:elementSpec" mode="tangle">
  <xsl:if test="$verbose='true'">
    <xsl:message> tagdoc <xsl:value-of
    select="@ident"/>
    <xsl:if test="@id">: <xsl:value-of select="@id"/></xsl:if>
    </xsl:message>
  </xsl:if>
  <xsl:call-template name="bitOut">
    <xsl:with-param name="grammar"></xsl:with-param>
    <xsl:with-param name="content">
      <Wrapper>
	<xsl:variable name="thisGi" select="@ident"/>
	<xsl:if test="not(starts-with(@ident,'%'))">
	  <xsl:text>
	  </xsl:text>
	  <xsl:call-template name="defineElement"/>
	</xsl:if>
      </Wrapper>
    </xsl:with-param>
  </xsl:call-template>
</xsl:template>



<xsl:template match="tei:elementSpec" mode="tangleadd"/>



<xsl:template match="tei:elementSpec/@ident"/>



<xsl:template match="tei:elementSpec[@mode='add']" mode="tangleadd">
  <xsl:call-template name="defineElement"/>
</xsl:template>



<xsl:template match="tei:elementSpec[@mode='delete']" mode="tangle">
  <rng:define name="{@ident}"><rng:notAllowed/></rng:define>
</xsl:template>



<xsl:template match="tei:elementSpec[@mode='replace' or
	      @mode='change']" mode="tangle">
  <xsl:call-template name="defineElement"/>
</xsl:template>



<xsl:template match="tei:gloss" mode="show">
<xsl:if test="text()">
  (<xsl:apply-templates/>)
</xsl:if>
</xsl:template>




<xsl:template match="tei:gloss"/>



<xsl:template match="tei:index">
  <xsl:call-template name="makeAnchor">
    <xsl:with-param name="name">IDX-<xsl:number level="any"/></xsl:with-param>
  </xsl:call-template>
</xsl:template>



<xsl:template match="tei:macroSpec" mode="tangle">
  <xsl:param name="msection"/>
  <xsl:param name="filename"/>
  <xsl:choose>
    <xsl:when test="generate-id()=generate-id(key('PATTERNS',@ident)[last()])">
      <xsl:variable name="entCont">
	<BLAH>
	  <xsl:choose>
	    <xsl:when test="not($msection='') and tei:content/rng:group">
	      <rng:choice>
		<xsl:apply-templates select="tei:content/rng:group/rng:*"/>	     
	      </rng:choice>
	    </xsl:when>
	    <xsl:otherwise>
	      <xsl:apply-templates select="tei:content/rng:*"/>
	    </xsl:otherwise>
	  </xsl:choose>
	</BLAH>
      </xsl:variable>
      <xsl:variable name="entCount">
	<xsl:for-each select="exsl:node-set($entCont)/BLAH">
	  <xsl:value-of select="count(rng:*)"/>
	</xsl:for-each>
      </xsl:variable>
      <xsl:choose>
	<xsl:when test='@ident="TEI.singleBase"'/>
	<xsl:when test='starts-with($entCont,"&#39;")'>
<xsl:if test="$verbose='true'">
	  <xsl:message>Omit <xsl:value-of select="$entCont"/> for
  <xsl:value-of select="@ident"/></xsl:message>
</xsl:if>
	</xsl:when>
	<xsl:when test='starts-with($entCont,"-")'>
<xsl:if test="$verbose='true'">
	  <xsl:message>Omit <xsl:value-of select="$entCont"/> for
  <xsl:value-of select="@ident"/></xsl:message>
</xsl:if>
	</xsl:when>
	<xsl:otherwise>
<xsl:if test="$verbose='true'">
	  <xsl:message>         ... so define ..<xsl:value-of
  select="@ident"/></xsl:message>
</xsl:if>
	  <xsl:call-template name="bitOut">
	    <xsl:with-param name="grammar">true</xsl:with-param>
	    <xsl:with-param name="content">
	      <Wrapper>
		<rng:define name="{@ident}">
		  <xsl:if test="starts-with(@ident,'macro.component')">
		    <xsl:attribute name="combine">choice</xsl:attribute>
		  </xsl:if>
		  <xsl:choose>
		    <xsl:when test="starts-with(@ident,'type')"><xsl:copy-of select="exsl:node-set($entCont)/BLAH/rng:*"/></xsl:when>
		    <xsl:when test="$entCount=0"><rng:empty/></xsl:when>
		    <xsl:when test="$entCount=1"><xsl:copy-of select="exsl:node-set($entCont)/BLAH/rng:*"/></xsl:when>
		    <xsl:when test="tei:content/rng:text|tei:content/rng:ref">
		      <rng:choice>
			<xsl:copy-of select="exsl:node-set($entCont)/BLAH/rng:*"/>
		      </rng:choice>
		    </xsl:when>
		    <xsl:otherwise>
		      <xsl:copy-of select="exsl:node-set($entCont)/BLAH/rng:*"/>
		    </xsl:otherwise>
		  </xsl:choose>
		</rng:define>
	      </Wrapper>
	    </xsl:with-param>
	  </xsl:call-template>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:when>
    <xsl:otherwise>
<xsl:if test="$verbose='true'">
      <xsl:message>ZAP pattern <xsl:value-of
  select="@ident"/></xsl:message>
</xsl:if>
    </xsl:otherwise>
  </xsl:choose>
  
</xsl:template>




<xsl:template match="tei:macroSpec/@ident"/>



<xsl:template match="tei:macroSpec/content/rng:*"/>



<xsl:template match="tei:macroSpec|tei:classSpec|tei:elementSpec|tei:specGrp|tei:module"
 mode="findfile">
    <xsl:choose>
      <xsl:when test="parent::tei:specGrp">
	<xsl:for-each select="key('CHUNKS',parent::tei:specGrp/@id)">
	  <xsl:call-template name="specGrpfile"/>
	</xsl:for-each>
      </xsl:when>
      <xsl:when test="parent::tei:module">
	<xsl:value-of select="parent::tei:module/@ident"/>
      </xsl:when>
      <xsl:otherwise>core</xsl:otherwise>
    </xsl:choose>
</xsl:template>



<xsl:template match="tei:memberOf" mode="tangleAtts">
  <xsl:variable name="ident">
    <xsl:value-of select="ancestor::tei:elementSpec/@ident|ancestor::tei:classSpec/@ident"/>
  </xsl:variable>
 <xsl:choose>
  <xsl:when  test="key('IDENTS',@key)">
  <xsl:for-each select="key('IDENTS',@key)[1]">
     <xsl:apply-templates  select="." mode="tagatts"/>
   </xsl:for-each>
  </xsl:when>
  <xsl:otherwise>
    <xsl:variable name="filename">
      <xsl:call-template name="findOddFile">
	<xsl:with-param name="ident"><xsl:value-of select="@key"/></xsl:with-param>
      </xsl:call-template>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="$filename=''">
	<xsl:message>ERROR: CANNOT OPEN A FILE FOR <xsl:value-of select="@key"/></xsl:message>
      </xsl:when>
      <xsl:otherwise>
	<xsl:for-each select="document($filename)">
	  <xsl:apply-templates  select="." mode="tagatts"/>
	</xsl:for-each>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:otherwise>
  </xsl:choose>
</xsl:template>


<xsl:template match="tei:memberOf" mode="tangleModel">
  <xsl:variable name="ident">
    <xsl:value-of select="ancestor::tei:elementSpec/@ident|ancestor::tei:classSpec/@ident"/>
  </xsl:variable>
  <xsl:choose>
  <xsl:when  test="key('IDENTS',@key)">
  <xsl:for-each select="key('IDENTS',@key)[1]">
<xsl:if test="$verbose='true'">
    <xsl:message>      .... added [<xsl:value-of 
select="$ident"/>] to  class [<xsl:value-of select="@ident"/>]</xsl:message>
</xsl:if>
    <rng:define name="{@ident}" combine="choice">
      <rng:ref name="{$ident}"/>
    </rng:define>
  </xsl:for-each>
  </xsl:when>
  <xsl:otherwise>
<xsl:if test="$verbose='true'">
   <xsl:message>      .... added (without lookup) [<xsl:value-of
  select="$ident"/>] to class [<xsl:value-of
  select="@key"/>]</xsl:message>
</xsl:if>
       <rng:define name="{@key}" combine="choice">
             <rng:ref name="{$ident}"/>
       </rng:define>
  </xsl:otherwise>
  </xsl:choose>
</xsl:template>



<xsl:template match="tei:mentioned">
  <xsl:text>&#8216;</xsl:text>
   <xsl:apply-templates/>
  <xsl:text>&#8217;</xsl:text>
</xsl:template>



<xsl:template match="tei:moduleRef" mode="tangle">
  <xsl:param name="filename"/>
  <xsl:variable name="schema">
    <xsl:choose>
      <xsl:when test="$filename='declarefs'"/>
      <xsl:when test="$filename='sharedheader'"/>
      <xsl:when test="$filename='mixed'"/>
      <xsl:when test="$filename='tei' and @key='teikeywords'">
	<xsl:value-of select="@key"/><xsl:text>.rng</xsl:text>
      </xsl:when>
      <xsl:when test="$filename='tei' and @key='teideclarations'">
	<xsl:value-of select="@key"/><xsl:text>.rng</xsl:text>
      </xsl:when>
      <xsl:when test="$filename='tei' and @key='header'">
	<xsl:value-of select="@key"/><xsl:text>.rng</xsl:text>
      </xsl:when>
      <xsl:when test="$filename='tei' and @key='core'">
	<xsl:value-of select="@key"/><xsl:text>.rng</xsl:text>
      </xsl:when>
      <xsl:when test="$filename='general' and @key='structure'">
	<xsl:value-of select="@key"/><xsl:text>.rng</xsl:text>
      </xsl:when>
      <xsl:when test="$filename='structure' and @key='frontmatter'">
	<xsl:value-of select="@key"/><xsl:text>.rng</xsl:text>
      </xsl:when>
      <xsl:when test="$filename='structure' and @key='backmatter'">
	<xsl:value-of select="@key"/><xsl:text>.rng</xsl:text>
      </xsl:when>
      <xsl:when test="@url">
	<xsl:value-of select="@url"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:if test="not(@key='')">
	  <xsl:value-of select="$schemaBaseURL"/>
	  <xsl:value-of select="@key"/>
	  <xsl:text>.rng</xsl:text>
	</xsl:if>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  <xsl:if test="string-length($schema)&gt;0">  
    <xsl:if test="$verbose='true'">
      <xsl:message>moduleRef in  <xsl:value-of 
      select="$filename"/> to <xsl:value-of select="$schema"/></xsl:message>
    </xsl:if>
    <xsl:call-template name="bitOut">
      <xsl:with-param name="grammar">true</xsl:with-param>
      <xsl:with-param name="content">
	<Wrapper>
	  <xsl:choose>
	    <xsl:when test="key('NAMES',@key)">
	      <xsl:for-each select="key('NAMES',@key)">
		<xsl:choose>
		  <xsl:when test="@mode='change'">
		    <rng:include href="{$schema}">
		      <xsl:apply-templates mode="tangle"/>
		    </rng:include>
		    <xsl:apply-templates mode="tangleadd"/>
		  </xsl:when>
		  <xsl:when test="@mode='replace'">
		    <xsl:apply-templates mode="tangle"/>
		  </xsl:when>
		  <xsl:when test="@mode='delete'"/>
		  <xsl:otherwise>
		    <rng:include href="{$schema}"/>
		  </xsl:otherwise>
		</xsl:choose>
	      </xsl:for-each>
	    </xsl:when>
	    <xsl:otherwise>
	      <rng:include href="{$schema}"/>
	    </xsl:otherwise>
	  </xsl:choose>
	</Wrapper>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:if>
</xsl:template>



<xsl:template match="tei:module[@mode='change']" mode="tangle" >
  <xsl:call-template name="bitOut">
    <xsl:with-param name="grammar">true</xsl:with-param>
    <xsl:with-param name="content">
      <Wrapper>
	<rng:include href="{$schemaBaseURL}{@ident}.rng">
	    <xsl:apply-templates mode="tangle"/>
	</rng:include>
      </Wrapper>
    </xsl:with-param>
  </xsl:call-template>
</xsl:template>



<xsl:template match="tei:module[@mode='replace']" mode="tangle" >
  <xsl:call-template name="bitOut">
    <xsl:with-param name="grammar">true</xsl:with-param>
    <xsl:with-param name="content">
      <Wrapper>
	    <xsl:apply-templates mode="tangle"/>
      </Wrapper>
    </xsl:with-param>
  </xsl:call-template>
</xsl:template>



<xsl:template match="tei:msection" mode="tangle">
<xsl:if test="$verbose='true'">
  <xsl:message>     .... msection <xsl:value-of
  select="@keywords"/></xsl:message>
</xsl:if>
&lt;![<xsl:value-of select="@keywords"/><xsl:text>[</xsl:text>
<xsl:apply-templates mode="tangle"/>
  <xsl:text>]]&gt;</xsl:text>
</xsl:template>



<xsl:template match="tei:p/@ident">
      <xsl:call-template name="makeAnchor">
	<xsl:with-param name="name">GDX-<xsl:number level="any"/></xsl:with-param>
      </xsl:call-template>
      <xsl:call-template name="ttembolden">
	<xsl:with-param name="text">
           <xsl:apply-templates/>
	</xsl:with-param>
      </xsl:call-template>
</xsl:template>




<xsl:template match="tei:specGrp" mode="ok">
  <xsl:param name="filename"/>
  <xsl:call-template name="processSchemaFragment">
    <xsl:with-param name="filename" select="$filename"/>
  </xsl:call-template>
</xsl:template>



<xsl:template match="tei:specGrp">
  <xsl:call-template name="makeAnchor">
    <xsl:with-param name="name"><xsl:value-of select="@id"/></xsl:with-param>
  </xsl:call-template>
  <xsl:call-template name="processSchemaFragment"/>
</xsl:template>



<xsl:template match="tei:tag">
  <xsl:call-template name="typewriter">
    <xsl:with-param name="text">
      <xsl:text>&lt;</xsl:text>
      <xsl:apply-templates/>
      <xsl:text>&gt;</xsl:text>
    </xsl:with-param>
  </xsl:call-template>
</xsl:template>




<xsl:template match="tei:title">
 <xsl:choose>
  <xsl:when test="parent::tei:titleStmt">
   <xsl:apply-templates/>
  </xsl:when>
  <xsl:when test="@level='A'">
    &#8216;<xsl:apply-templates/>'
  </xsl:when>
  <xsl:otherwise>
      <xsl:call-template name="italicize">
      <xsl:with-param name="text">
           <xsl:apply-templates/>
         </xsl:with-param>
      </xsl:call-template>
  </xsl:otherwise>
 </xsl:choose>
</xsl:template>



<xsl:template match="teix:*">
  <xsl:text>
</xsl:text>
<xsl:for-each select="ancestor::teix:*">
  <xsl:text> </xsl:text>
</xsl:for-each>
<xsl:text>&lt;</xsl:text>
<xsl:value-of select="name(.)"/>
<xsl:for-each select="@*">
  <xsl:text> </xsl:text>
<xsl:value-of select="name(.)"/>="<xsl:value-of select="."/>"</xsl:for-each>
<xsl:text>&gt;</xsl:text>
<xsl:apply-templates/>
<xsl:if test="node()[last()]/self::teix:*"> 
  <xsl:text>
</xsl:text>
  <xsl:for-each select="ancestor::teix:*">
    <xsl:text> </xsl:text>
  </xsl:for-each>
</xsl:if>
<xsl:text>&lt;/</xsl:text>
<xsl:value-of select="name(.)"/>
<xsl:text>&gt;</xsl:text>
</xsl:template>




<xsl:template match="token" mode="commentline">
  <xsl:call-template name="italicize">
      <xsl:with-param name="text">
        <xsl:value-of select="translate(.,'&#10;','')"/>
      </xsl:with-param>
  </xsl:call-template>
  <xsl:if test="following-sibling::token">
    <xsl:text>
</xsl:text>
  <xsl:choose>
    <xsl:when test="contains(.,'--&gt;')">
         <xsl:apply-templates select="following-sibling::token[1]" 
                 mode="normalline"/>  
    </xsl:when>
    <xsl:otherwise>
 <xsl:apply-templates select="following-sibling::token[1]" mode="commentline"/>
      </xsl:otherwise>
   </xsl:choose>
 </xsl:if>
</xsl:template>



<xsl:template match="token" mode="normalline">
<xsl:choose>
  <xsl:when test="contains(.,'&lt;!--')">
    <xsl:call-template name="italicize">
      <xsl:with-param name="text">
      <xsl:value-of select="translate(.,'&#10;','')"/>
      </xsl:with-param>
    </xsl:call-template>
  <xsl:if test="following-sibling::token">
    <xsl:text>
</xsl:text>
  <xsl:choose>
    <xsl:when test="contains(.,'--&gt;')">
         <xsl:apply-templates select="following-sibling::token[1]" 
                 mode="normalline"/>  
    </xsl:when>
    <xsl:otherwise>
 <xsl:apply-templates select="following-sibling::token[1]" mode="commentline"/>
      </xsl:otherwise>
   </xsl:choose>
 </xsl:if>
</xsl:when>
<xsl:otherwise>
  <xsl:call-template name="breakline"/>  
  <xsl:if test="following-sibling::token">
    <xsl:text>
</xsl:text>
    <xsl:apply-templates select="following-sibling::token[1]" 
      mode="normalline"/>
  </xsl:if>
</xsl:otherwise>
</xsl:choose>
</xsl:template>



<xsl:template match="token" mode="verbatimline">
<xsl:call-template name="breakline"/>  
<xsl:if test="following-sibling::token">
  <xsl:text>
</xsl:text>
    <xsl:apply-templates select="following-sibling::token[1]" 
      mode="verbatimline"/>
  </xsl:if>
</xsl:template>



<xsl:template match="token" mode="word">
  <xsl:param name="len"/>
    <xsl:choose>
      <xsl:when test="$len +string-length(.) &gt; $wrapLength">
        <xsl:text>
</xsl:text>
       <xsl:value-of select="."/><xsl:text> </xsl:text>
       <xsl:if test="following-sibling::token">
         <xsl:apply-templates select="following-sibling::token[1]" mode="word">
           <xsl:with-param name="len" select="8"/>
         </xsl:apply-templates>
       </xsl:if>
      </xsl:when>
      <xsl:otherwise>
       <xsl:value-of select="."/><xsl:text> </xsl:text>
       <xsl:if test="following-sibling::token">
         <xsl:apply-templates select="following-sibling::token[1]" mode="word">
           <xsl:with-param name="len" select="$len + string-length(.)"/>
         </xsl:apply-templates>
       </xsl:if>
      </xsl:otherwise>
    </xsl:choose>
</xsl:template>




<xsl:template name="attributeBody">		
  <xsl:variable name="name">
    <xsl:choose>
      <xsl:when test="tei:altIdent">
	<xsl:value-of select="tei:altIdent"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:value-of select="@ident"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  <rng:attribute name="{$name}">
    <xsl:if test="tei:defaultVal">
      <xsl:attribute name="a:defaultValue">
	<xsl:value-of select="normalize-space(tei:defaultVal)"/>
      </xsl:attribute>
    </xsl:if>
    <rng:ref name="{ancestor::tei:attList/../@ident}.attributes.{@ident}.content"/>
  </rng:attribute>
</xsl:template>



<xsl:template name="breakline">
 <xsl:choose>
  <xsl:when test="string-length(.)&lt;$wrapLength">
      <xsl:value-of select="."/>
  </xsl:when>
  <xsl:otherwise>
    <xsl:variable name="words" select="estr:tokenize(.)"/>
    <xsl:apply-templates select="$words[1]" mode="word">
      <xsl:with-param name="len" select="0"/>
    </xsl:apply-templates>
  </xsl:otherwise>
 </xsl:choose>
</xsl:template>



<xsl:template name="copyright">
  <xsl:comment>
 Text Encoding Initiative Consortium:
 Guidelines for Electronic Text Encoding and Interchange.
 
 TEI P5
 
 Copyright (c) 2005 TEI Consortium. Permission to copy in any form
 is granted, provided this notice is included in all copies.
 These materials may not be altered; modifications to these schemata should
 be performed only as specified by the Guidelines, for example in the
 chapter entitled 'Modifications of the TEI'
 These materials are subject to revision by the TEI Consortium. Current versions
 are available from the Consortium website at http://www.tei-c.org
 
 TEI P5 Relax NG Schema generated 
 <xsl:value-of select="edate:date-time()"/>
 by teiodds.xsl
 
 </xsl:comment>
</xsl:template>
<xsl:template name="defineAnAttribute">

  <rng:define name="{ancestor::tei:attList/../@ident}.attributes.{@ident}">
    <xsl:choose>
      <xsl:when test="@mode='delete'">
	<rng:notAllowed/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:choose>
	  <xsl:when test="@usage='req'">
	    <xsl:call-template name="attributeBody"/>
	  </xsl:when>
	  <xsl:when test="parent::tei:attList[@org='choice']">
	    <xsl:call-template name="attributeBody"/>
	  </xsl:when>
	  <xsl:otherwise>
	    <rng:optional>
	      <xsl:call-template name="attributeBody"/>
	    </rng:optional>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:otherwise>
    </xsl:choose>	
  </rng:define>

  <xsl:choose>
    <xsl:when test="@mode='delete'"/>
    <xsl:when test="@mode='change' and not(tei:datatype or tei:valList)"/>
    <xsl:otherwise>
      <rng:define name="{ancestor::tei:attList/../@ident}.attributes.{@ident}.content">
	<xsl:choose>
	  <xsl:when test="tei:datatype[rng:ref/@name='datatype.Code']">
	    <xsl:choose>
	      <xsl:when test="tei:valList[@type='closed']">
		<rng:choice>
		  <xsl:for-each select="tei:valList/tei:valItem">
		    <rng:value><xsl:value-of select="@ident"/></rng:value>
		  </xsl:for-each>
		</rng:choice>
	      </xsl:when>
	      <xsl:otherwise>
		<rng:text/>
	      </xsl:otherwise>
	    </xsl:choose>
	  </xsl:when>
	  <xsl:when test="tei:datatype/rng:*">
	    <xsl:copy-of select="tei:datatype/rng:*"/>
	  </xsl:when>
	  <xsl:when test="tei:valList[@type='closed']">
	    <rng:choice>
	      <xsl:for-each select="tei:valList/tei:valItem">
		<rng:value><xsl:value-of select="@ident"/></rng:value>
	      </xsl:for-each>
	    </rng:choice>
	  </xsl:when>
	  <xsl:otherwise>
	    <rng:text/>
	  </xsl:otherwise>
	</xsl:choose>
      </rng:define>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>



<xsl:template name="defineElement">
  <xsl:variable name="name">
    <xsl:choose>
      <xsl:when test="tei:altIdent">
	<xsl:value-of select="tei:altIdent"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:value-of select="@ident"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  <xsl:choose>
    <xsl:when test="tei:content/rng:notAllowed">
      <rng:define name="{$thisGi}"><rng:notAllowed/></rng:define>
    </xsl:when>
    <xsl:otherwise>
      <rng:define  name="{@ident}">
	<rng:element  name="{$name}">
	  <xsl:if test="@ns">
	    <xsl:attribute name="ns"><xsl:value-of select="@ns"/></xsl:attribute>
	  </xsl:if>
	  <rng:ref name="{@ident}.content"/>
	</rng:element>
      </rng:define>
      <xsl:if test="tei:content">
	<rng:define name="{@ident}.content">
	  <xsl:apply-templates select="tei:content/rng:*"/>
	  <xsl:if test="not(@ns)">
	    <rng:ref name="tei.global.attributes"/>
	  </xsl:if>
	  <xsl:apply-templates select="tei:classes/tei:memberOf" mode="tangleAtts"/>
	  <xsl:apply-templates select="tei:attList" mode="tangle"/>
	  <rng:ref name="{@ident}.newattributes"/>
	  <xsl:if test="not(starts-with(@ident,'%'))">
	    <rng:optional>
	      <rng:attribute name="TEIform" a:defaultValue="{@ident}">
		<rng:text/>
	      </rng:attribute>
	    </rng:optional>
	  </xsl:if>
	</rng:define>
      </xsl:if>
      <xsl:choose>
	<xsl:when test="tei:attList/tei:attDef[@mode='add']">
	  <rng:define name="{@ident}.newattributes" combine="FREDDY">
	    <xsl:apply-templates select="tei:attList" mode="tangle"/>
	  </rng:define>
	</xsl:when>
	<xsl:otherwise>
	  <rng:define name="{@ident}.newattributes" combine="choice">
	    <rng:empty/>
	  </rng:define> 
          <xsl:apply-templates select="tei:classes/tei:memberOf" mode="tangleModel"/>
	</xsl:otherwise>
      </xsl:choose>

      <xsl:call-template name="defineRelaxAttributes"/>

    </xsl:otherwise>
  </xsl:choose>      
</xsl:template>




<xsl:template name="defineRelaxAttributes">
  <xsl:for-each select="tei:attList//tei:attDef">
    <xsl:if test="not(@ident='xmlns')">
      <xsl:call-template name="defineAnAttribute"/>
    </xsl:if>
  </xsl:for-each>
</xsl:template>



<xsl:template name="findOddFile">
  <xsl:param name="ident"/>
  <xsl:variable name="result">
    <xsl:for-each
     select="document($TEITAGS)/Table">
      <xsl:value-of select="key('TAGIDENTS',$ident)/@filelocation"/>
    </xsl:for-each>
  </xsl:variable>
<xsl:if test="$verbose='true'">
  <xsl:message>FILE <xsl:value-of select="$ident"/>: <xsl:value-of select="$result"/>
  </xsl:message>
</xsl:if>
  <xsl:if test="not($result='')">
    <xsl:value-of select="$ODDROOT"/>
    <xsl:text>Source/</xsl:text>
    <xsl:value-of select="$result"/>
  </xsl:if>
</xsl:template>



<xsl:template name="generateChildren">
<xsl:param name="what"/>
   <xsl:for-each select="document('dtdcat.xml')">
    <xsl:for-each select="key('ELEMENTS',$what)/Contains">
      <xsl:if test="not(. = preceding-sibling::Contains)">        
        <xsl:value-of select="."/>
        <xsl:text> </xsl:text>
      </xsl:if>
      </xsl:for-each>      
    </xsl:for-each>
</xsl:template>



<xsl:template name="generateClassParents">
 <xsl:choose>
   <xsl:when test="not(tei:classes)">(none)   </xsl:when>
 <xsl:otherwise>
  <xsl:for-each select="tei:classes/tei:memberOf">
      <xsl:choose>
	<xsl:when test="key('IDENTS',@key)">
	<xsl:variable name="Key">
		<xsl:value-of select="@key"/>
		</xsl:variable>
	<xsl:for-each select="key('IDENTS',@key)">
	  <xsl:if test="not(generate-id(.)=generate-id(key('IDENTS',$Key)[1]))">
	  <xsl:text> |  </xsl:text>
	  </xsl:if>
	  <xsl:call-template name="makeLink">
	    <xsl:with-param
	    name="class">classlink</xsl:with-param>
	    <xsl:with-param name="url">ref-<xsl:value-of select="@id"/>.html</xsl:with-param>
	    <xsl:with-param name="text">
	    <xsl:value-of select="@ident"/>
	    </xsl:with-param>
	  </xsl:call-template>
	</xsl:for-each>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:value-of select="@key"/>
	</xsl:otherwise>
      </xsl:choose>
      <xsl:if test="following-sibling::tei:memberOf"><xsl:text>, </xsl:text></xsl:if>
  </xsl:for-each>
</xsl:otherwise>
</xsl:choose>
</xsl:template>




<xsl:template name="generateMembers">
   <xsl:variable name="this" select="@ident"/>
   <xsl:for-each select="key('CLASSMEMBERS',$this)">
     <xsl:call-template name="linkTogether">
       <xsl:with-param name="inner" select="@ident"/>
       <xsl:with-param name="origid" select="@id"/>
     </xsl:call-template>
     <xsl:if test="count(key('CLASSMEMBERS',@ident))&gt;0">
       [<xsl:for-each select="key('CLASSMEMBERS',@ident)">
       <xsl:choose>
	 <xsl:when test="$oddmode='html'">
	   <a href="ref-{@id}.html">
	     <xsl:value-of select="@ident"/>
	   </a>
	 </xsl:when>
	 <xsl:when test="$oddmode='pdf'">
	   <fo:inline font-style="italic"><xsl:value-of select="@id"/></fo:inline>
	 </xsl:when>
	 <xsl:when test="$oddmode='tei'">
	   <emph><xsl:value-of select="@id"/></emph>
	 </xsl:when>
	 <xsl:otherwise>
	   <xsl:value-of select="@id"/>
	 </xsl:otherwise>
       </xsl:choose>
       <xsl:text> </xsl:text>
       </xsl:for-each>]
     </xsl:if>
  <xsl:text> </xsl:text>
</xsl:for-each>
</xsl:template>



<xsl:template name="generateParents">
 <xsl:param name="what"/>
 <xsl:variable name="mums">
   <mums>
     <xsl:for-each select="document('dtdcat.xml')">
      <xsl:for-each select="key('ELEMENTPARENTS',$what)">
        <mum><xsl:value-of select="../@id"/></mum>
      </xsl:for-each>
  </xsl:for-each>
  </mums>
 </xsl:variable>
 <xsl:variable name="mums2">
    <mums>
    <xsl:for-each select="exsl:node-set($mums)/mums/mum">
      <xsl:sort select="."/>
              <mum><xsl:value-of select="."/></mum>
    </xsl:for-each>
    </mums>
 </xsl:variable>
    <xsl:for-each select="exsl:node-set($mums2)/mums/mum">
      <xsl:if test="not(. = preceding-sibling::mum)">        
        <xsl:value-of select="."/><xsl:text> </xsl:text>
      </xsl:if>
    </xsl:for-each>
</xsl:template>



<xsl:template name="generateTitle">
  <xsl:apply-templates
    select="ancestor-or-self::tei:TEI/tei:text/tei:front//tei:docTitle/text()"/>
</xsl:template>



<xsl:template name="linkStyle"/>



<xsl:template name="linkTogether">
<xsl:param name="inner"/>
<xsl:param name="origid"/>
  <xsl:choose>
    <xsl:when test="$oddmode='html'">
      <a href="ref-{$origid}.html"><xsl:value-of select="$inner"/></a>
    </xsl:when>
    <xsl:when test="$oddmode='pdf'">
     <fo:inline><xsl:value-of select="$inner"/></fo:inline>
    </xsl:when>
    <xsl:when test="$oddmode='tei'">
      <xsl:value-of select="$inner"/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:value-of select="$inner"/>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>



<xsl:template name="makeTagsetInfo">
   <xsl:variable name="File">
    <xsl:choose>
     <xsl:when test="parent::tei:elementSpec">
       <xsl:apply-templates select=".." mode="findfile"/>
     </xsl:when>
    <xsl:when test="parent::tei:macroSpec">
     <xsl:apply-templates select=".." mode="findfile"/>
  </xsl:when>
  <xsl:when test="parent::tei:classSpec">
     <xsl:apply-templates select=".." mode="findfile"/>
  </xsl:when>
  <xsl:when test="ancestor::tei:module/@ident">
    <xsl:value-of select="ancestor::tei:module/@ident"/>
  </xsl:when>
  <xsl:otherwise>
    <xsl:apply-templates select=".." mode="findfile"/>
 </xsl:otherwise>
</xsl:choose>
</xsl:variable>
<xsl:if test="$verbose='true'">
<xsl:message>  tagset <xsl:value-of select="../@id"/>: <xsl:value-of select="$File"/></xsl:message>
</xsl:if>
<xsl:value-of
 select="key('PUBLICIDS',$File)/parent::tei:macroSpec/tei:gloss"/>
  [<xsl:value-of select="$File"/>]
</xsl:template>



<xsl:template name="processSchemaFragment">
  <xsl:param name="filename"/>
  <xsl:variable name="secnum">
    <xsl:call-template name="sectionNumber"/>
  </xsl:variable>
  <xsl:if test="@id">
    <xsl:comment>[<xsl:value-of select="@id"/>] <xsl:value-of
    select="$secnum"/>
    <xsl:if test="@n">
      <xsl:text>: </xsl:text>
      <xsl:value-of select="@n"/>
    </xsl:if>
    </xsl:comment>
  </xsl:if>
  <xsl:message>
    <xsl:text>    [specGrp</xsl:text>
    <xsl:if test="@id">
      <xsl:text> </xsl:text>
      <xsl:value-of select="@id"/> 
    </xsl:if>
    <xsl:text> in </xsl:text>
    <xsl:value-of select="$filename"/> 
  </xsl:message>
  <xsl:apply-templates mode="tangle">
    <xsl:with-param name="filename" select="$filename"/>
  </xsl:apply-templates>
  <xsl:message>    ] end of <xsl:value-of select="@id"/></xsl:message>
  <xsl:if test="@id">
    <xsl:comment> end of [<xsl:value-of select="@id"/>]  <xsl:value-of select="$secnum"/>    
    </xsl:comment>
  </xsl:if>
</xsl:template>




<xsl:template name="processSpecDesc">
  <xsl:variable name="name">
    <xsl:value-of select="@key"/>
  </xsl:variable>
  <xsl:variable name="atts">
    <xsl:value-of select="normalize-space(@atts)"/>
  </xsl:variable>
  <xsl:choose>
    <xsl:when test="$name=''">
        <xsl:message>ERROR: no key attribute on specDesc</xsl:message>
     </xsl:when>
    <xsl:when test="key('IDENTS',$name)">
      <xsl:apply-templates select="key('IDENTS',$name)" mode="show">
	<xsl:with-param name="atts" select="$atts"/>
      </xsl:apply-templates>
    </xsl:when>
    <xsl:otherwise>
      <xsl:variable name="filename">
	<xsl:call-template name="findOddFile">
	  <xsl:with-param name="ident"><xsl:value-of select="$name"/></xsl:with-param>
	</xsl:call-template>
      </xsl:variable>
      <xsl:choose>
	<xsl:when test="$filename=''">
	  <xsl:message>ERROR: CANNOT OPEN A FILE FOR <xsl:value-of select="$name"/></xsl:message>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:apply-templates select="document($filename)/*" mode="show">
	    <xsl:with-param name="atts" select="$atts"/>
	  </xsl:apply-templates>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>



<xsl:template name="processatts">
 <xsl:param name="values"/>
 <xsl:if test="not($values = '')">
   <xsl:apply-templates 
      select="key('IDS',substring-before($values,' '))"/>
   <xsl:call-template name="processatts">
   <xsl:with-param name="values" select="substring-after($values,' ')"/>
  </xsl:call-template>
 </xsl:if>
</xsl:template>




<xsl:template name="sectionNumber">
  <xsl:for-each select="(ancestor::tei:div1|ancestor::tei:div2|ancestor::tei:div3|ancestor::tei:div4)[last()]">
    <xsl:for-each select="ancestor-or-self::tei:div1">
     <xsl:number from="tei:body" level="any" /><xsl:text>.</xsl:text>
    </xsl:for-each>
    <xsl:number level="multiple" count="tei:div2|tei:div3|tei:div4" from="tei:div1"/>
  </xsl:for-each>
</xsl:template>



<xsl:template name="specGrpfile">
  <xsl:choose>
    <xsl:when test="parent::tei:module">
      <xsl:value-of select="parent::tei:module/@ident"/>
    </xsl:when>
    <xsl:otherwise>
	<xsl:for-each select="key('CHUNKS',parent::tei:specGrp/@id)[1]">
	  <xsl:call-template name="specGrpfile"/>
	</xsl:for-each>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>




</xsl:stylesheet>
