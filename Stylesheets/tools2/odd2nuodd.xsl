<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet version="2.0" 
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
	xpath-default-namespace="http://www.tei-c.org/ns/1.0"
	xmlns="http://www.tei-c.org/ns/1.0"
>

<!--

 $Id$
 Sebastian Rahtz 2011/03/26

Read an ODD with <elementSpec mode="delete"> statements and rewrite it
as <moduleRef include="* * *">

-->
  <xsl:key name="IDS" match="*" use="@xml:id"/>

  <xsl:key name="EbyM" match="elementSpec" use="@module"/>

  <xsl:key name="deletedE" match="elementSpec[@mode='delete']" use="@ident"/>

  <xsl:param name="P5">/usr/share/xml/tei/odd/p5subset.xml</xsl:param>
  <xsl:param name="method">include</xsl:param>

  <xsl:variable name="orig" select="/"/>

<!-- identifty transforms -->
  <xsl:template match="@*|text()|comment()|processing-instruction()">
    <xsl:copy-of select="."/>
  </xsl:template>

  <xsl:template match="@*|text()|comment()|processing-instruction()" mode="stage2">
    <xsl:copy-of select="."/>
  </xsl:template>

  <xsl:template match="*">
    <xsl:copy>
      <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="*" mode="stage2">
    <xsl:copy>
      <xsl:apply-templates
	  select="*|@*|processing-instruction()|comment()|text()" mode="stage2"/>
    </xsl:copy>
  </xsl:template>
  

<!-- work in two phases, so we can zap empty specGrp on pass 2-->
  <xsl:template match="/">
    <xsl:variable name="stage1">
      <xsl:apply-templates/>
    </xsl:variable>
    <xsl:for-each select="$stage1">
      <xsl:apply-templates mode="stage2"/>
    </xsl:for-each>
  </xsl:template>
  
<!-- ignore elementSpec @mode='delete' -->
  <xsl:template match="elementSpec[@mode='delete']"/>
  
  <!-- for any moduleRef, look up all the members of it in P5;
       if they are not deleted by this odd, add them to a list to be
       included -->
  <xsl:template match="moduleRef/@include"/>
  <xsl:template match="moduleRef/@except"/>

  <xsl:template match="moduleRef[@key]">
    <xsl:variable name="here" select="."/>
    <xsl:copy>
      <xsl:apply-templates select="@*"/>
      <xsl:variable name="module" select="@key"/>
      <xsl:choose>
	<xsl:when test="$method='include' and @include">
	  <xsl:copy-of select="@include"/>
	</xsl:when>	
	<xsl:when test="$method='except' and @except">
	  <xsl:copy-of select="@except"/>
	</xsl:when>	
	<xsl:when test="$method='include' and @except">
	  <xsl:variable name="includelist">
	    <xsl:for-each select="document($P5)">
	      <xsl:for-each select="key('EbyM',$module)">
		<xsl:sort select="@ident"/>
		<xsl:variable name="e" select="@ident"/>
		  <xsl:for-each select="tokenize($here/@except,' ')">
		  <xsl:if test="not(.=$e)">
		    <xsl:value-of select="$e"/>
		    <xsl:text> </xsl:text>
		  </xsl:if>
		</xsl:for-each>
	      </xsl:for-each>
	    </xsl:for-each>
	  </xsl:variable>
	  <xsl:if test="not($includelist='')">
	    <xsl:attribute name="include" select="normalize-space($includelist)"/>
	  </xsl:if>
	</xsl:when>
	<xsl:when test="$method='include'">
	  <xsl:variable name="includelist">
	    <xsl:for-each select="document($P5)">
	      <xsl:for-each select="key('EbyM',$module)">
		<xsl:sort select="@ident"/>
		<xsl:variable name="e" select="@ident"/>
		<xsl:for-each select="$orig">
		  <xsl:if test="not(key('deletedE',$e))">
		    <xsl:value-of select="$e"/>
		    <xsl:text> </xsl:text>
		  </xsl:if>
		</xsl:for-each>
	      </xsl:for-each>
	    </xsl:for-each>
	  </xsl:variable>
	  <xsl:if test="not($includelist='')">
	    <xsl:attribute name="include"
			   select="normalize-space($includelist)"/>
	  </xsl:if>
	</xsl:when>
	<xsl:when test="$method='except' and @include">
	  <xsl:variable name="exceptlist">
	    <xsl:for-each select="document($P5)">
	      <xsl:for-each select="key('EbyM',$module)">
		<xsl:sort select="@ident"/>
		<xsl:variable name="e" select="@ident"/>
		<xsl:for-each select="$orig">
		  <xsl:if test="not(key('deletedE',$e))">
		    <xsl:value-of select="$e"/>
		    <xsl:text> </xsl:text>
		  </xsl:if>
		</xsl:for-each>
	      </xsl:for-each>
	    </xsl:for-each>
	  </xsl:variable>
	  <xsl:if test="not($exceptlist='')">
	    <xsl:attribute name="except"  select="normalize-space($exceptlist)"/>
	  </xsl:if>
	</xsl:when>
	<xsl:when test="$method='except'">
	  <xsl:variable name="exceptlist">
	    <xsl:for-each select="document($P5)">
	      <xsl:for-each select="key('EbyM',$module)">
		<xsl:sort select="@ident"/>
		<xsl:variable name="e" select="@ident"/>
		<xsl:for-each select="$orig">
		  <xsl:if test="key('deletedE',$e)">
		    <xsl:value-of select="$e"/>
		    <xsl:text> </xsl:text>
		  </xsl:if>
		</xsl:for-each>
	      </xsl:for-each>
	    </xsl:for-each>
	  </xsl:variable>
	  <xsl:if test="not($exceptlist='')">
	    <xsl:attribute name="except" select="normalize-space($exceptlist)"/>
	  </xsl:if>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:message terminate="yes">Method <xsl:value-of
	  select="$method"/> not supported</xsl:message>
	</xsl:otherwise>
      </xsl:choose>
	</xsl:copy>
      </xsl:template>

  <xsl:template mode="stage2" match="specGrp[not(*)]"/>

  <xsl:template mode="stage2" match="specGrpRef">
    <xsl:choose>
      <xsl:when test="starts-with(@target,'#')">
	<xsl:for-each
	    select="key('IDS',substring-after(@target,'#'))">
	  <xsl:if test="*">
	    <specGrpRef target="#{@xml:id}"/>
	  </xsl:if>
	</xsl:for-each>
      </xsl:when>
      <xsl:otherwise>
	<xsl:copy-of select="."/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
</xsl:stylesheet>
