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
      as <moduleRef include="* * *" or except="* * *">
      
  -->
  <!-- where is a copy of P5? -->
  <xsl:param name="defaultSource">http://www.tei-c.org/Vault/P5/current/xml/tei/odd/p5subset.xml</xsl:param>
  
  <!-- do you want moduleRef generated with @include or @except? -->
  <xsl:param name="method">include</xsl:param>



  <xsl:key name="EbyM" match="elementSpec" use="@module"/>

  <xsl:key name="deletedE" match="elementSpec[@mode='delete']" use="@ident"/>
  <xsl:key name="changedE" match="elementSpec[@mode='change']" use="@ident"/>
  <xsl:key name="changedE" match="elementSpec[@mode='replace']" use="@ident"/>

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
  
  <xsl:template match="moduleRef/@include"/>
  <xsl:template match="moduleRef/@except"/>

  <!-- for any moduleRef, look up all the members of it in P5;
       if they are not deleted by this odd, add them to a list to be
       included -->
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
	  <xsl:variable name="not">
	    <xsl:for-each select="tokenize($here/@except,' ')">
	      <not ident="{.}"/>
	    </xsl:for-each>
	  </xsl:variable>
	  <xsl:variable name="includelist">
	    <xsl:for-each select="document($defaultSource)">
	      <xsl:for-each select="key('EbyM',$module)">
		<xsl:sort select="@ident"/>
		<xsl:variable name="e" select="@ident"/>
		<xsl:if test="not($not/not[@ident=$e])">
		  <xsl:value-of select="$e"/>
		    <xsl:text> </xsl:text>
		</xsl:if>
	      </xsl:for-each>
	    </xsl:for-each>
	  </xsl:variable>
	  <xsl:if test="not($includelist='')">
	    <xsl:attribute name="include" select="normalize-space($includelist)"/>
	  </xsl:if>
	</xsl:when>
	<xsl:when test="$method='include'">
	  <xsl:variable name="includelist">
	    <xsl:for-each select="document($defaultSource)">
	      <xsl:for-each select="key('EbyM',$module)">
		<xsl:sort select="@ident"/>
		<xsl:variable name="e" select="@ident"/>
		<xsl:for-each select="$orig">
		  <xsl:choose>
		  <xsl:when test="key('deletedE',$e)"/>
		  <xsl:otherwise>
		    <xsl:value-of select="$e"/>
		    <xsl:text> </xsl:text>
		  </xsl:otherwise>
		  </xsl:choose>
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
	  <xsl:variable name="yes">
	    <xsl:for-each select="tokenize($here/@include,' ')">
	      <yes ident="{.}"/>
	    </xsl:for-each>
	  </xsl:variable>
	  <xsl:variable name="exceptlist">
	    <xsl:for-each select="document($defaultSource)">
	      <xsl:for-each select="key('EbyM',$module)">
		<xsl:sort select="@ident"/>
		<xsl:variable name="e" select="@ident"/>
		<xsl:for-each select="$orig">
		  <xsl:if test="not($yes/yes[@ident=$e])">
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
	    <xsl:for-each select="document($defaultSource)">
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
	    select="id(substring(@target,2))">
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
