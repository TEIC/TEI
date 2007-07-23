<?xml version="1.0"?>
<xsl:stylesheet 
    xmlns:edate="http://exslt.org/dates-and-times"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:tei="http://www.tei-c.org/ns/1.0" 
    exclude-result-prefixes="tei edate" version="1.0">
  <!-- 
       
       P4 to P5 converter 
       
       Sebastian Rahtz <sebastian.rahtz@oucs.ox.ac.uk>
       
       $Date: 2007/07/20 $  $Id: //TEI/web/P5/p4top5.xsl#14 $
       
       Copyright 2007 TEI Consortium
       
       Permission is hereby granted, free of charge, to any person obtaining
       a copy of this software and any associated documentation gfiles (the
       ``Software''), to deal in the Software without restriction, including
       without limitation the rights to use, copy, modify, merge, publish,
       distribute, sublicense, and/or sell copies of the Software, and to
       permit persons to whom the Software is furnished to do so, subject to
       the following conditions:
       
       The above copyright notice and this permission notice shall be included
       in all copies or substantial portions of the Software.
       
  -->
  <xsl:output method="xml" encoding="utf-8"
	      indent="yes" cdata-section-elements="tei:eg" omit-xml-declaration="yes"/>
  
  <xsl:variable name="processor">
    <xsl:value-of select="system-property('xsl:vendor')"/>
  </xsl:variable>
  
  <xsl:variable name="today">
    <xsl:choose>
      <xsl:when test="function-available('edate:date-time')">
	<xsl:value-of select="edate:date-time()"/>
      </xsl:when>
      <xsl:when test="contains($processor,'SAXON')">
	<xsl:value-of select="Date:toString(Date:new())"
		      xmlns:Date="/java.util.Date"/>
      </xsl:when>
      <xsl:otherwise>0000-00-00</xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  
  <xsl:variable name="uc">ABCDEFGHIJKLMNOPQRSTUVWXYZ</xsl:variable>
  <xsl:variable name="lc">abcdefghijklmnopqrstuvwxyz</xsl:variable>
  
  <xsl:template match="*">
    <xsl:choose>
      <xsl:when test="namespace-uri()=''">
	<xsl:element namespace="http://www.tei-c.org/ns/1.0" name="{local-name(.)}">
	  <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
	</xsl:element>
      </xsl:when>
      <xsl:otherwise>
	<xsl:copy>
	  <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
	</xsl:copy>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
  
  <xsl:template match="@*|processing-instruction()|comment()">
    <xsl:copy/>
  </xsl:template>
  
  
  <xsl:template match="text()">
    <xsl:value-of select="."/>
  </xsl:template>
  
  
  <!-- change of name, or replaced by another element -->
  <xsl:template match="teiCorpus.2">
    <teiCorpus xmlns="http://www.tei-c.org/ns/1.0">
      <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
    </teiCorpus>
  </xsl:template>
  
  
  <xsl:template match="TEI.2">
    <TEI xmlns="http://www.tei-c.org/ns/1.0">
      <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
    </TEI>
  </xsl:template>
  
  <xsl:template match="xref">
    <xsl:element namespace="http://www.tei-c.org/ns/1.0" name="ref">
      <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
    </xsl:element>
  </xsl:template>
  
  
  <xsl:template match="xptr">
    <xsl:element namespace="http://www.tei-c.org/ns/1.0" name="ptr">
      <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
    </xsl:element>
  </xsl:template>
  
  
  <xsl:template match="figure[@url]">
    <figure xmlns="http://www.tei-c.org/ns/1.0">
      <graphic xmlns="http://www.tei-c.org/ns/1.0">
	<xsl:copy-of select="@*"/>
      </graphic>
      <xsl:apply-templates/>
    </figure>
  </xsl:template>
  
  
  <xsl:template match="figure/@url"/>
  
  <xsl:template match="figure/@doc"/>
  
  <xsl:template match="figure[@doc]">
    <figure xmlns="http://www.tei-c.org/ns/1.0">
      <graphic xmlns="http://www.tei-c.org/ns/1.0" url="{unparsed-entity-uri(@doc)}">
	<xsl:apply-templates select="@*"/>
      </graphic>
      <xsl:apply-templates/>
    </figure>
  </xsl:template>
  
  <xsl:template match="event">
    <incident  xmlns="http://www.tei-c.org/ns/1.0">
      <xsl:apply-templates select="@*|*|text()|comment()|processing-instruction()"/>
    </incident>
  </xsl:template>
  
  <xsl:template match="state">
    <refState  xmlns="http://www.tei-c.org/ns/1.0">
      <xsl:apply-templates select="@*|*|text()|comment()|processing-instruction()"/>
    </refState>
  </xsl:template>
  
  
  <!-- lost elements -->
  <xsl:template match="dateRange">
    <date xmlns="http://www.tei-c.org/ns/1.0">
      <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
    </date>
  </xsl:template>
  
  
  <xsl:template match="dateRange/@from">
    <xsl:copy-of select="."/>
  </xsl:template>
  
  <xsl:template match="dateRange/@to">
    <xsl:copy-of select="."/>
  </xsl:template>
  
  <xsl:template match="langUsage"/>
  
  <!-- attributes lost -->
  <!-- dropped from TEI. Added as new change records later -->
  <xsl:template match="@date.created"/>
  
  <xsl:template match="@date.updated"/>
  
  <!-- dropped from TEI. No replacement -->
  <xsl:template match="refsDecl/@doctype"/>
  
  <!-- attributes changed name -->
  
  <xsl:template match="date/@value">
    <xsl:attribute name="when">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  
  
  <xsl:template match="@url">
    <xsl:attribute name="target">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  
  
  <xsl:template match="@doc">
    <xsl:attribute name="target">
      <xsl:value-of select="unparsed-entity-uri(.)"/>
    </xsl:attribute>
  </xsl:template>
  
  
  <xsl:template match="@id">
    <xsl:choose>
      <xsl:when test="parent::lang">
	<xsl:attribute name="ident">
	  <xsl:value-of select="."/>
	</xsl:attribute>
      </xsl:when>
      <xsl:otherwise>
	<xsl:attribute name="xml:id">
	  <xsl:value-of select="."/>
	</xsl:attribute>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
  
  <xsl:template match="@lang">
    <xsl:attribute name="xml:lang">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  
  
  <xsl:template match="change/@date"/>
  
  <xsl:template match="date/@certainty">
    <xsl:attribute name="cert">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  
  <!-- all pointing attributes preceded by # -->
  
  <xsl:template match="@ana|@active|@adj|@adjFrom|@adjTo|@children|@children|@class|@code|@code|@copyOf|@corresp|@decls|@domains|@end|@exclude|@fVal|@feats|@follow|@from|@hand|@inst|@langKey|@location|@mergedin|@new|@next|@old|@origin|@otherLangs|@parent|@passive|@perf|@prev|@render|@resp|@sameAs|@scheme|@script|@select|@since|@start|@synch|@target|@targetEnd|@to|@to|@value|@value|@who">
    <xsl:attribute name="{name(.)}">
      <xsl:call-template name="splitter">
	<xsl:with-param name="val">
	  <xsl:value-of select="."/>
	</xsl:with-param>
      </xsl:call-template>
    </xsl:attribute>
  </xsl:template>
  
  
  <xsl:template name="splitter">
    <xsl:param name="val"/>
    <xsl:choose>
      <xsl:when test="contains($val,' ')">
	<xsl:text>#</xsl:text>
	<xsl:value-of select="substring-before($val,' ')"/>
	<xsl:text> </xsl:text>
	<xsl:call-template name="splitter">
	  <xsl:with-param name="val">
	    <xsl:value-of select="substring-after($val,' ')"/>
	  </xsl:with-param>
	</xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
	<xsl:text>#</xsl:text>
	<xsl:value-of select="$val"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
  
<!-- fool around with selected elements -->

  <xsl:template match="editionStmt/editor">
    <respStmt xmlns="http://www.tei-c.org/ns/1.0">    
      <resp><xsl:value-of select="@role"/></resp>
      <name><xsl:apply-templates/></name>
    </respStmt>
  </xsl:template>
  
  <!-- header -->  
  
  <xsl:template match="teiHeader">
    <teiHeader xmlns="http://www.tei-c.org/ns/1.0">
      <xsl:apply-templates select="@*|*|comment()|processing-instruction()"/>
      
      <xsl:if test="not(revisionDesc) and (@date.created or @date.updated)">
	<revisionDesc  xmlns="http://www.tei-c.org/ns/1.0">
	  <xsl:if test="@date.updated">
	    <change  xmlns="http://www.tei-c.org/ns/1.0">>
	    <label>updated</label>
	    <date  xmlns="http://www.tei-c.org/ns/1.0">
	      <xsl:value-of select="@date.updated"/>
	    </date>
	    <label  xmlns="http://www.tei-c.org/ns/1.0">Date edited</label>
	    </change>
	  </xsl:if>
	  <xsl:if test="@date.created">
	    <change  xmlns="http://www.tei-c.org/ns/1.0">
	      <label>created</label>
	      <date  xmlns="http://www.tei-c.org/ns/1.0">
		<xsl:value-of select="@date.created"/>
	      </date>
	      <label  xmlns="http://www.tei-c.org/ns/1.0">Date created</label>
	    </change>
	  </xsl:if>
	</revisionDesc>
      </xsl:if>
      <!--
	  <change when="{$today}"  xmlns="http://www.tei-c.org/ns/1.0">Converted to TEI P5 XML by p4top5.xsl
	  written by Sebastian
	  Rahtz at Oxford University Computing Services.</change>
	  </revisionDesc>
	  </xsl:if>
      -->
    </teiHeader>
  </xsl:template>
  
  <xsl:template match="revisionDesc">
    <revisionDesc xmlns="http://www.tei-c.org/ns/1.0">
      <xsl:apply-templates
	  select="@*|*|comment()|processing-instruction()"/>
    </revisionDesc>
  </xsl:template>
  
  <xsl:template match="publicationStmt">
    <publicationStmt xmlns="http://www.tei-c.org/ns/1.0">
      <xsl:apply-templates select="@*|*|comment()|processing-instruction()"/>
      <!--
	  <availability xmlns="http://www.tei-c.org/ns/1.0">
	  <p xmlns="http://www.tei-c.org/ns/1.0">Licensed under <ptr xmlns="http://www.tei-c.org/ns/1.0" target="http://creativecommons.org/licenses/by-sa/2.0/uk/"/></p>
	  </availability>
      -->
    </publicationStmt>
  </xsl:template>
  
  <!-- tagsDecl has a compulsory namespace child now -->
  <xsl:template match="tagsDecl">
    <xsl:if test="*">
      <tagsDecl xmlns="http://www.tei-c.org/ns/1.0">
	<namespace name="http://www.tei-c.org/ns/1.0">
	  <xsl:apply-templates select="*|comment()|processing-instruction"/>
	</namespace>
      </tagsDecl>
    </xsl:if>
  </xsl:template>
  
  
  <xsl:template match="sourceDesc/p[string-length(.)=0]"/>
  
  <!-- start creating the new choice element -->
  <xsl:template match="corr[@sic]">
    <choice  xmlns="http://www.tei-c.org/ns/1.0">
      <corr  xmlns="http://www.tei-c.org/ns/1.0">
	<xsl:value-of select="text()" />
      </corr>
      <sic  xmlns="http://www.tei-c.org/ns/1.0">
	<xsl:value-of select="@sic" />
      </sic>
    </choice>
  </xsl:template>
  
  <xsl:template match="sic[@corr]">
    <choice  xmlns="http://www.tei-c.org/ns/1.0">
      <sic  xmlns="http://www.tei-c.org/ns/1.0">
	<xsl:value-of select="text()" />
      </sic>
      <corr  xmlns="http://www.tei-c.org/ns/1.0">
	<xsl:value-of select="@corr" />
      </corr>
    </choice>
  </xsl:template>
  
  <xsl:template match="abbr[@expan]">
    <choice  xmlns="http://www.tei-c.org/ns/1.0">
      <abbr  xmlns="http://www.tei-c.org/ns/1.0">
	<xsl:value-of select="text()" />
      </abbr>
      <expan  xmlns="http://www.tei-c.org/ns/1.0">
	<xsl:value-of select="@expan" />
      </expan>
    </choice>
  </xsl:template>
  
  <xsl:template match="expan[@abbr]">
    <choice xmlns="http://www.tei-c.org/ns/1.0">
      <expan xmlns="http://www.tei-c.org/ns/1.0">
	<xsl:value-of select="text()" />
      </expan>
      <abbr xmlns="http://www.tei-c.org/ns/1.0">
	<xsl:value-of select="@abbr" />
      </abbr>
    </choice>
  </xsl:template>
  
  <!-- special consideration for <change> element -->
  <xsl:template match="change">
    <change xmlns="http://www.tei-c.org/ns/1.0">
      
      <xsl:apply-templates select="date"/>
      
      <xsl:if test="respStmt/resp">
	<label>
	  <xsl:value-of select="respStmt/resp/text()"/>
	</label>
      </xsl:if>
      <p>
	<xsl:for-each select="respStmt/name">
	  <name xmlns="http://www.tei-c.org/ns/1.0">
	    <xsl:apply-templates
		select="@*|*|comment()|processing-instruction()|text()"/>
	  </name>
	</xsl:for-each>
	<xsl:for-each select="item">
	  <xsl:apply-templates
	      select="@*|*|comment()|processing-instruction()|text()"/>
	</xsl:for-each>
      </p>
    </change>
  </xsl:template>
  
  <xsl:template match="q/@direct"/>
  
  <xsl:template match="q">
    <q xmlns="http://www.tei-c.org/ns/1.0">
      <xsl:apply-templates
	  select="@*|*|comment()|processing-instruction()|text()"/>
    </q>
  </xsl:template>

  
<!-- if we are reading the P4 with a DTD,
       we need to avoid copying the default values
       of attributes -->
  
  <xsl:template match="@targOrder">
    <xsl:if test="not(translate(.,$uc,$lc) ='u')">
      <xsl:attribute name="targOrder">
	<xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:if>
  </xsl:template>
  
  
  <xsl:template match="@opt">
    <xsl:if test="not(translate(.,$uc,$lc) ='n')">
      <xsl:attribute name="opt">
	<xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:if>
  </xsl:template>
  
  
  <xsl:template match="@to">
    <xsl:if test="not(translate(.,$uc,$lc) ='ditto')">
      <xsl:attribute name="to">
	<xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:if>
  </xsl:template>
  
  
  <xsl:template match="@default">
    <xsl:choose>
      <xsl:when test="translate(.,$uc,$lc)= 'no'"/>
      <xsl:otherwise>
	<xsl:attribute name="default">
	  <xsl:value-of select="."/>
	</xsl:attribute>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
  
  <xsl:template match="@part">
    <xsl:if test="not(translate(.,$uc,$lc) ='n')">
      <xsl:attribute name="part">
	<xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:if>
  </xsl:template>
  
  
  <xsl:template match="@full">
    <xsl:if test="not(translate(.,$uc,$lc) ='yes')">
      <xsl:attribute name="full">
	<xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:if>
  </xsl:template>
  
  
  <xsl:template match="@from">
    <xsl:if test="not(translate(.,$uc,$lc) ='root')">
      <xsl:attribute name="from">
	<xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:if>
  </xsl:template>
  
  
  <xsl:template match="@status">
    <xsl:choose>
      <xsl:when test="parent::teiHeader">
	<xsl:if test="not(translate(.,$uc,$lc) ='new')">
	  <xsl:attribute name="status">
	    <xsl:value-of select="."/>
	  </xsl:attribute>
	</xsl:if>
      </xsl:when>
      <xsl:when test="parent::del">
	<xsl:if test="not(translate(.,$uc,$lc) ='unremarkable')">
	  <xsl:attribute name="status">
	    <xsl:value-of select="."/>
	  </xsl:attribute>
	</xsl:if>
      </xsl:when>
      <xsl:otherwise>
	<xsl:attribute name="status">
	  <xsl:value-of select="."/>
	</xsl:attribute>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
  
  <xsl:template match="@place">
    <xsl:if test="not(translate(.,$uc,$lc) ='unspecified')">
      <xsl:attribute name="place">
	<xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:if>
  </xsl:template>
  
  
  <xsl:template match="@sample">
    <xsl:if test="not(translate(.,$uc,$lc) ='complete')">
      <xsl:attribute name="sample">
	<xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:if>
  </xsl:template>
  
  
  <xsl:template match="@org">
    <xsl:if test="not(translate(.,$uc,$lc) ='uniform')">
      <xsl:attribute name="org">
	<xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:if>
  </xsl:template>
  
  <xsl:template match="teiHeader/@type">
    <xsl:if test="not(translate(.,$uc,$lc) ='text')">
      <xsl:attribute name="type">
	<xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:if>
  </xsl:template>
  
  <!-- yes|no to boolean -->
  
  <xsl:template match="@anchored">
    <xsl:attribute name="anchored">
      <xsl:choose>
	<xsl:when test="translate(.,$uc,$lc)='yes'">true</xsl:when>
	<xsl:when test="translate(.,$uc,$lc)='no'">false</xsl:when>
      </xsl:choose>
    </xsl:attribute>
  </xsl:template>
  
  <xsl:template match="sourceDesc/@default"/>
  
  <xsl:template match="@tei">
    <xsl:attribute name="tei">
      <xsl:choose>
	<xsl:when test="translate(.,$uc,$lc)='yes'">true</xsl:when>
	<xsl:when test="translate(.,$uc,$lc)='no'">false</xsl:when>
      </xsl:choose>
    </xsl:attribute>
  </xsl:template>
  
  <xsl:template match="@TEIform"/>  
  
</xsl:stylesheet>
