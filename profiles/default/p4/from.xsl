<?xml version="1.0"?>
<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:tei="http://www.tei-c.org/ns/1.0" 
    xmlns:xs="http://www.w3.org/2001/XMLSchema"
    xmlns="http://www.tei-c.org/ns/1.0"
    version="2.0" exclude-result-prefixes="xs">
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>

         <p>This software is dual-licensed:

1. Distributed under a Creative Commons Attribution-ShareAlike 3.0
Unported License http://creativecommons.org/licenses/by-sa/3.0/ 

2. http://www.opensource.org/licenses/BSD-2-Clause
		


Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

* Redistributions of source code must retain the above copyright
notice, this list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright
notice, this list of conditions and the following disclaimer in the
documentation and/or other materials provided with the distribution.

This software is provided by the copyright holders and contributors
"as is" and any express or implied warranties, including, but not
limited to, the implied warranties of merchantability and fitness for
a particular purpose are disclaimed. In no event shall the copyright
holder or contributors be liable for any direct, indirect, incidental,
special, exemplary, or consequential damages (including, but not
limited to, procurement of substitute goods or services; loss of use,
data, or profits; or business interruption) however caused and on any
theory of liability, whether in contract, strict liability, or tort
(including negligence or otherwise) arising in any way out of the use
of this software, even if advised of the possibility of such damage.
</p>
         <p>Author: See AUTHORS</p>
         
         <p>Copyright: 2013, TEI Consortium</p>
      </desc>
   </doc>

  <!-- 
       
       P4 to P5 converter 
       
       Sebastian Rahtz <sebastian.rahtz@oucs.ox.ac.uk>
       
       $Date$  $Id$
       
  -->
  <xsl:output method="xml" encoding="utf-8"
    cdata-section-elements="tei:eg" omit-xml-declaration="yes"/>
  
  <xsl:variable name="processor">
    <xsl:value-of select="system-property('xsl:vendor')"/>
  </xsl:variable>
  
  <xsl:variable name="today">
      <xsl:value-of select="format-dateTime(current-dateTime(),'[Y]-[M02]-[D02]T[H02]:[m02]:[s02]Z')"/>
  </xsl:variable>
  
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
  
    <xsl:template match="teiCorpus.2">
    <teiCorpus>
      <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
    </teiCorpus>
  </xsl:template>
  
  <xsl:template match="witness/@sigil">
    <xsl:attribute name="xml:id">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>

  <xsl:template match="witList">
    <listWit>
      <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
    </listWit>
  </xsl:template>
  
  
  <xsl:template match="TEI.2">
    <TEI>
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
    <figure>
      <graphic>
	<xsl:apply-templates select="@*"/>
      </graphic>
      <xsl:apply-templates/>
    </figure>
  </xsl:template>
  
  <xsl:template match="figure/@entity"/>

  
  <xsl:template match="figure[@entity]">
    <figure>
      <graphic>
	<xsl:attribute name="url">
	  <xsl:choose>
	    <xsl:when test="unparsed-entity-uri(@entity)=''">
	      <xsl:text>ENTITY_</xsl:text>
	      <xsl:value-of select="@entity"/>
	    </xsl:when>
	    <xsl:otherwise>
	      <xsl:value-of select="unparsed-entity-uri(@entity)"/>
	    </xsl:otherwise>
	  </xsl:choose>
	</xsl:attribute>
	<xsl:apply-templates select="@*"/>
      </graphic>
      <xsl:apply-templates/>
    </figure>
  </xsl:template>
  
  <xsl:template match="state">
    <refState>
      <xsl:apply-templates select="@*|*|text()|comment()|processing-instruction()"/>
    </refState>
  </xsl:template>
  
    <xsl:template match="@targType">
      <xsl:attribute name="type">
	<xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:template>

  <!-- lost elements -->
  <xsl:template match="dateRange">
    <date>
      <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
    </date>
  </xsl:template>
  
  
  <xsl:template match="dateRange/@from">
    <xsl:copy-of select="."/>
  </xsl:template>
  
  <xsl:template match="dateRange/@to">
    <xsl:copy-of select="."/>
  </xsl:template>
  
  <xsl:template match="language">
    <xsl:element namespace="http://www.tei-c.org/ns/1.0"
		 name="language">
      <xsl:choose>
	<xsl:when test="@id">
	  <xsl:attribute name="ident">
	    <xsl:value-of select="@id"/>
	  </xsl:attribute>
	</xsl:when>
	<xsl:when test=".='English'">
	  <xsl:attribute name="ident">en</xsl:attribute>
	</xsl:when>
      </xsl:choose>
      <xsl:apply-templates select="*|processing-instruction()|comment()|text()"/>
    </xsl:element>
  </xsl:template>
  
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
  
  <xsl:template match="figure/@url">
    <xsl:attribute name="url">
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
  
  <xsl:template match="variantEncoding/@location">
    <xsl:copy-of select="."/>
  </xsl:template>

  <xsl:template match="@ana|@active|@adj|@adjFrom|@adjTo|@children|@class|@code|@copyOf|@corresp|@decls|@domains|@end|@exclude|@fVal|@feats|@follow|@hand|@inst|@langKey|@location|@mergedin|@new|@next|@old|@origin|@otherLangs|@parent|@passive|@perf|@prev|@render|@resp|@sameAs|@scheme|@script|@select|@since|@start|@synch|@target|@targetEnd|@value|@value|@who|@wit">
    <xsl:variable name="vals">
      <xsl:for-each select="tokenize(.,' ')">
        <a>
	  <xsl:choose>
          <xsl:when test="starts-with(.,'http') or starts-with(.,'ftp') or starts-with(.,'mailto')">
            <xsl:sequence select="."/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:text>#</xsl:text>
	    <xsl:sequence select="."/>
          </xsl:otherwise>
        </xsl:choose>
	</a>
      </xsl:for-each>
    </xsl:variable>
    <xsl:attribute name="{name(.)}" select="string-join($vals/tei:a,' ')"/>
  </xsl:template>
 
  
 <!-- fool around with selected elements -->
  
 <!-- imprint is no longer allowed inside bibl -->
 <xsl:template match="bibl/imprint">
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="editionStmt/editor">
    <respStmt>    
      <resp><xsl:value-of select="@role"/></resp>
      <name><xsl:apply-templates/></name>
    </respStmt>
  </xsl:template>
  
  <!-- header -->  
  
  <xsl:template match="teiHeader">
    <teiHeader>
      <xsl:apply-templates select="@*|*|comment()|processing-instruction()"/>
      
      <xsl:if test="not(revisionDesc) and (@date.created or @date.updated)">
	<revisionDesc>
	  <xsl:if test="@date.updated">
	    <change>
	    <label>updated</label>
	    <date>
	      <xsl:value-of select="@date.updated"/>
	    </date>
	    <label>Date edited</label>
	    </change>
	  </xsl:if>
	  <xsl:if test="@date.created">
	    <change>
	      <label>created</label>
	      <date>
		<xsl:value-of select="@date.created"/>
	      </date>
	      <label>Date created</label>
	    </change>
	  </xsl:if>
	</revisionDesc>
      </xsl:if>
      <!--
	  <change when="{$today}">Converted to TEI P5 XML by p4top5.xsl
	  written by Sebastian
	  Rahtz at Oxford University Computing Services.</change>
	  </revisionDesc>
	  </xsl:if>
      -->
    </teiHeader>
  </xsl:template>
  
  <xsl:template match="revisionDesc">
    <revisionDesc>
      <xsl:apply-templates
	  select="@*|*|comment()|processing-instruction()"/>
    </revisionDesc>
  </xsl:template>
  
  <xsl:template match="publicationStmt">
    <publicationStmt>
      <xsl:apply-templates select="@*|*|comment()|processing-instruction()"/>
      <!--
	  <availability>
	  <p>Licensed under <ptr target="http://creativecommons.org/licenses/by-sa/2.0/uk/"/></p>
	  </availability>
      -->
    </publicationStmt>
  </xsl:template>
  
 <!-- space does not have @extent any more -->
  <xsl:template match="space/@extent">
    <xsl:attribute name="quantity">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>

 <!-- word key -->
  <xsl:template match="entryFree/@key">
    <xsl:attribute name="sortKey">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>

  <!-- tagsDecl has a compulsory namespace child now -->
  <xsl:template match="tagsDecl">
    <xsl:if test="*">
      <tagsDecl>
	<namespace name="http://www.tei-c.org/ns/1.0">
	  <xsl:apply-templates select="*|comment()|processing-instruction"/>
	</namespace>
      </tagsDecl>
    </xsl:if>
  </xsl:template>
  
  <!-- orgTitle inside orgName? redundant -->
  <xsl:template match="orgName/orgTitle">
      <xsl:apply-templates/>
  </xsl:template>

 <!-- no need for empty <p> in sourceDesc -->  
  <!-- xsl:template match="sourceDesc/p[string-length(.)=0]"/-->
<!-- No, an empty sourceDesc is invalid in tei_all (LB) -->  

  <!-- start creating the new choice element -->
  <xsl:template match="corr[@sic]">
    <choice>
      <corr>
	<xsl:value-of select="text()" />
      </corr>
      <sic>
	<xsl:value-of select="@sic" />
      </sic>
    </choice>
  </xsl:template>
  
  <xsl:template match="gap/@desc"/>
  
  <xsl:template match="gap">
    <gap>
      <xsl:apply-templates select="@*"/>
      <xsl:if test="@desc">
	<desc>
	  <xsl:value-of select="@desc"/>
	</desc>
      </xsl:if>
    </gap>
  </xsl:template>
    
  <xsl:template match="sic[@corr]">
    <choice>
      <sic>
	<xsl:apply-templates/>
      </sic>
      <corr>
	<xsl:value-of select="@corr" />
      </corr>
    </choice>
  </xsl:template>
  
  <xsl:template match="abbr[@expan]">
    <choice>
      <abbr>
	<xsl:apply-templates/>
      </abbr>
      <expan>
	<xsl:value-of select="@expan" />
      </expan>
    </choice>
  </xsl:template>
  
  <xsl:template match="expan[@abbr]">
    <choice>
      <expan>
	<xsl:apply-templates/>
      </expan>
      <abbr>
	<xsl:value-of select="@abbr" />
      </abbr>
    </choice>
  </xsl:template>
  
  <!-- special consideration for <change> element -->
  <xsl:template match="change">
    <change>
      <xsl:apply-templates select="item/@*"/>
      
      <xsl:apply-templates select="date"/>
      
      <xsl:if test="respStmt/resp">
	<label>
	  <xsl:value-of select="respStmt/resp/text()"/>
	</label>
      </xsl:if>
	<xsl:for-each select="respStmt/name">
	  <name>
	    <xsl:apply-templates
		select="@*|*|comment()|processing-instruction()|text()"/>
	  </name>
	</xsl:for-each>
	<xsl:for-each select="item">
	  <xsl:apply-templates
	      select="*|comment()|processing-instruction()|text()"/>
	</xsl:for-each>
    </change>
  </xsl:template>


  <xsl:template match="respStmt[resp]">
    <respStmt>
      <xsl:choose>
	<xsl:when test="resp/name">
	  <resp>
	    <xsl:value-of select="resp/text()"/>
	  </resp>
	    <xsl:for-each select="resp/name">
	      <name>
		<xsl:apply-templates/>
	      </name>
	    </xsl:for-each>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:apply-templates/>
	  <name>
	  </name>
	</xsl:otherwise>
      </xsl:choose>
    </respStmt>
  </xsl:template>

  <xsl:template match="q/@direct"/>
  
  <xsl:template match="q">
    <q>
      <xsl:apply-templates
	  select="@*|*|comment()|processing-instruction()|text()"/>
    </q>
  </xsl:template>

  
<!-- if we are reading the P4 with a DTD,
       we need to avoid copying the default values
       of attributes -->
  
  <xsl:template match="@targOrder">
    <xsl:if test="not(lower-case(.) ='u')">
      <xsl:attribute name="targOrder">
	<xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:if>
  </xsl:template>
  
  
  <xsl:template match="@opt">
    <xsl:if test="not(lower-case(.) ='n')">
      <xsl:attribute name="opt">
	<xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:if>
  </xsl:template>
  
  
  <xsl:template match="@to">
    <xsl:if test="not(lower-case(.) ='ditto')">
      <xsl:attribute name="to">
	<xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:if>
  </xsl:template>

  <xsl:template match="interp/@value">
      <xsl:attribute name="inst">
	<xsl:value-of select="."/>
      </xsl:attribute>
  </xsl:template>
  
  
  <xsl:template match="@default">
    <xsl:choose>
      <xsl:when test="lower-case(.)= 'no'"/>
      <xsl:otherwise>
	<xsl:attribute name="default">
	  <xsl:value-of select="."/>
	</xsl:attribute>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
  
  <xsl:template match="@part">
    <xsl:if test="not(lower-case(.) ='n')">
      <xsl:attribute name="part">
	<xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:if>
  </xsl:template>
  
  
  <xsl:template match="@full">
    <xsl:if test="not(lower-case(.) ='yes')">
      <xsl:attribute name="full">
	<xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:if>
  </xsl:template>
  
  
  <xsl:template match="@from">
    <xsl:if test="not(lower-case(.) ='root')">
      <xsl:attribute name="from">
	<xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:if>
  </xsl:template>
  
  
  <xsl:template match="@status">
    <xsl:choose>
      <xsl:when test="parent::teiHeader">
	<xsl:if test="not(lower-case(.) ='new')">
	  <xsl:attribute name="status">
	    <xsl:value-of select="."/>
	  </xsl:attribute>
	</xsl:if>
      </xsl:when>
      <xsl:when test="parent::del">
	<xsl:if test="not(lower-case(.) ='unremarkable')">
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
    <xsl:if test="not(lower-case(.) ='unspecified')">
      <xsl:attribute name="place">
	<xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:if>
  </xsl:template>
  
  
  <xsl:template match="@sample">
    <xsl:if test="not(lower-case(.) ='complete')">
      <xsl:attribute name="sample">
	<xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:if>
  </xsl:template>
  
  
  <xsl:template match="@org">
    <xsl:if test="not(lower-case(.) ='uniform')">
      <xsl:attribute name="org">
	<xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:if>
  </xsl:template>
  
  <xsl:template match="@type">
    <xsl:if test="not(normalize-space(.)='')">
	<xsl:attribute name="type">
	  <xsl:value-of select="translate(.,' ','_')"/>
	</xsl:attribute>
    </xsl:if>
  </xsl:template>

  <xsl:template match="teiHeader/@type">
    <xsl:if test="not(lower-case(.) ='text')">
      <xsl:attribute name="type">
	<xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:if>
  </xsl:template>
  
  <!-- yes|no to boolean -->
  
  <xsl:template match="@anchored">
    <xsl:attribute name="anchored">
      <xsl:choose>
	<xsl:when test="lower-case(.)='yes'">true</xsl:when>
	<xsl:when test="lower-case(.)='no'">false</xsl:when>
      </xsl:choose>
    </xsl:attribute>
  </xsl:template>
  
  <xsl:template match="sourceDesc/@default"/>
  

<!-- attributes suppressed in P5 -->

  <xsl:template match="@tei"/>
  
  <xsl:template match="@langKey"/>  
  
  <xsl:template match="@TEIform"/>  

<!-- assorted atts -->

  <xsl:template match="gi/@TEI">  	 
    <xsl:if test=".='yes'">
      <xsl:attribute name="scheme">TEI</xsl:attribute>
    </xsl:if>
  </xsl:template>

  <xsl:template match="@old"/>  

  <xsl:template match="ref/@from"/>  

  <xsl:template match="@mergedin">  
    <xsl:attribute name="mergedIn">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>

<!-- deal with the loss of div0 -->  

  <xsl:template match="div0|div1|div2|div3|div4|div5|div6">
    <xsl:element name="div" namespace="http://www.tei-c.org/ns/1.0">
      <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
    </xsl:element>
  </xsl:template>


<!-- from Conal Tuohy -->
<xsl:template match="orig[@reg]">
  <choice>
    <orig>
      <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
    </orig>
    <reg>
      <xsl:value-of select="@reg"/>
    </reg>
  </choice>
</xsl:template>

<xsl:template match="reg[@orig]">
  <choice>
    <reg><xsl:apply-templates select="*|@*|processing-instruction()|
    comment()|text()"/></reg>
    <orig><xsl:value-of select="@orig"/></orig>
  </choice>
</xsl:template>

<xsl:template match="@orig|@reg"/>

<!-- remove default values for attributes -->

<xsl:template match="row/@role[.='data']"/>
<xsl:template match="cell/@role[.='data']"/>
<xsl:template match="cell/@rows[.='1']"/>
<xsl:template match="cell/@cols[.='1']"/>
<xsl:template match="q/@broken[.='no']"/>

  <xsl:template match="text[not(parent::TEI.2)]">
    <floatingText>
	<xsl:apply-templates 
	    select="*|@*|processing-instruction()|comment()|text()"/>
    </floatingText>
  </xsl:template>

<!-- from CES -->
  <xsl:template match="cesdoc">
    <cesDoc>
    <xsl:apply-templates 
        select="*|@*|processing-instruction()|comment()|text()"/>
    </cesDoc>
</xsl:template>

<!-- found in Perseus -->

  <xsl:template match="lg1|lg2|lg3|lg4">
    <lg>
    <xsl:apply-templates 
        select="*|@*|processing-instruction()|comment()|text()"/>
    </lg>
  </xsl:template>

<!-- from OTA DTD -->
  <xsl:template match="spkr">
    <speaker>
    <xsl:apply-templates 
        select="*|@*|processing-instruction()|comment()|text()"/>
    </speaker>
  </xsl:template>

  <xsl:template match="letter">
    <floatingText type="letter">
      <body>
	<xsl:apply-templates 
	    select="*|@*|processing-instruction()|comment()|text()"/>
      </body>
    </floatingText>
  </xsl:template>

<!--  and index -->

<xsl:template match="index">
<index indexName="level1">
<xsl:value-of select="@level1"/>
<xsl:if test="@level2">
<index indexName="level2">
<xsl:value-of select="@level2"/>
<xsl:if test="@level3">
<index indexName="level3">
<xsl:value-of select="@level3"/>
</index></xsl:if>
</index></xsl:if>
</index>
</xsl:template>


<!-- JC Additions 2013-06 -->

<!-- foreName wasn't getting lower-cased. -->
  <xsl:template match="foreName"><forename><xsl:apply-templates 
    select="@*|*|processing-instruction()|comment()|text()"/>
  </forename></xsl:template>

<!-- events can have @desc -->
  <xsl:template match="event/@desc"/>
  <xsl:template match="event">
    <incident>
      <xsl:apply-templates select="@*"/>
      <xsl:if test="@desc">
        <desc>
          <xsl:value-of select="@desc"/>
        </desc>
      </xsl:if>
    </incident>
  </xsl:template>
  
<!-- and vocal can have desc as well -->
  <xsl:template match="vocal/@desc"/>
  <xsl:template match="vocal">
    <vocal>
      <xsl:apply-templates select="@*"/>
      <xsl:if test="@desc">
        <desc>
          <xsl:value-of select="@desc"/>
        </desc>
      </xsl:if>
    </vocal>
  </xsl:template>
  
  <!-- birth/@date wasn't being handled; because @date wasn't enforced as a date datatype
    I check if @date is indeed a date and if it is make it a @when and if not make it an @n
  -->
  <xsl:template match="birth/@date"><xsl:choose>
    <xsl:when test="string(.) castable as xs:date"><xsl:attribute name="when"><xsl:value-of select="."/></xsl:attribute></xsl:when>
    <xsl:otherwise><xsl:attribute name="n"><xsl:value-of select="."/></xsl:attribute></xsl:otherwise>
  </xsl:choose></xsl:template>


<!-- JC Additions 2013-11 -->
  
  <!-- handList and hand not catered for: they become handNotes and handNote -->
  <xsl:template match="handList"><handNotes><xsl:apply-templates select="@*|*|processing-instruction()|comment()|text()"/></handNotes></xsl:template>

  <!-- hand inside handList -->
  <xsl:template match="handList/hand">
    <handNote>
      <xsl:apply-templates select="@*|*|processing-instruction()|comment()|text()"/>
      <xsl:if test="@scribe"><xsl:text>  Scribe: </xsl:text><xsl:value-of select="@scribe"/></xsl:if>
      <xsl:if test="@style"><xsl:text>  Style: </xsl:text><xsl:value-of select="@style"/></xsl:if>
      <xsl:if test="@character"><xsl:text>  Character: </xsl:text><xsl:value-of select="@character"/></xsl:if>
      <xsl:if test="@first"><xsl:text>  Main hand : </xsl:text><xsl:value-of select="@first"/></xsl:if>
      <xsl:if test="@mainLang"><xsl:text> Main language: </xsl:text><xsl:value-of select="@mainLang"/>
      </xsl:if>
      <xsl:if test="@resp"><xsl:text>  Responsibiity: </xsl:text><xsl:value-of select="@resp"/></xsl:if>
      
    </handNote>
  </xsl:template>

  <!-- @ink on hand -->
  <xsl:template match="hand/@ink">
    <xsl:attribute name="medium">
      <xsl:value-of select="."/>
      <xsl:text>_ink</xsl:text>
    </xsl:attribute>
  </xsl:template>
  
  <!-- suppress other attributes  on hand -->
  <xsl:template match="hand/@scribe"/>
  <xsl:template match="hand/@style"/>
  <xsl:template match="hand/@character"/>
  <xsl:template match="hand/@first"/>
  <xsl:template match="hand/@mainLang"/>
  <xsl:template match="hand/@resp"/>
  
  
</xsl:stylesheet>
