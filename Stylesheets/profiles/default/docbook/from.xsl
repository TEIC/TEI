<xsl:stylesheet 
    xmlns="http://www.tei-c.org/ns/1.0"
    xpath-default-namespace="http://docbook.org/ns/docbook"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
    version="2.0">
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p>This software is dual-licensed:

1. Distributed under a Creative Commons Attribution-ShareAlike 3.0
Unported License http://creativecommons.org/licenses/by-sa/3.0/ 

2. http://www.opensource.org/licenses/BSD-2-Clause
		
All rights reserved.

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
         <p>Author: Sebastian Rahtz</p>
         <p>Id: $Id$</p>
         <p>Copyright: 2008, TEI Consortium</p>
      </desc>
   </doc>


<xsl:output method="xml" indent="yes" encoding="utf-8"/>
  <xsl:variable name="processor">
    <xsl:value-of select="system-property('xsl:vendor')"/>
  </xsl:variable>

<xsl:template match="abbrev">
  <abbr>
    <xsl:apply-templates/>
  </abbr>
</xsl:template>

<xsl:template match="abstract">
  <div type="abstract">
    <xsl:call-template name="ID"/>
    <xsl:apply-templates/>
  </div>
</xsl:template>

<xsl:template match="ackno">
 <xsl:apply-templates/>
</xsl:template>

<xsl:template match="acronym">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="address">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="affiliation">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="anchor">
 <anchor>
   <xsl:call-template name="ID"/>
 </anchor>
</xsl:template>

<xsl:template match="appendix">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="application">
 <ident type="application" >
   <xsl:apply-templates/>
 </ident>
</xsl:template>

<xsl:template match="info|artheader|articleinfo"/>

<xsl:template match="info|artheader|articleinfo" mode="header">
  <teiHeader >
    <fileDesc>
      <titleStmt>
	<xsl:apply-templates select="title"/>
	<author>
	  <xsl:choose>
	    <xsl:when test="author">
	      <xsl:apply-templates select="author"/>
	    </xsl:when>
	    <xsl:otherwise>
	      <xsl:text>unknown author</xsl:text>
	  </xsl:otherwise>
	  </xsl:choose>
	</author>
      </titleStmt>
      <editionStmt>
          <edition>
            <date>
	      <xsl:choose>
		<xsl:when test="pubdate">
		  <xsl:apply-templates select="pubdate"/>
		</xsl:when>
		<xsl:otherwise>
		  <xsl:call-template name="whatsTheDate"/> 
		</xsl:otherwise>
	      </xsl:choose>
            </date>
          </edition>
        </editionStmt>
      <publicationStmt>
        <p></p>
      </publicationStmt>
    <sourceDesc>
      <p>Converted from a Docbook original</p>
    </sourceDesc>
    </fileDesc>
    <xsl:if test="keywordset">
      <profileDesc>
        <textClass>
	  <xsl:apply-templates select="keywordset"/>
        </textClass>
      </profileDesc>
    </xsl:if>
  </teiHeader>
</xsl:template>

<xsl:template match="article|book">
  <TEI>
    <xsl:call-template name="ID"/>
    <xsl:apply-templates select="info|artheader|articleinfo" mode="header"/>
    <text>
      <xsl:choose>
	<xsl:when test="artheader/abstract">
	  <front>
	    <xsl:apply-templates select="artheader/abstract"/>
	  </front>
	</xsl:when>
	<xsl:when test="articleinfo/abstract">
	  <front>
	    <xsl:apply-templates select="articleinfo/abstract"/>
	  </front>
	</xsl:when>
	<xsl:otherwise>
	</xsl:otherwise>
      </xsl:choose>
      <body>
	<xsl:apply-templates />
      </body>
    </text>
  </TEI>
</xsl:template>

<xsl:template match="attribution">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="author">
    <name>
      <xsl:choose>
	<xsl:when test="surname">
	  <persName>
	    <xsl:apply-templates/>
	  </persName>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:apply-templates/>
	</xsl:otherwise>
      </xsl:choose>
    </name>
</xsl:template>

<xsl:template match="authorblurb">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="authorgroup">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="bibliodiv">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="bibliography">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="bibliomisc">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="bibliomixed">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="blockquote">
  <q>  
    <xsl:apply-templates/>
  </q>
</xsl:template>

<xsl:template match="chapter">
  <div>  
    <xsl:call-template name="ID"/>
    <xsl:apply-templates/>
  </div>
</xsl:template>

<xsl:template match="citation">
  <cit>  
    <xsl:apply-templates/>
  </cit>
</xsl:template>

<xsl:template match="computeroutput">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="copyright">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="email">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="emphasis">
  <xsl:choose>
    <xsl:when test="@role='strong'">
      <hi>
	<xsl:apply-templates/>
      </hi>
    </xsl:when>
    <xsl:otherwise>
      <hi>
	<xsl:call-template name="Role"/>
	<xsl:apply-templates/>
      </hi>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template match="entry">
  <cell>
    <xsl:apply-templates/>
  </cell>
</xsl:template>

<xsl:template match="example">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="figure">
  <figure>
    <xsl:apply-templates/>
    <figDesc></figDesc>
  </figure>
</xsl:template>

<xsl:template match="foreignphrase">
  <foreign><xsl:apply-templates/></foreign>
</xsl:template>

<xsl:template match="imageobject">
  <graphic url="{imagedata/@fileref}"/>
</xsl:template>

<xsl:template match="imagedata">
</xsl:template>

<xsl:template match="inlinemediaobject|mediaobject">

 <xsl:apply-templates/>

</xsl:template>

<xsl:template match="issuenum">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="itemizedlist">
  <list  type="unordered">
    <xsl:apply-templates/>
  </list>
</xsl:template>

<xsl:template match="keyword">
  <term>
    <xsl:call-template name="Lang"/>
    <xsl:apply-templates/>
  </term>
</xsl:template>

<xsl:template match="keywordset">
  <keywords scheme="adhoc">
    <xsl:apply-templates/>
  </keywords>
</xsl:template>

<xsl:template match="link">
  <ref target="#{@linkend}">
    <xsl:apply-templates/>
  </ref>
</xsl:template>

<xsl:template match="listitem">
  <item >
    <xsl:apply-templates/>
  </item>
</xsl:template>

<xsl:template match="literal">
  <code rend="{@role}">
    <xsl:apply-templates/>
  </code>
</xsl:template>

<xsl:template match="literallayout">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="note">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="orderedlist">
  <list  type="ordered">
    <xsl:apply-templates/>
  </list>
</xsl:template>

<xsl:template match="orgname">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="othercredit">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="para">
  <xsl:choose>
    <xsl:when test="normalize-space(.)=''"/>
    <xsl:when test="parent::listitem and count(../para)=1">
	<xsl:apply-templates/>
    </xsl:when>
    <xsl:otherwise>
      <p>  
	<xsl:apply-templates/>
      </p>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template match="productname">
 <ident rend="productname">
   <xsl:apply-templates/>
 </ident>
</xsl:template>

<xsl:template match="pubdate">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="quote">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="row">
  <row>
    <xsl:apply-templates/>
  </row>
</xsl:template>

<xsl:template match="screen">
<eg >
  <xsl:apply-templates/>
</eg>
</xsl:template>

<xsl:template match="section|sect1|sect2|sect3|sect4">
  <xsl:variable name="gi">
    <xsl:choose>
      <xsl:when test="local-name(.)='section'">div</xsl:when>
      <xsl:otherwise>
	<xsl:text>div</xsl:text>
	<xsl:value-of select="substring-after(local-name(.),'sect')"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  <xsl:element name="{$gi}">
    <xsl:call-template name="ID"/>
    <xsl:apply-templates/>
  </xsl:element>
</xsl:template>


<xsl:template match="surname">
  <surname >
    <xsl:apply-templates/>
  </surname>
</xsl:template>

<xsl:template match="symbol">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="table">
  <xsl:copy>
    <xsl:apply-templates/>
  </xsl:copy>
</xsl:template>

<xsl:template match="tbody">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="tgroup">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="title">
  <xsl:choose>
    <xsl:when test="parent::articleinfo">
      <title>
	<xsl:apply-templates/>
      </title>
    </xsl:when>
    <xsl:otherwise>
      <head>
	<xsl:apply-templates/>
      </head>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template match="artheader/title">
  <title>
    <xsl:call-template name="Lang"/>
    <xsl:apply-templates/>
  </title>
</xsl:template>

<xsl:template match="toc"/>


<xsl:template match="tocentry">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="token">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="ulink">
 <ref target="{@url}" >
   <xsl:apply-templates/>
 </ref>
</xsl:template>

<xsl:template match="userinput">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="volumenum">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="warning">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="wordasword">
 <w>
  <xsl:apply-templates/>
 </w>
</xsl:template>

<xsl:template match="xref">
  <ptr target="#{@linkend}" type="{@role}" />
</xsl:template>

<!-- use general-purpose templates to add standard attributes -->
<xsl:template name="Role">
    <xsl:if test="@role">
      <xsl:attribute name="rend">
	<xsl:value-of select="@role"/>
      </xsl:attribute>
    </xsl:if>
</xsl:template>

<xsl:template name="Lang">
    <xsl:if test="@lang">
      <xsl:attribute name="xml:lang">
	<xsl:value-of select="@lang"/>
      </xsl:attribute>
    </xsl:if>
</xsl:template>

<xsl:template name="ID">
    <xsl:if test="@id">
      <xsl:attribute name="xml:id">
	<xsl:value-of select="@id"/>
      </xsl:attribute>
    </xsl:if>
</xsl:template>

  <xsl:template name="whatsTheDate">
      <xsl:value-of select="format-dateTime(current-dateTime(),'[Y]-[M02]-[D02]T[H02]:[m02]:[s02]Z')"/>
  </xsl:template>

</xsl:stylesheet>
