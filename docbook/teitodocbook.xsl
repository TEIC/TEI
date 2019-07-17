<xsl:stylesheet 
    xmlns:teix="http://www.tei-c.org/ns/Examples" 
    xmlns="http://docbook.org/ns/docbook"
    xpath-default-namespace="http://www.tei-c.org/ns/1.0"
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns:xlink="http://www.w3.org/1999/xlink"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
    xmlns:mml="http://www.w3.org/1998/Math/MathML"
    exclude-result-prefixes="#all"
    version="2.0">
<xsl:import href="../common/verbatim.xsl"/>
<xsl:import href="../common/functions.xsl"/>

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
         <p>Author: Sebastian Rahtz</p>
         <p>Id: $Id: from.xsl 10017 2012-01-10 14:45:54Z rahtz $</p>
         <p>Copyright: 2013, TEI Consortium</p>
      </desc>
   </doc>



<xsl:output method="xml" indent="yes" encoding="utf-8"/>
<xsl:param name="debug"/>
<xsl:param name="spaceCharacter">&#160;</xsl:param>

<xsl:template match="text()|comment()">
  <xsl:copy-of select="."/>
</xsl:template>

<xsl:template match="@*"/>

<xsl:template match="abbr">
  <abbrev>
    <xsl:apply-templates select="@*|*|text()|comment()"/>
  </abbrev>
</xsl:template>

<xsl:template match="div[@type='abstract']"/>

<xsl:template match="anchor">
 <anchor>
    <xsl:apply-templates select="@*|*|text()|comment()"/>
 </anchor>
</xsl:template>

<xsl:template match="TEI">
  <article xmlns="http://docbook.org/ns/docbook"
	   xmlns:xlink="http://www.w3.org/1999/xlink" version="5.0">
    <xsl:apply-templates select="teiHeader"/>
    <xsl:apply-templates select="text/*"/>
  </article>
</xsl:template>

<xsl:template match="teiHeader">
  <info>
    <xsl:apply-templates select="@*|*|text()|comment()"/>
    <xsl:for-each select="../text/*/div[@type='abstract']">
      <abstract>
	<xsl:apply-templates select="@*|*|text()|comment()"/>
      </abstract>
    </xsl:for-each>
<!--  Front matter from text/front can only really be 
      handled here. -->
    <xsl:apply-templates select="following-sibling::text/front/docAuthor"/>
    <xsl:apply-templates select="following-sibling::text/front/docDate"/>
  </info>
</xsl:template>
  
  <xsl:template match="text/front"/>

<xsl:template match="persName">
  <personname>
    <xsl:apply-templates/>
  </personname>
</xsl:template>

<xsl:template match="email">
    <email>
      <xsl:apply-templates/>
    </email>
</xsl:template>

<xsl:template match="affiliation">
  <affiliation>
    <orgname>
      <xsl:apply-templates/>
    </orgname>
  </affiliation>
</xsl:template>

<xsl:template match="author | docAuthor">
  <author>
      <xsl:choose>
	<xsl:when test="*">
	  <xsl:apply-templates/>
	</xsl:when>
	<xsl:otherwise>
	  <personname>
	    <firstname>
	      <xsl:value-of select="substring-before(.,' ')"/>
	    </firstname>
	    <surname>
	      <xsl:value-of select="substring-after(.,' ')"/>
	    </surname>
	  </personname>
	</xsl:otherwise>
      </xsl:choose>
  </author>
</xsl:template>
  
  <xsl:template match="date | docDate">
    <date>
      <xsl:apply-templates select="@*|node()"/>
    </date>
  </xsl:template>

<xsl:template match="q|quote|said">
  <xsl:element name="{if (tei:match(@rend,'block')) then 'blockquote' else 'quote'}">
    <xsl:apply-templates select="@*|*|text()|comment()"/>
  </xsl:element>
</xsl:template>

<xsl:template match="cit">
  <citation>  
    <xsl:apply-templates select="@*|*|text()|comment()"/>
  </citation>
</xsl:template>

<xsl:template match="hi">
  <emphasis role="bold">
    <xsl:apply-templates select="@*|*|text()|comment()"/>
  </emphasis>
</xsl:template>

<xsl:template match="cell">
  <td>
    <xsl:apply-templates select="@*|*|text()|comment()"/>
  </td>
</xsl:template>

<xsl:template match="figure">
  <figure>
    <xsl:apply-templates select="@*"/>
    <xsl:apply-templates select="head"/>
    <xsl:apply-templates select="*[not(self::head)]"/>
  </figure>
</xsl:template>

<xsl:template match="foreign">
  <foreignphrase>
    <xsl:apply-templates select="@*|*|text()|comment()"/>
  </foreignphrase>
</xsl:template>

<xsl:template match="graphic">
  <mediaobject>
    <imageobject>
      <imagedata fileref="{@url}"/>
    </imageobject>
  </mediaobject>
</xsl:template>

<xsl:template match="list">
  <xsl:choose>
    <xsl:when test="tei:isGlossList(.)">
      <variablelist>
	<xsl:for-each select="label">
	  <varlistentry>
	    <term>
	      <xsl:apply-templates select="@*|*|text()|comment()"/>
	      </term>
	      <listitem>
		<xsl:for-each
		    select="following-sibling::item[1]">
		  <xsl:call-template name="pOrNot"/>
		</xsl:for-each>
	      </listitem>
	  </varlistentry>
	</xsl:for-each>
      </variablelist>
    </xsl:when>
    <xsl:when test="tei:isOrderedList(.)">
      <itemizedlist>
	<xsl:apply-templates select="@*|*|text()|comment()"/>
      </itemizedlist>
    </xsl:when>
    <xsl:otherwise>
      <itemizedlist>
	<xsl:apply-templates select="@*|*|text()|comment()"/>
      </itemizedlist>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template match="term">
  <keyword>
    <xsl:apply-templates select="@*|*|text()|comment()"/>
  </keyword>
</xsl:template>

<xsl:template match="keywords">
  <keywordset>
    <xsl:apply-templates select="@*|*|text()|comment()"/>
  </keywordset>
</xsl:template>

<xsl:template match="item">
  <listitem>
    <xsl:call-template name="pOrNot"/>
  </listitem>
</xsl:template>

<xsl:template match="code">
  <literal role="{@rend}">
    <xsl:apply-templates select="@*|*|text()|comment()"/>
  </literal>
</xsl:template>

<xsl:template match="p">
  <para>
    <xsl:apply-templates select="@*|*|text()|comment()"/>
  </para>
</xsl:template>

<xsl:template match="row">
  <tr>
    <xsl:apply-templates select="@*|*|text()|comment()"/>
  </tr>
</xsl:template>

<xsl:template match="div|div1|div2|div3|div4|div5|div6">
  <xsl:variable name="gi">
    <xsl:choose>
      <xsl:when test="local-name(.)='div'">
	<xsl:value-of select="concat('sect',count(ancestor::div)+1)"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:text>sect</xsl:text>
	<xsl:value-of select="substring-after(local-name(.),'div')"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  <xsl:element name="{$gi}" xmlns="http://docbook.org/ns/docbook">
    <xsl:apply-templates select="@*|*|text()|comment()"/>
  </xsl:element>
</xsl:template>


<xsl:template match="surname">
  <surname>
    <xsl:apply-templates select="@*|*|text()|comment()"/>
  </surname>
</xsl:template>

<xsl:template match="forename">
  <firstname>
    <xsl:apply-templates select="@*|*|text()|comment()"/>
  </firstname>
</xsl:template>

<xsl:template match="table">
  <table>
    <xsl:apply-templates select="head"/>
    <xsl:if test="row[@role='label']">
      <thead>
	<xsl:apply-templates select="row[@role='label']"/>
      </thead>
    </xsl:if>
    <tbody>
	<xsl:apply-templates select="row[not(@role='label')]"/>
    </tbody>
  </table>
</xsl:template>

<xsl:template match="tbody">
  <xsl:apply-templates select="@*|*|text()|comment()"/>
</xsl:template>

<xsl:template match="tgroup">
  <xsl:apply-templates select="@*|*|text()|comment()"/>
</xsl:template>

<xsl:template match="head">
  <title>
    <xsl:apply-templates select="@*|*|text()|comment()"/>
  </title>
</xsl:template>

<xsl:template match="ref">
  <link linkend="{substring-after(@target, '#')}">
    <xsl:apply-templates select="@*|*|text()|comment()"/>
  </link>
</xsl:template>

<xsl:template match="ptr">
  <xsl:choose>
    <xsl:when test="starts-with(@target,'#')">
      <xref linkend="{substring-after(@target, '#')}"/>
    </xsl:when>
    <xsl:otherwise>
      <link xlink:href="{@target}">
	<xsl:value-of select="@target"/>
      </link>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>


<xsl:template match="table/head">
  <caption>
    <xsl:apply-templates select="@*|*|text()|comment()"/>
  </caption>
</xsl:template>
  
<xsl:template match="formula">
  <equation>
    <xsl:apply-templates select="@*|node()"/>
  </equation>
</xsl:template>
  
  <xsl:template match="formula/mml:math">
    <xsl:copy-of select="."/>
  </xsl:template>

<xsl:template match="note[@place='foot']">
  <footnote>
    <xsl:call-template name="pOrNot"/>
  </footnote>
</xsl:template>

<xsl:template match="eg">
  <programlisting>
    <xsl:apply-templates select="@*|*|text()|comment()"/>
  </programlisting>
</xsl:template>

<!-- catch all -->

<xsl:template match="*">
  <xsl:if test="$debug='true'">
    <xsl:message>Unknown element <xsl:value-of
    select="name()"/></xsl:message>
  </xsl:if>
  <xsl:apply-templates select="*|text()|comment()"/>
</xsl:template>

<!-- use general-purpose templates to add standard attributes -->
<xsl:template match="@rend">
  <xsl:attribute name="role">
    <xsl:value-of select="."/>
  </xsl:attribute>
</xsl:template>

<xsl:template match="@xml:lang">
  <xsl:copy-of select="."/>
</xsl:template>

<xsl:template match="@xml:id">
  <xsl:copy-of select="."/>
</xsl:template>

<xsl:template match="gi">
  <tag>
    <xsl:text>&lt;</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>&gt;</xsl:text>
  </tag>
</xsl:template>

<xsl:template match="att">
  <varname>
    <xsl:text>@</xsl:text>
    <xsl:apply-templates/>
  </varname>
</xsl:template>

<xsl:template match="body">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="emph">
  <emphasis>
    <xsl:apply-templates/>
  </emphasis>
</xsl:template>

<xsl:template match="fileDesc">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="front">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="ident">
  <varname>
    <xsl:apply-templates/>
  </varname>
</xsl:template>

<xsl:template match="publicationStmt"/>

<xsl:template match="revisionDesc"/>

<xsl:template match="soCalled">
  <quote>
    <xsl:apply-templates/>
  </quote>
</xsl:template>
<xsl:template match="sourceDesc"/>

<xsl:template match="title">
  <xsl:choose>
    <xsl:when test="ancestor::fileDesc">
      <title>
       <xsl:apply-templates select="@*|*|text()|comment()"/>
      </title>
    </xsl:when>
    <xsl:otherwise>
      <citetitle>
	<xsl:apply-templates select="@*|*|text()|comment()"/>
      </citetitle>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template match="titleStmt">
  <xsl:apply-templates/>
</xsl:template>

 <xsl:template name="pOrNot">
   <xsl:choose>
     <xsl:when test="p">
       <xsl:apply-templates select="@*|*|text()|comment()"/>
     </xsl:when>
     <xsl:otherwise>
       <para>
	 <xsl:apply-templates select="@*|*|text()|comment()"/>
       </para>
     </xsl:otherwise>
   </xsl:choose>
 </xsl:template>

 <xsl:template match="teix:egXML">
    <xsl:param name="simple">false</xsl:param>
    <xsl:param name="highlight"/>
    <programlisting>
      <xsl:choose>
        <xsl:when test="$simple='true'">
          <xsl:apply-templates mode="verbatim">
            <xsl:with-param name="highlight">
              <xsl:value-of select="$highlight"/>
            </xsl:with-param>
          </xsl:apply-templates>
        </xsl:when>
        <xsl:otherwise>
          <xsl:apply-templates mode="verbatim">
            <xsl:with-param name="highlight">
              <xsl:value-of select="$highlight"/>
            </xsl:with-param>
          </xsl:apply-templates>
        </xsl:otherwise>
      </xsl:choose>
    </programlisting>
 </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>show an XML element in a verbatim context</desc>
  </doc>

  <xsl:template name="Element">
    <xsl:param name="content"/>
    <emphasis role="element">
      <xsl:copy-of select="$content"/>
    </emphasis>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>show an XML element name in a verbatim context</desc>
  </doc>
  <xsl:template name="ElementName">
    <xsl:param name="content"/>
    <emphasis role="elementname">
      <xsl:copy-of select="$content"/>
    </emphasis>
  </xsl:template>

   <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>show an XML element name highlighted in a verbatim context</desc>
  </doc>
 <xsl:template name="HighlightElementName">
    <xsl:param name="content"/>
    <emphasis role="highlightelementname">
      <xsl:copy-of select="$content"/>
    </emphasis>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>show an XML attribute value in a verbatim context</desc>
  </doc>

  <xsl:template name="AttributeValue">
    <xsl:param name="content"/>
    <emphasis role="attributevalue">
      <xsl:copy-of select="$content"/>
    </emphasis>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>show an XML attribute in a verbatim context</desc>
  </doc>

  <xsl:template name="Attribute">
    <xsl:param name="content"/>
    <emphasis role="attribute">
      <xsl:copy-of select="$content"/>
    </emphasis>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>show an XML namespace in a verbatim context</desc>
  </doc>
  <xsl:template name="Namespace">
    <xsl:param name="content"/>
    <emphasis role="namespace">
      <xsl:copy-of select="$content"/>
    </emphasis>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>show an XML comment in a verbatim context</desc>
  </doc>
  <xsl:template name="Comment">
    <xsl:param name="content"/>
    <emphasis role="comment">
      <xsl:copy-of select="$content"/>
    </emphasis>
  </xsl:template>

  <xsl:template match="encodingDesc"/>

</xsl:stylesheet>
