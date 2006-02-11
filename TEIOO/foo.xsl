<?xml version="1.0" encoding="UTF-8"?>
<!-- 
 #  The Contents of this file are made available subject to the terms of
 # the GNU Lesser General Public License Version 2.1

 # Sebastian Rahtz / University of Oxford
 # copyright 2003

 # This stylesheet is derived from the OpenOffice to Docbook conversion
 #  Sun Microsystems Inc., October, 2000

 #  GNU Lesser General Public License Version 2.1
 #  =============================================
 #  Copyright 2000 by Sun Microsystems, Inc.
 #  901 San Antonio Road, Palo Alto, CA 94303, USA
 #
 #  This library is free software; you can redistribute it and/or
 #  modify it under the terms of the GNU Lesser General Public
 #  License version 2.1, as published by the Free Software Foundation.
 #
 #  This library is distributed in the hope that it will be useful,
 #  but WITHOUT ANY WARRANTY; without even the implied warranty of
 #  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 #  Lesser General Public License for more details.
 #
 #  You should have received a copy of the GNU Lesser General Public
 #  License along with this library; if not, write to the Free Software
 #  Foundation, Inc., 59 Temple Place, Suite 330, Boston,
 #  MA  02111-1307  USA
 #
 #
-->
<xsl:stylesheet xmlns:style="http://openoffice.org/2000/style" xmlns:text="http://openoffice.org/2000/text" xmlns:office="http://openoffice.org/2000/office" xmlns:table="http://openoffice.org/2000/table" xmlns:draw="http://openoffice.org/2000/drawing" xmlns:fo="http://www.w3.org/1999/XSL/Format" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:meta="http://openoffice.org/2000/meta" xmlns:number="http://openoffice.org/2000/datastyle" xmlns:svg="http://www.w3.org/2000/svg" xmlns:chart="http://openoffice.org/2000/chart" xmlns:dr3d="http://openoffice.org/2000/dr3d" xmlns:math="http://www.w3.org/1998/Math/MathML" xmlns:form="http://openoffice.org/2000/form" xmlns:script="http://openoffice.org/2000/script" xmlns:config="http://openoffice.org/2001/config" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0" office:class="text" exclude-result-prefixes="office meta  table number dc fo xlink chart math script xsl draw svg dr3d form config text style">

  <xsl:key name="headchildren" match="text:p | text:alphabetical-index
| table:table | text:span | text:ordered-list | office:annotation |
text:unordered-list | text:footnote | text:a | text:list-item |
draw:plugin | draw:text-box | text:footnote-body | text:section"
use="generate-id((..|preceding-sibling::text:h[@text:level='1']|preceding-sibling::text:h[@text:level='2']|preceding-sibling::text:h[@text:level='3']|preceding-sibling::text:h[@text:level='4']|preceding-sibling::text:h[@text:level='5'])[last()])"/>

  <xsl:key name="children" match="text:h[@text:level='2']"
use="generate-id(preceding-sibling::text:h[@text:level='1'][1])"/>

  <xsl:key name="children" match="text:h[@text:level='3']"
use="generate-id(preceding-sibling::text:h[@text:level='2' or
@text:level='1'][1])"/>

  <xsl:key name="children" match="text:h[@text:level='4']"
use="generate-id(preceding-sibling::text:h[@text:level='3' or
@text:level='2' or @text:level='1'][1])"/>

  <xsl:key name="children" match="text:h[@text:level='5']"
use="generate-id(preceding-sibling::text:h[@text:level='4' or
@text:level='3' or @text:level='2' or @text:level='1'][1])"/>

  <xsl:key name="secondary_children" match="text:p[@text:style-name =
'Index 2']"
use="generate-id(preceding-sibling::text:p[@text:style-name = 'Index
1'][1])"/>

  <xsl:key name="STYLES" match="style:style" use="@style:name"/>

  <xsl:param name="freestanding"></xsl:param>

  <xsl:variable name="META">
  </xsl:variable>

  <xsl:output encoding="utf-8" indent="yes"/>

  <xsl:strip-space elements="text:span"/>

  <xsl:variable name="document-title">
    <xsl:choose>
      <xsl:when test="/office:document-content/office:document-meta/dc:title">
        <xsl:value-of select="/office:document-content/office:document-meta/dc:title"/>
      </xsl:when>
      <xsl:when test="/office:document-content/office:body/text:p[@text:style-name='Title']">
        <xsl:value-of select="/office:document-content/office:body/text:p[@text:style-name='Title'][1]"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>Untitled Document</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>

  
<xsl:template match="/office:document-content">
<xsl:text disable-output-escaping="yes">&lt;!DOCTYPE TEI.2 SYSTEM &quot;http://www.oucs.ox.ac.uk/dtds/tei-oucs.dtd&quot; [
	</xsl:text>
    <xsl:for-each select="descendant::text:variable-decl">
      <xsl:variable name="name">
        <xsl:value-of select="@text:name"/>
      </xsl:variable>
      <xsl:if test="contains(@text:name,'entitydecl')">
        <xsl:text disable-output-escaping="yes">&lt;!ENTITY </xsl:text>
        <xsl:value-of select="substring-after(@text:name,'entitydecl_')"/>
        <xsl:text> &quot;</xsl:text>
        <xsl:value-of select="/descendant::text:variable-set[@text:name= $name][1]"/>
        <xsl:text disable-output-escaping="yes">&quot;&gt;</xsl:text>
      </xsl:if>
    </xsl:for-each>
    <xsl:text disable-output-escaping="yes">]&gt;</xsl:text>
    <TEI.2 lang="{/office:document-content/office:document-meta/dc:language}">
      <xsl:call-template name="teiHeader"/>
      <text>
        <body>
          <xsl:apply-templates/>
        </body>
      </text>
    </TEI.2>
  </xsl:template>

  
<xsl:template match="text:section">
    <xsl:choose>
      <xsl:when test="@text:name='ArticleInfo'">
      </xsl:when>
      <xsl:when test="@text:name='Abstract'">
        <div type="abstract">
          <xsl:apply-templates/>
        </div>
      </xsl:when>
      <xsl:when test="@text:name='Appendix'">
        <div>
          <xsl:apply-templates/>
        </div>
      </xsl:when>
      <xsl:otherwise>
        <xsl:variable name="sectvar">
          <xsl:text>div</xsl:text>
  <!--          <xsl:value-of select="count(ancestor::text:section)+1"/>-->
        </xsl:variable>
        <xsl:variable name="idvar">
          <xsl:text> id=&quot;</xsl:text>
          <xsl:value-of select="@text:name"/>
          <xsl:text>&quot;</xsl:text>
        </xsl:variable>
        <xsl:text disable-output-escaping="yes">&lt;</xsl:text>
        <xsl:value-of select="$sectvar"/>
        <xsl:value-of select="$idvar"/>
        <xsl:text disable-output-escaping="yes">&gt;</xsl:text>
        <xsl:apply-templates/>
        <xsl:text disable-output-escaping="yes">&lt;/</xsl:text>
        <xsl:value-of select="$sectvar"/>
        <xsl:text disable-output-escaping="yes">&gt;</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  
<xsl:template match="text:h[@text:level='1']">
    <xsl:choose>
      <xsl:when test=".='Abstract'">
        <div type="abstract">
          <xsl:apply-templates select="key('headchildren', generate-id())"/>
          <xsl:apply-templates select="key('children', generate-id())"/>
        </div>
      </xsl:when>
      <xsl:otherwise>
        <xsl:call-template name="make-section">
          <xsl:with-param name="current" select="@text:level"/>
          <xsl:with-param name="prev" select="1"/>
        </xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  
<xsl:template match="text:h[@text:level='2'] | text:h[@text:level='3']| text:h[@text:level='4'] | text:h[@text:level='5']">
    <xsl:variable name="level" select="@text:level"/>
    <xsl:if test="not(normalize-space(.)='')">
    <xsl:call-template name="make-section">
      <xsl:with-param name="current" select="$level"/>
      <xsl:with-param name="prev" select="preceding-sibling::text:h[@text:level &lt; $level][1]/@text:level "/>
    </xsl:call-template>
  </xsl:if>
</xsl:template>

  
<xsl:template name="make-section">
    <xsl:param name="current"/>
    <xsl:param name="prev"/>
    <div>
      <!--
<xsl:value-of select="@text:level"/>, <xsl:value-of select="normalize-space(.)"/>:
        <xsl:for-each select="key('children',generate-id())">
<xsl:text>  </xsl:text><xsl:value-of select="@text:level"/>, <xsl:value-of select="normalize-space(.)"/>;
        </xsl:for-each>
-->
    <xsl:call-template name="id.attribute"/>
    <xsl:choose>
      <xsl:when test="$current &gt; $prev+1">
          <head/>
          <xsl:call-template name="make-section">
            <xsl:with-param name="current" select="$current"/>
            <xsl:with-param name="prev" select="$prev +1"/>
          </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
          <head><xsl:apply-templates/></head>
          <xsl:variable name="this">
            <xsl:value-of select="generate-id()"/>
          </xsl:variable>
          <xsl:for-each select="key('headchildren', $this)">
            <xsl:if test="not(parent::text:h)">
              <xsl:apply-templates select="."/> 
            </xsl:if>
          </xsl:for-each>
          <xsl:apply-templates select="key('children', generate-id())"/>
      </xsl:otherwise>
    </xsl:choose>
    </div>
  </xsl:template>

  
<xsl:template match="text:variable-set|text:variable-get">
    <xsl:choose>
      <xsl:when test="contains(@text:name,'entitydecl')">
        <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
        <xsl:value-of select="substring-after(@text:name,'entitydecl_')"/>
        <xsl:text disable-output-escaping="yes">;</xsl:text>
      </xsl:when>
    </xsl:choose>
  </xsl:template>

  
<xsl:template match="text:p[@text:style-name='XMLComment']">
    <xsl:comment>
      <xsl:value-of select="."/>
    </xsl:comment>
  </xsl:template>

  
<xsl:template match="text:section[@text:name = 'ArticleInfo']/text:p[not(@text:style-name='XMLComment')]">
    <xsl:apply-templates/>
  </xsl:template>

  
<xsl:template match="text:ordered-list">
    <list type="ordered">
      <xsl:apply-templates/>
    </list>
  </xsl:template>
  
<xsl:template match="office:body">
    <xsl:apply-templates select="key('headchildren', generate-id())"/>
    <xsl:apply-templates select="text:h[@text:level='1']"/>
</xsl:template>
  
<xsl:template match="text:p[@text:style-name='Document Title']">
    <title>
      <xsl:apply-templates/>
    </title>
  </xsl:template>
  
<xsl:template match="text:p[@text:style-name='Author']">
  <author><xsl:apply-templates/></author>
</xsl:template>

<xsl:template match="text:p[@text:style-name='lg']">
  <lg><xsl:apply-templates/></lg>
</xsl:template>

<xsl:template match="text:p[@text:style-name='Title']">
  <title>
      <xsl:apply-templates/>
</title>
</xsl:template>
  
<xsl:template match="text:p[@text:style-name='Date']">
  <date>
      <xsl:apply-templates/>
  </date>
</xsl:template>
  
<xsl:template match="text:p[@text:style-name='signed']">
  <signed>
      <xsl:apply-templates/>
  </signed>
</xsl:template>
  
<xsl:template match="text:p[@text:style-name='Section Title']">
    <head>
      <xsl:apply-templates/>
    </head>
  </xsl:template>
  
<xsl:template match="text:p[@text:style-name='Appendix Title']">
    <head>
      <xsl:apply-templates/>
    </head>
  </xsl:template>
  
<xsl:template match="text:p[@text:style-name='VarList Item' or @text:style-name='List Contents']">
    <xsl:if test="not(preceding-sibling::text:p[@text:style-name='VarList Item'])">
      <xsl:text disable-output-escaping="yes">&lt;item&gt;</xsl:text>
    </xsl:if>
    <xsl:apply-templates/>
    <xsl:if test="not(following-sibling::text:p[@text:style-name='VarList Item'])">
      <xsl:text disable-output-escaping="yes">&lt;/item&gt;</xsl:text>
    </xsl:if>
  <xsl:variable name="next">
    <xsl:for-each select="following-sibling::text:p[1]">
        <xsl:value-of select="@text:style-name"/>      
    </xsl:for-each>
  </xsl:variable>
<xsl:choose>
    <xsl:when test="$next='VarList Term'"/>
    <xsl:when test="$next='List Heading'"/>
    <xsl:when test="$next='VarList Item'"/>
    <xsl:when test="$next='List Contents'"/>
    <xsl:otherwise>
      <xsl:text disable-output-escaping="yes">&lt;/list&gt;</xsl:text>
    </xsl:otherwise>
  </xsl:choose>

</xsl:template>
  
<xsl:template match="text:p[@text:style-name='VarList Term' or @text:style-name='List Heading']">
  <xsl:variable name="prev">
    <xsl:for-each select="preceding-sibling::text:p[1]">
        <xsl:value-of select="@text:style-name"/>      
    </xsl:for-each>
  </xsl:variable>
  <xsl:choose>
    <xsl:when test="$prev='VarList Term'"/>
    <xsl:when test="$prev='List Heading'"/>
    <xsl:when test="$prev='VarList Item'"/>
    <xsl:when test="$prev='List Contents'"/>
    <xsl:otherwise>
          <xsl:text disable-output-escaping="yes">&lt;list type="gloss"&gt;</xsl:text>
    </xsl:otherwise>
  </xsl:choose>
    <label>
      <xsl:apply-templates/>
    </label>
  </xsl:template>
  
<xsl:template match="text:p[@text:style-name='Screen']">
    <Screen>
      <xsl:apply-templates/>
    </Screen>
  </xsl:template>


<xsl:template match="text:p[@text:style-name='Output']">
    <Output>
      <xsl:apply-templates/>
    </Output>
  </xsl:template>
  
 
<xsl:template match="office:annotation/text:p">
    <note>
      <remark>
        <xsl:apply-templates/>
      </remark>
    </note>
  </xsl:template>

  
<xsl:template match="table:table">
<xsl:if test="not(parent::office:body)">
 <table rend="frame" id="{@table:name}">
   <xsl:if test="following-sibling::text:p[@text:style-name='Table']">
   <head>
   <xsl:value-of select="following-sibling::text:p[@text:style-name='Table']"/>
   </head>
  </xsl:if>
  <xsl:call-template name="generictable"/>
 </table>
</xsl:if>
</xsl:template>

  
<xsl:template name="generictable">
    <xsl:variable name="cells" select="count(descendant::table:table-cell)"/>
    <xsl:variable name="rows">
      <xsl:value-of select="count(descendant::table:table-row) "/>
    </xsl:variable>
    <xsl:variable name="cols">
      <xsl:value-of select="$cells div $rows"/>
    </xsl:variable>
    <xsl:variable name="numcols">
      <xsl:choose>
        <xsl:when test="child::table:table-column/@table:number-columns-repeated">
          <xsl:value-of select="number(table:table-column/@table:number-columns-repeated+1)"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="$cols"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>

    <xsl:apply-templates/>
  </xsl:template>
  
<xsl:template name="colspec">
    <xsl:param name="left"/>
    <xsl:if test="number($left &lt; ( table:table-column/@table:number-columns-repeated +2)  )">
      <xsl:element name="colspec">
        <xsl:attribute name="colnum">
          <xsl:value-of select="$left"/>
        </xsl:attribute>
        <xsl:attribute name="colname">c
                    <xsl:value-of select="$left"/>
                </xsl:attribute>
      </xsl:element>
      <xsl:call-template name="colspec">
        <xsl:with-param name="left" select="$left+1"/>
      </xsl:call-template>
    </xsl:if>
  </xsl:template>
  
<xsl:template match="table:table-column">
    <xsl:apply-templates/>
  </xsl:template>
  
<xsl:template match="table:table-header-rows">
    <xsl:apply-templates/>
</xsl:template>
  
<xsl:template match="table:table-header-rows/table:table-row">
    <row role="label">
      <xsl:apply-templates/>
    </row>
  </xsl:template>
  
<xsl:template match="table:table/table:table-row">
    <row>
      <xsl:apply-templates/>
    </row>
</xsl:template>

<xsl:template match="table:table-cell/text:h">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="table:table-cell">
  <cell>
      <xsl:if test="@table:number-columns-spanned &gt;'1'">
        <xsl:attribute name="cols">
          <xsl:value-of select="@table:number-columns-spanned"/>
        </xsl:attribute>
      </xsl:if>
      <xsl:if test="text:h">
	<xsl:attribute name="role">label</xsl:attribute>
      </xsl:if>
      <xsl:apply-templates/>
  </cell>
</xsl:template>
  
<xsl:template match="text:p">
    <xsl:choose>
      <xsl:when test="parent::text:list-item">
        <xsl:apply-templates/>
      </xsl:when>
      <xsl:when test="@text:style-name='Table'"/>
      <xsl:when test="normalize-space(.)=''"/>
      <xsl:when test="text:span[@text:style-name = 'XrefLabel']"/>
      <xsl:otherwise>
          <p>
            <xsl:call-template name="id.attribute"/>
            <xsl:call-template name="styleLookup"/>
          </p>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
<xsl:template match="text:unordered-list">
    <xsl:choose>
      <xsl:when test="@text:style-name='Var List'">
        <list>
          <xsl:apply-templates/>
        </list>
      </xsl:when>
      <xsl:when test="@text:style-name='UnOrdered List'">
        <list type="unordered">
          <xsl:apply-templates/>
        </list>
      </xsl:when>
      <xsl:otherwise>
        <list type="unordered">
          <xsl:apply-templates/>
        </list>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
<xsl:template match="text:list-item">
  <xsl:choose>
    <xsl:when test="parent::text:unordered-list/@text:style-name='Var List'">
      <item>
        <xsl:for-each select="text:p[@text:style-name='VarList Term']">
          <xsl:apply-templates select="."/>
        </xsl:for-each>
      </item>
    </xsl:when>
    <xsl:otherwise>
      <item>
        <xsl:apply-templates/>
      </item>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>
  
<xsl:template match="draw:plugin">
    <xptr url="{@xlink:href}"/>
  </xsl:template>
  
<xsl:template match="text:footnote-citation"/>
  
<xsl:template match="text:footnote">
    <note place="foot">
      <xsl:apply-templates/>
    </note>
  </xsl:template>
  
<xsl:template match="text:footnote-body">
    <xsl:apply-templates/>
  </xsl:template>

<xsl:template match="text:endnote-citation"/>
  
  
<xsl:template match="text:endnote">
    <note place="end">
      <xsl:apply-templates/>
    </note>
  </xsl:template>
  
<xsl:template match="text:endnote-body">
    <xsl:apply-templates/>
  </xsl:template>
  
<xsl:template match="draw:text-box"/>
  
<xsl:template match="draw:image">
    <xsl:choose>
      <xsl:when test="parent::text:p[@text:style-name='Mediaobject']">
        <figure rend="display" url="{@xlink:href}">
          <head>
            <xsl:value-of select="."/>
          </head>
        </figure>
      </xsl:when>
      <xsl:otherwise>
        <figure rend="inline" url="{@xlink:href}"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  
<xsl:template match="text:span">
  <xsl:variable name="Style">
    <xsl:value-of select="@text:style-name"/>
  </xsl:variable>
    <xsl:choose>
      <xsl:when test="$Style='Emphasis'">
        <emph>
          <xsl:apply-templates/>
        </emph>
      </xsl:when>
      <xsl:when test="$Style='Underline'">
        <hi rend="ul">
          <xsl:apply-templates/>
        </hi>
      </xsl:when>
      <xsl:when test="$Style='SmallCaps'">
        <hi rend="sc">
          <xsl:apply-templates/>
        </hi>
      </xsl:when>
      <xsl:when test="$Style='Emphasis Bold'">
        <hi>
          <xsl:apply-templates/>
        </hi>
      </xsl:when>
      <xsl:when test="$Style='Highlight'">
        <hi>
          <xsl:apply-templates/>
        </hi>
      </xsl:when>
      <xsl:when test="$Style='q'">
        <q>
          <xsl:choose>
            <xsl:when test="starts-with(.,'&#x2018;')">
               <xsl:value-of 
select="substring-before(substring-after(.,'&#x2018;'),'&#x2019;')"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:apply-templates/>
            </xsl:otherwise>
          </xsl:choose>
        </q>
      </xsl:when>
      <xsl:when test="$Style='date'">
        <date>
          <xsl:apply-templates/>
        </date>
      </xsl:when>
      <xsl:when test="$Style='l'">
        <l>
          <xsl:apply-templates/>
        </l>
      </xsl:when>
      <xsl:when test="$Style='Filespec'">
        <Filespec>
          <xsl:apply-templates/>
        </Filespec>
      </xsl:when>
      <xsl:when test="$Style='gi'">
        <gi>
          <xsl:apply-templates/>
        </gi>
      </xsl:when>
      <xsl:when test="$Style='Code'">
        <Code>
          <xsl:apply-templates/>
        </Code>
      </xsl:when>
      <xsl:when test="$Style='Input'">
        <Input>
          <xsl:apply-templates/>
        </Input>
      </xsl:when>
      <xsl:when test="$Style='Internet Link'">
          <xsl:apply-templates/>
      </xsl:when>
      <xsl:when test="$Style='SubScript'">
        <hi rend="sub">
          <xsl:apply-templates/>
        </hi>
      </xsl:when>
      <xsl:when test="$Style='SuperScript'">
        <hi rend="sup">
          <xsl:apply-templates/>
        </hi>
      </xsl:when>
      <xsl:when test="../text:h">
         <xsl:apply-templates/>
      </xsl:when>
      <xsl:when test="normalize-space(.)=''"/>
      <xsl:otherwise>
        <xsl:call-template name="styleLookup"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

<xsl:template name="styleLookup">
  <xsl:variable name="name">
    <xsl:value-of select="@text:style-name"/>
  </xsl:variable>
  <xsl:variable name="contents">
    <xsl:apply-templates/>
  </xsl:variable>
  <xsl:for-each select="key('STYLES',$name)">
      <xsl:choose>
       <xsl:when test="style:properties[@fo:font-weight='bold']">
        <hi><xsl:copy-of select="$contents"/></hi>
       </xsl:when>
      <xsl:when test="style:properties[@fo:font-style='italic']">
        <emph><xsl:copy-of select="$contents"/></emph>
      </xsl:when>
      <xsl:otherwise>
        <xsl:copy-of select="$contents"/>
      </xsl:otherwise>
    </xsl:choose>
    </xsl:for-each>
</xsl:template>
  
<xsl:template match="text:a">
    <xsl:choose>
      <xsl:when test="contains(@xlink:href,'://')">
        <xsl:choose>
          <xsl:when test=".=@xlink:href">
            <xptr url="{@xlink:href}"/>
          </xsl:when>
          <xsl:otherwise>
            <xref url="{@xlink:href}">
             <xsl:apply-templates/>
            </xref>        
          </xsl:otherwise>
        </xsl:choose>
      </xsl:when>
      <xsl:when test="not(contains(@xlink:href,'#'))">
        <xref url="{@xlink:href}">
          <xsl:apply-templates/>
        </xref>
      </xsl:when>
      <xsl:otherwise>
        <xsl:variable name="linkvar" 
          select="substring-after(@xlink:href,'#')"/>
        <xsl:choose>
          <xsl:when test=".=$linkvar">
            <ptr target="{$linkvar}"/>
          </xsl:when>
          <xsl:otherwise>
            <ref target="{$linkvar}">
              <xsl:apply-templates/>
            </ref>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
<xsl:template match="text:line-break">
  <xsl:if test="not(parent::text:span[@text:style-name='l'])">
    <lb/>
  </xsl:if>
</xsl:template>
  
<xsl:template match="text:tab-stop">
  <xsl:text disable-output-escaping="yes">	</xsl:text>
</xsl:template>
  
<xsl:template match="text:reference-ref">
    <ptr target="{@text:ref-name}"/>
  </xsl:template>
  
<xsl:template name="id.attribute">
    <xsl:if test="child::text:reference-mark-start">
      <xsl:attribute name="id">
        <xsl:value-of select="child::text:reference-mark-start/@text:name"/>
      </xsl:attribute>
    </xsl:if>
<!-- Constraints imposed by OOo method of displaying 
reference-ref text means that xreflabel and endterm are lost -->
  </xsl:template>
  
<xsl:template match="text:reference-mark-start"/>
  
<xsl:template match="text:reference-mark-end"/>
  
<xsl:template match="comment">
    <xsl:comment>
      <xsl:value-of select="."/>
    </xsl:comment>
  </xsl:template>
  
<xsl:template match="text:alphabetical-index-mark-start">
    <xsl:element name="indexterm">
      <xsl:attribute name="class">
        <xsl:text disable-output-escaping="yes">startofrange</xsl:text>
      </xsl:attribute>
      <xsl:attribute name="id">
        <xsl:value-of select="@text:id"/>
      </xsl:attribute>
<!--<xsl:if test="@text:key1">-->
      <xsl:element name="primary">
        <xsl:value-of select="@text:key1"/>
      </xsl:element>
<!--</xsl:if>-->
      <xsl:if test="@text:key2">
        <xsl:element name="secondary">
          <xsl:value-of select="@text:key2"/>
        </xsl:element>
      </xsl:if>
    </xsl:element>
  </xsl:template>
  
<xsl:template match="text:alphabetical-index-mark-end">
    <xsl:element name="indexterm">
      <xsl:attribute name="startref">
        <xsl:value-of select="@text:id"/>
      </xsl:attribute>
      <xsl:attribute name="class">
        <xsl:text disable-output-escaping="yes">endofrange</xsl:text>
      </xsl:attribute>
    </xsl:element>
  </xsl:template>
  
<xsl:template match="text:alphabetical-index">
    <xsl:element name="index">
      <xsl:element name="title">
        <xsl:value-of select="text:index-body/text:index-title/text:p"/>
      </xsl:element>
      <xsl:apply-templates select="text:index-body"/>
    </xsl:element>
  </xsl:template>
  
<xsl:template match="text:index-body">
    <xsl:for-each select="text:p[@text:style-name = 'Index 1']">
      <xsl:element name="indexentry">
        <xsl:element name="primaryie">
          <xsl:value-of select="."/>
        </xsl:element>
        <xsl:if test="key('secondary_children', generate-id())">
          <xsl:element name="secondaryie">
            <xsl:value-of select="key('secondary_children', generate-id())"/>
          </xsl:element>
        </xsl:if>
      </xsl:element>
    </xsl:for-each>
  </xsl:template>
<!--
These seem to have no obvious translation
-->
  
<xsl:template match="text:bookmark-start"/>

<xsl:template match="text:bookmark-end"/>
  
<xsl:template match="text:bookmark"/>
  
<xsl:template match="text:endnotes-configuration"/>
  
<xsl:template match="text:file-name"/>
  
<xsl:template match="text:footnotes-configuration"/>
  
<xsl:template match="text:linenumbering-configuration"/>
  
<xsl:template match="text:list-level-style-bullet"/>
  
<xsl:template match="text:list-level-style-number"/>
  
<xsl:template match="text:list-style"/>
  
<xsl:template match="text:outline-level-style"/>
  
<xsl:template match="text:outline-style"/>
  
<xsl:template match="text:s"/>

  
<xsl:template match="text:*">
      [[[UNTRANSLATED <xsl:value-of select="name(.)"/>
    <xsl:apply-templates/>]]]
</xsl:template>

<xsl:template name="teiHeader">
    <teiHeader>
      <fileDesc>
        <titleStmt>
          <title>
            <xsl:value-of select="$document-title"/>
          </title>
          <author>
<xsl:value-of select="/office:document-content/office:document-meta/meta:initial-creator"/>
          </author>
        </titleStmt>
        <editionStmt>
          <edition>
            <date>
<xsl:value-of select="/office:document-content/office:document-meta/meta:creation-date"/>
            </date>
          </edition>
        </editionStmt>
        <publicationStmt>
          <authority></authority>
          <address>
            <email></email>
          </address>
        </publicationStmt>
        <sourceDesc>
          <p><xsl:apply-templates select="/office:document-content/office:document-meta/meta:generator"/>Written by OpenOffice</p>
        </sourceDesc>
      </fileDesc>
      <profileDesc>
         <langUsage>
            <language id="{/office:document-content/office:document-meta/dc:language}">ISO <xsl:value-of select="/office:document-content/office:document-meta/dc:language"/></language>
         </langUsage>
      <xsl:if test="/office:document-content/office:document-meta/meta:keywords">
        <textClass>
          <keywords>
            <list>
              <xsl:for-each select="/office:document-content/office:document-meta/meta:keywords/meta:keyword">
                <item>
                  <xsl:value-of select="."/>
                </item>
              </xsl:for-each>
            </list>
          </keywords>
        </textClass>
    </xsl:if>
      </profileDesc>
      <revisionDesc>
        <change>
          <date> <xsl:apply-templates select="/office:document-content/office:document-meta/dc:date"/></date>
          <respStmt>
            <name> <xsl:apply-templates select="/office:document-content/office:document-meta/dc:creator"/></name>
          </respStmt>
          <item>revision</item>
        </change>
      </revisionDesc>
    </teiHeader>
  </xsl:template>


  <!-- sections of the OO format we don't need at present -->
  
<xsl:template match="office:automatic-styles"/>
  
<xsl:template match="office:font-decls"/>
  
<xsl:template match="office:document-meta"/>
  
<xsl:template match="office:script"/>
  
<xsl:template match="office:settings"/>
  
<xsl:template match="office:styles"/>
  
<xsl:template match="style:*"/>

  
<xsl:template match="dc:*">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="meta:creation-date">
  <xsl:apply-templates/>
</xsl:template>
  
<xsl:template match="meta:editing-cycles"/>
  
<xsl:template match="meta:editing-duration"/>
  
<xsl:template match="meta:generator"/>
  
<xsl:template match="meta:user-defined"/>

<!--
<xsl:template match="text()">
  <xsl:apply-templates select="normalize-space(.)"/>
</xsl:template>
-->
</xsl:stylesheet>
