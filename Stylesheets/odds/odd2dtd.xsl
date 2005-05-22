<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
    xmlns:xd="http://www.pnp-software.com/XSLTdoc"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:t="http://www.thaiopensource.com/ns/annotations" 
    xmlns:xs="http://www.w3.org/2001/XMLSchema" 
    xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0" 
    xmlns:edate="http://exslt.org/dates-and-times"
    xmlns:exsl="http://exslt.org/common"
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns:rng="http://relaxng.org/ns/structure/1.0"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    exclude-result-prefixes="a t xd tei fo exsl rng edate xs" 
    extension-element-prefixes="exsl edate rng"
    version="1.0">
 
  <xsl:import href="teiodds.xsl"/>
<xd:doc type="stylesheet">
    <xd:short>
    TEI stylesheet for making DTD from ODD
    </xd:short>
    <xd:detail>
    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
    </xd:detail>
    <xd:author>Sebastian Rahtz sebastian.rahtz@oucs.ox.ac.uk</xd:author>
    <xd:cvsId>$Id$</xd:cvsId>
    <xd:copyright>2005, TEI Consortium</xd:copyright>
  </xd:doc>


  <xsl:output method="text"/>
  <xsl:param name="verbose"></xsl:param>
  <xsl:param name="outputDir">DTD</xsl:param>
  <xsl:param name="appendixWords">  </xsl:param>
  <xsl:param name="filesuffix"></xsl:param>
  <xsl:param name="headingNumberSuffix">  </xsl:param>
  <xsl:param name="numberBackHeadings">  </xsl:param>
  <xsl:param name="numberFrontHeadings">  </xsl:param>
  <xsl:param name="numberHeadings">  </xsl:param>
  <xsl:param name="numberHeadingsDepth">  </xsl:param>
  <xsl:param name="oddmode">dtd</xsl:param>       
  <xsl:param name="prenumberedHeadings">  </xsl:param>
  <xsl:key name="Modules"   match="tei:moduleSpec" use="1"/>

<xsl:template match="/">
<xsl:choose>
  <xsl:when test=".//tei:schemaSpec">
    <xsl:apply-templates select=".//tei:schemaSpec" />
  </xsl:when>
  <xsl:otherwise>
    <xsl:call-template name="byModule"/>
  </xsl:otherwise>
</xsl:choose>
</xsl:template>

<xsl:template name="byModule">
  <xsl:for-each select="key('Modules',1)">
    <xsl:sort select="@ident" order="descending"/>
    <xsl:if test="$verbose='true'">
      <xsl:message>   file [<xsl:value-of select="@ident"/>]
    </xsl:message>
    </xsl:if>
    <exsl:document method="text" href="{$outputDir}/{@ident}.dtd">
      <xsl:text>&lt;!-- TEI P5 DTD. Generated </xsl:text>
      <xsl:value-of select="edate:date-time()"/>
      <xsl:call-template name="copyright"/>
 --&gt;
<xsl:if test="@type='core'">
<!-- legacy declaration of omissability indicators -->
&lt;!ENTITY % TEI.XML 'INCLUDE' &gt;
&lt;![%TEI.XML;[
&lt;!ENTITY % om.RO '' &gt;
&lt;!ENTITY % om.RR '' &gt;
]]&gt;
&lt;!ENTITY % om.RO '' &gt;
&lt;!ENTITY % om.RR '' &gt;
<xsl:call-template name="NameList"/>

<xsl:for-each select="key('DefClasses',1)">
  <xsl:choose>
    <xsl:when test="@type='atts'">    
      <xsl:apply-templates select="." mode="processAtts"/>
    </xsl:when>
    <xsl:when test="@type='model'">    
      <xsl:apply-templates select="." mode="processModel"/>
    </xsl:when>
    <xsl:when test="@type='default'">    
      <xsl:apply-templates select="." mode="processDefaultAtts"/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:apply-templates select="." mode="process"/>
    </xsl:otherwise>
  </xsl:choose>
</xsl:for-each>
</xsl:if>

<xsl:apply-templates select="key('ClassModule',@ident)"  mode="tangle"/>

<xsl:apply-templates select="key('ElementModule',@ident)"  mode="tangle">      
  <xsl:sort select="@ident"/>
</xsl:apply-templates>
<xsl:if test="@type='core'">
  <xsl:for-each select="key('Modules',1)">
    <xsl:sort select="@ident" order="descending"/>
    <xsl:if test="not(@type='core')">
&lt;!ENTITY % TEI.<xsl:value-of select="@ident"/> 'IGNORE' &gt;
<xsl:if test="key('DeclModules',concat(@ident,'-decl'))">
&lt;!ENTITY % file.<xsl:value-of select="@ident"/>-decl PUBLIC '-//TEI P5//<xsl:value-of select="tei:altIdent[@type='FPI']"/>//EN' '<xsl:value-of select="@ident"/>-decl.dtd' &gt;
&lt;![%TEI.<xsl:value-of select="@ident"/>;[
%file.<xsl:value-of select="@ident"/>-decl;
]]&gt;
</xsl:if>
    </xsl:if>
  </xsl:for-each>
</xsl:if>
&lt;!--
Start macro declarations
--&gt;
<xsl:for-each select="key('MacroModule',@ident)">
  <xsl:apply-templates select="." mode="tangle"/>
</xsl:for-each>
&lt;!--
End of macro declarations
--&gt;
<!-- set up the modules and their guards -->
<xsl:if test="@type='core'">
  <xsl:for-each select="key('Modules',1)">
    <xsl:sort select="@ident" order="descending"/>
    <xsl:if test="not(@type='core')">
&lt;![%TEI.<xsl:value-of select="@ident"/>;[
&lt;!ENTITY % file.<xsl:value-of select="@ident"/> PUBLIC '-//TEI P5//<xsl:value-of select="tei:altIdent[@type='FPI']"/>//EN' '<xsl:value-of select="@ident"/>.dtd' &gt;
%file.<xsl:value-of select="@ident"/>;
]]&gt;
    </xsl:if>
  </xsl:for-each>
</xsl:if>


    </exsl:document>
  </xsl:for-each>
</xsl:template>

<xsl:template match="tei:schemaSpec">
<xsl:choose>
  <xsl:when test="$outputDir='' or $outputDir='-'">
    <xsl:call-template name="schemaOut"/>
  </xsl:when>
  <xsl:otherwise>
    <exsl:document method="text" href="{$outputDir}/{@ident}.dtd">
      <xsl:call-template name="schemaOut"/>
    </exsl:document>
  </xsl:otherwise>
</xsl:choose>
</xsl:template>

<xsl:template name="schemaOut">
      <xsl:text>&lt;!-- TEI P5 DTD. Generated </xsl:text>
      <xsl:value-of select="edate:date-time()"/>
      <xsl:call-template name="copyright"/>
 --&gt;
&lt;!--predeclared classes --&gt;
<xsl:for-each select="key('DefClasses',1)">
  <xsl:choose>
    <xsl:when test="@type='atts'">    
      <xsl:apply-templates select="." mode="processAtts"/>
    </xsl:when>
    <xsl:when test="@type='model'">    
      <xsl:apply-templates select="." mode="processModel"/>
    </xsl:when>
    <xsl:when test="@type='default'">    
      <xsl:apply-templates select="." mode="processDefaultAtts"/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:apply-templates select="." mode="process"/>
    </xsl:otherwise>
  </xsl:choose>
</xsl:for-each>

&lt;!--classes--&gt;
      <xsl:apply-templates select="key('CLASSDOCS',1)"  mode="tangle"/>

&lt;!--macros--&gt;
      <xsl:for-each select="key('MACRODOCS',1)">
	<xsl:apply-templates select="." mode="tangle"/>
      </xsl:for-each>

&lt;!--elements--&gt;
      <xsl:apply-templates select="key('ELEMENTDOCS',1)"  mode="tangle">      
	<xsl:sort select="@ident"/>
      </xsl:apply-templates>

</xsl:template>

<xsl:template match="tei:macroSpec[@id='TEIGIS' or @xml:id='TEIGIS']" mode="tangle"/>

<xsl:template name="NameList">
  <!-- walk over all the elementSpec elements and make list of 
       elements -->
  <xsl:for-each select="//tei:elementSpec">
    <xsl:sort select="@ident"/>
    <xsl:if test="not(starts-with(@ident,'%'))">
      <ident id="{@ident}"/>
      <xsl:text>&lt;!ENTITY % n.</xsl:text>
      <xsl:value-of select="@ident"/>
      <xsl:text> "</xsl:text>
      <xsl:choose>
	<xsl:when test="tei:altIdent">
	  <xsl:value-of select="tei:altIdent"/>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:value-of select="@ident"/>
	</xsl:otherwise>
      </xsl:choose>
      <xsl:text>"&gt;&#10;</xsl:text>
    </xsl:if>
  </xsl:for-each>
</xsl:template>


<xsl:template name="italicize"/>
<xsl:template name="linkStyle"/>
<xsl:template name="makeAnchor"/>
<xsl:template name="makeLink"/>

<xsl:template match="tei:elemDecl" mode="literal">
  <xsl:text> %om.</xsl:text>
  <xsl:value-of select="@omit"/>
  <xsl:text>; </xsl:text>
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="rng:element[rng:anyName]">
  <xsl:text>ANY</xsl:text>
</xsl:template>

<xsl:template match="rng:zeroOrMore">
  <xsl:variable name="body">
  <xsl:call-template name="content">
    <xsl:with-param name="sep" select="','"/>
  </xsl:call-template>    
  </xsl:variable>
  <xsl:if test="not($body='')"><xsl:value-of select="$body"/>*</xsl:if>
</xsl:template>

<xsl:template match="rng:oneOrMore">
  <xsl:variable name="body">
  <xsl:call-template name="content">
    <xsl:with-param name="sep" select="','"/>
  </xsl:call-template>
  </xsl:variable>
  <xsl:if test="not($body='')"><xsl:value-of select="$body"/>+</xsl:if>
</xsl:template>

<xsl:template match="rng:optional">
  <xsl:variable name="body">
  <xsl:call-template name="content">
    <xsl:with-param name="sep" select="','"/>
  </xsl:call-template>
  </xsl:variable>
  <xsl:if test="not($body='')"><xsl:value-of select="$body"/>?</xsl:if>
</xsl:template>

<xsl:template match="rng:choice">
  <xsl:choose>
    <xsl:when test="rng:value">
      (<xsl:for-each select="rng:value">
      <xsl:value-of select="."/>
      <xsl:if test="following-sibling::rng:value">|</xsl:if>
      </xsl:for-each>)
    </xsl:when>
    <xsl:otherwise>
      <xsl:call-template name="content">
	<xsl:with-param name="sep" select="' | '"/>
      </xsl:call-template>     
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>


<xsl:template match="rng:group">
  <xsl:call-template name="content">
    <xsl:with-param name="sep" select="','"/>
  </xsl:call-template>
</xsl:template>

<xsl:template name="content">
  <xsl:param name="sep"/>
  <xsl:variable name="parent" select="local-name(..)"/>
  <xsl:variable name="contentbody">
    <xsl:variable name="members">
    <M>
      <xsl:for-each select="rng:*|processing-instruction()">
	<xsl:variable name="F">
	  <xsl:choose>
	    <xsl:when test="self::processing-instruction()">
	      <xsl:apply-templates select="."/>
	    </xsl:when>
	    <xsl:when test="self::rng:ref and $parameterize='false'">
	      <xsl:choose>
		<xsl:when test="key('CLASSES',@name)">
		  <xsl:variable name="exists">
		    <xsl:call-template name="checkClass">
		      <xsl:with-param name="id" select="@name"/>
		    </xsl:call-template>
		  </xsl:variable>	
		  <xsl:if test="not($exists='')">
		    <xsl:apply-templates select="."/>
		  </xsl:if>
		</xsl:when>
		<xsl:when test="key('MACROS',@name)">
		  <xsl:apply-templates select="."/>
		</xsl:when>
		<xsl:when test="key('ELEMENTS',@name)">
		  <xsl:apply-templates select="."/>
		</xsl:when>
	      </xsl:choose>
	    </xsl:when>
	    <xsl:otherwise>
	      <xsl:apply-templates select="."/>
	    </xsl:otherwise>
	  </xsl:choose>
	</xsl:variable>
	<xsl:if test="not($F='')">
	  <N>
	    <xsl:value-of select="$F"/>
	  </N>
	</xsl:if>
      </xsl:for-each>
    </M>
    </xsl:variable>
    <xsl:for-each select="exsl:node-set($members)/M">
      <xsl:choose>
	<xsl:when test="starts-with(N[1],'(') and count(N)=1"/>
	<xsl:when test="$parent='content' and count(N)=1">
	  <xsl:text>(</xsl:text>
	</xsl:when>
	<xsl:when test="count(N)=1 and starts-with(N,'%')">
	  <xsl:text>(</xsl:text>
	</xsl:when>
	<xsl:when test="count(N)&gt;1">
	  <xsl:text>(</xsl:text>
	</xsl:when>
      </xsl:choose>
      <xsl:for-each select="N">
	<xsl:value-of select="."/>
	<xsl:choose>
	  <xsl:when test="self::N[1]='|'"/>
	  <xsl:when test="self::N[1]='('"/>
	  <xsl:when test="self::N[1]=')'and position() &lt; last()">
	    <xsl:value-of select="$sep"/> 
	  </xsl:when>
	  <xsl:when test="following-sibling::N[1]='('"/>
	  <xsl:when test="following-sibling::N[1]=')'"/>
	  <xsl:when test="following-sibling::N[1]='|'"/>
	  <xsl:when test="position() &lt; last()">
	    <xsl:value-of select="$sep"/> 
	  </xsl:when>
	</xsl:choose>
      </xsl:for-each>
      <xsl:choose>
	<xsl:when test="starts-with(N[1],'(') and count(N)=1"/>
	<xsl:when test="$parent='content' and count(N)=1">
	  <xsl:text>)</xsl:text>
	</xsl:when>
	<xsl:when test="count(N)=1 and starts-with(N,'%')">
	  <xsl:text>)</xsl:text>
	</xsl:when>
	<xsl:when test="count(N)&gt;1">
	  <xsl:text>)</xsl:text>
	</xsl:when>
      </xsl:choose>
    </xsl:for-each>
  </xsl:variable>
  <xsl:choose>
    <xsl:when test="$contentbody=''"/>
    <xsl:when test="$contentbody='()'"/>
    <xsl:when test="contains($contentbody, '| ()')">
      <xsl:value-of select="substring-before($contentbody,'| ()')"/>
      <xsl:value-of select="substring-after($contentbody,'| ()')"/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:value-of select="$contentbody"/>
    </xsl:otherwise>
  </xsl:choose>

</xsl:template>


<xsl:template name="oldcontent">
<!--
  <xsl:choose>
    <xsl:when test="count(rng:*|processing-instruction())=1 and rng:ref">
      <xsl:text>(</xsl:text>
    </xsl:when>
    <xsl:when test="count(rng:*|processing-instruction())=1 and rng:element[@name='when']">
      <xsl:text>(</xsl:text>
    </xsl:when>
    <xsl:when test="count(rng:*|processing-instruction())&gt;1">
      <xsl:text>(</xsl:text>
    </xsl:when>
  </xsl:choose>
-->
  <xsl:for-each select="rng:*|processing-instruction()">
    <xsl:variable name="F">
      <xsl:value-of select="local-name((following-sibling::rng:*|following-sibling::processing-instruction())[1])"/>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="self::processing-instruction()">
	<xsl:apply-templates select="."/>
	<xsl:choose>
	  <xsl:when test="$F='teidtd'">
	    <xsl:value-of select="$sep"/>
	  </xsl:when>
	  <xsl:when test=".=')' and not($F='')">
	    <xsl:value-of select="$sep"/>
	  </xsl:when>                
	</xsl:choose>
      </xsl:when>
      <xsl:when test="$F='teidtd'">
	<xsl:apply-templates select="."/>
      </xsl:when>      
      <xsl:when test="$F='choice'">
	  <xsl:apply-templates select="."/>
	  <xsl:value-of select="$sep"/>
      </xsl:when>
      <xsl:when test="$F='group'">
	  <xsl:apply-templates select="."/>
	  <xsl:value-of select="$sep"/>
      </xsl:when>
      <xsl:when test="$F='oneOrMore'">
	<xsl:apply-templates select="."/>
	<xsl:value-of select="$sep"/>
      </xsl:when>
      <xsl:when test="$F='optional'">
	  <xsl:apply-templates select="."/>
	  <xsl:value-of select="$sep"/>
      </xsl:when>
      <xsl:when test="$F='ref' and $parameterize='false'">
	<xsl:apply-templates select="."/>
	  <xsl:for-each select="following-sibling::rng:ref[1]">
	    <xsl:choose>
	      <xsl:when test="key('CLASSES',@name)">
		<xsl:variable name="exists">
		  <xsl:call-template name="checkClass">
		    <xsl:with-param name="id" select="@name"/>
		  </xsl:call-template>
		</xsl:variable>	
		<xsl:if test="not($exists='')">
		  <xsl:value-of select="$sep"/>
		</xsl:if>
	      </xsl:when>
	    <xsl:when test="key('MACROS',@name)">
	      <xsl:value-of select="$sep"/>
	    </xsl:when>
	    <xsl:when test="key('ELEMENTS',@name)">
	      <xsl:value-of select="$sep"/>
	    </xsl:when>
	    </xsl:choose>
	  </xsl:for-each>
      </xsl:when>
      <xsl:when test="$F='ref'">
	<xsl:apply-templates select="."/>
	<xsl:value-of select="$sep"/>
      </xsl:when>
      <xsl:when test="$F='zeroOrMore'">
	<xsl:apply-templates select="."/>
	<xsl:value-of select="$sep"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:apply-templates select="."/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:for-each>
  <xsl:choose>
    <xsl:when test="count(rng:*|processing-instruction())=1 and rng:ref">
      <xsl:text>)</xsl:text>
    </xsl:when>
    <xsl:when test="count(rng:*|processing-instruction())=1 and rng:element[@name='when']">
      <xsl:text>)</xsl:text>
    </xsl:when>
    <xsl:when test="count(rng:*|processing-instruction())&gt;1">
      <xsl:text>)</xsl:text>
    </xsl:when>
  </xsl:choose>
</xsl:template>

<xsl:template match="tei:datatype">
  <xsl:choose>
    <xsl:when test="rng:data/@type='ID'">
      <xsl:text> ID</xsl:text>
    </xsl:when>
    
    <xsl:when test="rng:data/@type='IDREF'">
      <xsl:text> IDREF</xsl:text>
    </xsl:when>
    
    <xsl:when test="rng:data/@type='IDREFS'">
      <xsl:text> IDREFS</xsl:text>
    </xsl:when>
    
    <xsl:when test="rng:data/@type='NMTOKEN'">
      <xsl:text> NMTOKEN</xsl:text>
    </xsl:when>
    
    <xsl:when test="rng:data/@type='NMTOKENS'">
      <xsl:text> NMTOKENS</xsl:text>
    </xsl:when>
    
    <xsl:otherwise>
      <xsl:text> CDATA</xsl:text>
    </xsl:otherwise>
  </xsl:choose>
  
</xsl:template>

<xsl:template match="rng:element">
  <xsl:choose>
    <xsl:when test="parent::tei:content/parent::tei:macroSpec">
      <xsl:call-template name="topLevel"/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:value-of select="@name"/>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<!--  <xsl:template match="tei:text()"/>-->

<xsl:template match="rng:empty">
  <xsl:text>EMPTY</xsl:text>
</xsl:template>

<xsl:template match="rng:data">
  <xsl:choose>
    <xsl:when test="@type='ID'"> ID </xsl:when>
    <xsl:when test="@type='IDREF'"> IDREF </xsl:when>
    <xsl:when test="@type='IDREFS'"> IDREFS </xsl:when>
    <xsl:otherwise> CDATA </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template match="rng:text">
  <xsl:choose>
    <xsl:when test="parent::tei:content/parent::tei:macroSpec">
      <xsl:call-template name="topLevel"/>
    </xsl:when>
    <xsl:when test="parent::tei:content">
      <xsl:text> (#PCDATA)</xsl:text>              
    </xsl:when>
    <xsl:otherwise>
      <xsl:text> #PCDATA</xsl:text>              
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template match="tei:macroSpec[@type='dt']/tei:content/rng:text">
  <xsl:text> CDATA</xsl:text>              
</xsl:template>

<xsl:template match="tei:macroSpec[@type='epe']/tei:content/rng:text">
  <xsl:text>CDATA</xsl:text>              
</xsl:template>


<xsl:template match="rng:text" mode="simple">
  <xsl:choose>
    <xsl:when test="parent::tei:content/parent::tei:macroSpec[@type='dt']">
      <xsl:text> CDATA</xsl:text>              
    </xsl:when>
    <xsl:when test="parent::tei:content and not(following-sibling::rng:*) and not (preceding-sibling::rng:*)">
      <xsl:text>(#PCDATA)</xsl:text>
    </xsl:when>
    <xsl:when test="parent::tei:content and not(following-sibling::rng:*) and not (preceding-sibling::rng:*)">
      <xsl:text>(#PCDATA)</xsl:text>
    </xsl:when>
    <xsl:otherwise>
      <xsl:text>#PCDATA</xsl:text>      
    </xsl:otherwise>
    
  </xsl:choose>
  
</xsl:template>

<xsl:template match="processing-instruction()[name(.)='teidtd']">
  <xsl:value-of select="."/>
</xsl:template>

<xsl:template match="processing-instruction()[name(.)='teidtd']" mode="tangle">
  <xsl:value-of select="."/>
</xsl:template>

<xsl:template match="rng:ref">
  <xsl:choose>
    <xsl:when test="parent::tei:content/parent::tei:macroSpec">
      <xsl:call-template name="topLevel"/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:call-template name="refbody"/>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template match="rng:ref" mode="simple">
      <xsl:call-template name="refbody"/>
</xsl:template>

<xsl:template name="refbody">
<xsl:variable name="R">
  <xsl:choose>
    <xsl:when test="$parameterize='false'">
      <xsl:choose>
	<xsl:when test="key('CLASSES',@name)">
	  <xsl:variable name="exists">
	    <xsl:call-template name="checkClass">
	      <xsl:with-param name="id" select="@name"/>
	    </xsl:call-template>
	  </xsl:variable>	
	  <xsl:if test="not($exists='')">
	    <xsl:text>%</xsl:text>
	    <xsl:value-of select="@name"/>   
	    <xsl:text>;</xsl:text>
	  </xsl:if>
	</xsl:when>
	<xsl:when test="key('MACROS',@name)">
	  <xsl:text>%</xsl:text>
	  <xsl:value-of select="@name"/>   
	  <xsl:text>;</xsl:text>
	</xsl:when>
	<xsl:when test="key('ELEMENTS',@name)">
	  <xsl:for-each select="key('ELEMENTS',@name)">
	    <xsl:choose>
	      <xsl:when test="tei:altIdent">
		<xsl:value-of select="normalize-space(tei:altIdent)"/>
	      </xsl:when>
	      <xsl:otherwise>
		<xsl:value-of select="@ident"/>
	      </xsl:otherwise>
	    </xsl:choose>
	  </xsl:for-each>
	</xsl:when>
      </xsl:choose>
    </xsl:when>
    <xsl:when test="@name='IGNORE' or @name='INCLUDE'">
	<xsl:value-of select="@name"/>   
    </xsl:when>
    <xsl:when test="contains(@name, '.')">
      <xsl:text>%</xsl:text>
      <xsl:value-of select="@name"/>   
      <xsl:text>;</xsl:text>
    </xsl:when>
    <xsl:otherwise>
      <xsl:value-of select="@name"/>   
    </xsl:otherwise>
  </xsl:choose>
</xsl:variable>
<xsl:value-of select="$R"/>
</xsl:template>

<xsl:template name="topLevel">
  <xsl:variable name="unit">
      <xsl:apply-templates select="." mode="simple"/>
  </xsl:variable>
  <xsl:if test="not($unit='')">
    <xsl:if test="preceding-sibling::rng:*">
      <xsl:choose>
	<xsl:when test="preceding-sibling::processing-instruction()"></xsl:when>
	<xsl:otherwise>
	  <xsl:text>|</xsl:text>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:if>
      <xsl:value-of select="$unit"/>
  </xsl:if>
</xsl:template>

<xsl:template match="rng:element" mode="simple">
  <xsl:value-of select="@name"/>
</xsl:template>

<xsl:template match="tei:gloss|tei:remarks|tei:desc"/>

<xsl:template match="tei:macroSpec" mode="tangle">
 <xsl:if test="$verbose='true'">
   <xsl:message>     .... macroSpec <xsl:value-of
   select="@ident"/> occurs <xsl:value-of
   select="count(key('MACROS',@ident))"/>, <xsl:value-of select="@module"/></xsl:message>
 </xsl:if>
<xsl:choose>
<!-- This is an odd situation. A macro is declared twice,
and we want the occurence of from a module called "xxx-decl". 
However, we want it at the position of the first declaration.
So, at the first, process the second; at the second, do nothing.
-->
  <xsl:when test="count(key('MACROS',@ident))=2 and $parameterize='false'">
    <xsl:choose>
      <xsl:when test="contains(@module,'-decl')"/>
      <xsl:otherwise>
	<xsl:for-each  select="key('MACROS',@ident)[2]">
	    <xsl:call-template name="macroBody"/>
	</xsl:for-each>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:when>
  <xsl:when test="@depend and $parameterize='true'">
    <xsl:if test="$verbose='true'">
      <xsl:message>Dependency on <xsl:value-of select="@depend"/> for
      <xsl:value-of select="@ident"/></xsl:message>
    </xsl:if>
    <xsl:text>&#10; &lt;![%TEI.</xsl:text>
    <xsl:value-of select="@depend"/>;[
    <xsl:call-template name="macroBody"/>
    <xsl:text>&#10;]]&gt;</xsl:text>
  </xsl:when>
  <xsl:when test="@depend and count(key('ElementModule',@depend))=0">
    <xsl:if test="$verbose='true'">
      <xsl:message>Dependency on <xsl:value-of select="@depend"/>, but
      not used in this schema </xsl:message>
    </xsl:if>
  </xsl:when>
  <xsl:otherwise>
    <xsl:call-template name="macroBody"/>
  </xsl:otherwise>
</xsl:choose>
</xsl:template>

<xsl:template name="macroBody">
  <xsl:text>
&lt;!ENTITY </xsl:text>   
  <xsl:if test="@type='defaultpe' or @type='pe' or @type='epe' or @type='dt'">
    <xsl:text>%</xsl:text>
  </xsl:if>
  <xsl:text> </xsl:text>
  <xsl:value-of select="@ident"/>
  <xsl:text> '</xsl:text>
  <xsl:for-each select="tei:content">
    <xsl:apply-templates
     select="tei:*|rng:*|processing-instruction()"/>
  </xsl:for-each>
  <xsl:text>' &gt;&#10;</xsl:text>
</xsl:template>

<xsl:template match="tei:elementSpec" mode="tangle">
  <xsl:if test="$verbose='true'">
    <xsl:message>     .... elementSpec <xsl:value-of
    select="@ident"/></xsl:message>
  </xsl:if>
  <xsl:choose>
    <xsl:when test="starts-with(@ident,'%')">
      <xsl:call-template name="elementBody"/>
    </xsl:when>
    <xsl:when test="$parameterize='false'">
      <xsl:call-template name="elementBody"/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:text>&#10;&lt;!ENTITY % </xsl:text>
      <xsl:value-of select="@ident"/>
      <xsl:text> 'INCLUDE' &gt;&#10;&lt;![ %</xsl:text>
      <xsl:value-of select="@ident"/>
      <xsl:text>; [&#10;</xsl:text>
      <xsl:call-template name="elementBody"/>
      <xsl:text>&#10;]]&gt;&#10;</xsl:text>
    </xsl:otherwise>
  </xsl:choose>

</xsl:template>


<xsl:template name="elementBody">

  <xsl:variable name="ename">
    <xsl:choose>
      <xsl:when test="$parameterize='false'">
	<xsl:choose>
	  <xsl:when test="tei:altIdent">
	    <xsl:value-of select="normalize-space(tei:altIdent)"/>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:value-of select="@ident"/>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:when>
      <xsl:when test="starts-with(@ident,'%')">
	<xsl:value-of select="@ident"/>    
      </xsl:when>
      <xsl:otherwise>
	<xsl:text>%n.</xsl:text>
	<xsl:value-of select="@ident"/>    
	<xsl:text>;</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  <xsl:text>&#10;&lt;!--doc:</xsl:text>
  <xsl:choose>
    <xsl:when test="not(tei:desc='')">
      <xsl:value-of select="tei:desc"/>
    </xsl:when>
    <xsl:when test="not(tei:gloss='')">
      <xsl:value-of select="tei:gloss"/>
    </xsl:when>
  </xsl:choose>
  <xsl:text> --&gt;&#10;&lt;!ELEMENT </xsl:text>
  <xsl:value-of select="$ename"/>
  <xsl:if test="$parameterize='true'">
    <xsl:text> %om.RR;</xsl:text>
  </xsl:if>
  <xsl:text> </xsl:text>
  <xsl:apply-templates select="tei:content/rng:*" />
  <xsl:text>&gt;</xsl:text>
  <xsl:text>&#10;&lt;!ATTLIST </xsl:text>
  <xsl:value-of select="$ename"/>
  <xsl:if test="$parameterize='true'">
    <xsl:text>&#10; %tei.global.attributes;</xsl:text>
  </xsl:if>
  <xsl:if test="$parameterize='true'">
    <xsl:apply-templates select="tei:classes/tei:memberOf" mode="tangleAtts"/>
  </xsl:if>
  <xsl:call-template name="attributeList"/>
  <xsl:if test="not(starts-with(@ident,'%'))">
    <xsl:text>&#10; TEIform CDATA &#39;</xsl:text>
    <xsl:value-of select="@ident"/>
    <xsl:text>&#39; </xsl:text>
  </xsl:if>
  <xsl:text> &gt;</xsl:text>
</xsl:template>

<xsl:template name="attclasses">
  <xsl:for-each select="tei:classes/tei:memberOf">
    <xsl:for-each select="key('IDENTS',@key)[1]">
      <xsl:if test="@type='atts'">
	%<xsl:value-of select="@ident"/>
	<xsl:text>.attributes;</xsl:text>
      </xsl:if>
    </xsl:for-each>
  </xsl:for-each>
</xsl:template>

<xsl:template name="classAtt">
  <xsl:if test="$verbose='true'">
    <xsl:message>    ....  <xsl:value-of
    select="@ident"/>.attributes</xsl:message>  
  </xsl:if>
  <xsl:variable name="thisclass">
    <xsl:value-of select="@ident"/>   
  </xsl:variable>
  <xsl:choose>
    <xsl:when test="$parameterize='true'">
      <xsl:text>&#10;&lt;!ENTITY % </xsl:text>
      <xsl:value-of select="$thisclass"/>
      <xsl:text>.attributes &#39;</xsl:text>
      <xsl:call-template name="attclasses"/>
      <xsl:call-template name="attributeList"/>
      <xsl:text>&#39;&gt; </xsl:text>
    </xsl:when>
    <xsl:otherwise>
      <xsl:for-each select="tei:attList/tei:attDef">
	<xsl:text>&#10;&lt;!ENTITY % </xsl:text>
	<xsl:value-of select="$thisclass"/>
	<xsl:text>.attribute.</xsl:text>
	<xsl:value-of select="translate(@ident,':','')"/>
	<xsl:text> '</xsl:text>
	<xsl:apply-templates select="." mode="tangle"/>
	<xsl:text>'&gt;&#10;</xsl:text>
      </xsl:for-each>
    </xsl:otherwise>
  </xsl:choose>
  
</xsl:template>

<xsl:template match="tei:classSpec" mode="tagatts">
  <xsl:if test="@type='atts' or @type='both'">
    <xsl:if test="$verbose='true'">
      <xsl:message>      .... added contents of [%<xsl:value-of
      select="@ident"/>.attributes;]</xsl:message> 
    </xsl:if>
   <xsl:text>&#10; %</xsl:text>
   <xsl:value-of select="@ident"/>
   <xsl:text>.attributes;</xsl:text>
  </xsl:if>
</xsl:template>


<xsl:template match="tei:moduleRef" mode="tangle">
  <xsl:if test="$verbose='true'">
    <xsl:message>  moduleRef to <xsl:value-of select="@key"/>
    </xsl:message>
  </xsl:if>
  <xsl:text>
&lt;!ENTITY </xsl:text>   
  <xsl:text>%</xsl:text>
  <xsl:text> file.</xsl:text>
  <xsl:value-of select="@key"/>
  <xsl:text> </xsl:text>
  <xsl:text>PUBLIC</xsl:text>
  <xsl:text> '-//TEI P5//</xsl:text>
  <xsl:value-of select="key('FILES',@key)/tei:altIdent[@type='FPI']"/>
  <xsl:text>//EN' '</xsl:text>
  <xsl:value-of select="@key"/>
  <xsl:text>.dtd'</xsl:text>
  <xsl:text> &gt;&#10;</xsl:text>
  
  <xsl:text>%file.</xsl:text>
  <xsl:value-of select="@key"/>
  <xsl:text>;&#10;</xsl:text>
</xsl:template>


<xsl:template match="tei:classSpec" mode="tangle">
  <xsl:if test="$verbose='true'">
    <xsl:message>    .. classSpec <xsl:value-of
    select="@ident"/>,<xsl:value-of select="@type"/>  </xsl:message>
  </xsl:if>
  <xsl:choose>
    <xsl:when test="@type='atts'">
      <xsl:call-template name="classAtt"/>
    </xsl:when>
    
    <xsl:when test="@type='model'">
      <xsl:call-template name="classModel"/>
    </xsl:when>
   
    <xsl:when test="@type='both'">
      <xsl:call-template name="classAtt"/>
      <xsl:call-template name="classModel"/>
    </xsl:when>
  </xsl:choose>
</xsl:template>

<xsl:template match="tei:classSpec" mode="process">
  <xsl:choose>    
    <xsl:when test="@type='atts'">
      <xsl:call-template name="classAtt"/>
    </xsl:when>
    
    <xsl:when test="@type='model'">
      <xsl:call-template name="classModel"/>
    </xsl:when>
    
    <xsl:when test="@type='both'">
      <xsl:call-template name="classModel"/>
      <!--    <xsl:call-template name="classAtt"/>-->
    </xsl:when>
  </xsl:choose>
</xsl:template>



<xsl:template match="tei:classSpec" mode="processModel">
  <xsl:call-template name="classModel"/>
</xsl:template>

<xsl:template match="tei:classSpec" mode="processAtts">
  <xsl:call-template name="classAtt"/>
</xsl:template>


<xsl:template name="classModel">
  <xsl:if test="$verbose='true'">
    <xsl:message>    ....model <xsl:value-of
    select="@ident"/></xsl:message>
  </xsl:if>
  <xsl:variable name="thisclass">
    <xsl:value-of select="@ident"/>   
  </xsl:variable>
  <xsl:if test="$parameterize='true'">
    <xsl:text>&#10;&lt;!ENTITY % x.</xsl:text>
    <xsl:value-of select="$thisclass"/>
    <xsl:text>&#10;"" &gt;</xsl:text>
  </xsl:if>
  <xsl:text>&#10;&lt;!ENTITY % </xsl:text>
    <xsl:value-of select="$thisclass"/>
    <xsl:text> "</xsl:text>
    <xsl:if test="$parameterize='true'">
      <xsl:text>%x.</xsl:text>
      <xsl:value-of select="$thisclass"/>
      <xsl:text>; </xsl:text>
    </xsl:if>
    <xsl:variable name="members">
      <M>
	<xsl:for-each select="key('CLASSMEMBERS',@ident)">
	  <xsl:sort select="@ident"/>
	  <xsl:variable name="N">
	    <xsl:choose>
	      <xsl:when test="$parameterize='false'">
		<xsl:choose>
		  <xsl:when test="key('CLASSES',@ident)">
		    <xsl:variable name="exists">
		      <xsl:call-template name="checkClass">
			<xsl:with-param name="id" select="@ident"/>
		      </xsl:call-template>
		    </xsl:variable>	
		    <xsl:if test="not($exists='')">
		      <xsl:text>%</xsl:text>
		      <xsl:value-of select="@ident"/>
		      <xsl:text>;</xsl:text>
		    </xsl:if>
		  </xsl:when>
		  <xsl:when test="key('MACROS',@ident)">
		      <xsl:text>%</xsl:text>
		      <xsl:value-of select="@ident"/>
		      <xsl:text>;</xsl:text>
		  </xsl:when>
		  <xsl:when test="key('ELEMENTS',@ident)">
		    <xsl:choose>
		      <xsl:when test="tei:altIdent">
			<xsl:value-of select="normalize-space(tei:altIdent)"/>
		      </xsl:when>
		      <xsl:otherwise>
			<xsl:value-of select="@ident"/>
		      </xsl:otherwise>
		    </xsl:choose>
		  </xsl:when>
		</xsl:choose>
	      </xsl:when>
	      <xsl:when test="self::tei:elementSpec">
		<xsl:text>%n.</xsl:text>
		<xsl:value-of select="@ident"/>
		<xsl:text>;</xsl:text>
	      </xsl:when>
	      <xsl:otherwise>
		<xsl:text>%</xsl:text>
		<xsl:value-of select="@ident"/>
		<xsl:text>;</xsl:text>
	      </xsl:otherwise>
	    </xsl:choose>
	  </xsl:variable>
	  <xsl:if test="not($N='')">
	    <N>
	      <xsl:if test="starts-with($N,'%tei.')">
		<xsl:attribute name="type">class</xsl:attribute>
	      </xsl:if>
	      <xsl:value-of select="$N"/>
	    </N>
	  </xsl:if>
	</xsl:for-each>
      </M>
    </xsl:variable>
<!-- a class model needs bracketing if all its members are also classes-->
    <xsl:for-each select="exsl:node-set($members)/M">
      <xsl:if test="count(N[@type]) = 2 and count(N[@type])=count(N)">(</xsl:if>
      <xsl:for-each select="N">
	<xsl:value-of select="."/>
	<xsl:if test="position() &lt; last()"> | </xsl:if>
      </xsl:for-each>
      <xsl:if test="count(N) = 2 and count(N[@type])=count(N)">)</xsl:if>
    </xsl:for-each>
    <xsl:text>"&gt; </xsl:text>
    
</xsl:template>


<xsl:template match="tei:commDecl" mode="tangle">
  <xsl:text>
  &lt;!--</xsl:text>
  <xsl:apply-templates/>
  <xsl:text>--&gt;</xsl:text>        
</xsl:template>

<xsl:template match="tei:classSpec" mode="processDefaultAtts">
  <xsl:text>
&lt;!ENTITY % </xsl:text><xsl:value-of select="@ident"/>
    <xsl:text>.attributes &#39;&#39;&gt;</xsl:text>
</xsl:template>

<xsl:template name="attributeList">
  <xsl:apply-templates select="tei:attList/tei:*"  mode="tangle"/>
</xsl:template>
    
<xsl:template match="tei:attDef" mode="tangle">
  <xsl:text>&#10; </xsl:text>
  <xsl:choose>
    <xsl:when test="tei:altIdent">
      <xsl:value-of select="normalize-space(tei:altIdent)"/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:value-of select="@ident"/>
    </xsl:otherwise>
  </xsl:choose>
  <xsl:text> </xsl:text>
  <xsl:choose>
    <xsl:when test="tei:valList[@type='closed']">
      <xsl:text> (</xsl:text>
      <xsl:for-each select="tei:valList/tei:valItem">
	<xsl:value-of select="@ident"/>
	<xsl:if test="following-sibling::tei:valItem">|</xsl:if> 
      </xsl:for-each>
      <xsl:text>)</xsl:text>
    </xsl:when>
    <xsl:when test="tei:datatype[rng:ref/@name='datatype.Code']">
      <xsl:text> CDATA </xsl:text>
    </xsl:when>
    <xsl:when test="tei:datatype/rng:*">
	<xsl:apply-templates select="tei:datatype"/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:text> CDATA </xsl:text>
    </xsl:otherwise>
  </xsl:choose>
  <xsl:choose>
    <xsl:when test="string-length(tei:defaultVal) &gt;0">
      <xsl:text> &#34;</xsl:text>
      <xsl:value-of select="tei:defaultVal"/>
      <xsl:text>&#34; </xsl:text>
    </xsl:when>
    <xsl:when test="@usage='req'">
      <xsl:text> #REQUIRED</xsl:text>        
    </xsl:when>
    <xsl:otherwise>
      <xsl:text> #IMPLIED</xsl:text>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template name="bitOut">
  <xsl:param name="grammar"/>
  <xsl:param name="TAG"/>
  <xsl:param name="content"/>
  <xsl:copy-of select="$content"/>
</xsl:template>

<xsl:template name="CR">
<xsl:text>&#10;</xsl:text>
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
      <xsl:variable name="loc">
	<xsl:value-of select="$TEISERVER"/>
	<xsl:text>copytag.xq?name=</xsl:text>
	<xsl:value-of select="@key"/>
      </xsl:variable>
      <xsl:if test="$verbose">
	<xsl:if test="$verbose='true'">
	  <xsl:message>Accessing TEISERVER: <xsl:value-of
	select="$loc"/></xsl:message>
	</xsl:if>
      </xsl:if>
      <xsl:apply-templates select="document($loc)/tei:TEI/*" mode="tagatts"/>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template match="tei:attRef" mode="tangle">
  <xsl:text>&#10; %</xsl:text>
  <xsl:value-of select="@class"/>
  <xsl:text>.attribute.</xsl:text>
  <xsl:value-of select="@key"/>
  <xsl:text>;</xsl:text>
</xsl:template>

<xsl:template name="checkClass">
  <xsl:param name="id"/>
<!-- should a class be included?
 * it must exist in the ODD spec
 * it must have other classes which point to it
 * whatever it points to must have members which are in the ODD
-->
  <xsl:for-each select="key('CLASSMEMBERS',$id)">
    <xsl:choose>
      <xsl:when test="self::tei:elementSpec and key('ELEMENTS',@ident)">
	<xsl:text>true</xsl:text>
      </xsl:when>
      <xsl:when test="self::tei:macroSpec and key('MACROS',@ident)">
	<xsl:text>true</xsl:text>
      </xsl:when>
      <xsl:when test="self::tei:classSpec and key('CLASSES',@ident)">
	<xsl:call-template name="checkClass">
	  <xsl:with-param name="id" select="@ident"/>
	</xsl:call-template>
      </xsl:when>
    </xsl:choose>
  </xsl:for-each>
</xsl:template>
</xsl:stylesheet>
