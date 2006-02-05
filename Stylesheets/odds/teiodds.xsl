<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
    xmlns:xd="http://www.pnp-software.com/XSLTdoc"
    xmlns:s="http://www.ascc.net/xml/schematron" 
    xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
    xmlns:xs="http://www.w3.org/2001/XMLSchema" 
    xmlns:rng="http://relaxng.org/ns/structure/1.0"
    xmlns:teix="http://www.tei-c.org/ns/Examples"
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    xmlns:edate="http://exslt.org/dates-and-times"
    xmlns:exsl="http://exslt.org/common"
    xmlns:estr="http://exslt.org/strings"
    exclude-result-prefixes="exsl estr rng edate teix fo a tei s xd xs" 
    extension-element-prefixes="edate exsl estr"
    version="1.0">
  
  <xsl:import href="../common/verbatim.xsl"/>
  <xd:doc type="stylesheet">
    <xd:short>
      TEI stylesheet for processing TEI ODD markup
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
  
  <xsl:include href="RngToRnc.xsl"/>
  
  
  <xsl:output encoding="utf-8" method="xml" indent="yes"/>
  
  <xsl:param name="simplify">false</xsl:param>
  <xsl:param name="localsource"/>
  <xsl:param name="TEIC">false</xsl:param>
  <xsl:param name="patternPrefix"/>
  <xsl:param name="lang">en</xsl:param>
  <xsl:param name="lookupDatabase">false</xsl:param>
  <xsl:param name="TEISERVER">http://localhost/Query/</xsl:param>
  <xsl:param name="verbose">false</xsl:param>
  <xsl:param name="schemaBaseURL">http://localhost/schema/relaxng/</xsl:param>
  
  <xsl:key name="LOCALIDENTS"   match="tei:*"   use="@ident"/>
  <xsl:key name="MACROS"   match="tei:macroSpec" use="@ident"/>
  <xsl:key name="ELEMENTS" match="tei:elementSpec" use="@ident"/>
  <xsl:key name="CLASSES"  match="tei:classSpec" use="@ident"/>
  <xsl:key name="RNGREFS" match="rng:ref" use="@name"/>
  <xsl:key name="CLASSMEMBERS" match="tei:elementSpec|tei:classSpec" use="tei:classes/tei:memberOf/@key"/>
  <xsl:key name="IDENTS"   match="tei:elementSpec|tei:classSpec|tei:macroSpec"   use="@ident"/>
  <xsl:key name="IDS"      match="tei:*[@xml:id]" use="@xml:id"/>
  <xsl:key name="DATATYPES" match="tei:macroSpec[@type='dt']" use='1'/>
  <xsl:key name="MACRODOCS" match="tei:macroSpec" use='1'/>
  <xsl:key name="CLASSDOCS" match="tei:classSpec" use='1'/>
  <xsl:key name="ELEMENTDOCS" match="tei:elementSpec" use='1'/>
  <xsl:key name='NameToID' match="tei:*" use="@ident"/>
  <xsl:key name="ElementModule" match="tei:elementSpec" use="@module"/>
  <xsl:key name="ClassModule"   match="tei:classSpec" use="@module"/>
  <xsl:key name="MacroModule"   match="tei:macroSpec" use="@module"/>
  <xsl:key name="Modules"    match="tei:moduleSpec" use="1"/>
  <xsl:key name="predeclaredClasses"    match="tei:classSpec[@predeclare='true']" use="1"/>
  <xsl:key name="PredeclareMacros"
	   match="tei:macroSpec[@predeclare='true']" use="@ident"/>
  <xsl:key name="PredeclareMacrosModule"    
	   match="tei:macroSpec[@predeclare='true']" use="@module"/>  
  <xsl:key name="PredeclareAllMacros"    
	   match="tei:macroSpec[@predeclare='true']" use='1'/>
  
  <xsl:variable name="parameterize">
    <xsl:choose>
      <xsl:when test="$TEIC='false'">true</xsl:when>
      <xsl:when test="//tei:schemaSpec">false</xsl:when>
      <xsl:otherwise>true</xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  
  <!-- lookup table of element contents, and templates to access the result -->
  <xsl:key name="ELEMENTPARENTS" match="Contains" use="."/>
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
  
  
  
  <xsl:template match="*" mode="literal">
    <xsl:text>
    </xsl:text>
    <xsl:for-each select="ancestor::rng:*">
      <xsl:text> </xsl:text>
    </xsl:for-each>
    <xsl:text>&lt;</xsl:text>
    <xsl:value-of select="local-name(.)"/>
    <xsl:for-each select="@*">
      <xsl:text> </xsl:text>
    <xsl:value-of select="local-name(.)"/>="<xsl:value-of select="."/>"</xsl:for-each>
    <xsl:choose>
      <xsl:when test="child::node()">
	<xsl:text>&gt;</xsl:text>
	<xsl:apply-templates mode="literal"/>
	<xsl:if test="node()[last()]/self::rng:*"> 
	  <xsl:text>&#10;</xsl:text>
	</xsl:if>
	<xsl:for-each select="ancestor::rng:*">
	  <xsl:text> </xsl:text>
	</xsl:for-each>
	<xsl:text>&lt;/</xsl:text>
	<xsl:value-of select="local-name(.)"/>
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
  
  
  
  <xsl:template match="rng:ref">
    <xsl:copy>
      <xsl:attribute name="name">
	<xsl:if test="key('IDENTS',@name)">
	  <xsl:value-of select="$patternPrefix"/>
	</xsl:if>
	<xsl:value-of select="@name"/>
      </xsl:attribute>
    </xsl:copy>
  </xsl:template>
  
  <xsl:template match="rng:*">
    <xsl:copy>
      <xsl:copy-of select="@*"/>
      <xsl:apply-templates select="rng:*|tei:*|text()|comment()"/>
    </xsl:copy>
  </xsl:template>
  
  <xsl:template match="rng:zeroOrMore">
    <xsl:choose>
      <xsl:when test="count(rng:*)=1 and rng:zeroOrMore">
	<xsl:apply-templates select="rng:*|tei:*|text()|comment()"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:copy>
	  <xsl:copy-of select="@*"/>
	  <xsl:apply-templates select="rng:*|tei:*|text()|comment()"/>
	</xsl:copy>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
  
  <xsl:template match="tei:*" mode="tangle"/>
  
  <xsl:template match="tei:attRef" mode="tangle">
    <ref name="{@name}" xmlns="http://relaxng.org/ns/structure/1.0"/>
  </xsl:template>
  
  <xsl:template match="tei:attDef" mode="tangle">
    <xsl:param name="element"/>
    <xsl:variable name="I">
      <xsl:value-of select="translate(@ident,':','')"/>
    </xsl:variable>
    <xsl:if test="not(@ident='xmlns')">
      <xsl:choose>
	<xsl:when test="ancestor::tei:elementSpec">
	  <xsl:call-template name="makeAnAttribute"/>
	</xsl:when>
	
	<xsl:when test="ancestor::tei:classSpec">
	  <define name="{$element}.attribute.{translate(@ident,':','')}" xmlns="http://relaxng.org/ns/structure/1.0">
	    <xsl:call-template name="makeAnAttribute"/>
	  </define>
	</xsl:when>
      </xsl:choose>
    </xsl:if>
  </xsl:template>
  
  <xsl:template match="tei:attList" mode="tangle">
    <xsl:param name="element"/>
    <xsl:choose>
      <xsl:when test="@org='choice'">
	<rng:optional>
	  <rng:choice>
	    <xsl:apply-templates select="tei:*" mode="tangle">
	      <xsl:with-param name="element" select="$element"/>
	    </xsl:apply-templates>
	  </rng:choice>
	</rng:optional>
      </xsl:when>
      <xsl:otherwise>
	<xsl:apply-templates select="tei:*" mode="tangle">
	  <xsl:with-param name="element" select="$element"/>
	</xsl:apply-templates>
      </xsl:otherwise>
    </xsl:choose>
    
  </xsl:template>
  
  
  
  <xsl:template match="tei:author">
    <xsl:apply-templates/><xsl:text>, </xsl:text>
  </xsl:template>
  
  
  <xsl:template match="tei:classSpec" mode="tangle">
    <xsl:variable name="c" select="@ident"/>
    <xsl:if test="$verbose='true'">
      <xsl:message> classSpec <xsl:value-of
      select="@ident"/> (type <xsl:value-of select="@type"/>)</xsl:message>
    </xsl:if>
    <xsl:choose>
      <xsl:when test="@type='model'">
	<xsl:apply-templates select="." mode="processModel">
	  <xsl:with-param name="declare">true</xsl:with-param>
<!--	    <xsl:choose>
	      <xsl:when test="@module='tei'">true</xsl:when>
	      <xsl:otherwise>false</xsl:otherwise>
	    </xsl:choose>
	  </xsl:with-param>
-->
	</xsl:apply-templates>
      </xsl:when>
      <xsl:when test="@type='atts'">
	<xsl:call-template name="bitOut">
	  <xsl:with-param name="grammar">true</xsl:with-param>
	  <xsl:with-param name="content">
	    <Wrapper>
	      <define name="{$patternPrefix}{@ident}.attributes"
		      xmlns="http://relaxng.org/ns/structure/1.0">
		<xsl:if test="$parameterize='true'">
		  <xsl:for-each select="tei:classes/tei:memberOf">
		    <xsl:for-each select="key('IDENTS',@key)[1]">
		      <xsl:if test="@type='atts'">
			<ref xmlns="http://relaxng.org/ns/structure/1.0"
			     name="{@ident}.attributes"/>
		      </xsl:if>
		    </xsl:for-each>
		  </xsl:for-each>
		</xsl:if>
		<xsl:choose>
		  <xsl:when test="tei:attList//tei:attDef">
		    <xsl:for-each select="tei:attList//tei:attDef">
		      <xsl:if test="not(@ident='xmlns')">
			<ref xmlns="http://relaxng.org/ns/structure/1.0"
			     name="{$c}.attribute.{translate(@ident,':','')}"/>
		      </xsl:if>
		    </xsl:for-each>
		  </xsl:when>
		  <xsl:otherwise>
		    <notAllowed  xmlns="http://relaxng.org/ns/structure/1.0"/>
		  </xsl:otherwise>
		</xsl:choose>
	      </define>
	      <xsl:apply-templates select="tei:attList" mode="tangle">
		<xsl:with-param name="element" select="@ident"/>
	      </xsl:apply-templates>
	    </Wrapper>
	  </xsl:with-param>
	</xsl:call-template>
      </xsl:when>
    </xsl:choose>
  </xsl:template>
  
  
  
  <xsl:template match="tei:classSpec" mode="processModel">
    <xsl:param name="declare">false</xsl:param>
    <xsl:if test="$verbose='true'">
      <xsl:message>      .... model class <xsl:value-of  select="@ident"/></xsl:message>
    </xsl:if>
    
    <xsl:variable name="thisClass">
      <xsl:value-of select="@ident"/>   
    </xsl:variable>
    <xsl:call-template name="bitOut">
      <xsl:with-param name="grammar">true</xsl:with-param>
      <xsl:with-param name="content">
	<Wrapper>
	  <xsl:choose>
	    <xsl:when test="$parameterize='true'">
	      <xsl:apply-templates 
		  select="tei:classes/tei:memberOf"  
		  mode="tangleModel"/>
		<define name="{$patternPrefix}{$thisClass}"
			xmlns="http://relaxng.org/ns/structure/1.0">
		  <xsl:if test="@predeclare='true'">
		    <xsl:attribute name="combine">choice</xsl:attribute>
		  </xsl:if>
		  <notAllowed
		      xmlns="http://relaxng.org/ns/structure/1.0"/>
		</define>
	    </xsl:when>
	    <xsl:otherwise>
	      <define name="{$patternPrefix}{$thisClass}" xmlns="http://relaxng.org/ns/structure/1.0">
		<rng:choice>
		  <xsl:choose>
		    <xsl:when test="count(key('CLASSMEMBERS',$thisClass))&gt;0">
		      <xsl:for-each select="key('CLASSMEMBERS',$thisClass)">
			<ref name="{$patternPrefix}{@ident}" xmlns="http://relaxng.org/ns/structure/1.0"/>
		      </xsl:for-each>
		    </xsl:when>
		    <xsl:otherwise>
		      <notAllowed   xmlns="http://relaxng.org/ns/structure/1.0"/>
		    </xsl:otherwise>
		  </xsl:choose>
		</rng:choice>
	      </define>
	    </xsl:otherwise>
	  </xsl:choose>
	</Wrapper>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  
  
  <xsl:template match="tei:classSpec" mode="tangleadd">
    <xsl:apply-templates mode="tangleadd"/>
  </xsl:template>
  
  
  <xsl:template match="tei:classSpec/@ident"/>
  
  
  <xsl:template match="tei:classSpec|tei:elementSpec|tei:macroSpec" mode="weave">     
    <xsl:call-template name="refdoc"/>
  </xsl:template>
  
  
  <xsl:template match="tei:code">
    <xsl:call-template name="typewriter">
      <xsl:with-param name="text">
	<xsl:apply-templates/>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  
  
  <xsl:template match="tei:desc" mode="doc">
    <xsl:choose>
      <xsl:when test="$lang='en' and not(@xml:lang)">
	<xsl:value-of select="."/>
      </xsl:when>
      <xsl:when test="@xml:lang=$lang">
	<xsl:value-of select="."/>
      </xsl:when>
    </xsl:choose>
  </xsl:template>
  

  <xsl:template match="tei:desc"/>
  
  <xsl:template match="tei:desc" mode="tangle"/>
  
  <xsl:template match="tei:divGen[@type='classcat']">
    <xsl:apply-templates select="key('CLASSDOCS',1)" mode="weave">
      <xsl:sort select="@ident"/>
    </xsl:apply-templates>
  </xsl:template>
  
  
  
  <xsl:template match="tei:divGen[@type='macrocat']">
    <xsl:apply-templates select="key('MACRODOCS',1)"  mode="weave">
      <xsl:sort select="@ident"/>
    </xsl:apply-templates>
  </xsl:template>
  
  
  
  <xsl:template match="tei:divGen[@type='tagcat']">
    <xsl:apply-templates select="key('ELEMENTDOCS',1)"  mode="weave">
      <xsl:sort select="@ident"/>
    </xsl:apply-templates>
  </xsl:template>
  
  
  
  <xsl:template match="tei:editor">
    <xsl:apply-templates/>:
  </xsl:template>
  
  <xsl:template match="tei:elementSpec" mode="tangle">
    <xsl:if test="$verbose='true'">
      <xsl:message> elementSpec <xsl:value-of
      select="@ident"/>
      <xsl:if test="@xml:id">: <xsl:value-of select="@xml:id"/></xsl:if>
      </xsl:message>
    </xsl:if>
    <xsl:call-template name="bitOut">
      <xsl:with-param name="grammar"></xsl:with-param>
      <xsl:with-param name="content">
	<Wrapper>
	  <xsl:variable name="name">
	    <xsl:choose>
	      <xsl:when test="tei:altIdent">
		<xsl:value-of select="normalize-space(tei:altIdent)"/>
	      </xsl:when>
	      <xsl:otherwise>
		<xsl:value-of select="@ident"/>
	      </xsl:otherwise>
	    </xsl:choose>
	  </xsl:variable>
	  <xsl:choose>
	    <xsl:when test="tei:content/notAllowed">
	      <define name="{$patternPrefix}{@ident}" xmlns="http://relaxng.org/ns/structure/1.0">
		<notAllowed xmlns="http://relaxng.org/ns/structure/1.0"/>
	      </define>
	    </xsl:when>
	    <xsl:otherwise>
	      <xsl:variable name="Attributes">
		<xsl:call-template name="summarizeAttributes"/>
	      </xsl:variable>
	      <define  name="{$patternPrefix}{@ident}"  xmlns="http://relaxng.org/ns/structure/1.0">
		<element  name="{$name}" xmlns="http://relaxng.org/ns/structure/1.0">
		  <xsl:if test="@ns">
		    <xsl:attribute name="ns"><xsl:value-of select="@ns"/></xsl:attribute>
		  </xsl:if>
		  <xsl:if test="not($oddmode='tei')">
		    <a:documentation>
		      <xsl:apply-templates
			  select="tei:gloss" mode="doc"/>
		      <xsl:apply-templates select="tei:desc" mode="doc"/>
		    </a:documentation>
		  </xsl:if>
		  <xsl:choose>
		    <xsl:when test="$simplify='true'">
		      <xsl:call-template name="defineContent"/>
		      <xsl:if test="not($Attributes='') or $TEIC='true'">
			<xsl:call-template name="defineAttributes"/>
		      </xsl:if>
		    </xsl:when>
		    <xsl:otherwise>
		      <ref name="{$patternPrefix}{@ident}.content"
			   xmlns="http://relaxng.org/ns/structure/1.0"/>
		      <xsl:if test="not($Attributes='') or $TEIC='true'">
			<ref name="{$patternPrefix}{@ident}.attributes"
			     xmlns="http://relaxng.org/ns/structure/1.0"/>
		      </xsl:if>
		    </xsl:otherwise>
		  </xsl:choose>
		  
		</element>
	      </define>
	      <xsl:if test="$simplify='false'">
		<define name="{$patternPrefix}{@ident}.content" xmlns="http://relaxng.org/ns/structure/1.0">
		  <xsl:call-template name="defineContent"/>
		</define>
		<xsl:if test="not($Attributes='') or $TEIC='true'">
		  <define name="{$patternPrefix}{@ident}.attributes"  xmlns="http://relaxng.org/ns/structure/1.0">
			<xsl:call-template name="defineAttributes"/>
		  </define>
		</xsl:if>
	      </xsl:if>
	      <xsl:if test="$parameterize='true'">
		<xsl:apply-templates select="tei:classes/tei:memberOf"
				     mode="tangleModel"/>
	      </xsl:if>
	    </xsl:otherwise>
	  </xsl:choose>      
	</Wrapper>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  
 <xsl:template name="summarizeAttributes">
   <xsl:for-each select=".//tei:attDef">x</xsl:for-each>
   <xsl:for-each select="tei:classes/tei:memberOf">
     <xsl:for-each select="key('CLASSES',@key)">
       <xsl:if test="@type='atts'">x</xsl:if>
     </xsl:for-each>
   </xsl:for-each>
  </xsl:template>

  <xsl:template name="defineAttributes">
    <xsl:variable name="name" select="@ident"/>
    <xsl:if test="$parameterize='true'">
      <xsl:if test="$TEIC='true'">
	<rng:ref name="{$patternPrefix}att.global.attributes"/>
      </xsl:if>
      <xsl:for-each select="tei:classes/tei:memberOf">
	<xsl:for-each select="key('CLASSES',@key)">
	  <xsl:if test="@type='atts'">
	    <ref name="{$patternPrefix}{@ident}.attributes" xmlns="http://relaxng.org/ns/structure/1.0"/>
	  </xsl:if>
	</xsl:for-each>
      </xsl:for-each>
    </xsl:if>
    <xsl:apply-templates select="tei:attList" mode="tangle">
      <xsl:with-param name="element">
	<xsl:value-of select="$name"/>
      </xsl:with-param>
    </xsl:apply-templates>
    
    <!-- place holder to make sure something gets into the
	     pattern -->
    <empty xmlns="http://relaxng.org/ns/structure/1.0"/>

<!--
    <xsl:choose>
      <xsl:when test="$TEIC='true'">
	<optional xmlns="http://relaxng.org/ns/structure/1.0">
	  <attribute name="TEIform" a:defaultValue="{@ident}" xmlns="http://relaxng.org/ns/structure/1.0">
	    <text xmlns="http://relaxng.org/ns/structure/1.0"/>
	  </attribute>
	</optional>
      </xsl:when>
      <xsl:otherwise>
	<empty xmlns="http://relaxng.org/ns/structure/1.0"/>
      </xsl:otherwise>
    </xsl:choose>
-->
  </xsl:template>
  
  <xsl:template name="defineContent">
    <xsl:variable name="Contents">
      <BLAH>
	<xsl:choose>
	  <xsl:when test="tei:content/tei:valList[@type='closed']">
	    <rng:choice >
	      <xsl:for-each select="tei:content/tei:valList/tei:valItem">  
		<rng:value ><xsl:value-of select="@ident"/></rng:value>
		<xsl:if test="not($oddmode='tei')">
		  <a:documentation>
		    <xsl:apply-templates select="tei:gloss" mode="doc"/>
		    <xsl:apply-templates select="tei:desc" mode="doc"/>
		  </a:documentation>
		</xsl:if>
	      </xsl:for-each>
	    </rng:choice>
	  </xsl:when>
	  <xsl:when test="tei:content">
	    <xsl:apply-templates select="tei:content/*"/>
	  </xsl:when>
	  <xsl:otherwise>
	    <rng:empty />
	  </xsl:otherwise>
	</xsl:choose>
      </BLAH>
    </xsl:variable>
    
    <xsl:choose>
      <xsl:when test="count(exsl:node-set($Contents)/BLAH/*)=0">
	<rng:empty/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:for-each select="exsl:node-set($Contents)/BLAH">
	  <xsl:copy-of select="*"/>
	</xsl:for-each>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
  <xsl:template match="tei:elementSpec/@ident"/>
  
  <xsl:template match="tei:gloss" mode="doc">
    <xsl:if test="not(.='')">
      <xsl:text>(</xsl:text>
      <xsl:choose>
	<xsl:when test="$lang='en' and not(@xml:lang)">
	  <xsl:apply-templates/>
	</xsl:when>
	<xsl:when test="@xml:lang=$lang">
	  <xsl:apply-templates/>
	</xsl:when>
      </xsl:choose>
      <xsl:text>) </xsl:text>
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
    <xsl:variable name="entCont">
      <BLAH>
	<xsl:choose>
	  <xsl:when test="not($msection='') and tei:content/rng:group">
	    <rng:choice >
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
	  <xsl:message> macroSpec <xsl:value-of select="@ident"/></xsl:message>
	</xsl:if>
	<xsl:call-template name="bitOut">
	  <xsl:with-param name="grammar">true</xsl:with-param>
	  <xsl:with-param name="content">
	    <Wrapper>
	      <define name="{$patternPrefix}{@ident}" 
		      xmlns="http://relaxng.org/ns/structure/1.0">
		<xsl:if test="$parameterize='true'">
		  <xsl:if test="starts-with(@ident,'macro.component')
				or @predeclare='true'">
		    <xsl:attribute name="combine">choice</xsl:attribute>
		  </xsl:if>
		</xsl:if>
		<xsl:choose>
		  <xsl:when test="starts-with(@ident,'type')">
		    <xsl:copy-of select="exsl:node-set($entCont)/BLAH/rng:*"/>
		  </xsl:when>
		  <xsl:when test="$entCount=0">
		    <rng:choice>
		      <notAllowed xmlns="http://relaxng.org/ns/structure/1.0"/>
		    </rng:choice>
		  </xsl:when>
		  <xsl:when test="$entCount=1">
		    <xsl:copy-of select="exsl:node-set($entCont)/BLAH/rng:*"/>
		  </xsl:when>
		  <xsl:when test="tei:content/rng:text|tei:content/rng:ref">
		    <rng:choice >
		      <xsl:copy-of
			  select="exsl:node-set($entCont)/BLAH/rng:*"/>
		    </rng:choice>
		  </xsl:when>
		  <xsl:otherwise>
		    <xsl:copy-of select="exsl:node-set($entCont)/BLAH/rng:*"/>
		  </xsl:otherwise>
		</xsl:choose>
	      </define>
	    </Wrapper>
	  </xsl:with-param>
	</xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
  <xsl:template match="tei:macroSpec/@ident"/>
  
  <xsl:template match="tei:macroSpec/content/rng:*"/>
  
  <xsl:template match="tei:memberOf" mode="tangleModel">
    <xsl:variable name="owner">
      <xsl:value-of select="ancestor::tei:elementSpec/@ident|ancestor::tei:classSpec/@ident"/>
    </xsl:variable>
    <xsl:for-each select="key('IDENTS',@key)">
      <xsl:if test="@type='model'">
	<define name="{@ident}" combine="choice"  xmlns="http://relaxng.org/ns/structure/1.0">
	  <ref name="{$patternPrefix}{$owner}" xmlns="http://relaxng.org/ns/structure/1.0"/>
	</define>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>
  
  <xsl:template match="tei:mentioned">
    <xsl:text>&#8216;</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>&#8217;</xsl:text>
  </xsl:template>
  
  <xsl:template match="tei:moduleRef" mode="tangle" >
    <xsl:variable name="This" select="@key"/>
    <xsl:if test="$verbose='true'">
      <xsl:message>      .... import module [<xsl:value-of
      select="$This"/> <xsl:value-of select="@url"/>] </xsl:message>
    </xsl:if>
    <xsl:call-template name="bitOut">
      <xsl:with-param name="grammar">true</xsl:with-param>
      <xsl:with-param name="content">
	<Wrapper>
	  <xsl:choose>
	    <xsl:when test="@url and $parameterize='true'">
	      <include href="{@url}"
		       xmlns="http://relaxng.org/ns/structure/1.0">
		<xsl:copy-of select="tei:content/*"/>
	      </include>
	    </xsl:when>
	    <xsl:when test="@url and $parameterize='false'">
	      <xsl:comment>Start of import of <xsl:value-of select="@url"/></xsl:comment>
	      <rng:div>
		<xsl:for-each select="document(@url)/rng:grammar">
		  <xsl:copy-of select="@*"/>
		  <xsl:copy-of select="*"/>
		</xsl:for-each>
		<xsl:copy-of select="tei:content/*"/>
	      </rng:div>
	      <xsl:comment>End of import of <xsl:value-of select="@url"/></xsl:comment>
	    </xsl:when>
	    <xsl:otherwise>
	      <include href="{$schemaBaseURL}{$This}.rng" xmlns="http://relaxng.org/ns/structure/1.0">
		<xsl:attribute name="ns">
		  <xsl:choose>
		    <xsl:when test="ancestor::tei:schemaSpec/@ns">
		      <xsl:value-of select="ancestor::tei:schemaSpec/@ns"/>
		    </xsl:when>
		    <xsl:otherwise>http://www.tei-c.org/ns/1.0</xsl:otherwise>
		  </xsl:choose>
		</xsl:attribute>
		<xsl:for-each  select="../tei:*[@module=$This and not(@mode='add')]">
		  <xsl:apply-templates mode="tangle" select="."/>
		</xsl:for-each>
	      </include>
	    </xsl:otherwise>
	  </xsl:choose>
	</Wrapper>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  
  
  <xsl:template match="tei:remarks" mode="tangle"/>
  
  <xsl:template match="tei:specGrp" mode="ok">
    <xsl:param name="filename"/>
    <xsl:if test="$verbose='true'">
      <xsl:message>       processing spec grp  <xsl:value-of
      select="@ident"/></xsl:message>
    </xsl:if>
    
    <xsl:call-template name="processSchemaFragment">
      <xsl:with-param name="filename" select="$filename"/>
    </xsl:call-template>
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
	<xsl:if test="preceding-sibling::tei:title"><br/></xsl:if>
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
  
  
  
  <xsl:template name="compositeNumber">
    <xsl:for-each select="ancestor::tei:div1|ancestor::tei:body/tei:div">
      <xsl:number level="any"/>
      <xsl:text>.</xsl:text>
    </xsl:for-each>
    <xsl:number level="any" from="tei:div1"/>
  </xsl:template>
  
  <xsl:template name="copyright">
    <xsl:apply-templates 
	select="/tei:TEI/tei:teiHeader/tei:fileDesc/tei:publicationStmt/tei:availability" mode="copyrighttext"/>
  </xsl:template>

  <xsl:template match="tei:p" mode="copyrighttext">
    <xsl:text>&#10;</xsl:text>
    <xsl:apply-templates/>
  </xsl:template>
  
  <xsl:template match="tei:list" mode="copyrighttext">
    <xsl:text>&#10;</xsl:text>
    <xsl:apply-templates/>
  </xsl:template>
  
  <xsl:template match="tei:item" mode="copyrighttext">
    <xsl:text>&#10; *</xsl:text>
    <xsl:apply-templates/>
  </xsl:template>
  
  <xsl:template name="attributeDatatype">
    <xsl:variable name="this" select="@ident"/>
    <xsl:choose>
      <xsl:when test="tei:datatype[rng:ref/@name='datatype.Code']">
	<xsl:choose>
	  <xsl:when test="tei:valList[@type='closed']">
	    <choice xmlns="http://relaxng.org/ns/structure/1.0">
	      <xsl:for-each select="tei:valList/tei:valItem">
		<value  xmlns="http://relaxng.org/ns/structure/1.0">
		  <xsl:value-of select="@ident"/>
		</value>
		<xsl:if test="not($oddmode='tei')">
		  <a:documentation>
		    <xsl:apply-templates select="tei:gloss" mode="doc"/>
		    <xsl:apply-templates select="tei:desc" mode="doc"/>
		  </a:documentation>
		</xsl:if>
	      </xsl:for-each>
	    </choice>
	  </xsl:when>
	  <xsl:otherwise>
	    <text  xmlns="http://relaxng.org/ns/structure/1.0"/>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:when>
      <xsl:when test="tei:valList[@type='closed']">
	<choice xmlns="http://relaxng.org/ns/structure/1.0">
	  <xsl:for-each select="tei:valList/tei:valItem">
	    <value xmlns="http://relaxng.org/ns/structure/1.0">
	      <xsl:value-of select="@ident"/>
	    </value>
	    <xsl:if test="not($oddmode='tei')">
	      <a:documentation>
		<xsl:apply-templates select="tei:gloss" mode="doc"/>
		<xsl:apply-templates select="tei:desc" mode="doc"/>
	      </a:documentation>
	    </xsl:if>
	  </xsl:for-each>
	</choice>
      </xsl:when>
      <xsl:when test="tei:datatype/rng:*">
	<xsl:apply-templates select="tei:datatype/rng:*" mode="forceRNG"/>
      </xsl:when>
      <xsl:otherwise>
	<text xmlns="http://relaxng.org/ns/structure/1.0"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
  <xsl:template name="makeSimpleAttribute">
    <xsl:variable name="name">
      <xsl:choose>
	<xsl:when test="tei:altIdent">
	  <xsl:value-of select="normalize-space(tei:altIdent)"/>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:if test="@ns='http://www.w3.org/XML/1998/namespace'">xml:</xsl:if>
	  <xsl:value-of select="@ident"/>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    
    <rng:attribute name="{$name}" >
      <xsl:if test="@ns">
	<xsl:copy-of select="@ns"/>
      </xsl:if>
      <xsl:if test="tei:defaultVal">
	<xsl:attribute name="a:defaultValue">
	  <xsl:value-of select="normalize-space(tei:defaultVal)"/>
	</xsl:attribute>
      </xsl:if>
      <xsl:if test="not($oddmode='tei')">
	<a:documentation>
	  <xsl:apply-templates select="tei:gloss" mode="doc"/>
	  <xsl:apply-templates select="tei:desc" mode="doc"/>
	</a:documentation>
      </xsl:if>
      <xsl:call-template name="attributeDatatype"/>
    </rng:attribute>
  </xsl:template>
  
  <xsl:template name="makeAnAttribute">
    <xsl:choose>
      <xsl:when test="@usage='req'">
	<xsl:call-template name="makeSimpleAttribute"/>
      </xsl:when>
      <xsl:when test="parent::tei:attList[@org='choice']">
	<xsl:call-template name="makeSimpleAttribute"/>
      </xsl:when>
      <xsl:otherwise>
	<rng:optional >
	  <xsl:call-template name="makeSimpleAttribute"/>
	</rng:optional>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
  
  <xsl:template name="generateClassParents">
    <xsl:choose>
      <xsl:when test="not(tei:classes)"> (none)   </xsl:when>
      <xsl:otherwise>
	<xsl:for-each select="tei:classes/tei:memberOf">
	  <xsl:if test="preceding-sibling::tei:memberOf">
	    <xsl:text>: </xsl:text>
	  </xsl:if>
	  <xsl:choose>
	    <xsl:when test="key('CLASSES',@key)">
	      <xsl:for-each select="key('CLASSES',@key)">
		<xsl:call-template name="makeLink">
		  <xsl:with-param name="class">classlink</xsl:with-param>
		  <xsl:with-param name="name"><xsl:value-of select="@ident"/></xsl:with-param>
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
	</xsl:for-each>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
  <xsl:template name="showAttClasses">
    <xsl:variable name="clatts">
      <xsl:for-each select="tei:classes/tei:memberOf">
	<xsl:choose>
	  <xsl:when test="key('CLASSES',@key)">
	    <xsl:for-each select="key('CLASSES',@key)">
	      <xsl:if test="@type='atts'">
		<xsl:call-template name="makeLink">
		  <xsl:with-param name="class">classlink</xsl:with-param>
		  <xsl:with-param name="name"><xsl:value-of select="@ident"/></xsl:with-param>
		  <xsl:with-param name="text">
		    <xsl:value-of select="@ident"/>
		  </xsl:with-param>
		</xsl:call-template>
		<xsl:text> </xsl:text>
	      </xsl:if>
	    </xsl:for-each>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:value-of select="@key"/>
	    <xsl:text> </xsl:text>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:for-each>
    </xsl:variable>
    <xsl:if test="not($clatts='')">
      <xsl:text> [</xsl:text>
      <xsl:copy-of select="$clatts"/>
      <xsl:text>] </xsl:text>
    </xsl:if>
  </xsl:template>
  
  <xsl:template name="generateMembers">
    <xsl:variable name="this" select="@ident"/>
    <xsl:choose>
      <xsl:when test="key('CLASSMEMBERS',$this)">
	<xsl:for-each select="key('CLASSMEMBERS',$this)">
	  <xsl:sort select="@ident"/>
	  <xsl:text> </xsl:text>
	  <xsl:call-template name="linkTogether">
	    <xsl:with-param name="name" select="@ident"/>
	  </xsl:call-template>
	  <xsl:if test="count(key('CLASSMEMBERS',@ident))&gt;0">
	    <xsl:text>  [</xsl:text>
	    <xsl:variable name="Key" select="@ident"/>
	    <xsl:for-each select="key('CLASSMEMBERS',@ident)">
	      <xsl:sort select="@ident"/>
	      <xsl:text> </xsl:text>
	      <xsl:call-template name="showElement">
		<xsl:with-param name="name" select="@ident"/>
	      </xsl:call-template>
	    </xsl:for-each>
	    <xsl:text>] </xsl:text>
	  </xsl:if>
	</xsl:for-each>
      </xsl:when>
      <xsl:when test="$lookupDatabase='true'">
	<xsl:choose>
	  <xsl:when test="not($localsource='')">
	    <xsl:for-each select="document($localsource)/tei:TEI">
	      <xsl:for-each select="tei:elementSpec[tei:classes/tei:memberOf[@key=$this]]">
		<xsl:call-template name="showElement">
		  <xsl:with-param name="name" select="@ident"/>
		</xsl:call-template>
		<xsl:if test="following::item">
		  <xsl:text>: &#10;</xsl:text>
		</xsl:if>
	      </xsl:for-each>
	    </xsl:for-each>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:variable name="address">
	      <xsl:value-of select="$TEISERVER"/>
	      <xsl:text>classmembers.xq?class=</xsl:text>
	      <xsl:value-of select="@ident"/>
	    </xsl:variable>
	    <xsl:if test="$verbose='true'">
	      <xsl:message>Accessing TEISERVER: <xsl:value-of
	      select="$address"/></xsl:message>
	    </xsl:if>
	    <xsl:for-each
		select="document($address)/list/item">
	      <xsl:call-template name="showElement">
		<xsl:with-param name="name" select="."/>
	      </xsl:call-template>
	      <xsl:if test="following::item">
		<xsl:text>: &#10;</xsl:text>
	      </xsl:if>
	    </xsl:for-each>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:when>
    </xsl:choose>
  </xsl:template>
  
  <xsl:template name="showElement">
    <xsl:param name="name"/>
    <xsl:choose>
      <xsl:when test="$oddmode='tei'">
	<tei:ref target="#{$name}"><xsl:value-of select="$name"/></tei:ref>
      </xsl:when>
      <xsl:when test="$oddmode='html'">
	<xsl:choose>
	  <xsl:when test="key('IDENTS',$name) and $splitLevel=-1">
	    <a class="link_element" href="#{$name}">
	      <xsl:value-of select="$name"/>
	    </a>
	  </xsl:when>
	  <xsl:when test="key('IDENTS',$name)">
	    <a class="link_element" href="ref-{$name}.html">
	      <xsl:value-of select="$name"/>
	    </a>
	  </xsl:when>
	  <xsl:otherwise>
	    <a href="{concat($TEISERVER,'tag.xq?name=',$name)}">
	      <xsl:value-of select="$name"/>
	    </a>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:when>
      <xsl:when test="$oddmode='pdf'">
	<fo:inline font-style="italic"><xsl:value-of select="$name"/></fo:inline>
      </xsl:when>
      <xsl:otherwise>
	<xsl:value-of select="$name"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
  
  
  <xsl:template name="generateParents">
    <xsl:param name="what"/>
    <xsl:variable name="mums">
      <mums>
	<xsl:for-each select="document('dtdcat.xml')">
	  <xsl:for-each select="key('ELEMENTPARENTS',$what)">
	    <mum><xsl:value-of select="../@id|../@xml:id"/></mum>
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
  
  
  
  
  <xsl:template name="linkStyle"/>
  
  
  
  <xsl:template name="linkTogether">
    <xsl:param name="name"/>
    <xsl:param name="reftext"/>
    <xsl:variable name="link">
      <xsl:choose>
	<xsl:when test="$reftext=''"><xsl:value-of select="$name"/></xsl:when>
	<xsl:otherwise><xsl:value-of select="$reftext"/></xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="not(key('IDENTS',$name))">
	<xsl:choose>
	  <xsl:when test="$oddmode='tei'">
	    <tei:ref>
	      <xsl:attribute name="target">
		<xsl:value-of select="$TEISERVER"/>tag.xq?name=<xsl:value-of
		select="$name"/>
	      </xsl:attribute>
	      <xsl:value-of select="$link"/>
	    </tei:ref>
	  </xsl:when>
	  <xsl:otherwise>
	    <a>
	      <xsl:attribute name="href">
		<xsl:value-of select="$TEISERVER"/>tag.xq?name=<xsl:value-of
		select="$name"/>
	      </xsl:attribute>
	      <xsl:value-of select="$link"/>
	    </a>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:when>
      <xsl:when test="$oddmode='html' and $splitLevel=-1">
	<a class="link_odd" href="#{$name}"><xsl:value-of select="$link"/></a>
      </xsl:when>
      <xsl:when test="$oddmode='html'">
	<a class="link_odd" href="{concat('ref-',$name,'.html')}"><xsl:value-of select="$link"/></a>
      </xsl:when>
      <xsl:when test="$oddmode='pdf'">
	<fo:inline><xsl:value-of select="$link"/></fo:inline>
      </xsl:when>
      <xsl:when test="$oddmode='tei'">
	<tei:ref target="#{$name}"><xsl:value-of select="$link"/></tei:ref>
      </xsl:when>
      <xsl:otherwise>
	<xsl:value-of select="$link"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
  
  
  <xsl:template name="makeTagsetInfo">
    <xsl:value-of select="@module"/>
    <xsl:if test="$verbose='true'">
      <xsl:message>  tagset <xsl:value-of select="@xml:id"/>: <xsl:value-of select="@module"/></xsl:message>
    </xsl:if>
  </xsl:template>
  
  
  
  <xsl:template name="processSchemaFragment">
    <xsl:param name="filename"/>
    <xsl:variable name="secnum">
      <xsl:call-template name="sectionNumber"/>
    </xsl:variable>
    <!--
	<xsl:if test="@xml:id">
	<xsl:comment>[<xsl:value-of select="@xml:id"/>] <xsl:value-of
	select="$secnum"/>
	<xsl:if test="@n">
	<xsl:text>: </xsl:text>
	<xsl:value-of select="@n"/>
	</xsl:if>
	</xsl:comment>
	</xsl:if>
    -->
    <xsl:apply-templates mode="tangle"/>
    
    <!--
	<xsl:if test="@xml:id">
	<xsl:comment> end of [<xsl:value-of select="@xml:id"/>]  <xsl:value-of select="$secnum"/>    
	</xsl:comment>
	</xsl:if>
    -->
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
      <xsl:when test="not($localsource='')">
	<xsl:for-each select="document($localsource)/tei:TEI">
	  <xsl:apply-templates select="tei:*[@ident=$name]" mode="show">
	    <xsl:with-param name="atts" select="$atts"/>
	  </xsl:apply-templates>
	</xsl:for-each>
      </xsl:when>
      <xsl:otherwise>
	<xsl:variable name="loc">
	  <xsl:value-of select="$TEISERVER"/>
	  <xsl:text>copytag.xq?name=</xsl:text>
	  <xsl:value-of select="$name"/>
	</xsl:variable>
	<xsl:if test="$verbose='true'">
	  <xsl:message>Accessing TEISERVER: <xsl:value-of
	  select="$loc"/></xsl:message>
	</xsl:if>
	
	<xsl:apply-templates select="document($loc)/tei:*" mode="show">
	  <xsl:with-param name="atts" select="$atts"/>
	</xsl:apply-templates>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
  
  
  <xsl:template name="processatts">
    <xsl:param name="values"/>
    <xsl:if test="not($values = '')">
      <xsl:apply-templates 
	  select="key('IDENTS',substring-before($values,' '))"/>
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
  
  <xsl:template name="make-ns-declaration">
    <xsl:param name="is-default"/>
    <xsl:param name="prefix"/>
    <xsl:param name="uri"/>
  </xsl:template>
  
  <xsl:template name="inhnamespace"/>
  
  <xsl:template match="s:*"/>
  
  <xsl:template match="tei:altIdent"/>
  
  <xsl:template match="a:*">
    <xsl:copy-of select="."/>
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
	  <rng:define name="{$patternPrefix}{@ident}.attributes" combine="choice" >
	    <rng:empty />
	  </rng:define>
	</Wrapper>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  
  <!-- Force an output element in the RNG namespace. 
       I don't see why this is necessary, but xsltproc gets
       it wrong otherwise. I suspect a bug there somewhere.
  -->
  <xsl:template match="*" mode="forceRNG">
    <xsl:element name="{local-name(.)}" xmlns="http://relaxng.org/ns/structure/1.0">
      <xsl:copy-of select="@*"/>
      <xsl:apply-templates mode="forceRNG"/>
    </xsl:element>
  </xsl:template>
  
  <xsl:template match="rng:ref" mode="forceRNG">
    <xsl:element name="{local-name(.)}" xmlns="http://relaxng.org/ns/structure/1.0">
      <xsl:attribute name="name">
	<xsl:if test="key('IDENTS',@name)">
	  <xsl:value-of select="$patternPrefix"/>
	</xsl:if>
	<xsl:value-of select="@name"/>
      </xsl:attribute>
    </xsl:element>
  </xsl:template>
  
  <xd:doc>
    <xd:short>Process elements  tei:schemaSpec</xd:short>
    <xd:detail>&#160;</xd:detail>
  </xd:doc>
  <xsl:template match="tei:schemaSpec">
    <xsl:call-template name="processSchemaFragment"/>
  </xsl:template>
  
  
  <xsl:template name="typewriter"/>
  
  <xsl:template name="refdoc"/>
  
</xsl:stylesheet>
