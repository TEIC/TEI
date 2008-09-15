<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet
  version="1.0"
  xmlns:s="http://www.ascc.net/xml/schematron"
  xmlns="http://www.w3.org/1999/xhtml"
  xmlns:xlink="http://www.w3.org/1999/xlink"
  xmlns:dbk="http://docbook.org/ns/docbook"
  xmlns:rng="http://relaxng.org/ns/structure/1.0"
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:teix="http://www.tei-c.org/ns/Examples"
  xmlns:xhtml="http://www.w3.org/1999/xhtml"
  xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
  xmlns:edate="http://exslt.org/dates-and-times"
  xmlns:estr="http://exslt.org/strings" xmlns:exsl="http://exslt.org/common"
  xmlns:html="http://www.w3.org/1999/xhtml"
  xmlns:pantor="http://www.pantor.com/ns/local"
  xmlns:xd="http://www.pnp-software.com/XSLTdoc"
  xmlns:xs="http://www.w3.org/2001/XMLSchema"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  exclude-result-prefixes="xlink dbk rng tei teix s xhtml a edate estr html pantor xd xs xsl"
  extension-element-prefixes="exsl estr edate" 
>
  <xsl:import href="teiodds.xsl"/>
  <xsl:import href="../xhtml/tei.xsl"/>
  <xsl:import href="../xhtml/tagdocs.xsl"/>
  <xsl:import href="RngToRnc.xsl"/>
  <xsl:param name="xhtml">true</xsl:param>
  <xd:doc type="stylesheet">
    <xd:short> TEI stylesheet for making HTML from ODD </xd:short>
    <xd:detail> This library is free software; you can redistribute it and/or
      modify it under the terms of the GNU Lesser General Public License as
      published by the Free Software Foundation; either version 2.1 of the
      License, or (at your option) any later version. This library is
      distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
      without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
      PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
      details. You should have received a copy of the GNU Lesser General Public
      License along with this library; if not, write to the Free Software
      Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA </xd:detail>
    <xd:author>See AUTHORS</xd:author>
    <xd:cvsId>$Id$</xd:cvsId>
    <xd:copyright>2008, TEI Consortium</xd:copyright>
  </xd:doc>
  <xsl:key match="tei:*" name="NameToID" use="@ident"/>

  <xsl:output method="xml"
	      encoding="utf-8"
	      />  


  <xsl:param name="BITS">Bits</xsl:param>
  <xsl:param name="STDOUT">false</xsl:param>
  <xsl:param name="TAG"/>
  <xsl:param name="alignNavigationPanel">left</xsl:param>
  <xsl:param name="authorWord"></xsl:param>
  <xsl:param name="autoToc">true</xsl:param>
  <xsl:param name="bottomNavigationPanel">true</xsl:param>
  <xsl:param
      name="cssFile">http://www.tei-c.org/release/xml/tei/stylesheet/tei.css</xsl:param>
  <xsl:param name="cssSecondaryFile">http://www.tei-c.org/release/xml/tei/stylesheet/odd.css</xsl:param>
  <xsl:param name="dateWord"></xsl:param>
  <xsl:param name="displayMode">rnc</xsl:param>
  <xsl:param name="feedbackWords">Contact</xsl:param>
  <xsl:param name="footnoteBackLink">true</xsl:param>
  <xsl:param name="footnoteFile">false</xsl:param>
  <xsl:param name="homeURL">index.html</xsl:param>
  <xsl:param name="indent-width" select="3"/>
  <xsl:param name="line-width" select="80"/>
  <xsl:param name="numberBackHeadings">A.1</xsl:param>
  <xsl:param name="numberFrontHeadings"></xsl:param>
  <xsl:param name="oddmode">html</xsl:param>
  <xsl:param name="pageLayout">Simple</xsl:param>
  <xsl:param name="prenumberedHeadings">false</xsl:param>
  <xsl:param name="searchURL"/>
  <xsl:param name="searchWords"/>
  <xsl:param name="showNamespaceDecls">false</xsl:param>
  <xsl:param name="showTitleAuthor">1</xsl:param>
  <xsl:param name="splitBackmatter">yes</xsl:param>
  <xsl:param name="splitFrontmatter">yes</xsl:param>
  <xsl:param name="splitLevel">0</xsl:param>
  <xsl:param name="subTocDepth">-1</xsl:param>
  <xsl:param name="tocDepth">3</xsl:param>
  <xsl:param name="tocElement">div</xsl:param>
  <xsl:param name="topNavigationPanel"></xsl:param>
  <xsl:param name="useHeaderFrontMatter">true</xsl:param>
  <xsl:param name="verbose">false</xsl:param>

<!-- these are ones to override -->
  <xsl:param name="feedbackURL">#</xsl:param>
  <xsl:param name="homeLabel">Home</xsl:param>
  <xsl:param name="homeWords">Home</xsl:param>
  <xsl:param name="institution"></xsl:param>
  <xsl:param name="outputDir">html</xsl:param>
  <xsl:param name="parentURL">http://www.example.com/</xsl:param>
  <xsl:param name="parentWords"></xsl:param>

  <xsl:key name="MODEL-CLASS-MODULE" match="tei:classSpec[@type='model']"  use="@module"/>
  <xsl:key name="ATT-CLASS-MODULE" match="tei:classSpec[@type='atts']"  use="@module"/>
  <xsl:key name="ELEMENT-MODULE" match="tei:elementSpec"
	   use="@module"/>
  <xsl:key name="MACRO-MODULE" match="tei:macroSpec"
	   use="@module"/>

  <xsl:key name="ELEMENT-ALPHA" match="tei:elementSpec"
	   use="substring(translate(@ident,'ABCDEFGHIJKLMNOPQRSTUVWXYZ','abcdefghijklmnopqrstuvwxyz'),1,1)"/>

  <xsl:key name="MODEL-CLASS-ALPHA" match="tei:classSpec[@type='model']"
	   use="substring(translate(@ident,'ABCDEFGHIJKLMNOPQRSTUVWXYZ','abcdefghijklmnopqrstuvwxyz'),7,1)"/>

  <xsl:key name="ATT-CLASS-ALPHA" match="tei:classSpec[@type='atts']"
	   use="substring(translate(@ident,'ABCDEFGHIJKLMNOPQRSTUVWXYZ','abcdefghijklmnopqrstuvwxyz'),5,1)"/>


  <xsl:template name="copyrightStatement"></xsl:template>

  <xsl:variable name="top" select="/"/>

  <xsl:template match="processing-instruction()"/>
 
  <xsl:variable name="headingNumberSuffix">
    <xsl:text> </xsl:text>
  </xsl:variable>

  <xsl:template name="processSchemaFragment">
    <xsl:param name="filename"/>
    <div class="schemaFragment">
      <xsl:if test="tei:classSpec">
        <h2>
          <xsl:call-template name="i18n">
            <xsl:with-param name="word">Classes defined</xsl:with-param>
          </xsl:call-template>
        </h2>
        <xsl:apply-templates mode="weave" select="tei:classSpec">
          <xsl:sort select="tei:altIdent|@ident"/>
        </xsl:apply-templates>
      </xsl:if>
      <xsl:if test="tei:elementSpec">
        <h2>
          <xsl:call-template name="i18n">
            <xsl:with-param name="word">Elements defined</xsl:with-param>
          </xsl:call-template>
        </h2>
        <xsl:apply-templates mode="weave" select="tei:elementSpec">
          <xsl:sort select="tei:altIdent|@ident"/>
        </xsl:apply-templates>
      </xsl:if>
      <xsl:if test="tei:macroSpec">
        <h2>
          <xsl:call-template name="i18n">
            <xsl:with-param name="word">Macros defined</xsl:with-param>
          </xsl:call-template>
        </h2>
        <xsl:apply-templates mode="weave" select="tei:macroSpec">
          <xsl:sort select="tei:altIdent|@ident"/>
        </xsl:apply-templates>
      </xsl:if>
      <xsl:apply-templates select="tei:specGrpRef"/>
    </div>
  </xsl:template>

  <xsl:template name="listSpecs">
    <xsl:for-each select="..//tei:schemaSpec">
      <hr/>
      <xsl:for-each select="tei:classSpec">
        <xsl:sort select="tei:altIdent"/>
        <xsl:sort select="@ident"/>
	<xsl:element name="{$tocElement}">
	  <xsl:attribute name="class">toclist0</xsl:attribute>
          <a xmlns="http://www.w3.org/1999/xhtml" 
	     class="toclist" href="#{@ident}">
            <xsl:choose>
              <xsl:when test="tei:altIdent">
                <xsl:value-of select="tei:altIdent"/>
              </xsl:when>
              <xsl:otherwise>
                <xsl:value-of select="@ident"/>
              </xsl:otherwise>
            </xsl:choose>
          </a>
	</xsl:element>
      </xsl:for-each>
      <hr/>
      <xsl:for-each select="tei:elementSpec">
        <xsl:sort select="tei:altIdent"/>
        <xsl:sort select="@ident"/>
	<xsl:element name="{$tocElement}">
	  <xsl:attribute name="class">toclist0</xsl:attribute>
          <a xmlns="http://www.w3.org/1999/xhtml"
	     class="toclist" href="#{@ident}">
            <xsl:choose>
              <xsl:when test="tei:altIdent">
                <xsl:value-of select="tei:altIdent"/>
              </xsl:when>
              <xsl:otherwise>
                <xsl:value-of select="@ident"/>
              </xsl:otherwise>
            </xsl:choose>
          </a>
        </xsl:element>
      </xsl:for-each>
      <hr/>
      <xsl:for-each select="tei:macroSpec">
        <xsl:sort select="tei:altIdent"/>
        <xsl:sort select="@ident"/>
	<xsl:element name="{$tocElement}">
	  <xsl:attribute name="class">toclist0</xsl:attribute>
          <a xmlns="http://www.w3.org/1999/xhtml"
	     class="toclist" href="#{@ident}">
            <xsl:choose>
              <xsl:when test="tei:altIdent">
                <xsl:value-of select="tei:altIdent"/>
              </xsl:when>
              <xsl:otherwise>
                <xsl:value-of select="@ident"/>
              </xsl:otherwise>
            </xsl:choose>
          </a>
        </xsl:element>
      </xsl:for-each>
    </xsl:for-each>
  </xsl:template>

<xsl:template match="tei:elementSpec[@mode='delete']">
<dt>Element <xsl:value-of select="@ident"/></dt>
<dd><b>DELETED</b></dd>
</xsl:template>

<xsl:template match="tei:divGen[@type='toc']">
  <xsl:call-template name="mainTOC"/>
</xsl:template>

<xsl:template name="oddTocEntry">
    <xsl:variable name="loc">
      <xsl:choose>
      <xsl:when test="$splitLevel=-1 or $STDOUT='true'">
	<xsl:text>#</xsl:text>
	<xsl:value-of select="@ident"/>
      </xsl:when>
      <xsl:otherwise> 
	<xsl:text>ref-</xsl:text>
	<xsl:value-of select="@ident"/>
	<xsl:value-of select="$outputSuffix"/>
      </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <div class="oddTocEntry">
      <a href="{$loc}">
	<xsl:value-of select="@ident"/>
      </a>
    </div>
</xsl:template>

<xsl:template name="lineBreak">
  <xsl:param name="id"/>
  <xsl:text disable-output-escaping="yes">&lt;br/&gt;</xsl:text>
</xsl:template>

<xsl:template match="rng:ref/@name" mode="attributetext">
    <xsl:variable name="me">
      <xsl:choose>
        <xsl:when test="contains(.,'.attributes')">
          <xsl:value-of select="substring-before(.,'.attributes')"/>
        </xsl:when>
        <xsl:when test="contains(.,'.content')">
          <xsl:value-of select="substring-before(.,'.content')"/>
        </xsl:when>
        <xsl:when test="contains(.,'.attribute.')">
          <xsl:value-of select="substring-before(.,'.attribute.')"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="."/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:variable name="n" select="."/>
    <xsl:choose>
      <xsl:when test="contains(.,'.localattributes')">
	<xsl:value-of select="$n"/>
      </xsl:when>
      <xsl:when test="contains(.,'.content')">
	<xsl:value-of select="$n"/>
      </xsl:when>
      <xsl:when test="ancestor::teix:egXML">
	<xsl:value-of select="$n"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:for-each select="$top">
	  <xsl:call-template name="linkTogether">
	    <xsl:with-param name="name">
	      <xsl:value-of select="$me"/>
	    </xsl:with-param>
	    <xsl:with-param name="reftext">
	      <xsl:value-of select="$n"/>
	    </xsl:with-param>
	  </xsl:call-template>
	</xsl:for-each>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="tei:divGen[@type='index']"/>

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

  <xsl:template match="tei:listRef" mode="weave"/>

  <xsl:template match="tei:ptr" mode="weave">
    <xsl:choose>
      <xsl:when test="parent::tei:listRef">
	<xsl:choose>
	<xsl:when test="starts-with(@target,'#') and key('IDS',substring-after(@target,'#'))">
	  <xsl:call-template name="makeInternalLink">
	    <xsl:with-param name="target"
			    select="substring-after(@target,'#')"/>
	    <xsl:with-param name="ptr">true</xsl:with-param>
	    <xsl:with-param name="dest">
	      <xsl:call-template name="generateEndLink">
		<xsl:with-param name="where">
		  <xsl:value-of select="substring-after(@target,'#')"/>
		</xsl:with-param>
	      </xsl:call-template>
	    </xsl:with-param>
	  </xsl:call-template>
	</xsl:when>
	<xsl:when test="starts-with(@target,'#')">
	  <xsl:variable name="Chapter">
	    <xsl:value-of select="substring(@target,2,2)"/>
	  </xsl:variable>
	  <xsl:choose>
	    <xsl:when test="$Chapter='AB' or
			    $Chapter='AI' or
			    $Chapter='CC' or
			    $Chapter='CE' or
			    $Chapter='CH' or
			    $Chapter='CO' or
			    $Chapter='DI' or
			    $Chapter='DR' or
			    $Chapter='DS' or
			    $Chapter='FS' or
			    $Chapter='FT' or
			    $Chapter='GD' or
			    $Chapter='HD' or
			    $Chapter='MS' or
			    $Chapter='ND' or
			    $Chapter='NH' or
			    $Chapter='PH' or
			    $Chapter='SA' or
			    $Chapter='SG' or
			    $Chapter='ST' or
			    $Chapter='TC' or
			    $Chapter='TD' or
			    $Chapter='TS' or
			    $Chapter='USE' or
			    $Chapter='VE' or
			    $Chapter='WD'">
	  <xsl:call-template name="makeExternalLink">
	    <xsl:with-param name="ptr">true</xsl:with-param>
	    <xsl:with-param name="dest">
	      <xsl:text>http://www.tei-c.org/release/doc/tei-p5-doc/</xsl:text>
	      <xsl:value-of select="$documentationLanguage"/>
	      <xsl:text>/html/</xsl:text>
	      <xsl:value-of select="$Chapter"/>
	      <xsl:text>.html</xsl:text>
	      <xsl:value-of select="@target"/>
	    </xsl:with-param>
	  </xsl:call-template>
	    </xsl:when>
	    <xsl:otherwise>
	      <xsl:text>«</xsl:text>
	      <xsl:value-of select="@target"/>
	      <xsl:text>»</xsl:text>
	    </xsl:otherwise>
	  </xsl:choose>
	</xsl:when>
	<xsl:otherwise>
	    <xsl:apply-imports/>
	</xsl:otherwise>
	</xsl:choose>
      </xsl:when>
      <xsl:otherwise>
        <xsl:apply-imports/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

<xsl:template match="tei:elementSpec[@mode='delete']" mode="weave"/>

  <xsl:template name="logoPicture">
    <img src="jaco001d.gif" alt="" width="180" />
  </xsl:template>
  


<xsl:template name="hdr2">
<xsl:comment>no nav </xsl:comment>
</xsl:template>



  <xsl:template match="tei:ident">
    <xsl:choose>
      <xsl:when test="@type='class' and key('CLASSES',.)">
	  <xsl:call-template name="linkTogether">
	    <xsl:with-param name="name">
	      <xsl:value-of select="."/>
	    </xsl:with-param>
	    <xsl:with-param name="reftext">
	      <xsl:value-of select="."/>
	    </xsl:with-param>
	  </xsl:call-template>
      </xsl:when>
      <xsl:when test="@type">
        <span class="ident-{@type}">
          <xsl:apply-templates/>
        </span>
      </xsl:when>
      <xsl:otherwise>
        <span class="ident">
          <xsl:apply-templates/>
        </span>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="tei:gi">
    <xsl:choose>
      <xsl:when test="parent::tei:ref or  string-length(@scheme)&gt;0">
	<span class="gi">
	  <xsl:text>&lt;</xsl:text>
	  <xsl:apply-templates/>
	  <xsl:text>&gt;</xsl:text>
	</span>
      </xsl:when>
      <xsl:when test="key('ELEMENTS',.)">
	<xsl:for-each select="key('ELEMENTS',.)">
	  <xsl:call-template name="linkTogether">
	    <xsl:with-param name="class">gi</xsl:with-param>
	    <xsl:with-param name="name">
	      <xsl:value-of select="@ident"/>
	    </xsl:with-param>
	    <xsl:with-param name="reftext">
	      <xsl:choose>
		<xsl:when test="tei:content/rng:empty">
		  <span class="emptySlash">
		    <xsl:value-of select="@ident"/>
		  </span>
		</xsl:when>
		<xsl:otherwise>
		  <xsl:value-of select="@ident"/>
		</xsl:otherwise>
	      </xsl:choose>
	    </xsl:with-param>
	  </xsl:call-template>
	</xsl:for-each>
      </xsl:when>
      <xsl:otherwise>
	<span class="gi">
	  <xsl:text>&lt;</xsl:text>
	  <xsl:apply-templates/>
	  <xsl:text>&gt;</xsl:text>
	</span>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>


  <xsl:template match="a:documentation" mode="verbatim"/>

  <xsl:template match="tei:hi[@rend='math']">
    <span class="math">
      <xsl:apply-templates/>
    </span>
  </xsl:template>

 
 <xsl:template match="tei:ptr[@type='cit']">
  <a class="citlink">
   <xsl:for-each select="key('IDS',substring-after(@target,'#'))">
     <xsl:attribute name="href">
       <xsl:apply-templates select="."  mode="generateLink"/>
    </xsl:attribute>
    <xsl:apply-templates select="." mode="xref"/>
  </xsl:for-each>
 </a>
</xsl:template>




  <xsl:template name="makeHTMLHeading">
    <xsl:param name="text"/>
    <xsl:param name="class">title</xsl:param>
    <xsl:param name="level">1</xsl:param>
    <xsl:if test="not($text='')">
      <xsl:choose>
        <xsl:when test="$level='1'">
          <xsl:element name="h1">
            <xsl:attribute name="class">
              <xsl:value-of select="$class"/>
            </xsl:attribute>
            <a href="index.html"><xsl:value-of select="$text"/></a>
          </xsl:element>
          </xsl:when>
          <xsl:otherwise>
      <xsl:element name="h{$level}">
        <xsl:attribute name="class">
          <xsl:value-of select="$class"/>
        </xsl:attribute>
        <xsl:value-of select="$text"/>
      </xsl:element>
        </xsl:otherwise>
        </xsl:choose>
    </xsl:if>
  </xsl:template>
  


<xsl:template match="tei:divGen[@type='macrocat']">

  <h3>Alphabetical list</h3>
  <xsl:apply-templates mode="weave" select="key('MACRODOCS',1)">
    <xsl:sort select="@ident"/>
  </xsl:apply-templates>

  <xsl:for-each select="key('MACRODOCS',1)">
    <xsl:sort select="@module"/>
    <xsl:if
	test="generate-id(.)=generate-id(key('MACRO-MODULE',@module)[1])">
      <div id='macro-{@module}'>
      <h3>
	<xsl:for-each select="key('MODULES',@module)">
	  <xsl:text>[</xsl:text>
	  <xsl:value-of select="@ident"/>
	  <xsl:text>] </xsl:text>
	  <xsl:value-of select="tei:desc"/>
	</xsl:for-each>
      </h3>
      <xsl:apply-templates mode="weave"
			   select="key('MACRO-MODULE',@module)">
	<xsl:sort select="@ident"/>
      </xsl:apply-templates>
      </div>
    </xsl:if>
  </xsl:for-each>
</xsl:template>


<xsl:template match="tei:divGen[@type='elementcat']">
  <div class="atozwrapper">
   <xsl:call-template name="atozHeader">
     <xsl:with-param name="Key">ELEMENT-ALPHA</xsl:with-param>
   </xsl:call-template>
    <xsl:for-each select="key('ELEMENTDOCS',1)">
    <xsl:sort select="translate(@ident,$uc,$lc)"/>
    <xsl:variable name="letter">
      <xsl:value-of select="substring(@ident,1,1)"/>
    </xsl:variable>
    <xsl:if
	test="generate-id(.)=generate-id(key('ELEMENT-ALPHA',$letter)[1])">
      <div  id="element-{$letter}" class="atoz">	
	<span class="listhead">
	  <xsl:value-of select="$letter"/>
	</span>
      <ul class="atoz">	
	<xsl:for-each select="key('ELEMENT-ALPHA',$letter)">
	  <xsl:sort select="@ident"/>
	  <li>
	    <xsl:apply-templates select="." mode="weave"/>
	  </li>
	</xsl:for-each>
      </ul>
      </div>
    </xsl:if>

    </xsl:for-each>
  </div>
  <div id="byMod">
    <xsl:for-each select="key('ELEMENTDOCS',1)">
      <xsl:sort select="@module"/>
      <xsl:if
	  test="generate-id(.)=generate-id(key('ELEMENT-MODULE',@module)[1])">
	<div>
	  <h3>
	    <xsl:for-each select="key('MODULES',@module)">
	      <xsl:text>[</xsl:text>
	      <xsl:value-of select="@ident"/>
	      <xsl:text>] </xsl:text>
	      <xsl:value-of select="tei:desc"/>
	    </xsl:for-each>
	  </h3>
	  <xsl:apply-templates mode="weave"
			       select="key('ELEMENT-MODULE',@module)">
	    <xsl:sort select="@ident"/>
	  </xsl:apply-templates>
	</div>
      </xsl:if>
    </xsl:for-each>
  </div>
</xsl:template>

<xsl:template match="tei:divGen[@type='modelclasscat']">
<div class="atozwrapper">
   <xsl:call-template name="atozHeader">
     <xsl:with-param name="Key">MODEL-CLASS-ALPHA</xsl:with-param>
   </xsl:call-template>

    <xsl:for-each select="key('MODELCLASSDOCS',1)">
    <xsl:sort select="translate(substring-after(@ident,'model.'),$uc,$lc)"/>
    <xsl:variable name="letter">
      <xsl:value-of select="substring(@ident,7,1)"/>
    </xsl:variable>
    <xsl:if
	test="generate-id(.)=generate-id(key('MODEL-CLASS-ALPHA',$letter)[1])">
      <div  id="element-{$letter}" class="atoz">		
	<span class="listhead">
	  <xsl:value-of select="$letter"/>
	</span>
	<ul class="atoz">	
	  <xsl:for-each select="key('MODEL-CLASS-ALPHA',$letter)">
	    <xsl:sort select="translate(substring-after(@ident,'model.'),$lc,$uc)"/>
	    <li>
	      <xsl:apply-templates select="." mode="weave"/>
	    </li>
	  </xsl:for-each>
	</ul>
      </div>
    </xsl:if>
  </xsl:for-each>
</div>
  <div id="byMod">
    <xsl:for-each select="key('MODELCLASSDOCS',1)">
      <xsl:sort select="@module"/>
      <xsl:if
	  test="generate-id(.)=generate-id(key('MODEL-CLASS-MODULE',@module)[1])">
	<div>
	  <h3>
	    <xsl:for-each select="key('MODULES',@module)">
	      <xsl:text>[</xsl:text>
	      <xsl:value-of select="@ident"/>
	      <xsl:text>] </xsl:text>
	      <xsl:value-of select="tei:desc"/>
	    </xsl:for-each>
	  </h3>
	  <xsl:apply-templates mode="weave"
			       select="key('MODEL-CLASS-MODULE',@module)">
	    <xsl:sort select="@ident"/>
	  </xsl:apply-templates>
	</div>
      </xsl:if>
    </xsl:for-each>
  </div>

</xsl:template>

<xsl:template match="tei:divGen[@type='attclasscat']">
  <div class="atozwrapper">
   <xsl:call-template name="atozHeader">
     <xsl:with-param name="Key">ATT-CLASS-ALPHA</xsl:with-param>
   </xsl:call-template>

   <xsl:for-each select="key('ATTCLASSDOCS',1)">
     <xsl:sort select="translate(substring-after(@ident,'att.'),$uc,$lc)"/>
     <xsl:variable name="letter">
       <xsl:value-of select="substring(@ident,5,1)"/>
     </xsl:variable>
     <xsl:if
	 test="generate-id(.)=generate-id(key('ATT-CLASS-ALPHA',$letter)[1])">
      <div  id="element-{$letter}" class="atoz">		
	<span class="listhead">
	  <xsl:value-of select="$letter"/>
	</span>
       <ul class="atoz">	
	 <xsl:for-each select="key('ATT-CLASS-ALPHA',$letter)">
	   <xsl:sort select="translate(substring-after(@ident,'att.'),$lc,$uc)"/>
	   <li>
	     <xsl:apply-templates select="." mode="weave"/>
	   </li>
	 </xsl:for-each>
       </ul>
      </div>
     </xsl:if>
   </xsl:for-each>
  </div>
   <div id="byMod">
     <xsl:for-each select="key('ATTCLASSDOCS',1)">
       <xsl:sort select="@module"/>
       <xsl:if
	   test="generate-id(.)=generate-id(key('ATT-CLASS-MODULE',@module)[1])">
	 <div>
	   <h3>
	     <xsl:for-each select="key('MODULES',@module)">
	      <xsl:text>[</xsl:text>
	      <xsl:value-of select="@ident"/>
	      <xsl:text>] </xsl:text>
	      <xsl:value-of select="tei:desc"/>
	     </xsl:for-each>
	   </h3>
	   <xsl:apply-templates mode="weave"
				select="key('ATT-CLASS-MODULE',@module)">
	     <xsl:sort select="@ident"/>
	   </xsl:apply-templates>
	 </div>
       </xsl:if>
     </xsl:for-each>
  </div>

</xsl:template>

<xsl:template name="atozHeader">
  <xsl:param name="Key"/>
  <div id="azindex">
    <span>
      <xsl:call-template name="i18n">
	<xsl:with-param name="word">Sorted alphabetically</xsl:with-param>
      </xsl:call-template>
    </span>
    <ul class="index">     
      <xsl:if test="count(key($Key,'a'))&gt;0">
      <li>
	<a onclick="hideallExcept('element-a');" href="#">a</a>
      </li>
      </xsl:if>
      <xsl:if test="count(key($Key,'b'))&gt;0">
      <li>
	<a onclick="hideallExcept('element-b');" href="#">b</a>
      </li>
      </xsl:if>
      <xsl:if test="count(key($Key,'c'))&gt;0">
      <li>
	<a onclick="hideallExcept('element-c');" href="#">c</a>
      </li>
      </xsl:if>
      <xsl:if test="count(key($Key,'d'))&gt;0">
      <li>
	<a onclick="hideallExcept('element-d');" href="#">d</a>
      </li>
      </xsl:if>
      <xsl:if test="count(key($Key,'e'))&gt;0">
      <li>
	<a onclick="hideallExcept('element-e');" href="#">e</a>
      </li>
      </xsl:if>
      <xsl:if test="count(key($Key,'f'))&gt;0">
      <li>
	<a onclick="hideallExcept('element-f');" href="#">f</a>
      </li>
      </xsl:if>
      <xsl:if test="count(key($Key,'g'))&gt;0">
      <li>
	<a onclick="hideallExcept('element-g');" href="#">g</a>
      </li>
      </xsl:if>
      <xsl:if test="count(key($Key,'h'))&gt;0">
      <li>
	<a onclick="hideallExcept('element-h');" href="#">h</a>
      </li>
      </xsl:if>
      <xsl:if test="count(key($Key,'i'))&gt;0">
      <li>
	<a onclick="hideallExcept('element-i');" href="#">i</a>
      </li>
      </xsl:if>
      <xsl:if test="count(key($Key,'j'))&gt;0">
      <li>
	<a onclick="hideallExcept('element-j');" href="#">j</a>
      </li>
      </xsl:if>
      <xsl:if test="count(key($Key,'k'))&gt;0">
      <li>
	<a onclick="hideallExcept('element-k');" href="#">k</a>
      </li>
      </xsl:if>
      <xsl:if test="count(key($Key,'l'))&gt;0">
      <li>
	<a onclick="hideallExcept('element-l');" href="#">l</a>
      </li>
      </xsl:if>
      <xsl:if test="count(key($Key,'m'))&gt;0">
      <li>
	<a onclick="hideallExcept('element-m');" href="#">m</a>
      </li>      
      </xsl:if>
      <xsl:if test="count(key($Key,'n'))&gt;0">
      <li>
	<a onclick="hideallExcept('element-n');" href="#">n</a>
      </li>
      </xsl:if>
      <xsl:if test="count(key($Key,'o'))&gt;0">
      <li>
	<a onclick="hideallExcept('element-o');" href="#">o</a>
      </li>
      </xsl:if>
      <xsl:if test="count(key($Key,'p'))&gt;0">
      <li>
	<a onclick="hideallExcept('element-p');" href="#">p</a>
      </li>
      </xsl:if>
      <xsl:if test="count(key($Key,'q'))&gt;0">
      <li>
	<a onclick="hideallExcept('element-q');" href="#">q</a>
      </li>
      </xsl:if>
      <xsl:if test="count(key($Key,'r'))&gt;0">
      <li>
	<a onclick="hideallExcept('element-r');" href="#">r</a>
      </li>
      </xsl:if>
      <xsl:if test="count(key($Key,'s'))&gt;0">
      <li>
	<a onclick="hideallExcept('element-s');" href="#">s</a>
      </li>
      </xsl:if>
      <xsl:if test="count(key($Key,'t'))&gt;0">
      <li>
	<a onclick="hideallExcept('element-t');" href="#">t</a>
      </li>
      </xsl:if>
      <xsl:if test="count(key($Key,'u'))&gt;0">
      <li>
	<a onclick="hideallExcept('element-u');" href="#">u</a>
      </li>
      </xsl:if>
      <xsl:if test="count(key($Key,'v'))&gt;0">
      <li>
	<a onclick="hideallExcept('element-v');" href="#">v</a>
      </li>
      </xsl:if>
      <xsl:if test="count(key($Key,'w'))&gt;0">
      <li>
	<a onclick="hideallExcept('element-w');" href="#">w</a>
      </li>
      </xsl:if>
      <xsl:if test="count(key($Key,'x'))&gt;0">
      <li>
	<a onclick="hideallExcept('element-x');" href="#">x</a>
      </li>
      </xsl:if>
      <xsl:if test="count(key($Key,'y'))&gt;0">
      <li>
	<a onclick="hideallExcept('element-y');" href="#">y</a>
      </li>
      </xsl:if>
      <xsl:if test="count(key($Key,'z'))&gt;0">
      <li>
	<a onclick="hideallExcept('element-z');" href="#">z</a>
      </li>
      </xsl:if>
      <li class="showall">
	<a onclick="showall();" href="#">
	 <xsl:call-template name="i18n">
	    <xsl:with-param name="word">Show all</xsl:with-param>
	 </xsl:call-template>
	</a>
      </li>
      <li class="showall">
	<a onclick="showByMod();" href="#">
	 <xsl:call-template name="i18n">
	    <xsl:with-param name="word">Show by module</xsl:with-param>
	 </xsl:call-template>
	</a>
      </li>
    </ul>
  </div>
</xsl:template>

<xsl:template name="formatHeadingNumber">
  <xsl:param name="text"/>
  <xsl:param name="toc"/>
  <span class="headingNumber">
    <xsl:choose>
      <xsl:when test="$toc =''">
	<xsl:copy-of select="$text"/>
      </xsl:when>
      <xsl:when test="number(normalize-space($text))&lt;10">
	<xsl:text>&#8194;</xsl:text>
	<xsl:copy-of select="$text"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:copy-of select="$text"/>
      </xsl:otherwise>
    </xsl:choose>
  </span>
</xsl:template>



  <xsl:template name="navInterSep"> </xsl:template>


  <xsl:template name="javascriptHook">
    <xsl:call-template name="jsForOdds"/>
  </xsl:template>

  <xsl:template name="jsForOdds">

  <script type="text/javascript">
      <xsl:comment>
        <xsl:text disable-output-escaping="yes">
var displayXML=0;
states=new Array()
states[0]="element-a"
states[1]="element-b"
states[2]="element-c"
states[3]="element-d"
states[4]="element-e"
states[5]="element-f"
states[6]="element-g"
states[7]="element-h"
states[8]="element-i"
states[9]="element-j"
states[10]="element-k"
states[11]="element-l"
states[12]="element-m"
states[13]="element-n"
states[14]="element-o"
states[15]="element-p"
states[16]="element-q"
states[17]="element-r"
states[18]="element-s"
states[19]="element-t"
states[20]="element-u"
states[21]="element-v"
states[22]="element-w"
states[23]="element-x"
states[24]="element-y"
states[25]="element-z"

function startUp() {
 hideallExcept('');
}

function hideallExcept(elm) {
for (var i = 0; i &lt; states.length; i++) {
 var layer;
 if (layer = document.getElementById(states[i]) ) {
  if (states[i] != elm) {
    layer.style.display = "none";
  }
  else {
   layer.style.display = "block";
      }
  }
 }
 var mod;
 if ( mod = document.getElementById('byMod') ) {
     mod.style.display = "none";
 }
}

function showall() {
 for (var i = 0; i &lt; states.length; i++) {
   var layer;
   if (layer = document.getElementById(states[i]) ) {
      layer.style.display = "block";
      }
  }
}

function showByMod() {
  hideallExcept('');
  var mod;
  if (mod = document.getElementById('byMod') ) {
     mod.style.display = "block";
     }
}

	</xsl:text>
      </xsl:comment>
    </script>

  </xsl:template>


  <xsl:template name="generateSubTitle">
    <xsl:value-of select="tei:head"/>
  </xsl:template>

  <xsl:template name="printLink"/>

  <xsl:template match="tei:titlePage" mode="paging">
    <xsl:apply-templates select="."/>
  </xsl:template>



</xsl:stylesheet>
