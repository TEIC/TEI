<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet
  xmlns:s="http://www.ascc.net/xml/schematron"
  exclude-result-prefixes="s html exsl estr edate a fo local rng tei teix xd"
  extension-element-prefixes="exsl estr edate" version="1.0"
  xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
  xmlns:edate="http://exslt.org/dates-and-times"
  xmlns:estr="http://exslt.org/strings" xmlns:exsl="http://exslt.org/common"
  xmlns:fo="http://www.w3.org/1999/XSL/Format"
  xmlns:html="http://www.w3.org/1999/xhtml"
  xmlns:local="http://www.pantor.com/ns/local"
  xmlns:rng="http://relaxng.org/ns/structure/1.0"
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:teix="http://www.tei-c.org/ns/Examples"
  xmlns:xd="http://www.pnp-software.com/XSLTdoc"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:include href="../common/tagdocs.xsl"/>

  <xd:doc type="stylesheet">
    <xd:short> TEI stylesheet dealing with elements from the tagdocs module,
      making HTML output. </xd:short>
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


  <xsl:param name="xrefName">a</xsl:param>
  <xsl:param name="urlName">href</xsl:param>
  <xsl:param name="ulName">ul</xsl:param>
  <xsl:param name="dlName">dl</xsl:param>
  <xsl:param name="codeName">span</xsl:param>
  <xsl:param name="colspan">colspan</xsl:param>
  <xsl:param name="ddName">dd</xsl:param>
  <xsl:param name="dtName">dt</xsl:param>
  <xsl:param name="hiName">span</xsl:param>
  <xsl:param name="itemName">li</xsl:param>
  <xsl:param name="labelName">dt</xsl:param>
  <xsl:param name="rendName">class</xsl:param>
  <xsl:param name="rowName">tr</xsl:param>
  <xsl:param name="tableName">table</xsl:param>
  <xsl:param name="cellName">td</xsl:param>
  <xsl:param name="divName">div</xsl:param>
  <xsl:param name="sectionName">div</xsl:param>
  <xsl:param name="segName">span</xsl:param>
  <xsl:param name="outputNS">http://www.w3.org/1999/xhtml</xsl:param>

  <xsl:template name="lineBreak">
    <xsl:param name="id"/>
    <xsl:text>&#10;</xsl:text>
  </xsl:template>

  <xsl:key match="tei:moduleSpec[@ident]" name="FILES" use="@ident"/>

  <xsl:variable name="top" select="/"/>

  <xd:doc>
    <xd:short>[odds] Document an element, macro, or class</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="refdoc">
    <xsl:if test="$verbose='true'">
      <xsl:message> refdoc for <xsl:value-of select="name(.)"/> - <xsl:value-of
          select="@ident"/>
      </xsl:message>
    </xsl:if>
    <xsl:variable name="objectname">
      <xsl:choose>
        <xsl:when test="tei:altIdent">
          <xsl:value-of select="tei:altIdent"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="@ident"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:variable name="name">
      <xsl:value-of select="$objectname"/>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="self::tei:classSpec and not(@ident='att.global') and
		      count(key('CLASSMEMBERS',@ident))=0">
    <xsl:if test="$verbose='true'">
      <xsl:message> class <xsl:value-of select="@ident"/> omitted as it has no members
      </xsl:message>
    </xsl:if>

      </xsl:when>
      <xsl:when test="$splitLevel=-1 or $STDOUT='true'">
	<xsl:apply-templates mode="weavebody" select="."/>
      </xsl:when>
      <xsl:otherwise> 
	<span class="refDocLink">
	  <a href="ref-{@ident}{$outputSuffix}">
          <xsl:value-of select="$name"/>
        </a>
	<xsl:text> </xsl:text>
	</span>
	<xsl:variable name="BaseFile">
          <xsl:value-of select="$masterFile"/>
          <xsl:if test="ancestor::tei:teiCorpus">
            <xsl:text>-</xsl:text>
            <xsl:choose>
              <xsl:when test="@xml:id">
                <xsl:value-of select="@xml:id"/>
              </xsl:when>
              <xsl:otherwise>
                <xsl:number/>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:if>
        </xsl:variable>
	<xsl:variable name="documentationLanguage">
	  <xsl:call-template name="generateDoc"/>
	</xsl:variable>
	<xsl:variable name="langs">
	  <xsl:value-of select="concat(normalize-space($documentationLanguage),' ')"/>
	</xsl:variable>
        <xsl:call-template name="outputChunk">
          <xsl:with-param name="ident">
            <xsl:text>ref-</xsl:text>
            <xsl:value-of select="@ident"/>
          </xsl:with-param>
          <xsl:with-param name="content">
            <html>
	      <xsl:call-template name="addLangAtt"/>
              <xsl:comment>THIS IS A GENERATED FILE. DO NOT EDIT (7) </xsl:comment>
              <head>
                <title>
		  <xsl:text>TEI </xsl:text>
		  <xsl:value-of select="substring-before(local-name(),'Spec')"/>
		  <xsl:text> </xsl:text>
		  <xsl:value-of select="$name"/>
		  <xsl:text> </xsl:text>
		  <xsl:call-template name="makeGloss">
		    <xsl:with-param name="langs"  select="$langs"/>
		  </xsl:call-template>
                </title>
		<xsl:choose>
		  <xsl:when test="$cssFile = ''"/>
		  <xsl:when test="$cssFileInclude='true'">
		    <style>
		      <include xmlns="http://www.w3.org/2001/XInclude"
			  href="{$cssFile}" 
			  parse="text"/>
		    </style>
		  </xsl:when>
		  <xsl:otherwise>
		    <link href="{$cssFile}" rel="stylesheet" type="text/css"/>
		  </xsl:otherwise>
		</xsl:choose>
                <xsl:if test="not($cssSecondaryFile = '')">
                  <link href="{$cssSecondaryFile}" rel="stylesheet" type="text/css"/>
                </xsl:if>
		<xsl:call-template name="generateLocalCSS"/>
		<xsl:call-template name="metaHTML">
		  <xsl:with-param name="title">
		    <xsl:value-of select="substring-before(local-name(),'Spec')"/>
		    <xsl:text> </xsl:text>
		    <xsl:value-of select="@ident"/>
		    <xsl:text> </xsl:text>
		    <xsl:call-template name="makeGloss">
		      <xsl:with-param name="langs" select="$langs"/>
		    </xsl:call-template>
		    <xsl:text> - </xsl:text>
		    <xsl:call-template name="generateTitle"/>
		  </xsl:with-param>
		</xsl:call-template>
		<meta http-equiv="Content-Type" 
		      content="application/xhtml+xml; charset=utf-8"/>
                <xsl:call-template name="includeJavascript"/>
                <xsl:call-template name="javascriptHook"/>
              </head>
              <body id="TOP">
                <xsl:attribute name="onload">
                  <xsl:text>startUp()</xsl:text>
                </xsl:attribute>
                <xsl:call-template name="bodyHook"/>
		<xsl:call-template name="teiTOP">
		  <xsl:with-param name="name">
		    <xsl:value-of select="$name"/>
		  </xsl:with-param>
		</xsl:call-template>
                <div class="main-content">
		  <xsl:call-template name="startDivHook"/>
		  <xsl:apply-templates mode="weavebody" select="."/>
                </div>
		<xsl:call-template name="stdfooter">
		  <xsl:with-param name="file">
		    <xsl:text>ref-</xsl:text>
		    <xsl:value-of select="@ident"/>
		  </xsl:with-param>
		</xsl:call-template>
		<xsl:call-template name="bodyEndHook"/>
              </body>
            </html>
          </xsl:with-param>
        </xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>


  <xd:doc>
    <xd:short>[html] </xd:short>
    <xd:param name="text">text</xd:param>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="embolden">
    <xsl:param name="text"/>
    <b>
      <xsl:copy-of select="$text"/>
    </b>
  </xsl:template>
  <xd:doc>
    <xd:short>[html] </xd:short>
    <xd:param name="text">text</xd:param>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="italicize">
    <xsl:param name="text"/>
    <em>
      <xsl:copy-of select="$text"/>
    </em>
  </xsl:template>
  <xd:doc>
    <xd:short>[html] make a link</xd:short>
    <xd:param name="class">class</xd:param>
    <xd:param name="id">id</xd:param>
    <xd:param name="name">name</xd:param>
    <xd:param name="text">text</xd:param>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="makeLink">
    <xsl:param name="class"/>
    <xsl:param name="name"/>
    <xsl:param name="text"/>
    <a class="{$class}">
      <xsl:attribute name="href">
        <xsl:choose>
          <xsl:when test="$splitLevel=-1">
            <xsl:text>#</xsl:text>
            <xsl:value-of select="$name"/>
          </xsl:when>
	  <xsl:when test="$STDOUT='true'">
	    <xsl:for-each select="key('IDENTS',$name)">
	      <xsl:call-template name="getSpecURL">
		<xsl:with-param name="name">
		  <xsl:value-of select="$name"/>
		</xsl:with-param>
		<xsl:with-param name="type">
		  <xsl:value-of select="substring-before(local-name(),'Spec')"/>
		</xsl:with-param>
	      </xsl:call-template>
	      </xsl:for-each>
	  </xsl:when>
          <xsl:otherwise>
            <xsl:text>ref-</xsl:text>
            <xsl:value-of select="$name"/>
	    <xsl:value-of select="$outputSuffix"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:attribute>
      <xsl:copy-of select="$text"/>
    </a>
  </xsl:template>
<xsl:template name="teiTOP">
  <xsl:param name="name"/>
  <div id="hdr">
    <xsl:call-template name="stdheader">
      <xsl:with-param name="title">
	<xsl:value-of select="$name"/>
      </xsl:with-param>
    </xsl:call-template>
  </div>
</xsl:template>

  <xd:doc>
    <xd:short>[html] Provide a footer for each reference document</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>

  <xsl:template name="refdocFooter">
    <xsl:call-template name="preAddressHook"/>
    <div style="margin: 20pt; font-weight: bold;">
      <a href="{$refDocFooterURL}">
	<xsl:value-of select="$refDocFooterText"/>
      </a>
    </div>
  </xsl:template>

  <xd:doc>
    <xd:short>[html] </xd:short>
    <xd:param name="text">text</xd:param>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="ttembolden">
    <xsl:param name="text"/>
    <b>
      <tt>
        <xsl:copy-of select="$text"/>
      </tt>
    </b>
  </xsl:template>
  <xd:doc>
    <xd:short>[html] </xd:short>
    <xd:param name="text">text</xd:param>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="typewriter">
    <xsl:param name="text"/>
    <tt>
      <xsl:copy-of select="$text"/>
    </tt>
  </xsl:template>


  <xsl:template name="showRNC">
    <xsl:param name="style"/>
    <xsl:param name="contents"/>
    <span class="{$style}">
      <xsl:value-of select="$contents"/>
    </span>
  </xsl:template>

  <xsl:template name="emptySlash">
    <xsl:param name="name"/>
    <span class="emptySlash">
	<xsl:value-of select="$name"/>
    </span>
  </xsl:template>

  <xd:doc>
    <xd:short>Process elements teix:egXML</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="teix:egXML">
    <div>
      <xsl:attribute name="class">
	<xsl:text>pre</xsl:text>
	<xsl:if test="not(*)">
	  <xsl:text> cdata</xsl:text>
	</xsl:if>
      </xsl:attribute>
      <xsl:call-template name="egXMLStartHook"/>
      <xsl:call-template name="makeAnchor"/>
      <xsl:apply-templates mode="verbatim"/>
      <xsl:call-template name="egXMLEndHook"/>
    </div>
  </xsl:template>

  <xd:doc>
    <xd:short>[html] </xd:short>
    <xd:param name="grammar">grammar</xd:param>
    <xd:param name="content">content</xd:param>
    <xd:param name="element">element</xd:param>
    <xd:detail> </xd:detail>
  </xd:doc>

  <xsl:template name="bitOut">
    <xsl:param name="grammar"/>
    <xsl:param name="content"/>
    <xsl:param name="element">pre</xsl:param>
    <xsl:choose>
      <xsl:when test="$displayMode='both'">
	<div class="displayRelax">
	 <button class="displayRelaxButton">
	  <span class="RNG_Compact">
	    <xsl:call-template name="i18n">
	      <xsl:with-param name="word">Compact to XML format</xsl:with-param>
	    </xsl:call-template>
	  </span>
	  <span class="RNG_XML">
	    <xsl:call-template name="i18n">
	      <xsl:with-param name="word">XML format to compact</xsl:with-param>
	    </xsl:call-template>
	  </span>
	 </button>
	 <pre class="RNG_XML">
	    <xsl:apply-templates mode="verbatim"
				 select="exsl:node-set($content)/*/*"/>
	 </pre>
	 <pre class="RNG_Compact">
	   <xsl:call-template name="make-body-from-r-t-f">
	     <xsl:with-param name="schema">
	       <xsl:for-each select="exsl:node-set($content)/*">
		 <xsl:call-template name="make-compact-schema"/>
	       </xsl:for-each>
	     </xsl:with-param>
	   </xsl:call-template>
	 </pre>
	</div>
      </xsl:when>
      <xsl:when test="$displayMode='rng'">
	<xsl:element name="{$element}">
	  <xsl:attribute name="class">eg</xsl:attribute>
	  <xsl:apply-templates mode="verbatim"
			       select="exsl:node-set($content)/*/*"/>
	</xsl:element>
      </xsl:when>
      <xsl:when test="$displayMode='rnc'">
	<xsl:element name="{$element}">
	  <xsl:attribute name="class">eg</xsl:attribute>
	  <xsl:call-template name="make-body-from-r-t-f">
	    <xsl:with-param name="schema">
	      <xsl:for-each select="exsl:node-set($content)/*">
		<xsl:call-template name="make-compact-schema"/>
	      </xsl:for-each>
	    </xsl:with-param>
	  </xsl:call-template>
	</xsl:element>
      </xsl:when>
      <xsl:otherwise>
	<xsl:element name="{$element}">
	  <xsl:attribute name="class">eg</xsl:attribute>
	  <xsl:for-each select="exsl:node-set($content)/*">
	    <xsl:apply-templates mode="literal"/>
	  </xsl:for-each>
	</xsl:element>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>


  <xsl:template name="showSpace">
    <xsl:text> </xsl:text>
  </xsl:template>


  <xsl:template match="tei:schemaSpec">
    
    <xsl:choose>
      <xsl:when test="tei:specGrpRef">
	<xsl:variable name="SPECS">
	  <tei:schemaSpec>
	    <xsl:copy-of select="@*"/>
	    <xsl:apply-templates mode="expandSpecs"/>
	  </tei:schemaSpec>
	</xsl:variable>
	<xsl:for-each select="exsl:node-set($SPECS)/tei:schemaSpec">
	  <xsl:call-template name="schemaSpecWeave"/>
	</xsl:for-each>
      </xsl:when>
      <xsl:otherwise>
	<xsl:call-template name="schemaSpecWeave"/>
      </xsl:otherwise>
    </xsl:choose>

  </xsl:template>

  <xsl:template name="schemaSpecWeave">
    <xsl:if test="$verbose='true'">
      <xsl:message>Processing schemaSpec <xsl:value-of
      select="@ident"/></xsl:message>
    </xsl:if>
    <xsl:if test="tei:classSpec[@type='model']">
      <h2>Schema <xsl:value-of select="@ident"/>: Model classes</h2>
      <xsl:apply-templates mode="weave" select="tei:classSpec[@type='model']">
	<xsl:sort select="@ident"/>
      </xsl:apply-templates>
    </xsl:if>
    
    
    <xsl:if test="tei:classSpec[@type='atts']">
      <h2>Schema <xsl:value-of select="@ident"/>: Attribute classes</h2>
      <xsl:apply-templates mode="weave" select="tei:classSpec[@type='atts']">
	<xsl:sort select="@ident"/>
      </xsl:apply-templates>
    </xsl:if>
    
    <xsl:if test="tei:macroSpec">
      <h2>Schema <xsl:value-of select="@ident"/>: Macros</h2>
      <xsl:apply-templates mode="weave" select="tei:macroSpec">
	<xsl:sort select="@ident"/>
      </xsl:apply-templates>
      
    </xsl:if>
    <h2>Schema <xsl:value-of select="@ident"/>: Elements</h2>
    <xsl:apply-templates mode="weave" select="tei:elementSpec">
      <xsl:sort select="@ident"/>
    </xsl:apply-templates>
    
  </xsl:template>


  <xd:doc>
    <xd:short>[odds] make a link</xd:short>
    <xd:param name="name">name</xd:param>
    <xd:param name="id">id</xd:param>
    <xd:detail> </xd:detail>
  </xd:doc>

  <xsl:template name="makeSectionHead">
    <xsl:param name="name"/>
    <xsl:param name="id"/>
    <h3 class="oddSpec">
      <xsl:call-template name="makeAnchor">
       <xsl:with-param name="name">
         <xsl:value-of select="$id"/>
       </xsl:with-param>
      </xsl:call-template>
      <xsl:value-of select="$name"/>
    </h3>
  </xsl:template>

  <xsl:template name="specHook">
    <xsl:param name="name"/>
  </xsl:template>


</xsl:stylesheet>
