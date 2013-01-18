<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns="http://www.tei-c.org/ns/1.0"
    xmlns:teix="http://www.tei-c.org/ns/Examples"
    xmlns:s="http://www.ascc.net/xml/schematron" 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns:estr="http://exslt.org/strings"
    xmlns:t="http://www.thaiopensource.com/ns/annotations"
    xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
    xmlns:rng="http://relaxng.org/ns/structure/1.0"
    exclude-result-prefixes="tei t a rng s teix" 
    version="2.0">
  
  
  <xsl:import href="/usr/share/xml/tei/stylesheet/odds2/teiodds.xsl"/>

  <xsl:import
      href="/usr/share/xml/tei/stylesheet/odds2/odd2lite.xsl"/>
  
  <xsl:param name="TEISERVER">http://tei.oucs.ox.ac.uk/Query/</xsl:param>
  <xsl:param name="displayMode">rnc</xsl:param>
  
  <xsl:param name="oddmode">tei</xsl:param>
  
  <xsl:param name="DOCUMENTATIONLANG">en</xsl:param>
  
  <xsl:variable name="top" select="/"/>
  
  <xsl:key name="ELEMENTINMOD" match="tei:elementSpec" use="@module"/>
  <xsl:key name="ACLASSINMOD" match="tei:classSpec[@type='atts']" use="@module"/>
  <xsl:key name="MCLASSINMOD" match="tei:classSpec[@type='model']" use="@module"/>
  <xsl:key name="MACROINMOD" match="tei:macroSpec" use="@module"/>
  
  <xsl:key name="REFS" 
	   match="tei:elementSpec|tei:macroSpec"
	   use=".//rng:ref/@name"/>
  
  <xsl:key name="IDENTS" match="tei:*[@ident]"	  use="@ident"/>
  
  <xsl:key name="EVERYTHING" 
	   match="tei:elementSpec|tei:macroSpec|tei:classSpec"
	   use="'1'"/>
  
  <xsl:key  name="LOCALCLASSREFS" match="tei:elementSpec|tei:classSpec"
	    use="tei:classes/tei:memberOf/@key"/>
  
  <xsl:template match="/">
    <TEI xmlns="http://www.tei-c.org/ns/1.0">
      <teiHeader>
	<fileDesc>
	  <titleStmt>
	    <title>TEI P5 Element and Class Catalogue</title>
	    <author></author>
	  </titleStmt>
	  <publicationStmt>
	    <p> </p>
	  </publicationStmt>
	  <sourceDesc>
	    <p>from source</p>
	  </sourceDesc>
	</fileDesc>
      </teiHeader>
      <text>
	<body>
	  <p>The sections in this document summarize all the TEI
	  modules, showing the elements, classes
	  and macros defined. </p>
	  <xsl:for-each select="//tei:moduleSpec">
	    <xsl:sort select="@ident"/>
	    <div>
	      <xsl:attribute name="xml:id">
		<xsl:text>Module_</xsl:text>
		<xsl:value-of select="@ident"/>
	      </xsl:attribute>
	      <head>		 
		<xsl:value-of select="@ident"/>
		<xsl:text> module</xsl:text>
	      </head>
	      <xsl:if test="count(key('MCLASSINMOD',@ident))&gt;0">
		<xsl:call-template name="section">
		  <xsl:with-param name="name">
		    <xsl:value-of select="@ident"/>
		    <xsl:text>_modelClasses</xsl:text>
		  </xsl:with-param>
		  <xsl:with-param name="content">
		    <head><xsl:value-of select="@ident"/> Model Classes</head>
		    <table rules="all" border="1">
		      <xsl:attribute name="preamble">
			<xsl:text>P{0.25\textwidth}|P{0.25\textwidth}|P{0.5\textwidth}|</xsl:text>
		      </xsl:attribute>
		      <row role="label">
			<cell>Name</cell>
<!--			<cell></cell>-->
			<cell>Used in content model of</cell>
			<cell>Members</cell>
		      </row>
		      <xsl:for-each select="key('MCLASSINMOD',@ident)">
			<xsl:sort select="@ident"/>
			<row>
			  <cell>
			    <xsl:attribute name="xml:id">
			      <xsl:text>summary_</xsl:text>
			      <xsl:value-of select="@ident">
			      </xsl:value-of>
			    </xsl:attribute>
			    <ref n="{normalize-space(tei:desc[1])}"
				 target="http://tei.oucs.ox.ac.uk/Query/tag.xq?name={@ident}">
			      <xsl:value-of select="@ident"/>
			    </ref>
			  </cell>
<!--
			  <cell>
			    <ref target="http://localhost/TV/draw.php?nt={@ident}">draw</ref>
			  </cell>
-->
			  <cell>
			    <xsl:for-each select="key('REFS',@ident)">
			      <xsl:if 
				  test="not(generate-id(.)=generate-id(key('REFS',@ident)[1]))"> 
			      <ref target="#summary_{@ident}">
				<xsl:choose>
				  <xsl:when
				      test="string-length(@ident)=0">
				  </xsl:when>
				  <xsl:otherwise>
				<xsl:value-of select="@ident"/> 
				  </xsl:otherwise>
				</xsl:choose>
			      </ref>
				<xsl:text>: </xsl:text>
			      </xsl:if>

			    </xsl:for-each>
			    <xsl:call-template name="refsbyclass"/>
			  </cell>
			  <cell >
			    <xsl:for-each select="key('LOCALCLASSREFS',@ident)">
			      <xsl:sort select="@ident"/>
			      <ref target="#summary_{@ident}">
				<xsl:choose>
				  <xsl:when
				      test="string-length(@ident)=0">
				  </xsl:when>
				  <xsl:otherwise>
				<xsl:value-of select="@ident"/> 
				  </xsl:otherwise>
				</xsl:choose>
			      </ref>
			      <xsl:text>: </xsl:text>
			    </xsl:for-each>
			  </cell>
			</row>
		      </xsl:for-each>
		    </table>
		  </xsl:with-param>
		</xsl:call-template>
	      </xsl:if>
	      <xsl:if test="count(key('ACLASSINMOD',@ident))&gt;0">
		<xsl:call-template name="section">
		  <xsl:with-param name="name">
		    <xsl:value-of select="@ident"/>
		    <xsl:text>_attributeClasses</xsl:text>
		  </xsl:with-param>
		  <xsl:with-param name="content">
		    <head><xsl:value-of select="@ident"/> Attribute Classes</head>
		    <table rules="all" border="1">
		      <xsl:attribute name="preamble">
			<xsl:text>P{0.25\textwidth}|P{0.5\textwidth}|P{0.25\textwidth}|</xsl:text>
		      </xsl:attribute>
		      <row role="label">
			<cell>Name</cell>
<!--			<cell></cell>-->
			<cell>Members</cell>
			<cell>Attributes</cell>
		      </row>
		      <xsl:for-each select="key('ACLASSINMOD',@ident)">
			<xsl:sort select="@ident"/>
			<row>
			  <cell>
			    <xsl:attribute name="xml:id">
			      <xsl:text>summary_</xsl:text>
			      <xsl:value-of select="@ident">
			      </xsl:value-of>
			    </xsl:attribute>
			    
			    <ref n="{normalize-space(tei:desc[1])}"
				 target="http://tei.oucs.ox.ac.uk/Query/tag.xq?name={@ident}">
			      <xsl:value-of select="@ident"/>
			    </ref>
			  </cell>
<!--
			  <cell>
			    <ref target="http://localhost/TV/draw.php?nt={@ident}">pic</ref>
			  </cell>
-->
			  <cell >
			    <xsl:variable name="ID" select="@ident"/>
			    <xsl:for-each select="key('LOCALCLASSREFS',@ident)">
			      <xsl:sort select="@ident"/>
			      <ref target="#summary_{@ident}">
				<xsl:choose>
				  <xsl:when
				      test="string-length(@ident)=0">
				  </xsl:when>
				  <xsl:otherwise>
				    <xsl:value-of select="@ident"/> 
				  </xsl:otherwise>
				</xsl:choose>
			      </ref>
			      <xsl:text>: </xsl:text>
			    </xsl:for-each>
			  </cell>
			  <cell>
			    <xsl:for-each select=".//tei:attDef">
			      <xsl:if
				  test="preceding-sibling::tei:attDef">
				<xsl:text>: </xsl:text>
			      </xsl:if>
			      <xsl:value-of select="@ident"/>
			    </xsl:for-each>
			  </cell>
			</row>
		      </xsl:for-each>
		    </table>
		  </xsl:with-param>
		</xsl:call-template>
	      </xsl:if>
	      <xsl:if test="count(key('ELEMENTINMOD',@ident))&gt;0">
		<xsl:call-template name="section">
		  <xsl:with-param name="name">
		    <xsl:value-of select="@ident"/>
		    <xsl:text>_Elements</xsl:text>
		  </xsl:with-param>
		  <xsl:with-param name="content">
		    <head><xsl:value-of select="@ident"/> Elements</head>
		    <table rend="rules">
		      <xsl:attribute name="preamble">
			<xsl:text>P{0.1\textwidth}|P{0.35\textwidth}|P{0.2\textwidth}|P{0.2\textwidth}|P{0.15\textwidth}|P{0.15\textwidth}|</xsl:text>
		      </xsl:attribute>
		      <row role="label">
			<cell>Name</cell>
<!--			<cell></cell>-->
			<cell>Description</cell>
			<cell>Classes</cell>
			<cell>Used in</cell>
			<cell>Attributes</cell>
			<cell>Content model</cell>
		      </row>
		      <xsl:for-each select="key('ELEMENTINMOD',@ident)">
			<xsl:sort select="@ident"/>
			<row>
			  <cell>
			    <xsl:attribute name="xml:id">
			      <xsl:text>summary_</xsl:text>
			      <xsl:value-of select="@ident">
			      </xsl:value-of>
			    </xsl:attribute>
			    <ref n="{normalize-space(tei:desc[1])}"
				 target="http://tei.oucs.ox.ac.uk/Query/tag.xq?name={@ident}">
			      <xsl:value-of select="@ident"/>
			    </ref>
			  </cell>
<!--
			  <cell>
			    <ref target="http://localhost/TV/draw.php?nt={@ident}">pic</ref>
			  </cell>
-->
			  <cell>
			  <xsl:choose>
			    <xsl:when test="tei:desc/@xml:lang=$DOCUMENTATIONLANG">
			      <xsl:value-of
				  select="tei:desc[@xml:lang=$DOCUMENTATIONLANG]"/>
			    </xsl:when>
			    <xsl:otherwise>
			      <xsl:value-of
				  select="normalize-space(tei:desc[not(@xml:lang)])"/>
			    </xsl:otherwise>
			  </xsl:choose>
			    </cell>
			  <cell><xsl:for-each select="tei:classes">
			    <xsl:for-each select="tei:memberOf">
			      <xsl:sort select="@key"/>
			      <ref target="#summary_{@key}">
				<xsl:for-each
				    select="key('IDENTS',@key)">
				  <xsl:choose>
				    <xsl:when test="@type='model'">
				      <hi><xsl:value-of
				      select="@ident"/></hi>
				      <xsl:text>: </xsl:text>
				    </xsl:when>
				    <xsl:otherwise>
				      <xsl:value-of select="@ident"/>
				      <xsl:text>: </xsl:text>
				    </xsl:otherwise>
				  </xsl:choose>
				</xsl:for-each>
			      </ref>
			    </xsl:for-each>
			  </xsl:for-each>
			  </cell>
			  <cell >
			    <xsl:for-each select="key('REFS',@ident)">
			      <xsl:sort select="@ident"/>
			      <ref target="#summary_{@ident}">
				<xsl:choose>
				  <xsl:when test="self::rng:ref"/>
				  <xsl:when
				      test="string-length(@ident)=0">
				    <xsl:value-of select="name(.)"/>
				    <xsl:text>: </xsl:text>
				  </xsl:when>
				  <xsl:otherwise>
				    <xsl:value-of select="@ident"/> 
				    <xsl:text>: </xsl:text>
				  </xsl:otherwise>
				</xsl:choose>
			      </ref>
			    </xsl:for-each>
			  </cell>
			  <cell>
			    <xsl:for-each select=".//tei:attDef">
			      <xsl:if
				  test="preceding-sibling::tei:attDef">
				<xsl:text>: </xsl:text>
			      </xsl:if>
			      <xsl:value-of select="@ident"/>
			    </xsl:for-each>
			  </cell>
			  
			      <cell><q rend="eg">
			      <xsl:call-template name="make-body-from-r-t-f">
			      <xsl:with-param name="schema">
			      <xsl:for-each  select="tei:content">
			      <xsl:call-template name="make-compact-schema"/>
			      </xsl:for-each>
			      </xsl:with-param>
			      </xsl:call-template>
			      </q></cell>

			</row>
		      </xsl:for-each>
		    </table>
		  </xsl:with-param>
		</xsl:call-template>
	      </xsl:if>
	      <xsl:if test="count(key('MACROINMOD',@ident))&gt;0">
		<xsl:call-template name="section">
		  <xsl:with-param name="name">
		    <xsl:value-of select="@ident"/>
		    <xsl:text>_Macros</xsl:text>
		  </xsl:with-param>
		  <xsl:with-param name="content">
		    <head><xsl:value-of select="@ident"/>
		    Macros</head>
		    <list type="gloss">
		      <xsl:for-each select="key('MACROINMOD',@ident)">
			<xsl:sort select="@ident"/>
			  <label>
			    <xsl:attribute name="xml:id">
			      <xsl:text>summary_</xsl:text>
			      <xsl:value-of select="@ident">
			      </xsl:value-of>
			    </xsl:attribute>
			    
			    <ref n="{normalize-space(tei:desc[1])}"
				 target="http://tei.oucs.ox.ac.uk/Query/tag.xq?name={@ident}">
			      <xsl:value-of select="@ident"/>
			    </ref>
			  </label>
			  <item>
			    <xsl:text>used in </xsl:text>
			    <xsl:variable name="ID" select="@ident"/>
			    <xsl:for-each select="key('REFS',@ident)">
			      <xsl:if
				  test="not(generate-id(.)=generate-id(key('REFS',$ID)[1]))"> 
				<xsl:text>: </xsl:text>
			      </xsl:if>
			      <ref target="#summary_{@ident}">
				<xsl:choose>
				  <xsl:when
				      test="string-length(@ident)=0">
5<xsl:value-of select="name(.)"/>
				  </xsl:when>
				  <xsl:otherwise>
				<xsl:value-of select="@ident"/> 
				  </xsl:otherwise>
				</xsl:choose>
			      </ref>
			    </xsl:for-each>
			    <q rend="eg">
			      <xsl:call-template name="make-body-from-r-t-f">
				<xsl:with-param name="schema">
				  <xsl:for-each  select="tei:content">
				    <xsl:call-template name="make-compact-schema"/>
				  </xsl:for-each>
				</xsl:with-param>
			      </xsl:call-template>
			    </q>
			  </item>
		      </xsl:for-each>
		    </list>
		  </xsl:with-param>
		</xsl:call-template>
	      </xsl:if>
	    </div>
	  </xsl:for-each>
<!--
	    <xsl:for-each select="key('EVERYTHING','1')">
		  <xsl:sort select="@ident"/>
		  <xsl:call-template name="refdoc"/>
		</xsl:for-each>
	  </div>
-->
	</body>
      </text>
    </TEI>
  </xsl:template>
  
  <xsl:template name="showElement">
    <xsl:param name="name"/>
    <ref target="#summary_{$name}"><xsl:value-of
    select="$name"/></ref>
    <xsl:value-of select="name(.)"/>
  </xsl:template>

  
  <xsl:template name="linkTogether">
    <xsl:param name="name"/>
    <xsl:param name="reftext"/>
    <xsl:param name="class"/>
    <xsl:variable name="link">
      <xsl:choose>
	<xsl:when test="$reftext=''"><xsl:value-of select="$name"/></xsl:when>
	<xsl:otherwise><xsl:value-of select="$reftext"/></xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <ref target="#summary_{$name}">
      <xsl:value-of select="$link"/>
    </ref>
  </xsl:template>
  
  <xsl:template match="teix:egXML">
    <xsl:copy-of select="."/>
  </xsl:template>
  
  <xsl:template name="section">
    <xsl:param name="name"/>
    <xsl:param name="content"/>
<!--    <exsl:document method="xml" 
		   encoding="utf-8" 
		   href="modList/{$name}.xml">-->
      <div type="oddcatalogue">
	<xsl:attribute name="xml:id">
	  <xsl:value-of select="$name"/>
	</xsl:attribute>
	<xsl:copy-of select="$content"/>
      </div>
<!--
    </exsl:document>
    <xi:include xmlns:xi="http://www.w3.org/2001/XInclude" href="modList/{$name}.xml"/>
    -->
  </xsl:template>
  
  <xsl:template name="refsbyclass">
    <xsl:if test="tei:classes/tei:memberOf/@key">
      <xsl:for-each
	  select="tei:classes/tei:memberOf">
	<xsl:text> [via </xsl:text>
	<xsl:value-of select="@key"/>
	<xsl:text>: </xsl:text>
	<xsl:for-each select="key('IDENTS',@key)">
	  <xsl:for-each  select="key('REFS',@ident)">
	    <xsl:if 
		test="not(generate-id(.)=generate-id(key('REFS',@ident)[1]))">:
	    </xsl:if>
	    <ref target="#summary_{@ident}">
	      <xsl:value-of select="@ident"/> 
	    </ref>
	  </xsl:for-each>
	<xsl:call-template name="refsbyclass"/>
	</xsl:for-each>
	<xsl:text> ] </xsl:text>
      </xsl:for-each>
    </xsl:if>
  </xsl:template>
</xsl:stylesheet>



