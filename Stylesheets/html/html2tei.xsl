<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
    xmlns:xs="http://www.w3.org/2001/XMLSchema"
    exclude-result-prefixes="tei xs"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
    xmlns="http://www.tei-c.org/ns/1.0" 
    xmlns:tei="http://www.tei-c.org/ns/1.0" 
    version="2.0" 
    xpath-default-namespace="http://www.w3.org/1999/xhtml">

  <xsl:output method="xml" indent="yes"/>

  <xsl:template match="html">
    <TEI>
      <xsl:apply-templates/>
    </TEI>
  </xsl:template>

  <xsl:template match="head">
    <teiHeader>
      <fileDesc>
        <titleStmt>
          <title>
            <xsl:value-of select="title"/>
          </title>
          <author>
            <xsl:value-of select="meta[@name='dc.Creator']/@content"/>
          </author>
        </titleStmt>
        <publicationStmt>
	  <p></p>
        </publicationStmt>
      </fileDesc>
    </teiHeader>
  </xsl:template>

  <xsl:template match="body">
    <text>
      <body>
      <xsl:variable name="Body">
	<HEAD level="1" magic="true">Start</HEAD>
        <xsl:apply-templates/>
      </xsl:variable>

      <xsl:variable name="Body2">
	<xsl:for-each select="$Body">
	  <xsl:apply-templates mode="pass1"/>
	</xsl:for-each>
      </xsl:variable>

      <xsl:for-each select="$Body2">
        <xsl:for-each-group select="tei:*" group-starting-with="tei:HEAD[@level='1']">
          <xsl:choose>
            <xsl:when test="self::tei:HEAD[@level='1']">
	      <xsl:call-template name="group-by-section"/>
            </xsl:when>
            <xsl:otherwise>
	      <xsl:call-template name="inSection"/>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:for-each-group>
      </xsl:for-each>
    </body>
    </text>
  </xsl:template>


  <xsl:template name="group-by-section">
    <xsl:variable name="ThisHeader" select="number(@level)"/>
    <xsl:variable name="NextHeader" select="number(@level)+1"/>
    <xsl:choose>
      <xsl:when test="@magic">
	  <xsl:for-each-group select="current-group() except ."
			      group-starting-with="tei:HEAD[number(@level)=$NextHeader]">
	    <xsl:choose>
	      <xsl:when test="self::tei:HEAD">
		<xsl:call-template name="group-by-section"/>
	      </xsl:when>
	    <xsl:otherwise>
	      <xsl:call-template name="inSection"/>
	    </xsl:otherwise>
	    </xsl:choose>
	  </xsl:for-each-group>
      </xsl:when>
      <xsl:otherwise>
	<div>
	  <xsl:if test="@style">
	    <xsl:attribute name="rend" select="@style"/>
	  </xsl:if>
	  <xsl:if test="not(@interpolated='true')">
	    <head>
	      <xsl:apply-templates mode="pass1"/>
	    </head>
	  </xsl:if>
	  <xsl:for-each-group select="current-group() except ."
			      group-starting-with="tei:HEAD[number(@level)=$NextHeader]">
	    <xsl:choose>
	      <xsl:when test="self::tei:HEAD">
		<xsl:call-template name="group-by-section"/>
	      </xsl:when>
	    <xsl:otherwise>
		<xsl:call-template name="inSection"/>
	    </xsl:otherwise>
	    </xsl:choose>
	  </xsl:for-each-group>
	</div>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>


  <xsl:template name="inSection">
    <xsl:for-each-group select="current-group()"
			group-adjacent="if (self::tei:GLOSS)
					then 1
					else 2">      
      <xsl:choose>
	<xsl:when test="current-grouping-key()=1">
	  <list type="gloss">
	    <xsl:for-each select="current-group()">
	      <xsl:element name="{@n}">
		<xsl:apply-templates mode="pass2"/>
	      </xsl:element>
	    </xsl:for-each>
	  </list>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:for-each select="current-group()">
	    <xsl:apply-templates select="." mode="pass2"/>
	  </xsl:for-each>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:for-each-group>
  </xsl:template>

  <xsl:template match="h1|h2|h3|h4|h5|h6|h7">
    <HEAD level="{substring(local-name(),2,1)}">
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </HEAD>
  </xsl:template>
		
  <xsl:template match="@*|text()|comment()|processing-instruction()" mode="pass1">
    <xsl:copy-of select="."/>
  </xsl:template>

  <xsl:template match="tei:p[not(node())]"
		mode="pass1"/>

  <xsl:template match="tei:HEAD" mode="pass1">
    <xsl:if test="preceding-sibling::tei:HEAD">
      <xsl:variable name="prev"
		    select="xs:integer(number(preceding-sibling::tei:HEAD[1]/@level))"/>
      <xsl:variable name="current"
		    select="xs:integer(number(@level))"/>
	<xsl:if test="($current - $prev) &gt;1 ">
	  <!--<xsl:message>Walk from <xsl:value-of select="$prev"/> to <xsl:value-of select="$current"/></xsl:message>-->
	  <xsl:for-each
	      select="$prev + 1   to $current - 1 ">
	    <HEAD interpolated='true' level="{.}"/>
	  </xsl:for-each>
	</xsl:if>
    </xsl:if>
    <xsl:copy>
    <xsl:apply-templates
	select="*|@*|processing-instruction()|comment()|text()"
	mode="pass1"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="*" mode="pass1">
    <xsl:copy>
      <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()" mode="pass1"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="@*|text()|comment()|processing-instruction()" mode="pass2">
    <xsl:copy-of select="."/>
  </xsl:template>

  <xsl:template match="tei:p[not(*) and normalize-space(.)='']" mode="pass2"/>

  <xsl:template match="*" mode="pass2">
    <xsl:copy>
      <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()" mode="pass2"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="br">
    <lb/>
  </xsl:template>

  <xsl:template match="a">
    <xsl:choose>
      <xsl:when test="@href">
        <ref target="{@href}">
          <xsl:apply-templates/>
        </ref>
      </xsl:when>
      <xsl:when test="@name">
        <anchor>
          <xsl:attribute name="xml:id" select="@name"/>
        </anchor>
        <xsl:apply-templates/>
      </xsl:when>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="li">
    <item>
      <xsl:apply-templates/>
    </item>
  </xsl:template>

  <xsl:template match="div">
      <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="link">
</xsl:template>

  <xsl:template match="meta">
</xsl:template>

  <xsl:template match="p">
    <p>
      <xsl:apply-templates select="*|@*|text()|comment()"/>
    </p>
  </xsl:template>
  <xsl:template match="p[@class='note']">
    <note>
      <xsl:apply-templates select="*|@*|text()|comment()"/>
    </note>
  </xsl:template>
  <xsl:template match="title">
</xsl:template>
  <xsl:template match="ul">
    <list type="unordered">
      <xsl:apply-templates/>
    </list>
  </xsl:template>
  <xsl:template match="ol">
    <list type="ordered">
      <xsl:apply-templates/>
    </list>
  </xsl:template>
  <xsl:template match="em">
    <hi rend="italic">
      <xsl:apply-templates/>
    </hi>
  </xsl:template>
  <xsl:template match="img">
    <graphic url="{@src}">
      <xsl:for-each select="@width">
        <xsl:attribute name="width">
          <xsl:value-of select="."/>
          <xsl:analyze-string select="." regex="^[0-9]+$">
            <xsl:matching-substring>
              <xsl:text>px</xsl:text>
            </xsl:matching-substring>
          </xsl:analyze-string>
        </xsl:attribute>
      </xsl:for-each>
      <xsl:for-each select="@height">
        <xsl:attribute name="height">
          <xsl:value-of select="."/>
          <xsl:analyze-string select="." regex="^[0-9]+$">
            <xsl:matching-substring>
              <xsl:text>px</xsl:text>
            </xsl:matching-substring>
          </xsl:analyze-string>
        </xsl:attribute>
      </xsl:for-each>
    </graphic>
  </xsl:template>

  <xsl:template match="pre">
    <eg>
      <xsl:apply-templates/>
    </eg>
  </xsl:template>

  <xsl:template match="strong">
    <hi rend="bold">
      <xsl:apply-templates/>
    </hi>
  </xsl:template>

  <xsl:template match="sup">
    <hi rend="sup">
      <xsl:apply-templates/>
    </hi>
  </xsl:template>

  <xsl:template match="@class">
    <xsl:attribute name="rend" select="."/>
  </xsl:template>

  <xsl:template match="@id">
    <xsl:attribute name="xml:id" select="."/>
  </xsl:template>

  <xsl:template match="@title"/>

  <xsl:template match="@*|comment()|text()">
    <xsl:copy-of select="."/>
  </xsl:template>

  <xsl:template match="span">
    <hi>
      <xsl:if test="@class">
	<xsl:attribute name="rend" select="@class"/>
      </xsl:if>
      <xsl:apply-templates select="@*|*|text()"/>
    </hi>
  </xsl:template>

  <xsl:template match="b">
    <hi rend="bold">
      <xsl:apply-templates select="@*|*|text()"/>
    </hi>
  </xsl:template>

  <xsl:template match="i">
    <hi rend="italic">
      <xsl:apply-templates select="@*|*|text()"/>
    </hi>
  </xsl:template>

  <xsl:template match="font">
      <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="blockquote">
    <quote>
      <xsl:apply-templates select="@*|*|text()"/>
    </quote>
  </xsl:template>

  <xsl:template match="tt">
    <code>
      <xsl:apply-templates select="@*|*|text()"/>
    </code>
  </xsl:template>

  <xsl:template match="code">
    <eg>
      <xsl:apply-templates select="@*|*|text()"/>
    </eg>
  </xsl:template>

  <xsl:template match="table">
    <table>
      <xsl:apply-templates select="@*|*|text()"/>
    </table>
  </xsl:template>

  <xsl:template match="td">
    <cell>
      <xsl:apply-templates select="@*|*|text()"/>
    </cell>
  </xsl:template>

  <xsl:template match="tr">
    <row>
      <xsl:apply-templates select="@*|*|text()"/>
    </row>
  </xsl:template>

  <xsl:template match="hr"/>

  <xsl:template match="*">
    <xsl:message>UNKNOWN TAG <xsl:value-of select="name()"/></xsl:message>
    <xsl:apply-templates/>
  </xsl:template>

</xsl:stylesheet>
