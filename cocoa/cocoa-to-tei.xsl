<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xs="http://www.w3.org/2001/XMLSchema" xpath-default-namespace="http://www.tei-c.org/ns/1.0" xmlns="http://www.tei-c.org/ns/1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="2.0" exclude-result-prefixes="xs">
  <xsl:output indent="yes" omit-xml-declaration="yes"/>
  <xsl:strip-space elements="body"/>
  <!-- This script is used to convert COCOA to TEI in three passes:
       a) tokenize raw text into lines and replace COCOA markup with XML
       b) convert XML to TEI
       c) add some more structure to the TEI
       
       It is released under a Creative Commons 
       Attribution+Non-Commercial license.
       
       James Cummings, Sebastian Rahtz. 2010.
       
  -->
  <xsl:param name="input"/>
  <xsl:param name="debug">false</xsl:param>
  <xsl:template name="main">
    <xsl:choose>
      <xsl:when test="unparsed-text-available($input)">
        <xsl:variable name="pass0">
          <xsl:for-each select="tokenize(unparsed-text($input), '\r?\n')">
            <xsl:analyze-string select="." regex="&lt;\s*([A-Za-z0-9]+)(\s*)(.*?)&gt;">
              <xsl:matching-substring>
                <xsl:element name="{regex-group(1)}">
                  <xsl:attribute name="value">
                    <xsl:value-of select="regex-group(3)"/>
                  </xsl:attribute>
                </xsl:element>
              </xsl:matching-substring>
              <xsl:non-matching-substring>
                <xsl:value-of select="."/>
                <xsl:text>
</xsl:text>
              </xsl:non-matching-substring>
            </xsl:analyze-string>
          </xsl:for-each>
        </xsl:variable>
        <xsl:if test="$debug='true'">
          <xsl:result-document href="pass0.xml">
            <xsl:copy-of select="$pass0"/>
          </xsl:result-document>
        </xsl:if>
        <xsl:variable name="pass1">
          <xsl:for-each select="$pass0">
                <body>
                  <xsl:apply-templates mode="pass1"/>
                </body>              
         </xsl:for-each>
        </xsl:variable>
        <xsl:if test="$debug='true'">
          <xsl:result-document href="pass1.xml">
            <xsl:copy-of select="$pass1"/>
          </xsl:result-document>
        </xsl:if>
        <xsl:variable name="pass2">
          <xsl:for-each select="$pass1">
            <xsl:apply-templates/>
          </xsl:for-each>
        </xsl:variable>
        <xsl:if test="$debug='true'">
          <xsl:result-document href="pass2.xml">
            <xsl:copy-of select="$pass2"/>
          </xsl:result-document>
        </xsl:if>
        <xsl:for-each select="$pass2">
          <xsl:apply-templates mode="pass2"/>
        </xsl:for-each>
      </xsl:when>
      <xsl:otherwise>
        <xsl:message>Cannot locate <xsl:value-of select="$input"/></xsl:message>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <!-- first pass -->
  <xsl:template match="TITLE" priority="-1" mode="pass1">
    <title>
      <xsl:value-of select="@value"/>
    </title>
  </xsl:template>
  <xsl:template match="A" mode="pass1">
    <author>
      <xsl:value-of select="@value"/>
    </author>
  </xsl:template>
  <xsl:template match="SN|SNN|SCENE|D" mode="pass1">
    <milestone unit="{local-name()}" type="nest">
      <xsl:copy-of select="@value"/>
      <xsl:attribute name="level">
        <xsl:choose>
          <xsl:when test="starts-with(@value,'ACT')">1</xsl:when>
          <xsl:when test="starts-with(@value,'Act')">1</xsl:when>
          <xsl:when test="starts-with(@value,'SCENE')">2</xsl:when>
          <xsl:when test="starts-with(@value,'Scene')">2</xsl:when>
          <xsl:otherwise>3</xsl:otherwise>
        </xsl:choose>
      </xsl:attribute>
    </milestone>
  </xsl:template>
  <xsl:template match="T" mode="pass1">
    <milestone unit="{local-name()}" type="flat">
      <xsl:copy-of select="@value"/>
    </milestone>
  </xsl:template>
  <xsl:template match="S|C|Q|QW" mode="pass1">
    <speaker>
      <xsl:value-of select="@value"/>
    </speaker>
  </xsl:template>
  <xsl:template match="SSD|LSD|Z" mode="pass1">
    <stage>
      <xsl:value-of select="@value"/>
    </stage>
  </xsl:template>
  <xsl:template match="P|W" mode="pass1">
    <head type="author">
      <xsl:value-of select="@value"/>
    </head>
  </xsl:template>
  <xsl:template match="Y" mode="pass1">
    <date>
      <xsl:value-of select="@value"/>
    </date>
  </xsl:template>
  <xsl:template match="L" mode="pass1">
    <xsl:analyze-string select="@value" regex="^[0-9]+$">
      <xsl:matching-substring>
      </xsl:matching-substring>
      <xsl:non-matching-substring>
        <l>
          <xsl:value-of select="."/>
        </l>
      </xsl:non-matching-substring>
    </xsl:analyze-string>
  </xsl:template>
  <xsl:template match="text()" mode="pass1">
    <xsl:for-each select="tokenize(., '\n')">
      <ab>
        <xsl:value-of select="normalize-space(.)"/>
      </ab>
    </xsl:for-each>
  </xsl:template>
  <!-- second pass processing -->
  <xsl:template match="@*|text()|comment()|processing-instruction">
    <xsl:copy-of select="."/>
  </xsl:template>
  <xsl:template match="*">
    <xsl:copy>
      <xsl:apply-templates select="@*|*|text()|comment()|processing-instruction"/>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="body">
    <body>
      <xsl:for-each-group select="*|text()" group-starting-with="milestone[@level='1']">
        <xsl:choose>
          <!-- We are dealing with a first level section, we now have
	     to further divide the section into subsections that we can then
	     finally work on -->
          <xsl:when test="self::milestone[@level='1']">
            <xsl:call-template name="group-by-section"/>
          </xsl:when>
          <!-- We have found some loose paragraphs. These are most probably
	     front matter. We can simply convert them without further
	     trying to split them up into sub sections. -->
          <xsl:otherwise>
            <xsl:call-template name="maintext"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:for-each-group>
    </body>
  </xsl:template>
  <xsl:template name="group-by-section">
    <xsl:variable name="next" select="number(@level)+1"/>
    <div>
      <head>
        <xsl:value-of select="@value"/>
      </head>
      <xsl:for-each-group select="current-group() except ." group-starting-with="milestone[@level=$next]">
        <xsl:choose>
          <xsl:when test="self::milestone[@level=$next]">
            <xsl:call-template name="group-by-section"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:call-template name="maintext"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:for-each-group>
    </div>
  </xsl:template>
  <xsl:template name="maintext">
    <xsl:for-each-group select="current-group()" group-starting-with="milestone[not(@level)]">
      <xsl:choose>
        <xsl:when test="self::milestone">
          <xsl:choose>
            <xsl:when test="starts-with(@value,'PROSE')">
              <p>
                <xsl:apply-templates select="current-group() except ."/>
              </p>
            </xsl:when>
            <xsl:when test="@value='SDD'">
              <stage>
                <xsl:apply-templates select="current-group() except ."/>
              </stage>
            </xsl:when>
            <xsl:when test="@value='SDE'">
              <stage type="exit">
                <xsl:apply-templates select="current-group() except ."/>
              </stage>
            </xsl:when>
            <xsl:when test="@value='SDP'">
              <stage>
                <xsl:apply-templates select="current-group() except ."/>
              </stage>
            </xsl:when>
            <xsl:when test="@value='SONG'">
              <lg>
                <xsl:apply-templates select="current-group() except ."/>
              </lg>
            </xsl:when>
            <xsl:when test="@value='TITLE'">
              <title>
                <xsl:apply-templates select="current-group() except ."/>
              </title>
            </xsl:when>
            <xsl:when test="starts-with(@value,'VERSE')">
              <xsl:for-each-group select="current-group() except ." group-starting-with="speaker">
                <xsl:choose>
                  <xsl:when test="self::speaker">
                    <sp>
                      <speaker>
                        <xsl:value-of select="."/>
                      </speaker>
                      <xsl:apply-templates select="current-group()            except ."/>
                    </sp>
                  </xsl:when>
                  <xsl:otherwise>
                    <xsl:apply-templates select="current-group()"/>
                  </xsl:otherwise>
                </xsl:choose>
              </xsl:for-each-group>
            </xsl:when>
          </xsl:choose>
        </xsl:when>
        <xsl:otherwise>
          <xsl:apply-templates select="current-group()"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:for-each-group>
  </xsl:template>
  <!-- pass 2 -->
  <xsl:template match="@*|text()|comment()|processing-instruction" mode="pass2">
    <xsl:copy-of select="."/>
  </xsl:template>
  <xsl:template match="*" mode="pass2">
    <xsl:copy>
      <xsl:apply-templates select="@*|*|text()|comment()|processing-instruction" mode="pass2"/>
    </xsl:copy>
  </xsl:template>
  <!-- abandon empty lines -->
  <xsl:template match="ab[not(node())]" mode="pass2" priority="1001"/>
  <!-- lines in verse -->
  <xsl:template match="ab[parent::lg or parent::sp]" mode="pass2">
    <l>
      <xsl:apply-templates select="@*|*|text()|comment()|processing-instruction" mode="pass2"/>
    </l>
  </xsl:template>
  <!-- lines in prose -->
  <xsl:template match="ab[parent::p]" mode="pass2">
    <xsl:apply-templates select="@*|*|text()|comment()|processing-instruction" mode="pass2"/>
    <lb/>
  </xsl:template>
<!-- abs are invalid in title elements. -->
  <xsl:template match="ab[parent::title]" mode="pass2">
    <xsl:apply-templates mode="#current"/>
    <xsl:if test="following::* or following::text()[string-length(normalize-space(.)) gt 1]"><lb/><xsl:text>&#x0A;</xsl:text></xsl:if>
  </xsl:template>
<!-- title elements are invalid as direct children of body. -->
  <xsl:template match="body/title" mode="pass2">
    <ab>
      <title><xsl:apply-templates mode="#current"/></title>
    </ab>
  </xsl:template>
  <xsl:template match="author" mode="pass2"/>
  <xsl:template match="date" mode="pass2"/>
  <xsl:template match="title[1]" mode="pass2"/>
  <xsl:template match="body" mode="pass2">
            <TEI>
              <teiHeader>
                <fileDesc>
                  <titleStmt>
                    <title>
		      <xsl:for-each select="title[1]">
			<xsl:apply-templates mode="pass2"/>
		      </xsl:for-each>
		    </title>
                    <author>
		      <xsl:for-each select="author">
			<xsl:apply-templates mode="pass2"/>
		      </xsl:for-each>
		    </author>
                  </titleStmt>
		  <xsl:if test="date">
		    <editionStmt>
		      <edition>
			<xsl:for-each select="date">
			  <xsl:apply-templates mode="pass2"/>
			</xsl:for-each>
		    </edition>
		    </editionStmt>
		  </xsl:if>		  
                  <publicationStmt>
                    <p/>
                  </publicationStmt>
                  <sourceDesc>
                    <p>Converted from COCOA format</p>
                  </sourceDesc>
                </fileDesc>
              </teiHeader>
              <text>
		<body>
		  <xsl:apply-templates
		      select="@*|*|text()|comment()|processing-instruction"
		      mode="pass2"/>
		</body>
	      </text>
	    </TEI>
  </xsl:template>

</xsl:stylesheet>
