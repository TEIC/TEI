<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
    xmlns="http://www.tei-c.org/ns/1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
    xpath-default-namespace="http://www.tei-c.org/ns/1.0"
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    version="2.0">
  <xsl:import href="../common/functions.xsl"/>
  <xsl:param name="method">modules</xsl:param>
  <xsl:param name="minimal">false</xsl:param>
  <xsl:param name="title">TEI Tree</xsl:param>
  <xsl:param name="includeheader">true</xsl:param>
  <xsl:param name="elementsonly">false</xsl:param>
  <xsl:output indent="no"/>
  <xsl:template match="/">
    <xsl:for-each select="//body">
      <TEI xmlns="http://www.tei-c.org/ns/1.0">
        <teiHeader type="text">
          <fileDesc>
            <titleStmt>
              <title type="main"><xsl:value-of select="$title"/></title>
            </titleStmt>
            <publicationStmt>
              <p>Stylesheets test file</p>
            </publicationStmt>
            <sourceDesc>
              <p/>
            </sourceDesc>
          </fileDesc>
        </teiHeader>
        <text>
          <body>
            <p>
              <eTree rend="d3CollapsableTree">
                <label>TEI</label>
		<xsl:choose>
		  <xsl:when test="$method='modules'">
                    <xsl:for-each select=".//moduleSpec">
                      <xsl:sort select="@ident"/>
		      <xsl:choose>
			<xsl:when test="$includeheader='false' and
					@ident='header'"/>
			<xsl:otherwise>
			  <eTree>
			    <label>
			      <xsl:value-of select="@ident"/>
			    </label>
			    <xsl:for-each
				select="//*[@module=current()/@ident]">
			      <xsl:sort select="@ident"/>
			      <xsl:choose>
				<xsl:when  test="$elementsonly='true' and not(self::elementSpec)"/>
				<xsl:when  test="self::classSpec and @type='atts'"/>
				<xsl:otherwise>
				  <xsl:call-template name="innards"/>
				</xsl:otherwise>
			      </xsl:choose>
			    </xsl:for-each>
			  </eTree>
			</xsl:otherwise>
		      </xsl:choose>
		    </xsl:for-each>
		  </xsl:when>
		  <xsl:when test="$method='classes'">
                    <xsl:for-each
			select=".//classSpec[@type='model' and not (classes/memberOf)]">
                      <xsl:sort select="@ident"/>
		      <xsl:call-template name="class"/>
		    </xsl:for-each>
		  </xsl:when>
		</xsl:choose>
              </eTree>
            </p>
          </body>
        </text>
      </TEI>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="class">
    <eTree>
      <label rend="highlight">
	<xsl:value-of select="@ident"/>
      </label>
      <xsl:for-each select="//*[classes/memberOf/@key=current()/@ident]">
	<xsl:sort select="@ident"/>
	<xsl:choose>
	  <xsl:when test="self::elementSpec">
	    <xsl:call-template name="innards"/>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:call-template name="class"/>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:for-each>
    </eTree>		      
  </xsl:template>

  <xsl:template name="innards">
    <xsl:choose>
      <xsl:when test="$minimal='false' and attList/attDef">
	<eTree>
	  <label>
	    <xsl:if test="self::classSpec">
	      <xsl:attribute name="rend">highlight</xsl:attribute>
	    </xsl:if>
            <xsl:value-of select="@ident"/>
	  </label>
          <xsl:for-each select="attList/attDef">
            <xsl:sort select="@ident"/>
	    <eTree>
              <label>@<xsl:value-of select="@ident"/></label>
	      <eLeaf>
		<label rend="desc">
		  <xsl:sequence select="tei:makeDescription(., true(), true())"/>
		  <xsl:text> (</xsl:text>
		  <xsl:value-of select="if (@usage='opt' or not(@usage)) then
					'Optional' else 'Mandatory'"/>
		  <xsl:text>)</xsl:text>
		</label>
	    </eLeaf>
	    </eTree>
          </xsl:for-each>
	</eTree>
      </xsl:when>
      <xsl:otherwise>
	<eLeaf>
	  <label>
            <xsl:value-of select="@ident"/>
	  </label>
	</eLeaf>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="tei:desc" mode="inLanguage">
    <xsl:value-of select="normalize-space(.)"/>
  </xsl:template>

</xsl:stylesheet>
