
<!-- 
Text Encoding Initiative Consortium XSLT stylesheet family
$Date$, $Revision$, $Author$

XSL stylesheet to format TEI XML documents to HTML or XSL FO

 
##LICENSE
--> 
<xsl:stylesheet
  xmlns:tei="http://www.tei-c.org/ns/1.0"

  xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
  version="1.0">

<xsl:template match="tei:figDesc"/>
<xsl:template match="tei:figure/tei:head"/>
<xsl:param name="dpi">96</xsl:param>

<xsl:template match="tei:figure">
  <xsl:if test="@file|@url|@entity">
    <xsl:call-template name="showGraphic">
      <xsl:with-param name="ID">
	<xsl:if test="@id|@xml:id">
	  <xsl:value-of select="@id|@xml:id"/>
	</xsl:if>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:if>
  <xsl:apply-templates/>
  <xsl:if test="tei:head">
    <p>
      <xsl:choose>
	<xsl:when test="ancestor::tei:front and
			$numberFrontFigures='true'">
	  <xsl:value-of select="$figureWord"/>
	  <xsl:text> </xsl:text>
	  <xsl:number level="any"   count="tei:figure[tei:head]" from="tei:front"/>.<xsl:text> </xsl:text>
	</xsl:when>
	<xsl:when test="ancestor::tei:back and
			$numberBackFigures='true'">
	  <xsl:value-of select="$figureWord"/>
	  <xsl:text> </xsl:text>
	  <xsl:number level="any"  count="tei:figure[tei:head]" from="tei:back"/>.<xsl:text> </xsl:text>
	</xsl:when>
	<xsl:when test="ancestor::tei:body and $numberFigures='true'" >
	  <xsl:value-of select="$figureWord"/>
	  <xsl:text> </xsl:text>
	  <xsl:number level="any"   count="tei:figure[tei:head]" from="tei:body"/>.<xsl:text> </xsl:text>
	</xsl:when>
      </xsl:choose>
      <xsl:apply-templates select="tei:head" mode="plain"/>
    </p>
  </xsl:if>
</xsl:template>

<xsl:template match="tei:graphic">
  <xsl:call-template name="showGraphic">
      <xsl:with-param name="ID">
	<xsl:for-each select="..">
	  <xsl:if test="@id|@xml:id">
	    <xsl:value-of select="@id|@xml:id"/>
	  </xsl:if>
	  </xsl:for-each>
      </xsl:with-param>
    </xsl:call-template>
</xsl:template>

<xsl:template name="showGraphic">
  <xsl:param name="ID"/>
  <xsl:if test="not($ID='')">
    <a name="{$ID}"/>
  </xsl:if>
  <xsl:variable name="File">
    <xsl:choose> 
      <xsl:when test="@url">
	<xsl:value-of select="@url"/>
	<xsl:if test="not(contains(@url,'.'))">
	  <xsl:value-of select="$graphicsSuffix"/>
	</xsl:if>
      </xsl:when>
      <xsl:when test="@file">
	<xsl:value-of select="@file"/>
	<xsl:if test="not(contains(@file,'.'))">
	  <xsl:value-of select="$graphicsSuffix"/>
	</xsl:if>
      </xsl:when>
      <xsl:when test="@entity">
	<xsl:variable name="entity">
	  <xsl:value-of select="unparsed-entity-uri(@entity)"/>
	</xsl:variable>
	<xsl:choose>
	  <xsl:when test="starts-with($entity,'file:')">
	    <xsl:value-of select="substring-after($entity,'file:')"/>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:value-of select="$entity"/>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:when>
      <xsl:otherwise>
	<xsl:message terminate="yes">Cant work out how to do a graphic </xsl:message>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  <xsl:variable name="Alt">
    <xsl:choose>
      <xsl:when test="tei:figDesc">
	<xsl:value-of select="tei:figDesc//text()"/>
      </xsl:when>
      <xsl:when test="tei:head">
	<xsl:value-of select="tei:head/text()"/>
      </xsl:when>
      <xsl:when test="parent::tei:figure/tei:figDesc">
	<xsl:value-of select="parent::tei:figure/tei:figDesc//text()"/>
      </xsl:when>
      <xsl:when test="parent::tei:figure/tei:head">
	<xsl:value-of select="parent::tei:figure/tei:head/text()"/>
      </xsl:when>
    </xsl:choose>
  </xsl:variable>
  <xsl:choose>
    <xsl:when test="$showFigures='true'">
      <span>
	<xsl:if test="not($ID='')">
	  <xsl:attribute name="name"><xsl:value-of select="$ID"/></xsl:attribute>
	</xsl:if>
	<img src="{$graphicsPrefix}{$File}">
	  <xsl:if test="@rend">
	      <xsl:attribute name="class"><xsl:value-of  select="@rend"/></xsl:attribute>
	  </xsl:if>
	  <xsl:if test="@width">
	    <xsl:call-template name="setDimension">
	      <xsl:with-param name="value">
		<xsl:value-of select="@width"/>
	      </xsl:with-param>
	      <xsl:with-param name="name">width</xsl:with-param>
	    </xsl:call-template>
	  </xsl:if>
	  <xsl:if test="@height">
	    <xsl:call-template name="setDimension">
	      <xsl:with-param name="value">
		<xsl:value-of select="@height"/>
	      </xsl:with-param>
	      <xsl:with-param name="name">height</xsl:with-param>
	    </xsl:call-template>
	  </xsl:if>
	  <xsl:attribute name="alt">
	    <xsl:value-of select="$Alt"/>
	  </xsl:attribute>
	  <xsl:call-template name="imgHook"/>
	</img>
      </span>
    </xsl:when>
    <xsl:otherwise>
      <hr/>
      <p><xsl:value-of select="$figureWord"/>
      <xsl:text> </xsl:text>
      <xsl:for-each
	  select="self::tei:figure|parent::tei:figure">
	<xsl:number level="any" count="tei:figure[tei:head]"/>
      </xsl:for-each>
      file <xsl:value-of select="$File"/>
      [<xsl:value-of select="$Alt"/>]
      </p>
      <hr/>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template name="imgHook"/>

<xsl:template name="setDimension">
  <xsl:param name="name"/>
  <xsl:param name="value"/>
  <xsl:variable name="calcvalue">
    <xsl:choose>
      <xsl:when test="contains($value,'in')">
	<xsl:value-of select="round($dpi * substring-before($value,'in'))"/>
      </xsl:when>
      <xsl:when test="contains($value,'pt')">
	<xsl:value-of select="round($dpi * (substring-before($value,'pt') div 72))"/>
      </xsl:when>
      <xsl:when test="contains($value,'cm')">
	<xsl:value-of select="round($dpi * (
			      substring-before($value,'cm') div 2.54 ))"/>
      </xsl:when>
      <xsl:when test="contains($value,'px')">
	<xsl:value-of select="substring-before($value,'px')"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:value-of select="$value"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  <xsl:if test="$calcvalue&gt;0">
    <xsl:attribute name="{$name}">
      <xsl:value-of  select="$calcvalue"/>
    </xsl:attribute>
  </xsl:if>
</xsl:template>

</xsl:stylesheet>
