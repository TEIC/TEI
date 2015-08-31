<xsl:stylesheet
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
    xmlns:rss="http://purl.org/rss/1.0/"
    xmlns:dc="http://purl.org/dc/elements/1.1/"
    xmlns:syn="http://purl.org/rss/1.0/modules/syndication/"
    xmlns:taxo="http://purl.org/rss/1.0/modules/taxonomy/"
    xmlns:svg="http://www.w3.org/2000/svg"
    xmlns:mathml="http://www.w3.org/1998/Math/MathML"
    xmlns:html="http://www.w3.org/1999/xhtml"
    exclude-result-prefixes="rdf dc syn taxo rss rdf html mathml tei"
    version="1.0">
  
  <xsl:param name="display-width" select="1000"/>

  <xsl:key name="transcription-by-zone-id"
	   match="tei:*[@facs]" 
	   use="substring-after(@facs, '#')"/>
  
  <xsl:template match="/">
    <html>
      <head>
	<title>facsimile/surface/zone/graphic</title>
	<style type="text/css">
	  div.surface-graphics {
	  position: relative;
	  border-width: 1px;
	  border-style: solid;
	  border-color: black;
	  padding: 0;
	  margin-bottom: 1em;
	  }
	  div.surface {
	  position: absolute;
	  border-width: 1px;
	  border-style: solid;
	  border-color: yellow;
	  }
	  div.zone {
	  position: absolute;
	  border-width: 1px;
	  border-style: solid;
	  border-color: blue;						
	  }
	</style>
      </head>
      <body>
<!-- process any surface which has a zone with graphics in -->	
	<xsl:for-each select="tei:TEI/tei:facsimile/tei:surface[tei:zone/tei:graphic]">
	  <!-- render each distinct surface graphically -->
	  <!-- first determine the area which exactly circumscribes
all the graphics which cover this surface -->

	  <xsl:variable name="left">
	    <xsl:for-each select="tei:zone">
	      <xsl:sort select="@ulx" data-type="number"/>
	      <xsl:if test="position() = 1">
		<xsl:value-of select="@ulx" />
	      </xsl:if>
	    </xsl:for-each>
	  </xsl:variable>

	  <xsl:variable name="right">
	    <xsl:for-each select="tei:zone">
	      <xsl:sort select="@lrx" data-type="number"
			order="descending" />
	      <xsl:if test="position() = 1">
		<xsl:value-of select="@lrx" />
	      </xsl:if>
	    </xsl:for-each>
	  </xsl:variable>

	  <xsl:variable name="top">
	    <xsl:for-each select="tei:zone">
	      <xsl:sort select="@uly" data-type="number"/>
	      <xsl:if test="position() = 1">
		<xsl:value-of select="@uly" />
	      </xsl:if>
	    </xsl:for-each>
	  </xsl:variable>

	  <xsl:variable name="bottom">
	    <xsl:for-each select="tei:zone">
	      <xsl:sort select="@lry" data-type="number"
			order="descending" />
	      <xsl:if test="position() = 1">
		<xsl:value-of select="@lry" />
	      </xsl:if>
	    </xsl:for-each>
	  </xsl:variable>

	  <xsl:variable name="surface-graphics-width" select="number($right) - number($left)"/>
	  <xsl:variable name="surface-graphics-height" select="number($bottom) - number($top)"/>
	  <xsl:variable name="scale" select="$display-width div $surface-graphics-width"/>
	  <!-- create a div to group together all the graphics in this surface -->
	  <div class="surface-graphics">
	    <xsl:attribute name="style">
	      width: <xsl:value-of select="$scale * $surface-graphics-width"/>px;
	      height: <xsl:value-of select="$scale * $surface-graphics-height"/>px;
	    </xsl:attribute>
	    
	    <xsl:comment>Zones containing facsimile images</xsl:comment>
	    <xsl:call-template name="render-zones">
	      <xsl:with-param name="zones" select="tei:zone[tei:graphic]"/>
	      <xsl:with-param name="surface-graphics-height" select="$surface-graphics-height"/>
	      <xsl:with-param name="surface-graphics-width" select="$surface-graphics-width"/>
	      <xsl:with-param name="left" select="$left"/>
	      <xsl:with-param name="top" select="$top"/>
	    </xsl:call-template>
	    
	    <xsl:if test="@ulx">
	      <xsl:comment>The physical surface itself</xsl:comment>
	      <xsl:variable name="surface-left">
		<xsl:value-of select="@ulx"/>
	      </xsl:variable>

	      <xsl:variable name="surface-top">
		<xsl:value-of select="@uly"/>
	      </xsl:variable>

	      <xsl:variable name="surface-right">
		<xsl:value-of select="@lrx"/>
	      </xsl:variable>

	      <xsl:variable name="surface-bottom">
		<xsl:value-of select="@lry"/>
	      </xsl:variable>

	      <xsl:variable name="surface-width" select="$surface-right - $surface-left"/>
	      <xsl:variable name="surface-height" select="$surface-bottom - $surface-top"/>
	      <div class="surface">
		<xsl:attribute name="style">
		  width: <xsl:value-of select="100 * $surface-width div number($surface-graphics-width)"/>%; 
		  left: <xsl:value-of select="100 * ($surface-left - $left) div number($surface-graphics-width)"/>%; 
		  top: <xsl:value-of select="100 * ($surface-top - $top) div number($surface-graphics-height)"/>%; 
		  height: <xsl:value-of select="100 * $surface-height div number($surface-graphics-height)"/>%
		</xsl:attribute>
	      </div>
	    </xsl:if>
	    <xsl:comment>Zones which do not contain facsimile images</xsl:comment>
	    <xsl:call-template name="render-zones">
	      <xsl:with-param name="zones" select="tei:zone[not(tei:graphic)]"/>
	      <xsl:with-param name="surface-graphics-height" select="$surface-graphics-height"/>
	      <xsl:with-param name="surface-graphics-width" select="$surface-graphics-width"/>
	      <xsl:with-param name="left" select="$left"/>
	      <xsl:with-param name="top" select="$top"/>
	    </xsl:call-template>
	  </div>
	</xsl:for-each>
	
      </body>
    </html>
  </xsl:template>
  
  <xsl:template name="test">
    <xsl:param name="param"/>
    <xsl:value-of select="count($param)"/>
  </xsl:template>
  
  <xsl:template name="render-zones">
    <xsl:param name="zones"/>
    <xsl:param name="surface-graphics-width"/>
    <xsl:param name="surface-graphics-height"/>
    <xsl:param name="left"/>
    <xsl:param name="top"/>
    <!-- render each zone, from largest to smallest -->
    <xsl:for-each select="$zones">
      <xsl:sort 
	  data-type="number"
	  order="descending"
	  select="(number(@lrx) - number(@ulx)) * (number(@lry) - number(@uly))"/>
      
      <!-- render a zone of the surface -->
      
      <xsl:variable name="zone-left" select="@ulx"/>
      <xsl:variable name="zone-top" select="@uly"/>
      <xsl:variable name="zone-right" select="@lrx"/>
      <xsl:variable name="zone-bottom" select="@lry"/>
      <xsl:variable name="zone-width" 
		    select="number($zone-right) - number($zone-left)"/>
      <xsl:variable name="zone-height" 
		    select="number($zone-bottom) - number($zone-top)"/>
      
      
      <xsl:variable name="zone-transcription" select="key('transcription-by-zone-id', @xml:id)"/>	
      
      <div class="zone" title="{$zone-transcription}">
	
	<xsl:attribute name="style">
	  width: <xsl:value-of select="100 * $zone-width div number($surface-graphics-width)"/>%; 
	  left: <xsl:value-of select="100 * ($zone-left - $left) div number($surface-graphics-width)"/>%; 
	  top: <xsl:value-of select="100 * ($zone-top - $top) div number($surface-graphics-height)"/>%; 
	  height: <xsl:value-of select="100 * $zone-height div number($surface-graphics-height)"/>%
	</xsl:attribute>
	<xsl:copy-of select="@xml:id"/>
	<xsl:if test="not(tei:graphic)"><xsl:comment>no graphic</xsl:comment></xsl:if>
	<xsl:if test="tei:graphic">
	  <img src="{tei:graphic/@url}" width="100%" height="100%"/>
	</xsl:if>
      </div>
    </xsl:for-each>
  </xsl:template>
  
  
  
  
</xsl:stylesheet>
<!--

techniques getting maximum and minimum, from Jeni Tennison 

1. Through an XPath: select the SessionStartupTime for which there is
no higher (or lower) value:

   JartaXmlReport[not(../JartaXmlReport/SessionStartupTime &gt;
                      SessionStartupTime)]/SessionStartupTime

2. Through a sort-and-pick-first approach, sorting in descending (or
ascending) order:

   <xsl:for-each select="JartaXmlReport">
      <xsl:sort select="SessionStartupTime" data-type="number"
                orders="descending" />
      <xsl:if test="position() = 1">
         <xsl:value-of select="SessionStartupTime" />
      </xsl:if>
   </xsl:for-each>

3. Through a recursive template that walks through from higher to
higher SessionStartupTime.  Apply it only to the first JartaXmlReport:

  <xsl:apply-templates select="JartaXmlReport[1]" mode="find-max" />

The template is something like:
  
<xsl:template match="JartaXmlReport" mode="find-max">
   <xsl:variable name="next"
                 select="following-sibling::JartaXmlReport
                            [SessionStartupTime &gt;
                             current()/SessionStartupTime]" />
   <xsl:choose>
      <xsl:when test="$next">
         <xsl:apply-templates select="$next" mode="find-max" />
      </xsl:when>
      <xsl:otherwise>
         <xsl:value-of select="SessionStartupTime" />
      </xsl:otherwise>
   </xsl:choose>
</xsl:template>
-->