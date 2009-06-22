<xsl:stylesheet
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
    xmlns:rss="http://purl.org/rss/1.0/"
    xmlns:dc="http://purl.org/dc/elements/1.1/"
    xmlns:syn="http://purl.org/rss/1.0/modules/syndication/"
    xmlns:taxo="http://purl.org/rss/1.0/modules/taxonomy/"
    xmlns:mathml="http://www.w3.org/1998/Math/MathML"
    xmlns:html="http://www.w3.org/1999/xhtml"
    exclude-result-prefixes="rdf dc syn taxo rss rdf html mathml tei"
    version="1.0">
  
  <xsl:import
   href="/usr/share/xml/tei/stylesheet/html/tei.xsl"/>

  <xsl:param name="cssSecondaryFile">surfaces.css</xsl:param>
  <xsl:param name="autoToc">false</xsl:param>
  <xsl:param name="display-width" select="1000"/>

  <xsl:key name="REFS" use="@facs" match="tei:*"/>

  <xsl:key name="transcription-by-zone-id"
	   match="tei:*[@facs]" 
	   use="substring-after(@facs, '#')"/>
  

  <xsl:template name="startDivHook">
    <xsl:for-each select="@facs">
      <xsl:for-each
	  select="key('IDS',substring-after(.,'#'))">
	<xsl:variable name="width">
	  <xsl:value-of select="number(@lrx) -  number(@ulx)"/>
	</xsl:variable>
	<xsl:variable name="height">
	  <xsl:value-of select="number(@lry) -  number(@uly)"/>
	</xsl:variable>
	<div class="surface-graphics">
	    <xsl:attribute name="style">
	      <xsl:text>width: </xsl:text>
	      <xsl:value-of select="$width"/>
	      <xsl:text>px; height: </xsl:text>
	      <xsl:value-of select="$height"/>
	      <xsl:text>px;</xsl:text>
	    </xsl:attribute>
	  <img src="{tei:graphic/@url}"/>
	  <xsl:call-template name="render-zones">
	    <xsl:with-param name="surface-graphics-height" select="$height"/>
	    <xsl:with-param name="surface-graphics-width" select="$width"/>
	    <xsl:with-param name="left" select="@ulx"/>
	    <xsl:with-param name="top" select="@uly"/>
	  </xsl:call-template>
	</div>
      </xsl:for-each>
    </xsl:for-each>
  </xsl:template>

<xsl:template name="bodyHook">
  <xsl:call-template name="writeJavascript">
    <xsl:with-param name="content">
<![CDATA[
 function showLine(which) {
 var pic= document.getElementById(which);
 pic.style.display = "block";
}
 function hideLine(which) {
 var pic= document.getElementById(which);
 pic.style.display = "none";

}
]]>
    </xsl:with-param>
  </xsl:call-template>
</xsl:template>


  <xsl:template match="tei:s">
    <span onMouseOver="showLine('{substring-after(@facs,'#')}')"
	  onMouseOut="hideLine('{substring-after(@facs,'#')}')">
      <xsl:apply-templates/>
    </span>
  </xsl:template>

  <xsl:template name="render-zones">

    <xsl:param name="surface-graphics-width"/>
    <xsl:param name="surface-graphics-height"/>
    <xsl:param name="left"/>
    <xsl:param name="top"/>
    <xsl:for-each select="tei:zone">

      <xsl:variable name="zone-left" select="@ulx"/>
      <xsl:variable name="zone-top" select="@uly"/>
      <xsl:variable name="zone-right" select="@lrx"/>
      <xsl:variable name="zone-bottom" select="@lry"/>
      <xsl:variable name="zone-width" 
		    select="number($zone-right) - number($zone-left)"/>
      <xsl:variable name="zone-height" 
		    select="number($zone-bottom) - number($zone-top)"/>
      
      <div class="zone" id="{@xml:id}">
	<xsl:attribute name="style">
	  width: <xsl:value-of select="100 * $zone-width div number($surface-graphics-width)"/>%; 
	  left: <xsl:value-of select="100 * ($zone-left - $left) div number($surface-graphics-width)"/>%; 
	  top: <xsl:value-of select="100 * ($zone-top - $top) div number($surface-graphics-height)"/>%; 
	  height: <xsl:value-of select="100 * $zone-height div number($surface-graphics-height)"/>%
	</xsl:attribute>
      </div>
    </xsl:for-each>
  </xsl:template>

</xsl:stylesheet>
