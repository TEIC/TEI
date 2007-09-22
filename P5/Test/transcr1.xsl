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


  <xsl:import
   href="/usr/share/xml/tei/stylesheet/html/tei.xsl"/>

<xsl:key name="REFS" use="@facs" match="tei:*"/>



<xsl:template name="bodyHook">
  <xsl:call-template name="writeJavascript">
    <xsl:with-param name="content">
<![CDATA[
if (!document.layers&&!document.all&&!document.getElementById)
event="test"
function showtip(current,e,text){

if (document.all||document.getElementById){
thetitle=text.split('<br>')
if (thetitle.length>1){
thetitles=''
for (i=0;i<thetitle.length;i++)
thetitles+=thetitle[i]
current.title=thetitles
}
else
current.title=text
}

else if (document.layers){
document.tooltip.document.write('<layer bgColor="white" style="border:1px solid black;font-size:12px;">'+text+'</layer>')
document.tooltip.document.close()
document.tooltip.left=e.pageX+5
document.tooltip.top=e.pageY+5
document.tooltip.visibility="show"
}
}
function hidetip(){
if (document.layers)
document.tooltip.visibility="hidden"
}
]]></xsl:with-param></xsl:call-template>

  <div id="tooltip" style="position:absolute;visibility:hidden"></div>
  <xsl:for-each select="/tei:TEI/tei:facsimile[.//tei:zone]/tei:surface">
    <map name="facs-{generate-id()}">
      <xsl:for-each select="tei:zone[not(tei:graphic)]">
	<area 
	    href="#"
	    onMouseout="hidetip()"
	    shape="rect">
<!-- coords wants left, top, right, bottom -->
	  <xsl:attribute name="coords">
	    <xsl:value-of select="@ulx"/>
	    <xsl:text>,</xsl:text>
	    <xsl:value-of select="@uly"/>
	    <xsl:text>,</xsl:text>
	    <xsl:value-of select="@lrx"/>
	    <xsl:text>,</xsl:text>
	    <xsl:value-of select="@lry"/>
	  </xsl:attribute>
	  <xsl:attribute name="onMouseOver">
	    <xsl:text>showtip(this,event,'</xsl:text>
	    <xsl:for-each select="key('REFS',concat('#',@xml:id))">
	      <xsl:value-of select="normalize-space(.)"/>
	    </xsl:for-each>
	    <xsl:text>')</xsl:text>
	  </xsl:attribute>
      </area>
      </xsl:for-each>
    </map>    
    <img width="{@xMax}" height="{@yMax}" src="{tei:graphic/@url}"  usemap="#facs-{generate-id()}"/>
  </xsl:for-each>
</xsl:template>

</xsl:stylesheet>

