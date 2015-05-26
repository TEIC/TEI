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
//D'autres scripts sur http://www.toutjavascript.com
//Si vous utilisez ce script, merci de m'avertir !  < webmaster@toutjavascript.com >
//Auteur original :Olivier Hondermarck  <webmaster@toutjavascript.com>
//Modifs compatibilité Netscape 6/Mozilla : Cédric Lamalle 09/2001 <cedric@cpac.embrapa.br>
//Correction Mac IE5 (Merci Fred)

var IB=new Object;
var posX=0;posY=0;
var xOffset=0;yOffset=25;
function showPopup(text,x) {
  contenu="<table border=0 cellspacing=0 cellpadding="+IB.nbpixel+"><tr bgcolor='"+IB.colcontour+"'><td><table border=0 cellpadding=2 cellspacing=0 bgcolor='"+IB.colfond+"'><tr><td><font size='3' face='arial' color='"+IB.coltext+"'>"+text+"</font></td></tr></table></td></tr></table>&nbsp;";
  var finalPosX=x + 40;
  if (finalPosX<0) finalPosX=0;
  if (document.layers) {
    document.layers["thePopup"].document.write(contenu);
    document.layers["thePopup"].document.close();
    document.layers["thePopup"].top=posY+yOffset;
    document.layers["thePopup"].left=finalPosX;
    document.layers["thePopup"].visibility="show";}
  if (document.all) {
    //var f=window.event;
    //doc=document.body.scrollTop;
    bulle.innerHTML=contenu;
    document.all["thePopup"].style.top=posY+yOffset;
    document.all["thePopup"].style.left=finalPosX;//f.x-xOffset;
    document.all["thePopup"].style.visibility="visible";
  }
  //modif CL 09/2001 - NS6 : celui-ci ne supporte plus document.layers mais document.getElementById
  else if (document.getElementById) {
    document.getElementById("thePopup").innerHTML=contenu;
    document.getElementById("thePopup").style.top=posY+yOffset;
    document.getElementById("thePopup").style.left=finalPosX;
    document.getElementById("thePopup").style.visibility="visible";
  }
}
function getMousePos(e) {
  if (document.all) {
  posX=event.x+document.body.scrollLeft; //modifs CL 09/2001 - IE : regrouper l'évènement
  posY=event.y+document.body.scrollTop;
  }
  else {
  posX=e.pageX; //modifs CL 09/2001 - NS6 : celui-ci ne supporte pas e.x et e.y
  posY=e.pageY; 
  }
}
function hidePopup() {
 if (document.layers) {
   document.layers["thePopup"].visibility="hide";
 }
 if (document.all) {
   document.all["thePopup"].style.visibility="hidden";
    }
 else if (document.getElementById){
   document.getElementById("thePopup").style.visibility="hidden";
 }
}

function InitPopup(ColText,ColFond,ColContour,NbPixel) {
	IB.ColText=ColText;IB.ColFond=ColFond;IB.ColContour=ColContour;IB.NbPixel=NbPixel;
	if (document.layers) {
		window.captureEvents(Event.MOUSEMOVE);window.onMouseMove=getMousePos;
		document.write("<LAYER name='bulle' top=0 left=0 visibility='hide'></LAYER>");
	}
	if (document.all) {
		document.write("<DIV id='bulle' style='position:absolute;top:0;left:0;visibility:hidden'></DIV>");
		document.onmousemove=getMousePos;
	}
	//modif CL 09/2001 - NS6 : celui-ci ne supporte plus document.layers mais document.getElementById
	else if (document.getElementById) {
	        document.onmousemove=getMousePos;
	        document.write("<DIV id='bulle' style='position:absolute;top:0;left:0;visibility:hidden'></DIV>");
	}

}
// appel à l'initialisation des infos bulles (laisser dans le BODY) 
InitPopup("#C61000","#FFFFFF","orange",0);
// InitPopup(couleur de text,	couleur de fond, couleur de contour taille contour)
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
	    onMouseout="hidePopup()"
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
	    <xsl:text>showPopup(this,event,'</xsl:text>
	    <xsl:for-each select="key('REFS',concat('#',@xml:id))">
	      <xsl:value-of select="normalize-space(.)"/>
	    </xsl:for-each>
	    <xsl:text>')</xsl:text>
	  </xsl:attribute>
      </area>
      </xsl:for-each>
    </map>    
    <img
	 src="{tei:graphic/@url}"  
	 usemap="#facs-{generate-id()}"/>
  </xsl:for-each>
</xsl:template>

</xsl:stylesheet>

