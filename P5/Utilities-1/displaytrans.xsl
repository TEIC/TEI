<xsl:stylesheet 
 xmlns:html="http://www.w3.org/1999/xhtml" 
 xmlns="http://www.w3.org/1999/xhtml"
 xmlns:tei="http://www.tei-c.org/ns/1.0"
 xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
 xmlns:rng="http://relaxng.org/ns/structure/1.0"
 extension-element-prefixes="exsl"
 xmlns:exsl="http://exslt.org/common"
 version="1.0">

<xsl:output method="xml" indent="yes"/>
<xsl:key name="E" match="tei:elementSpec" use="1"/>
<xsl:key name="A" match="tei:attDef" use="1"/>
<xsl:key name="C" match="tei:classSpec" use="1"/>
<xsl:key name="M" match="tei:macroSpec" use="1"/>
<xsl:key name="GLOSS" match="tei:gloss[not(@xml:lang)]" use="1"/>
<xsl:key name="DESC" match="tei:desc[not(@xml:lang)]" use="1"/>

  <xsl:output method="xml"
	      doctype-public="//W3C//DTD XHTML 1.1//EN"
	      doctype-system="http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd"
	      encoding="utf-8"
	      />  

<xsl:template match="/">
<html>
  <head>
    <script type="text/javascript">

var curLang='en';

/* Created by: Dustin Diaz :: http://www.dustindiaz.com/ */

function getElementsByClass(searchClass,node,tag) {
  var classElements = new Array();
  if (node == null)
    node = document;
  if (tag == null)
    tag = '*';
  var els = node.getElementsByTagName(tag);
  var elsLen = els.length;
  var pattern = new RegExp("(^|\\s)"+searchClass+"(\\s|$)");
  for (i = 0, j = 0; i &lt; elsLen; i++) {
    if (pattern.test(els[i].className) ) {
      classElements[j] = els[i];
      j++;
    }
  }
  return classElements;
}

function showLang(obj) {
  var thing = obj.selectedIndex;
  var which = obj.options[thing].value;
  toggleElementsByClass(curLang,'off');
  curLang=which;
  toggleElementsByClass(curLang,'on');
}

function toggleElementsByClass(searchClass,state,node,tag) {
  if (node == null)
    node = document;
  if (tag == null)
    tag = 'tr';
  var els = node.getElementsByTagName(tag);
  var elsLen = els.length;
  var pattern = new RegExp("(^|\\s)"+searchClass+"(\\s|$)");
  for (i = 0, j = 0; i &lt; elsLen; i++) {
    if (pattern.test(els[i].className) ) {
      if (state=='on') {
       els[i].style.display='table-row'
    }
      else {
       els[i].style.display='none'
       }
  }
 }
}

    </script>
    <style type="text/css">
      tr.element {
      height: 50px;
      }
      tr.element td {
      border-top: 1pt solid black;
      margin-top: 10pt;
      color:gray;
      }
      td {
      vertical-align: top; 
      }
      td.empty {
       border: 1pt solid red;
      }
      td.full {
       border: 1pt solid green;
      }
      tr.de,tr.ja,tr.zh-tw,tr.fr,tr.es {
      display: none; 
      }

    </style>
    <title>TEI Translated texts</title>
  </head>
  <body>
  <select name="language"  onchange="showLang(this)">
    <option value="*">All</option>
    <option value="de">German</option>
    <option value="es">Spanish</option>
    <option value="zh-tw">Chinese</option>
    <option value="ja">Japanese</option>
    <option value="fr">French</option>
  </select>
    <h1>Elements</h1>
    <table>
      <tr>
	<td></td><td/><td><b>Gloss</b></td><td><b>Description</b></td></tr>
    <xsl:for-each select="key('E',1)">
      <xsl:sort select="@module"/>
      <xsl:sort select="@ident"/>
      <xsl:call-template name="showme"/>
    </xsl:for-each>
    </table>

    <h1>Macros</h1>
    <table>
      <tr>
	<td></td><td/><td><b>Gloss</b></td><td><b>Description</b></td></tr>
    <xsl:for-each select="key('M',1)">
      <xsl:sort select="@module"/>
      <xsl:sort select="@ident"/>
      <xsl:call-template name="showme"/>
    </xsl:for-each>
    </table>

    <h1>Classes</h1>
    <table>
      <tr>
	<td></td><td/><td><b>Gloss</b></td><td><b>Description</b></td></tr>
    <xsl:for-each select="key('C',1)">
      <xsl:sort select="@module"/>
      <xsl:sort select="@ident"/>
      <xsl:call-template name="showme"/>
    </xsl:for-each>
    </table>
  </body>
</html>
</xsl:template>

<xsl:template name="showme">
      <tr class="element"><td><b><xsl:apply-templates
      select="@ident"/>
 (<xsl:value-of select="@module"/>)</b></td>
      <td></td>
      <td><xsl:apply-templates select="tei:gloss[not(@xml:lang)]"/></td>
      <td><xsl:apply-templates select="tei:desc[not(@xml:lang)]"/></td>
      </tr>
      <xsl:call-template name="display">
	<xsl:with-param name="lang">de</xsl:with-param>
      </xsl:call-template>
      <xsl:call-template name="display">
	<xsl:with-param name="lang">es</xsl:with-param>
      </xsl:call-template>
      <xsl:call-template name="display">
	<xsl:with-param name="lang">fr</xsl:with-param>
      </xsl:call-template>
      <xsl:call-template name="display">
	<xsl:with-param name="lang">ja</xsl:with-param>
      </xsl:call-template>
      <xsl:call-template name="display">
	<xsl:with-param name="lang">zh-tw</xsl:with-param>
      </xsl:call-template>
      <xsl:apply-templates select="tei:attList"/>
</xsl:template>

<xsl:template match="tei:attList">
  <xsl:for-each select=".//tei:attDef">
    <tr class="attribute">
      <td>  …<b><xsl:apply-templates
      select="@ident"/></b></td>
      <td></td>
      <td><xsl:apply-templates select="tei:gloss[not(@xml:lang)]"/></td>
      <td><xsl:apply-templates
      select="tei:desc[not(@xml:lang)]"/></td>
    </tr>
      <xsl:call-template name="display">
	<xsl:with-param name="lang">de</xsl:with-param>
      </xsl:call-template>
      <xsl:call-template name="display">
	<xsl:with-param name="lang">es</xsl:with-param>
      </xsl:call-template>
      <xsl:call-template name="display">
	<xsl:with-param name="lang">fr</xsl:with-param>
      </xsl:call-template>
      <xsl:call-template name="display">
	<xsl:with-param name="lang">ja</xsl:with-param>
      </xsl:call-template>
      <xsl:call-template name="display">
	<xsl:with-param name="lang">zh-tw</xsl:with-param>
      </xsl:call-template>

  </xsl:for-each>
</xsl:template>

<xsl:template name="display">
  <xsl:param name="lang"/>
  <tr class="{$lang}">
    <td></td><td><xsl:value-of select="$lang"/></td>

    <xsl:call-template name="glossme">
      <xsl:with-param name="lang" select="$lang"/>
    </xsl:call-template>
    
    <xsl:call-template name="descme">
      <xsl:with-param name="lang" select="$lang"/>
    </xsl:call-template>
    
  </tr>
</xsl:template>


<xsl:template name="descme">
  <xsl:param name="lang">en</xsl:param>
  <td>
    <xsl:choose>
      
    <xsl:when test="normalize-space(tei:desc[not(@xml:lang)])=''"/>
    <xsl:when test="normalize-space(tei:desc[@xml:lang=$lang])='' and not($lang='en')">
      <xsl:attribute name="class">
	<xsl:text> empty</xsl:text>
      </xsl:attribute>
    </xsl:when>
    <xsl:otherwise>
      <xsl:attribute name="class">
	<xsl:text> full</xsl:text>
      </xsl:attribute>
      </xsl:otherwise>
    </xsl:choose>
  <xsl:apply-templates select="tei:desc[@xml:lang=$lang]"/>
  </td>
</xsl:template>

<xsl:template name="glossme">
  <xsl:param name="lang">en</xsl:param>
  <td>
    <xsl:choose>    
    <xsl:when test="normalize-space(tei:gloss[not(@xml:lang)])=''"/>
    <xsl:when test="normalize-space(tei:gloss[@xml:lang=$lang])='' and not($lang='en')">
      <xsl:attribute name="class">
	<xsl:text> empty</xsl:text>
      </xsl:attribute>
    </xsl:when>
    <xsl:otherwise>
      <xsl:attribute name="class">
	<xsl:text> full</xsl:text>
      </xsl:attribute>
      </xsl:otherwise>
    </xsl:choose>
  <xsl:apply-templates select="tei:gloss[@xml:lang=$lang]"/>
  </td>
</xsl:template>

<xsl:template match="tei:gi">
  <xsl:text>&lt;</xsl:text>
  <xsl:apply-templates/>
  <xsl:text>&gt;</xsl:text>
</xsl:template>

</xsl:stylesheet>







