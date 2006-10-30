<xsl:stylesheet 
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
    <style>
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
    </style>
  </head>
  <body>
    <h1>Elements</h1>
    <table>
      <tr>
	<td></td><td/><td><b>gloss</b></td><td><b>desc</b></td></tr>
    <xsl:for-each select="key('E',1)">
      <xsl:sort select="@module"/>
      <xsl:sort select="@ident"/>
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
    </xsl:for-each>
    </table>
  </body>
</html>
</xsl:template>

<xsl:template name="display">
  <xsl:param name="lang"/>
  <tr>
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
  <xsl:attribute name="class">
    <xsl:value-of select="$lang"/>
    <xsl:choose>
    <xsl:when test="normalize-space(tei:desc[@xml:lang=$lang])='' and not($lang='en')">
      <xsl:text> empty</xsl:text>
    </xsl:when>
    <xsl:otherwise>
      <xsl:text> full</xsl:text>
    </xsl:otherwise>
    </xsl:choose>
  </xsl:attribute>
  <xsl:apply-templates select="tei:desc[@xml:lang=$lang]"/>
  </td>
</xsl:template>

<xsl:template name="glossme">
  <xsl:param name="lang">en</xsl:param>
  <td>
  <xsl:attribute name="class">
    <xsl:value-of select="$lang"/>
    <xsl:choose>
    <xsl:when test="normalize-space(tei:gloss[@xml:lang=$lang])='' and not($lang='en')">
      <xsl:text> empty</xsl:text>
    </xsl:when>
    <xsl:otherwise>
      <xsl:text> full</xsl:text>
    </xsl:otherwise>
    </xsl:choose>
  </xsl:attribute>
  <xsl:apply-templates select="tei:gloss[@xml:lang=$lang]"/>
  </td>
</xsl:template>

<xsl:template match="tei:gi">
  <xsl:text>&lt;</xsl:text>
  <xsl:apply-templates/>
  <xsl:text>&gt;</xsl:text>
</xsl:template>

</xsl:stylesheet>







