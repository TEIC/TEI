<?xml version="1.0" encoding="utf-8"?>
<!-- 
Text Encoding Initiative Consortium XSLT stylesheet family
XSL stylesheet to format TEI XML documents to HTML or XSL FO

This structured  bibliography handling comes from 
Peter Boot <pboot@attglobal.net>

##LICENSE
--> 
<xsl:stylesheet
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
  version="1.0">

  <xsl:template match="tei:biblStruct">
    <xsl:if test="@id|@xml:id">
      <a name="{@id|@xml:id}"></a>
    </xsl:if>
    <xsl:choose>
      <xsl:when test="@copyOf">
	<a class="biblink" href="{concat('#',substring(@copyOf,5,2))}">Zie <xsl:value-of select="substring(@copyOf,5,2)"/></a>
      </xsl:when>
      <xsl:otherwise>
	<xsl:choose>
	  <xsl:when test="descendant::tei:analytic">
	    <br/><xsl:apply-templates select="tei:analytic"/>
	    <center>
	      <table width="90%" border="0">
		<xsl:apply-templates select="tei:monogr" mode="monograll"/>
	      </table>
	    </center>
	  </xsl:when>
	  <xsl:otherwise>
	    <br/><xsl:apply-templates select="tei:monogr" mode="monogrfirst"/>
	    <center>
	      <table width="90%" border="0">
		<xsl:apply-templates select="tei:monogr" mode="monogrrest"/>
	      </table>
	    </center>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:otherwise>
    </xsl:choose>
</xsl:template>

<xsl:template match="tei:analytic">
  <xsl:apply-templates select="tei:author" mode="biblStruct"/>
  <i><xsl:apply-templates select="tei:title[not(@type='short')]" mode="withbr"/></i>
</xsl:template>

<xsl:template match="tei:monogr" mode="monograll">
  <tr><td>
    <xsl:choose>
      <xsl:when test="preceding-sibling::tei:monogr">
	Also in:
      </xsl:when>
      <xsl:otherwise>
	In:
      </xsl:otherwise>
    </xsl:choose>
  </td></tr>
  <tr><td>
    <xsl:apply-templates select="tei:author" mode="biblStruct"/>
    <i><xsl:apply-templates select="tei:title" mode="withbr"/></i>
    <xsl:apply-templates select="tei:respStmt"/>
    <xsl:apply-templates select="tei:editor"/>
    <xsl:apply-templates select="tei:edition"/>
    <xsl:apply-templates select="tei:imprint"/>
  </td></tr>
  <tr><td>
    <xsl:apply-templates select="tei:biblScope"/>
  </td></tr>
  <xsl:apply-templates select="following-sibling::tei:series"/>
</xsl:template>

<xsl:template match="tei:monogr" mode="monogrfirst">
  <xsl:apply-templates select="tei:author" mode="biblStruct"/>
  <i><xsl:apply-templates select="tei:title[not(@type='short')]" mode="withbr"/></i>
</xsl:template>

<xsl:template match="tei:monogr" mode="monogrrest">
  <tr><td>
    <xsl:apply-templates select="tei:respStmt"/>
    <xsl:apply-templates select="tei:editor"/>
    <xsl:apply-templates select="tei:edition"/>
    <xsl:apply-templates select="tei:imprint"/>
    <xsl:if test="child::tei:note">
      Zie noot: <xsl:apply-templates select="child::tei:note"/>
    </xsl:if>
  </td></tr>
  <tr><td>
    <xsl:apply-templates select="tei:biblScope"/>
  </td></tr>
  <xsl:apply-templates select="following-sibling::tei:series"/>
</xsl:template>

<xsl:template match="tei:series">
  <tr><td><xsl:apply-templates/></td></tr>
</xsl:template>

<xsl:template match="tei:author" mode="biblStruct">
  <xsl:value-of select="tei:name/@reg"/>
  <xsl:for-each select="name[position()>1]">, 
  <xsl:apply-templates/>
  </xsl:for-each>.
  <br/>
</xsl:template>

<xsl:template match="tei:author" mode="first">
  <xsl:value-of select="tei:name/@reg"/>
  <xsl:if test="name[position()>1]">
    <xsl:text>(e.a.)</xsl:text>
  </xsl:if>
  <xsl:text>: </xsl:text>
</xsl:template>

<xsl:template match="tei:editor" mode="first">
  <xsl:value-of select="tei:name/@reg"/>
  <xsl:text> (ed.)</xsl:text>
  <xsl:if test="name[position()>1]">
    <xsl:text> (e.a.)</xsl:text>
  </xsl:if>
  <xsl:text>: </xsl:text>
</xsl:template>

<xsl:template match="tei:imprint">
  <xsl:apply-templates select="tei:biblScope"/>
  <xsl:apply-templates select="tei:pubPlace"/>, 
  <xsl:apply-templates select="tei:date"/>. <xsl:apply-templates select="publisher"/>
</xsl:template>

<xsl:template match="tei:publisher">
  (<xsl:apply-templates/>).
</xsl:template>

<xsl:template match="tei:edition">
  <xsl:apply-templates/>.<br/>
</xsl:template>

<xsl:template match="tei:biblScope">
  <xsl:apply-templates/>
  <xsl:if test="ancestor::tei:biblStruct">. </xsl:if>
</xsl:template>

<xsl:template match="tei:editor">
  <xsl:apply-templates select="tei:name[position()=1]"/>
  <xsl:for-each select="tei:name[position()>1]">, 
  <xsl:apply-templates/>
  </xsl:for-each> (ed).<br/>
</xsl:template>

<xsl:template match="tei:respStmt">
  <xsl:apply-templates select="tei:resp"/> 
  <xsl:for-each select="tei:name[position()&lt;last()]"><xsl:apply-templates/>, </xsl:for-each>
  <xsl:apply-templates select="child::tei:name[position()=last()]"/>.
  <xsl:if test="ancestor::tei:biblStruct">
    <br/>
  </xsl:if>
</xsl:template>

<xsl:template match="tei:resp">
  <xsl:apply-templates/> 
</xsl:template>

<xsl:template match="tei:witList">
  <xsl:apply-templates select="./witness"/>
</xsl:template>

<xsl:template match="tei:witness">
  <p>
    <a name="{@sigil}"></a>
    <b>Sigle: <xsl:value-of select="@sigil"/></b><br/>
    <xsl:value-of select="text()"/><br/>
    <xsl:apply-templates select="tei:biblStruct"/>
    <xsl:if test="child::tei:note">
      <br/>Zie noot: <xsl:apply-templates select="child::tei:note"/>
    </xsl:if>
  </p>
</xsl:template>

<xsl:template match="tei:listBibl">
  <ol>
    <xsl:for-each select="tei:bibl">
      <li><xsl:apply-templates select="."/></li>
    </xsl:for-each>
  </ol>
</xsl:template>

<xsl:template match="tei:bibl">
   <xsl:variable name="ident">
    <xsl:apply-templates select="." mode="ident"/>
   </xsl:variable>
   <a name="{$ident}"/>
   <xsl:apply-templates/>
</xsl:template>

<xsl:template match="tei:title" mode="withbr">
  <xsl:value-of select="."/><br/>
</xsl:template>

<xsl:template match="tei:bibl/title">
  <xsl:choose>
    <xsl:when test="@rend='plain'">
      <xsl:value-of select="."/>
    </xsl:when>
    <xsl:when test="@level='a'">
      <xsl:text>&#x2018;</xsl:text>
      <xsl:value-of select="."/><xsl:text>&#x2019; </xsl:text>
    </xsl:when>
    <xsl:otherwise>
      <i><xsl:value-of select="."/></i>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>


</xsl:stylesheet>

