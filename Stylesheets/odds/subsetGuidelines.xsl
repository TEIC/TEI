<?xml version="1.0" encoding="utf-8"?>
<!-- $Date: 
Text Encoding Initiative Consortium XSLT stylesheet family
2001/10/01 $, $Revision$, $Author$

XSL stylesheet to format TEI XML documents using ODD markup

 
##LICENSE
-->
<xsl:stylesheet 
 xmlns:rng="http://relaxng.org/ns/structure/1.0"
  xmlns:eg="http://www.tei-c.org/ns/Examples"
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:exsl="http://exslt.org/common"
  xmlns:estr="http://exslt.org/strings"
  extension-element-prefixes="exsl estr"
  exclude-result-prefixes="tei exsl estr" 
  version="1.0">
<xsl:import href="../common/tei.xsl"/>
<xsl:import href="teiodds.xsl"/>

<xsl:param name="TEISERVER">http://localhost/Query/</xsl:param>
<xsl:param name="localsource"/>
<xsl:key name="MODS" match="tei:moduleSpec" use="@ident"/>
<xsl:key name="SPECS" match="tei:specGrp" use="@xml:id"/>
<xsl:key name="LOCAL"
	 match="tei:classSpec|tei:elementSpec|tei:macroSpec" use="@ident"/>
<xsl:key name="LOCALATT"
	 match="tei:attDef" use="concat(../../@ident,'::',@ident)"/>
<xsl:output method="xml" indent="yes"/>
<xsl:param name="verbose"></xsl:param>

<xsl:variable name="MAIN" select="/"/>

<xsl:template match="tei:text">
  <tei:text>
    <xsl:apply-templates/>
    <xsl:if test="not(tei:back)">
      <tei:back>
	<xsl:call-template name="CAT"/>
      </tei:back>
    </xsl:if>
  </tei:text>
</xsl:template>

<xsl:template match="tei:body">
  <tei:body>
    <tei:div0>
      <tei:head><xsl:call-template name="generateTitle"/></tei:head>
      <xsl:apply-templates/>
    </tei:div0>
  </tei:body>
</xsl:template>



<xsl:template match="tei:back">
  <xsl:apply-templates/>
  <xsl:call-template name="CAT"/>
</xsl:template>

<xsl:template name="CAT">
  <tei:div1 xml:id="REFCLA">
    <tei:head>Class catalogue</tei:head>
    <tei:divGen type="classcat"/>
  </tei:div1>
  <tei:div1 xml:id="REFENT">
    <tei:head>Macro catalogue</tei:head>
    <tei:divGen type="macrocat"/>
  </tei:div1>
  <tei:div1 xml:id="REFTAG">
    <tei:head>Element catalogue</tei:head>
    <tei:divGen type="tagcat"/>
  </tei:div1>
</xsl:template>

<xsl:template match="tei:schemaSpec">
  <tei:div>
    <tei:head>Schema [<xsl:value-of select="@ident"/>]</tei:head>
    <tei:eg>
      <xsl:apply-templates mode="verbatim"/>
    </tei:eg>
    <xsl:apply-templates select="tei:specGrp"/>
    <xsl:apply-templates select="tei:moduleRef"/>
    <xsl:apply-templates select="tei:*[@mode='add']"/>
  </tei:div>
</xsl:template>


<xsl:template match="tei:moduleRef">
  <xsl:variable name="test" select="@key"/>
  <xsl:call-template name="findNames">
    <xsl:with-param name="modname">
      <xsl:value-of select="$test"/>
    </xsl:with-param>
  </xsl:call-template>
</xsl:template>

<xsl:template name="findNames">
  <xsl:param name="modname"/>
  <xsl:variable name="KD" select="concat($modname,'-decl')"/>
  <xsl:choose>
    <xsl:when test="not($localsource='')">
      <xsl:variable name="Local">
	<List>
	  <xsl:for-each select="document($localsource)/tei:TEI">
	    <xsl:copy-of select="tei:*[@module=$modname]"/>
	    <xsl:copy-of select="tei:*[@module=$KD]"/>
	  </xsl:for-each>
	</List>
      </xsl:variable>
      <xsl:for-each select="exsl:node-set($Local)/List">
	<xsl:call-template name="processThing"/>
      </xsl:for-each>
    </xsl:when>
    <xsl:otherwise>
      <xsl:variable name="Remote">
	<xsl:value-of select="$TEISERVER"/>
	<xsl:text>allbymod.xq?module=</xsl:text>
	<xsl:value-of select="$modname"/>
      </xsl:variable>
      <xsl:for-each select="document($Remote)/List">
	<xsl:call-template name="processThing"/>
      </xsl:for-each>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template name="processThing">
  <xsl:variable name="me" select="@ident"/>
  <xsl:variable name="here" select="."/>
  <xsl:for-each select="$MAIN">
  <xsl:choose>
    <xsl:when test="key('LOCAL',$me)">
      <xsl:for-each select="key('LOCAL',$me)">
	<xsl:choose>
	  <xsl:when test="@mode='delete'"/>
	  <xsl:when test="@mode='change'">
	    <xsl:for-each select="$here">
	      <xsl:apply-templates select="." mode="change"/>
	    </xsl:for-each>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:for-each select="$here">
	      <xsl:apply-templates select="." mode="copy"/>
	    </xsl:for-each>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:for-each>
    </xsl:when>
    <xsl:otherwise>
      <xsl:for-each select="$here">
	<xsl:apply-templates select="."  mode="copy"/>
      </xsl:for-each>
    </xsl:otherwise>
  </xsl:choose>
  </xsl:for-each>
</xsl:template>

<xsl:template match="@mode"/>

<xsl:template match="tei:elementSpec|tei:classSpec|tei:macroSpec"
	      mode="add">
  <xsl:copy-of select="."/>
</xsl:template>

<xsl:template match="tei:elementSpec|tei:classSpec|tei:macroSpec" mode="change">
  <xsl:variable name="me" select="@ident"/>
  <xsl:copy>
    <xsl:apply-templates select="@*" mode="change"/>
    <xsl:for-each select="$MAIN">
      <xsl:for-each select="key('LOCAL',$me)">
	<xsl:choose>
	  <xsl:when test="@mode='delete'"/>
	  <xsl:when test="@mode='replace'">
	    <xsl:copy-of select="."/>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:copy-of select="tei:altIdent"/>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:for-each>
    </xsl:for-each>
    <xsl:apply-templates select="*|text()|comment()" mode="change"/>
  </xsl:copy>
</xsl:template>

<xsl:template match="tei:attDef"      mode="change">
  <xsl:variable name="me" select="concat(../../@ident,'::',@ident)"/>
  <xsl:copy>
    <xsl:apply-templates select="@*" mode="change"/>
    <xsl:for-each select="$MAIN">
      <xsl:for-each select="key('LOCALATT',$me)">
	<xsl:choose>
	  <xsl:when test="@mode='delete'"/>
	  <xsl:when test="@mode='replace'">
	    <xsl:copy-of select="."/>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:copy-of select="tei:altIdent"/>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:for-each>
    </xsl:for-each>
    <xsl:apply-templates select="tei:*|eg:*|text()|comment()" mode="change"/>
  </xsl:copy>
</xsl:template>

<xsl:template match="tei:attList"  mode="change">
  <xsl:variable name="me" select="../@ident"/>
  <xsl:copy>
    <xsl:apply-templates select="@*" mode="change"/>
    <xsl:for-each select="$MAIN">
      <xsl:for-each select="key('LOCAL',$me)/tei:attList">
	<xsl:copy-of select="tei:attDef[@mode='add']"/>
      </xsl:for-each>
    </xsl:for-each>
    <xsl:apply-templates select="*|text()|comment()" mode="change"/>
  </xsl:copy>
</xsl:template>

<xsl:template match="tei:*">
  <xsl:copy>
    <xsl:apply-templates select="@*|tei:*|rng:*|eg:*|text()|comment()"/>
  </xsl:copy>
</xsl:template>

<xsl:template match="@*|comment()|text()" mode="change">
  <xsl:copy/>
</xsl:template>

<xsl:template match="@*|comment()">
  <xsl:copy/>
</xsl:template>

<xsl:template match="text()">
  <xsl:copy-of select="."/>
</xsl:template>

<xsl:template match="eg:*" mode="change">
<xsl:copy-of select="."/>
</xsl:template>

<xsl:template match="eg:*">
<xsl:copy-of select="."/>
</xsl:template>

<xsl:template match="rng:*">
<xsl:copy-of select="."/>
</xsl:template>

<xsl:template match="rng:*" mode="copy">
<xsl:copy-of select="."/>
</xsl:template>


<xsl:template match="tei:*|rng:*" mode="change">
  <xsl:if test="not(@mode='delete')">
    <xsl:copy>
      <xsl:apply-templates select="@*|*|text()|comment()" mode="change"/>
    </xsl:copy>
  </xsl:if>
</xsl:template>


<xsl:template match="*|@*|processing-instruction()|text()" mode="copy">
 <xsl:copy>
  <xsl:apply-templates
   select="*|@*|processing-instruction()|comment()|text()" mode="copy"/>
 </xsl:copy>
</xsl:template>

<xsl:template name="verbatim">
  <xsl:param name="text"/>
  <xsl:param name="startnewline">false</xsl:param>
  <xsl:param name="autowrap">true</xsl:param>
     <div class="pre_eg">
        <xsl:if test="$startnewline='true'">
         <xsl:text>&#10;</xsl:text>
       </xsl:if>
       <xsl:choose>
         <xsl:when test="$autowrap='false'">
           <xsl:value-of select="."/>
         </xsl:when>
       <xsl:otherwise>           
       <xsl:variable name="lines" select="estr:tokenize($text,'&#10;')"/>
           <xsl:apply-templates select="$lines[1]" 
                mode="normalline"/>
     </xsl:otherwise>
   </xsl:choose>
     </div>
</xsl:template>

</xsl:stylesheet>
