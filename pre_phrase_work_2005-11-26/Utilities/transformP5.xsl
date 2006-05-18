<xsl:stylesheet 
 xmlns:teix="http://www.tei-c.org/ns/Examples"
 xmlns:tei="http://www.tei-c.org/ns/1.0"
 xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
 xmlns:rng="http://relaxng.org/ns/structure/1.0"
 extension-element-prefixes="exsl"
 exclude-result-prefixes="exsl teix rng tei"
 xmlns:exsl="http://exslt.org/common"
 version="1.0">

<xsl:output 
   method="xml"
   indent="yes"
   cdata-section-elements="tei:eg teix:egXML"
   omit-xml-declaration="yes"/>


<xsl:param name="PREFIX" select="'newSource'"/>
<xsl:variable name="top" select="/"/>
<xsl:variable name="uc">ABCDEFGHIJKLMNOPQRSTUVWXYZ</xsl:variable>
<xsl:variable name="lc">abcdefghijklmnopqrstuvwxyz</xsl:variable>

<xsl:include href="transformbody.xsl"/>

<xsl:key name="IDS" match="tei:*" use="@xml:id"/>


<xsl:template match="/">
  <xsl:variable name="outName">
    <xsl:value-of select="$PREFIX"/>-driver.xml</xsl:variable>
    <xsl:message>write <xsl:value-of select="$outName"/></xsl:message>
 <exsl:document         
   method="xml"
   cdata-section-elements="tei:eg teix:egXML" 
   omit-doctype-declaration="yes"
   omit-xml-declaration="yes"
        href="{$outName}">
<xsl:text disable-output-escaping="yes">&lt;?xml version="1.0"?&gt;
&lt;!-- $Author$ $Date$ $Id$ --&gt;
&lt;!DOCTYPE TEI [
&lt;!NOTATION PNG SYSTEM ""&gt;
&lt;!NOTATION HTML SYSTEM ""&gt;
&lt;!ENTITY % ENTS SYSTEM "ents.dtd"&gt;
%ENTS;
</xsl:text>
<xsl:for-each
 select=".//tei:elementSpec|.//tei:macroSpec|.//tei:classSpec|.//tei:div1[@xml:id]">
  <xsl:sort select="tei:ident"/>
  <xsl:variable name="filename">
     <xsl:choose>
   <xsl:when test="name(.)='div1'">
        <xsl:value-of select="translate(@xml:id,$uc,$lc)"/>
        <xsl:text>.odd</xsl:text>
    </xsl:when>
<xsl:when test='@xml:id'>
        <xsl:value-of select="translate(@xml:id,$uc,$lc)"/>
        <xsl:text>.odd</xsl:text>
</xsl:when>
    <xsl:otherwise>
       <xsl:text>mixno.odd</xsl:text>
    </xsl:otherwise>
    </xsl:choose>
    </xsl:variable>

  <xsl:variable name="ident">
     <xsl:choose>
   <xsl:when test="name(.)='div1'">
        <xsl:value-of select="translate(@xml:id,$uc,$lc)"/>
      <xsl:text>.odd</xsl:text>
    </xsl:when>
    <xsl:otherwise>
        <xsl:value-of select="tei:ident"/>
    </xsl:otherwise>
    </xsl:choose>
    </xsl:variable>

<xsl:variable name="entname">
<xsl:choose>   <xsl:when test="name(.)='div1'">
        <xsl:value-of select="$ident"/>
    </xsl:when>

    <xsl:otherwise>
    <xsl:value-of select="$filename"/>
    </xsl:otherwise>
    </xsl:choose>
    </xsl:variable>

    <xsl:text disable-output-escaping="yes">
&lt;!ENTITY </xsl:text>
    <xsl:value-of select="$entname"/>
    <xsl:text disable-output-escaping="yes"> SYSTEM "</xsl:text>
    <xsl:value-of select="$PREFIX"/>
    <xsl:text>/</xsl:text>
    <xsl:value-of  select="translate(ancestor-or-self::tei:div1/@xml:id,$lc,$uc)"/>
    <xsl:text>/</xsl:text>
    <xsl:value-of select="$filename"/>
    <xsl:text disable-output-escaping="yes">"&gt;</xsl:text>
  </xsl:for-each>
<xsl:text disable-output-escaping="yes">
]>
</xsl:text>
  <xsl:apply-templates/> 
</exsl:document>

</xsl:template>


<xsl:template match="teix:*|tei:*|rng:*">
 <xsl:copy>
  <xsl:apply-templates select="@*"/>
  <xsl:apply-templates 
      select="teix:*|tei:*|rng:*|comment()|processing-instruction()|text()"/>
 </xsl:copy>
</xsl:template>

<xsl:template match="@*|processing-instruction()|text()">
  <xsl:copy/>
</xsl:template>

<xsl:template match="comment()">
  <xsl:copy/>
</xsl:template>

<xsl:template match="tei:div1[@xml:id]">
    <xsl:variable name="ident">
    <xsl:value-of select="translate(@xml:id,$uc,$lc)"/>
      <xsl:text>.odd</xsl:text>
    </xsl:variable>
 <xsl:variable name="outName">
   <xsl:if test="not($PREFIX ='')">
     <xsl:value-of select="$PREFIX"/>
     <xsl:if test="not(substring($PREFIX,string-length($PREFIX),string-length($PREFIX))='/')">
       <xsl:text>/</xsl:text>
     </xsl:if>
   </xsl:if>
	<xsl:value-of  select="translate(ancestor-or-self::tei:div1/@xml:id,$lc,$uc)"/>
	<xsl:text>/</xsl:text>
	<xsl:value-of select="$ident"/>
    </xsl:variable>
    <xsl:message>write <xsl:value-of select="$ident"/></xsl:message>
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:value-of select="$ident"/>
    <xsl:text disable-output-escaping="yes">;</xsl:text>
    <exsl:document         
     method="xml"
     cdata-section-elements="tei:eg teix:egXML" 
     omit-doctype-declaration="yes"
     omit-xml-declaration="yes" 
     href="{$outName}">
<xsl:comment>
Copyright TEI Consortium. 

Licensed under the GNU General Public License. 

See the file COPYING for details.

$Date$
$Author$

</xsl:comment>
      <xsl:copy>
	<xsl:apply-templates select="@*"/>
	<xsl:apply-templates select="rng:*|teix:*|tei:*|comment()|processing-instruction()|text()"/>
      </xsl:copy>
    </exsl:document>
</xsl:template>

<xsl:template match="tei:elementSpec|tei:classSpec|tei:macroSpec">
    <xsl:call-template name="redoDoc"/>
</xsl:template>

<xsl:template name="redoDoc">
    <xsl:variable name="filename">
      <xsl:value-of select="translate(@xml:id,$uc,$lc)"/>
      <xsl:text>.odd</xsl:text>
    </xsl:variable>

    <xsl:variable name="ident">
      <xsl:value-of select="@xml:ident"/>
    </xsl:variable>

    <xsl:message>write <xsl:value-of select="$ident"/> (<xsl:value-of select="@xml:id"/>) to <xsl:value-of select="$filename"/> </xsl:message>
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <!--xsl:value-of select="tei:ident"/--><xsl:value-of select="$filename"/>
    <xsl:text disable-output-escaping="yes">;</xsl:text>
    <exsl:document         
     indent="yes"
     method="xml"
     cdata-section-elements="tei:eg teix:egXML" 
     omit-doctype-declaration="yes"
     omit-xml-declaration="yes"
        href="{$filename}">
<xsl:comment>Copyright 2005 TEI Consortium. 

Licensed under the GNU General Public License. 

See the file COPYING for details.
</xsl:comment>
      <xsl:copy>
	<xsl:apply-templates select="rng:*|teix:*|tei:*|@*|comment()|processing-instruction()|text()"/>
      </xsl:copy>
    </exsl:document>
</xsl:template>


</xsl:stylesheet>
