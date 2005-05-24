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

<xsl:template match="comment()[contains(.,'Copyright 2004 TEI')]"/>

<xsl:template match="comment()[contains(.,'Copyright TEI')]"/>

<xsl:template match="teix:*|tei:*|rng:*">
 <xsl:copy>
  <xsl:apply-templates select="@*"/>
  <xsl:apply-templates 
      select="teix:*|tei:*|rng:*|comment()|processing-instruction()|text()"/>
 </xsl:copy>
</xsl:template>

<xsl:template match="tei:div1[@id]">
    <xsl:variable name="ident">
    <xsl:value-of select="translate(@id,$uc,$lc)"/>
      <xsl:text>.odd</xsl:text>
    </xsl:variable>
 <xsl:variable name="outName">
   <xsl:if test="not($PREFIX ='')">
     <xsl:value-of select="$PREFIX"/>
     <xsl:if test="not(substring($PREFIX,string-length($PREFIX),string-length($PREFIX))='/')">
       <xsl:text>/</xsl:text>
     </xsl:if>
   </xsl:if>
	<xsl:value-of  select="translate(ancestor-or-self::tei:div1/@id,$lc,$uc)"/>
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
      <xsl:value-of select="translate(@id,$uc,$lc)"/>
      <xsl:text>.odd</xsl:text>
    </xsl:variable>

    <xsl:variable name="ident">
      <xsl:value-of select="@ident"/>
    </xsl:variable>

    <xsl:message>write <xsl:value-of select="$ident"/> (<xsl:value-of select="@id"/>) to <xsl:value-of select="$filename"/> </xsl:message>
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
<xsl:comment>Copyright 2004 TEI Consortium. 

Licensed under the GNU General Public License. 

See the file COPYING for details.
</xsl:comment>
      <xsl:copy>
	<xsl:apply-templates select="rng:*|teix:*|tei:*|@*|comment()|processing-instruction()|text()"/>
      </xsl:copy>
    </exsl:document>
</xsl:template>


<xsl:template
    match="tei:attDef/tei:datatype/rng:data[@type='IDREFS']">
     <rng:ref name="datatype.uriList"/>
</xsl:template>

<xsl:template
    match="tei:attDef/tei:datatype/rng:data[@type='IDREF']">
     <rng:ref name="datatype.uri"/>
</xsl:template>

<xsl:template match="@ana">
  <xsl:attribute name="{name(.)}">#<xsl:value-of
  select="."/></xsl:attribute>
</xsl:template>

<xsl:template match="@copyOf">
 <xsl:attribute name="copyOf">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="@corresp">
  <xsl:attribute name="{name(.)}">#<xsl:value-of
  select="."/></xsl:attribute>
</xsl:template>

<xsl:template match="@decls">
  <xsl:attribute name="{name(.)}">#<xsl:value-of
  select="."/></xsl:attribute>
</xsl:template>

<xsl:template match="@doc">
 <xsl:attribute name="target">
   <xsl:value-of select="unparsed-entity-uri(.)"/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="@domains">
  <xsl:attribute name="{name(.)}">#<xsl:value-of
  select="."/></xsl:attribute>
</xsl:template>

<xsl:template match="@end">
 <xsl:attribute name="end">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="@exclude">
  <xsl:attribute name="{name(.)}">#<xsl:value-of
  select="."/></xsl:attribute>
</xsl:template>

<xsl:template match="@hand">
 <xsl:attribute name="hand">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="@id">
  <xsl:choose>
    <xsl:when test="parent::tei:lang">
      <xsl:attribute name="ident">
	<xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:when>
    <xsl:otherwise>
      <xsl:attribute name="xml:id">
	<xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template match="@inst">
  <xsl:attribute name="{name(.)}">#<xsl:value-of
  select="."/></xsl:attribute>
</xsl:template>

<xsl:template match="@lang">
 <xsl:attribute name="xml:lang">
      <xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="@location">
 <xsl:attribute name="location">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="@mergedin">
 <xsl:attribute name="mergedin">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="@next">
 <xsl:attribute name="next">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="@prev">
 <xsl:attribute name="prev">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="@sameAs">
 <xsl:attribute name="sameAs">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="@select">
  <xsl:attribute name="{name(.)}">#<xsl:value-of
  select="."/></xsl:attribute>
</xsl:template>

<xsl:template match="@start">
 <xsl:attribute name="start">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="@synch">
  <xsl:attribute name="{name(.)}">#<xsl:value-of
  select="."/></xsl:attribute>
</xsl:template>

<xsl:template match="@target|@targets">
 <xsl:attribute name="{name(.)}">
   <xsl:if test="not(starts-with(.,'http'))">
     <xsl:text>#</xsl:text>
   </xsl:if>
   <xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="tei:add/@hand">
 <xsl:attribute name="hand">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="tei:add/@resp">
 <xsl:attribute name="resp">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="tei:addSpan/@hand">
 <xsl:attribute name="hand">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="tei:addSpan/@resp">
 <xsl:attribute name="resp">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="tei:addSpan/@to">
 <xsl:attribute name="to">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="tei:app/@from">
 <xsl:attribute name="from">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="tei:app/@to">
 <xsl:attribute name="to">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="tei:arc/@from">
 <xsl:attribute name="from">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="tei:arc/@to">
 <xsl:attribute name="to">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="tei:catRef/@scheme">
 <xsl:attribute name="scheme">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="tei:classCode/@scheme">
 <xsl:attribute name="scheme">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="tei:damage/@hand">
 <xsl:attribute name="hand">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="tei:damage/@resp">
 <xsl:attribute name="resp">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="tei:del/@hand">
 <xsl:attribute name="hand">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="tei:del/@resp">
 <xsl:attribute name="resp">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="tei:delSpan/@hand">
 <xsl:attribute name="hand">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="tei:delSpan/@resp">
 <xsl:attribute name="resp">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="tei:delSpan/@to">
 <xsl:attribute name="to">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="tei:eLeaf/@value">
 <xsl:attribute name="value">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="tei:eTree/@value">
 <xsl:attribute name="value">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="tei:event/@who">
 <xsl:attribute name="who">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="tei:expan/@resp">
 <xsl:attribute name="resp">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="tei:f/@fVal">
 <xsl:attribute name="fVal">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="tei:fs/@feats">
  <xsl:attribute name="{name(.)}">#<xsl:value-of
  select="."/></xsl:attribute>
</xsl:template>

<xsl:template match="tei:g/@ref">
  <xsl:choose>
    <xsl:when test="starts-with(.,'#')">
      <xsl:copy/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:attribute name="ref">
	<xsl:text>#</xsl:text><xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template match="tei:gap/@hand">
 <xsl:attribute name="hand">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="tei:gap/@resp">
 <xsl:attribute name="resp">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="tei:graphic/@url">
 <xsl:attribute name="url">
   <xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="tei:handShift/@new">
 <xsl:attribute name="new">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="tei:handShift/@old">
 <xsl:attribute name="old">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="tei:handShift/@resp">
 <xsl:attribute name="resp">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="tei:iNode/@children">
  <xsl:attribute name="{name(.)}">#<xsl:value-of
  select="."/></xsl:attribute>
</xsl:template>

<xsl:template match="tei:iNode/@follow">
 <xsl:attribute name="follow">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="tei:iNode/@parent">
 <xsl:attribute name="parent">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="tei:iNode/@value">
 <xsl:attribute name="value">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="tei:keywords/@scheme">
 <xsl:attribute name="scheme">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="tei:kinesic/@who">
 <xsl:attribute name="who">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="tei:leaf/@follow">
 <xsl:attribute name="follow">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="tei:leaf/@parent">
 <xsl:attribute name="parent">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="tei:leaf/@value">
 <xsl:attribute name="value">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="tei:move/@perf">
  <xsl:attribute name="{name(.)}">#<xsl:value-of
  select="."/></xsl:attribute>
</xsl:template>

<xsl:template match="tei:move/@who">
  <xsl:attribute name="{name(.)}">#<xsl:value-of
  select="."/></xsl:attribute>
</xsl:template>

<xsl:template match="tei:msContents/@class">
  <xsl:attribute name="{name(.)}">#<xsl:value-of
  select="."/></xsl:attribute>
</xsl:template>

<xsl:template match="tei:msItem/@class">
  <xsl:attribute name="{name(.)}">#<xsl:value-of
  select="."/></xsl:attribute>
</xsl:template>

<xsl:template match="tei:node/@adj">
  <xsl:attribute name="{name(.)}">#<xsl:value-of
  select="."/></xsl:attribute>
</xsl:template>

<xsl:template match="tei:node/@adjFrom">
  <xsl:attribute name="{name(.)}">#<xsl:value-of
  select="."/></xsl:attribute>
</xsl:template>

<xsl:template match="tei:node/@adjTo">
  <xsl:attribute name="{name(.)}">#<xsl:value-of
  select="."/></xsl:attribute>
</xsl:template>

<xsl:template match="tei:node/@value">
 <xsl:attribute name="value">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="tei:note/@targetEnd">
  <xsl:attribute name="{name(.)}">#<xsl:value-of
  select="."/></xsl:attribute>
</xsl:template>

<xsl:template match="tei:occupation/@code">
 <xsl:attribute name="code">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="tei:occupation/@scheme">
 <xsl:attribute name="scheme">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="tei:pause/@who">
 <xsl:attribute name="who">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="tei:reg/@resp">
 <xsl:attribute name="resp">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="tei:relation/@active">
  <xsl:attribute name="{name(.)}">#<xsl:value-of
  select="."/></xsl:attribute>
</xsl:template>

<xsl:template match="tei:relation/@passive">
  <xsl:attribute name="{name(.)}">#<xsl:value-of
  select="."/></xsl:attribute>
</xsl:template>

<xsl:template match="tei:restore/@hand">
 <xsl:attribute name="hand">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="tei:restore/@resp">
 <xsl:attribute name="resp">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="tei:root/@children">
  <xsl:attribute name="{name(.)}">#<xsl:value-of
  select="."/></xsl:attribute>
</xsl:template>

<xsl:template match="tei:root/@value">
 <xsl:attribute name="value">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="tei:setting/@who">
  <xsl:attribute name="{name(.)}">#<xsl:value-of
  select="."/></xsl:attribute>
</xsl:template>

<xsl:template match="tei:shift/@who">
 <xsl:attribute name="who">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="tei:socecStatus/@code">
 <xsl:attribute name="code">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="tei:socecStatus/@scheme">
 <xsl:attribute name="scheme">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="tei:sp/@who">
  <xsl:attribute name="{name(.)}">#<xsl:value-of
  select="."/></xsl:attribute>
</xsl:template>

<xsl:template match="tei:span/@from">
 <xsl:attribute name="from">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="tei:span/@to">
 <xsl:attribute name="to">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="tei:supplied/@hand">
 <xsl:attribute name="hand">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="tei:tagUsage/@render">
 <xsl:attribute name="render">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="tei:tech/@perf">
  <xsl:attribute name="{name(.)}">#<xsl:value-of
  select="."/></xsl:attribute>
</xsl:template>

<xsl:template match="tei:textLang/@langKey">
 <xsl:attribute name="langKey">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="tei:textLang/@otherLangs">
  <xsl:attribute name="{name(.)}">#<xsl:value-of
  select="."/></xsl:attribute>
</xsl:template>

<xsl:template match="tei:timeline/@origin">
 <xsl:attribute name="origin">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="tei:triangle/@value">
 <xsl:attribute name="value">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="tei:u/@who">
  <xsl:attribute name="{name(.)}">#<xsl:value-of
  select="."/></xsl:attribute>
</xsl:template>

<xsl:template match="tei:unclear/@hand">
 <xsl:attribute name="hand">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="tei:vocal/@who">
 <xsl:attribute name="who">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="tei:when/@since">
 <xsl:attribute name="since">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="tei:writing/@script">
 <xsl:attribute name="script">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="tei:writing/@who">
 <xsl:attribute name="who">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>


<xsl:template match="tei:xptr">
  <tei:ptr>
    <xsl:apply-templates
	select="*|@*|processing-instruction()|comment()|text()"/>
  </tei:ptr>
</xsl:template>

<xsl:template match="tei:xptr/@url">
      <xsl:attribute name="target"><xsl:value-of select="."/></xsl:attribute>
</xsl:template>

<xsl:template match="tei:xref">
  <tei:ref>
    <xsl:apply-templates
	select="*|@*|processing-instruction()|comment()|text()"/>
  </tei:ref>
</xsl:template>

<xsl:template match="tei:xref/@url">
      <xsl:attribute name="target"><xsl:value-of select="."/></xsl:attribute>
</xsl:template>

<xsl:template match="teix:add/@hand">
 <xsl:attribute name="hand">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="teix:add/@resp">
 <xsl:attribute name="resp">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="teix:addSpan/@hand">
 <xsl:attribute name="hand">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="teix:addSpan/@resp">
 <xsl:attribute name="resp">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="teix:addSpan/@to">
 <xsl:attribute name="to">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="teix:app/@from">
 <xsl:attribute name="from">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="teix:app/@to">
 <xsl:attribute name="to">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="teix:arc/@from">
 <xsl:attribute name="from">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="teix:arc/@to">
 <xsl:attribute name="to">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="teix:catRef/@scheme">
 <xsl:attribute name="scheme">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="teix:classCode/@scheme">
 <xsl:attribute name="scheme">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="teix:damage/@hand">
 <xsl:attribute name="hand">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="teix:damage/@resp">
 <xsl:attribute name="resp">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="teix:del/@hand">
 <xsl:attribute name="hand">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="teix:del/@resp">
 <xsl:attribute name="resp">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="teix:delSpan/@hand">
 <xsl:attribute name="hand">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="teix:delSpan/@resp">
 <xsl:attribute name="resp">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="teix:delSpan/@to">
 <xsl:attribute name="to">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="teix:eLeaf/@value">
 <xsl:attribute name="value">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="teix:eTree/@value">
 <xsl:attribute name="value">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="teix:event/@who">
 <xsl:attribute name="who">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="teix:expan/@resp">
 <xsl:attribute name="resp">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="teix:f/@fVal">
 <xsl:attribute name="fVal">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="teix:fs/@feats">
  <xsl:attribute name="{name(.)}">#<xsl:value-of
  select="."/></xsl:attribute>
</xsl:template>

<xsl:template match="teix:g/@ref">
  <xsl:choose>
    <xsl:when test="starts-with(.,'#')">
      <xsl:copy/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:attribute name="ref">
	<xsl:text>#</xsl:text><xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template match="teix:gap/@hand">
 <xsl:attribute name="hand">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="teix:gap/@resp">
 <xsl:attribute name="resp">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="teix:handShift/@new">
 <xsl:attribute name="new">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="teix:handShift/@old">
 <xsl:attribute name="old">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="teix:handShift/@resp">
 <xsl:attribute name="resp">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="teix:iNode/@children">
  <xsl:attribute name="{name(.)}">#<xsl:value-of
  select="."/></xsl:attribute>
</xsl:template>

<xsl:template match="teix:iNode/@follow">
 <xsl:attribute name="follow">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="teix:iNode/@parent">
 <xsl:attribute name="parent">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="teix:iNode/@value">
 <xsl:attribute name="value">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="teix:keywords/@scheme">
 <xsl:attribute name="scheme">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="teix:kinesic/@who">
 <xsl:attribute name="who">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="teix:leaf/@follow">
 <xsl:attribute name="follow">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="teix:leaf/@parent">
 <xsl:attribute name="parent">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="teix:leaf/@value">
 <xsl:attribute name="value">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="teix:move/@perf">
  <xsl:attribute name="{name(.)}">#<xsl:value-of
  select="."/></xsl:attribute>
</xsl:template>

<xsl:template match="teix:move/@who">
  <xsl:attribute name="{name(.)}">#<xsl:value-of
  select="."/></xsl:attribute>
</xsl:template>

<xsl:template match="teix:msContents/@class">
  <xsl:attribute name="{name(.)}">#<xsl:value-of
  select="."/></xsl:attribute>
</xsl:template>

<xsl:template match="teix:msItem/@class">
  <xsl:attribute name="{name(.)}">#<xsl:value-of
  select="."/></xsl:attribute>
</xsl:template>

<xsl:template match="teix:node/@adj">
  <xsl:attribute name="{name(.)}">#<xsl:value-of
  select="."/></xsl:attribute>
</xsl:template>

<xsl:template match="teix:node/@adjFrom">
  <xsl:attribute name="{name(.)}">#<xsl:value-of
  select="."/></xsl:attribute>
</xsl:template>

<xsl:template match="teix:node/@adjTo">
  <xsl:attribute name="{name(.)}">#<xsl:value-of
  select="."/></xsl:attribute>
</xsl:template>

<xsl:template match="teix:node/@value">
 <xsl:attribute name="value">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="teix:note/@targetEnd">
  <xsl:attribute name="{name(.)}">#<xsl:value-of
  select="."/></xsl:attribute>
</xsl:template>

<xsl:template match="teix:occupation/@code">
 <xsl:attribute name="code">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="teix:occupation/@scheme">
 <xsl:attribute name="scheme">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="teix:pause/@who">
 <xsl:attribute name="who">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="teix:reg/@resp">
 <xsl:attribute name="resp">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="teix:relation/@active">
  <xsl:attribute name="{name(.)}">#<xsl:value-of
  select="."/></xsl:attribute>
</xsl:template>

<xsl:template match="teix:relation/@passive">
  <xsl:attribute name="{name(.)}">#<xsl:value-of
  select="."/></xsl:attribute>
</xsl:template>

<xsl:template match="teix:restore/@hand">
 <xsl:attribute name="hand">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="teix:restore/@resp">
 <xsl:attribute name="resp">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="teix:root/@children">
  <xsl:attribute name="{name(.)}">#<xsl:value-of
  select="."/></xsl:attribute>
</xsl:template>

<xsl:template match="teix:root/@value">
 <xsl:attribute name="value">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="teix:setting/@who">
  <xsl:attribute name="{name(.)}">#<xsl:value-of
  select="."/></xsl:attribute>
</xsl:template>
<xsl:template match="teix:shift/@who">
 <xsl:attribute name="who">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="teix:socecStatus/@code">
 <xsl:attribute name="code">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="teix:socecStatus/@scheme">
 <xsl:attribute name="scheme">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="teix:sp/@who">
  <xsl:attribute name="{name(.)}">#<xsl:value-of
  select="."/></xsl:attribute>
</xsl:template>

<xsl:template match="teix:span/@from">
 <xsl:attribute name="from">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="teix:span/@to">
 <xsl:attribute name="to">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="teix:supplied/@hand">
 <xsl:attribute name="hand">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="teix:tagUsage/@render">
 <xsl:attribute name="render">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="teix:tech/@perf">
  <xsl:attribute name="{name(.)}">#<xsl:value-of
  select="."/></xsl:attribute>
</xsl:template>

<xsl:template match="teix:textLang/@langKey">
 <xsl:attribute name="langKey">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="teix:textLang/@otherLangs">
  <xsl:attribute name="{name(.)}">#<xsl:value-of
  select="."/></xsl:attribute>
</xsl:template>

<xsl:template match="teix:timeline/@origin">
 <xsl:attribute name="origin">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="teix:triangle/@value">
 <xsl:attribute name="value">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="teix:u/@who">
  <xsl:attribute name="{name(.)}">#<xsl:value-of
  select="."/></xsl:attribute>
</xsl:template>

<xsl:template match="teix:unclear/@hand">
 <xsl:attribute name="hand">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="teix:vocal/@who">
 <xsl:attribute name="who">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="teix:when/@since">
 <xsl:attribute name="since">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="teix:writing/@script">
 <xsl:attribute name="script">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="teix:writing/@who">
 <xsl:attribute name="who">
    <xsl:text>#</xsl:text><xsl:value-of select="."/>
 </xsl:attribute>
</xsl:template>

<xsl:template match="teix:xptr">
  <teix:ptr>
    <xsl:apply-templates
	select="*|@*|processing-instruction()|comment()|text()"/>
  </teix:ptr>
</xsl:template>

<xsl:template match="teix:xref">
  <teix:ref>
    <xsl:apply-templates
	select="*|@*|processing-instruction()|comment()|text()"/>
  </teix:ref>
</xsl:template>


</xsl:stylesheet>