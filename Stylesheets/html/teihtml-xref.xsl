<!-- 
Text Encoding Initiative Consortium XSLT stylesheet family
$Date$, $Revision$, $Author$

XSL stylesheet to format TEI XML documents to HTML or XSL FO

 
#include LICENSE
--> 
<xsl:stylesheet
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  exclude-result-prefixes="tei"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<xsl:param name="overrideMasterFile"></xsl:param>
<!-- cross-referencing -->

<!-- work out an ID for a given <div> -->
 <xsl:template match="*" mode="ident">
 <xsl:variable name="BaseFile">
 <xsl:value-of select="$masterFile"/>
 <xsl:call-template name="addCorpusID"/>
</xsl:variable>
  <xsl:choose>
  <xsl:when test="@id">
    <xsl:choose>
     <xsl:when test="$useIDs">
       <xsl:value-of select="@id"/>
     </xsl:when>
     <xsl:otherwise>
      <xsl:value-of select="$BaseFile"/>-<xsl:value-of select="name(.)"/>-<xsl:value-of select="generate-id()"/>
     </xsl:otherwise>
    </xsl:choose>
  </xsl:when>
  <xsl:when test="self::tei:div and not(ancestor::tei:div)"> 
  <xsl:variable name="xpath">
       <xsl:for-each select="ancestor-or-self::tei:*">
    <xsl:value-of select="name()" />
    <xsl:text />.<xsl:number />
    <xsl:if test="position() != last()">_</xsl:if>
  </xsl:for-each>
  </xsl:variable>
   <xsl:value-of select="substring-after($xpath,'TEI.1_text.1_')"/>
  </xsl:when>
  <xsl:when test="self::tei:divGen"> 
  <xsl:variable name="xpath">
       <xsl:for-each select="ancestor-or-self::tei:*">
    <xsl:value-of select="name()" />
    <xsl:text />.<xsl:number />
    <xsl:if test="position() != last()">_</xsl:if>
  </xsl:for-each>
  </xsl:variable>
  <xsl:value-of select="substring-after($xpath,'TEI_.1_text.1_')"/>
  </xsl:when>
  <xsl:otherwise>
   <xsl:value-of select="$BaseFile"/>-<xsl:value-of select="name(.)"/>-<xsl:value-of select="generate-id()"/>
  </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<!-- when a <div> is referenced, see whether its  plain anchor, 
 or needs a parent HTML name prepended -->


<xsl:template match="tei:TEI" mode="xrefheader">
  <xsl:variable name="BaseFile">
    <xsl:value-of select="$masterFile"/>
    <xsl:call-template name="addCorpusID"/>
  </xsl:variable>
  <xsl:value-of select="concat($BaseFile,$standardSuffix)"/>
</xsl:template>

<xsl:template match="*" mode="xrefheader">
  <xsl:variable name="ident">
    <xsl:apply-templates select="." mode="ident"/>
  </xsl:variable>
  <xsl:variable name="depth">
    <xsl:apply-templates select="." mode="depth"/>
  </xsl:variable>
  <xsl:variable name="Hash">
    <xsl:choose>
      <xsl:when test="$makeFrames='true' and not($STDOUT='true')">
	<xsl:value-of select="$masterFile"/>
	<xsl:call-template name="addCorpusID"/>
	<xsl:text>.html</xsl:text>
      </xsl:when>
      <xsl:otherwise>
	<xsl:value-of select="$overrideMasterFile"/>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:text>#</xsl:text>
  </xsl:variable>
  <xsl:choose>
    <xsl:when test="$rawIE='true' and $depth &lt;= $splitLevel">
      <xsl:text>JavaScript:void(gotoSection('','</xsl:text>
      <xsl:value-of select="$ident"/>
      <xsl:text>'));</xsl:text>
    </xsl:when>
    <xsl:when test="$STDOUT='true' and $depth &lt;= $splitLevel">
      <xsl:value-of select="$masterFile"/>
      <xsl:value-of select="$urlChunkPrefix"/>
      <xsl:value-of select="$ident"/>
    </xsl:when>
    <xsl:when test="ancestor::tei:back and not($splitBackmatter)">
      <xsl:value-of select="concat($Hash,$ident)"/>
    </xsl:when>
    <xsl:when test="ancestor::tei:front and not($splitFrontmatter)">
      <xsl:value-of select="concat($Hash,$ident)"/>
    </xsl:when>
    <xsl:when test="$splitLevel= -1 and ancestor::tei:teiCorpus">
      <xsl:value-of select="$masterFile"/>
      <xsl:call-template name="addCorpusID"/>
      <xsl:value-of select="$standardSuffix"/>
      <xsl:value-of select="concat($Hash,$ident)"/>
    </xsl:when>
    <xsl:when test="$splitLevel= -1">
      <xsl:value-of select="concat($Hash,$ident)"/>
    </xsl:when>
    <xsl:when test="$depth &lt;= $splitLevel">
      <xsl:value-of select="concat($ident,$standardSuffix)"/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:variable name="parent">
	<xsl:call-template name="locateParentdiv"/>
      </xsl:variable>
      <xsl:choose>
	<xsl:when test="$rawIE='true'">
	  <xsl:text>JavaScript:void(gotoSection("</xsl:text>
	  <xsl:value-of select="$ident"/>
	  <xsl:text>","</xsl:text>
	  <xsl:value-of select="$parent"/>
	  <xsl:text>"));</xsl:text>
	</xsl:when>
	<xsl:when test="$STDOUT='true'">
	  <xsl:value-of select="$masterFile"/>
	  <xsl:text>.ID=</xsl:text>
	  <xsl:value-of select="$parent"/>
	  <xsl:value-of select="concat($standardSuffix,'#')"/>
	  <xsl:value-of select="$ident"/>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:value-of select="$parent"/>
	  <xsl:value-of select="concat($standardSuffix,'#')"/>
	  <xsl:value-of select="$ident"/>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template name="locateParentdiv">
 <xsl:choose>
  <xsl:when test="ancestor-or-self::tei:div and $splitLevel &lt; 0">
     <xsl:apply-templates
     select="ancestor::tei:div[last()]" mode="ident"/>
  </xsl:when>
  <xsl:when test="ancestor-or-self::tei:div">
  <xsl:apply-templates
     select="ancestor::tei:div[last() - $splitLevel]" mode="ident"/>
  </xsl:when>
  <xsl:otherwise>
   <xsl:choose>
    <xsl:when test="$splitLevel = 0">
      <xsl:apply-templates select="ancestor::tei:div1|ancestor::tei:div0" mode="ident"/>
    </xsl:when>
    <xsl:when test="$splitLevel = 1">
      <xsl:apply-templates select="(ancestor::tei:div2|ancestor::tei:div1|ancestor::tei:div0)[last()]" mode="ident"/>
    </xsl:when>
    <xsl:when test="$splitLevel = 2">
      <xsl:apply-templates select="(ancestor::tei:div3|ancestor::tei:div2)[last()]" mode="ident"/>
    </xsl:when>
    <xsl:when test="$splitLevel = 3">
      <xsl:apply-templates select="(ancestor::tei:div4|ancestor::tei:div3)[last()]" mode="ident"/>
    </xsl:when>
    <xsl:when test="$splitLevel = 4">
      <xsl:apply-templates select="(ancestor::tei:div5|ancestor::tei:div4)[last()]" mode="ident"/>
    </xsl:when>
   </xsl:choose>
  </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template name="locateParent">
  <xsl:choose>
  <xsl:when test="self::tei:div">
  <xsl:apply-templates
     select="ancestor::tei:div[last() - $splitLevel + 1]" mode="ident"/>
  </xsl:when>
  <xsl:when test="ancestor::tei:div">
  <xsl:apply-templates
     select="ancestor::tei:div[last() - $splitLevel]" mode="ident"/>
  </xsl:when>
  <xsl:otherwise>
   <xsl:choose>
    <xsl:when test="$splitLevel = 0">
      <xsl:apply-templates select="ancestor::tei:div1|ancestor::tei:div0" mode="ident"/>
    </xsl:when>
    <xsl:when test="$splitLevel = 1">
      <xsl:apply-templates select="ancestor::tei:div2|ancestor::tei:div1|ancestor::tei:div0" mode="ident"/>
    </xsl:when>
    <xsl:when test="$splitLevel = 2">
      <xsl:apply-templates select="ancestor::tei:div3|ancestor::tei:div2" mode="ident"/>
    </xsl:when>
    <xsl:when test="$splitLevel = 3">
      <xsl:apply-templates select="ancestor::tei:div4|ancestor::tei:div3" mode="ident"/>
    </xsl:when>
    <xsl:when test="$splitLevel = 4">
      <xsl:apply-templates select="ancestor::tei:div5|ancestor::tei:div4" mode="ident"/>
    </xsl:when>
   </xsl:choose>
  </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template match="tei:ref">
  <xsl:call-template name="makeHyperLink">     
    <xsl:with-param name="url">
      <xsl:apply-templates mode="xrefheader" select="key('IDS',@target)"/>
    </xsl:with-param>
    <xsl:with-param name="class">
     <xsl:choose>
      <xsl:when test="@rend"><xsl:value-of select="@rend"/></xsl:when>
      <xsl:otherwise><xsl:value-of select="$class_ref"/></xsl:otherwise>     
      </xsl:choose>
    </xsl:with-param>
    <xsl:with-param name="body">
      <xsl:apply-templates/>
    </xsl:with-param>
  </xsl:call-template>
</xsl:template>

<xsl:template match="tei:anchor">
   <a name="{@id}"/>
</xsl:template>

<xsl:template match="tei:note" mode="xrefheader">
    <xsl:text>#Note</xsl:text>
    <xsl:call-template name="noteID"/>
</xsl:template>


<xsl:template match="tei:label|tei:figure|tei:table|tei:item|tei:p|tei:bibl|tei:anchor|tei:cell|tei:lg|tei:list|tei:sp" 
  mode="xrefheader">
  <xsl:variable name="ident">
   <xsl:apply-templates select="." mode="ident"/>
  </xsl:variable>
 <xsl:variable name="file">
 <xsl:apply-templates 
   select="ancestor::tei:*[starts-with(name(),'div')][1]"  
   mode="xrefheader"/>
 </xsl:variable>
 <xsl:choose>
  <xsl:when test="starts-with($file,'#')">
    <xsl:text>#</xsl:text><xsl:value-of select="$ident"/>
  </xsl:when>
  <xsl:when test="contains($file,'#')">
    <xsl:value-of select="substring-before($file,'#')"/>
    <xsl:text>#</xsl:text><xsl:value-of select="$ident"/>
  </xsl:when>
  <xsl:otherwise>
    <xsl:value-of select="$file"/>
    <xsl:text>#</xsl:text><xsl:value-of select="$ident"/>
  </xsl:otherwise>
 </xsl:choose>
</xsl:template>

<xsl:template match="tei:ptr">
  <xsl:call-template name="makeHyperLink">     
    <xsl:with-param name="url">
      <xsl:apply-templates mode="xrefheader" select="key('IDS',@target)"/>
    </xsl:with-param>
    <xsl:with-param name="class">
     <xsl:choose>
      <xsl:when test="@rend"><xsl:value-of select="@rend"/></xsl:when>
      <xsl:otherwise><xsl:value-of select="$class_ptr"/></xsl:otherwise>     
     </xsl:choose>
    </xsl:with-param>
<xsl:with-param name="body">
 <xsl:variable name="xx">
  <xsl:apply-templates mode="header" select="key('IDS',@target)">
    <xsl:with-param name="minimal" select="$minimalCrossRef"/>
 </xsl:apply-templates>
 </xsl:variable>
 <xsl:value-of select="normalize-space($xx)"/>
</xsl:with-param>
  </xsl:call-template>
</xsl:template>

<xsl:template match="tei:xref">
 <xsl:variable name="url">
    <xsl:call-template name="lookupURL"/>
 </xsl:variable>
  <xsl:call-template name="makeHyperLink">     
    <xsl:with-param name="url"><xsl:value-of select="$url"/></xsl:with-param>
    <xsl:with-param name="class">
     <xsl:choose>
      <xsl:when test="@rend"><xsl:value-of select="@rend"/></xsl:when>
      <xsl:otherwise><xsl:value-of select="$class_quicklink"/></xsl:otherwise>     
     </xsl:choose>
    </xsl:with-param>
<xsl:with-param name="body">
 <xsl:apply-templates/>
</xsl:with-param>
  </xsl:call-template>
</xsl:template>

<xsl:template match="tei:xptr">
  <xsl:variable name="url">
    <xsl:call-template name="lookupURL"/>
  </xsl:variable>
  <xsl:variable name="URL">
    <xsl:choose>
     <xsl:when test="starts-with($url,'mailto:')">
     <xsl:value-of select="substring-after($url,'mailto:')"/>
     </xsl:when>
     <xsl:when test="starts-with($url,'file:')">
      <xsl:value-of select="substring-after($url,'file:')"/>
     </xsl:when>
     <xsl:otherwise>
     <xsl:value-of select="$url"/>
     </xsl:otherwise>
     </xsl:choose>
    </xsl:variable>
  <xsl:call-template name="makeHyperLink">     
   <xsl:with-param name="url"><xsl:value-of select="$url"/></xsl:with-param>
   <xsl:with-param name="class">
    <xsl:choose>
     <xsl:when test="@rend"><xsl:value-of select="@rend"/></xsl:when>
     <xsl:otherwise><xsl:value-of select="$class_quicklink"/></xsl:otherwise>
    </xsl:choose>
  </xsl:with-param>
<xsl:with-param name="body">
  <xsl:element name="{$fontURL}">
    <xsl:value-of select="$URL"/>
   </xsl:element>
</xsl:with-param>
  </xsl:call-template>
</xsl:template>

<xsl:template name="lookupURL">
  <xsl:choose>
    <xsl:when test="@url"><xsl:value-of select="@url"/></xsl:when>
    <xsl:otherwise>
      <xsl:value-of select="unparsed-entity-uri(@doc)"/>
      <xsl:choose>
        <xsl:when test="contains(@from,'id (')">
          <xsl:text>#</xsl:text>
           <xsl:value-of select="substring(@from,5,string-length(normalize-space(@from))-1)"/>
        </xsl:when>
      </xsl:choose>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template name="makeAnchor">
  <xsl:if test="@id"><a name="{@id}"/></xsl:if>  
</xsl:template>

<xsl:template name="makeHyperLink">    
  <xsl:param name="url"/>
  <xsl:param name="class"/>
  <xsl:param name="body"/>
  <a><xsl:attribute name="href">
    <xsl:value-of select="$url"/>
  </xsl:attribute>
  <xsl:attribute name="class">
    <xsl:value-of select="$class"/>
  </xsl:attribute>
  <xsl:choose>
   <xsl:when test="@rend='noframe'">
     <xsl:attribute name="target">_top</xsl:attribute>
   </xsl:when>
   <xsl:when test="@rend='new'">
     <xsl:attribute name="target">_blank</xsl:attribute>
   </xsl:when>
   <xsl:when test="contains($url,'://') or starts-with($url,'.') or starts-with($url,'/')">
     <xsl:attribute name="target">_top</xsl:attribute>
   </xsl:when>
   <xsl:when test="substring($url,string-length($url),1)='/'">
     <xsl:attribute name="target">_top</xsl:attribute>
   </xsl:when>
   <xsl:when test="$splitLevel=-1">
     <xsl:attribute name="target">_top</xsl:attribute>
   </xsl:when>
  </xsl:choose>
  <!-- link title from "n" attribute -->
  <xsl:if test="@n">
    <xsl:attribute name="title">
     <xsl:value-of select="@n"/>
    </xsl:attribute> 
  </xsl:if>
   <!-- deal with extra attributes -->
  <xsl:call-template name="xrefHook"/>
    <xsl:copy-of select="$body"/>
  </a>
</xsl:template>
</xsl:stylesheet>
