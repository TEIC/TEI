<!-- 
Text Encoding Initiative Consortium XSLT stylesheet family
$Date$, $Revision$, $Author$

XSL stylesheet to format TEI XML documents to HTML or XSL FO

 
##LICENSE
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
  <xsl:when test="@id|@xml:id">
    <xsl:choose>
     <xsl:when test="$useIDs">
       <xsl:value-of select="@id|@xml:id"/>
     </xsl:when>
     <xsl:otherwise>
      <xsl:value-of select="$BaseFile"/>-<xsl:value-of select="local-name(.)"/>-<xsl:value-of select="generate-id()"/>
     </xsl:otherwise>
    </xsl:choose>
  </xsl:when>
  <xsl:when test="self::tei:div and not(ancestor::tei:div)"> 
  <xsl:variable name="xpath">
       <xsl:for-each select="ancestor-or-self::tei:*">
    <xsl:value-of select="local-name()" />
    <xsl:text />.<xsl:number />
    <xsl:if test="position() != last()">_</xsl:if>
  </xsl:for-each>
  </xsl:variable>
   <xsl:value-of select="substring-after($xpath,'TEI.1_text.1_')"/>
  </xsl:when>
  <xsl:when test="self::tei:divGen"> 
  <xsl:variable name="xpath">
    <xsl:for-each select="ancestor-or-self::tei:*">
      <xsl:value-of select="local-name()" />
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


<xsl:template match="tei:TEI" mode="generateLink">
  <xsl:variable name="BaseFile">
    <xsl:value-of select="$masterFile"/>
    <xsl:call-template name="addCorpusID"/>
  </xsl:variable>
  <xsl:value-of select="concat($BaseFile,$standardSuffix)"/>
</xsl:template>

<xsl:template match="*" mode="generateLink">
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
    <xsl:when test="$rawXML='true' and $depth &lt;= $splitLevel">
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
	<xsl:when test="$rawXML='true'">
	  <xsl:text>JavaScript:void(gotoSection("</xsl:text>
	  <xsl:value-of select="$ident"/>
	  <xsl:text>","</xsl:text>
	  <xsl:value-of select="$parent"/>
	  <xsl:text>"));</xsl:text>
	</xsl:when>
	<xsl:when test="$STDOUT='true'">
	  <xsl:value-of select="$masterFile"/>
	  <xsl:value-of select="$urlChunkPrefix"/>
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

<xsl:template match="tei:anchor">
   <a name="{@id|@xml:id}"/>
</xsl:template>

<xsl:template match="tei:note" mode="generateLink">
    <xsl:text>#Note</xsl:text>
    <xsl:call-template name="noteID"/>
</xsl:template>


<xsl:template match="tei:label|tei:figure|tei:table|tei:item|tei:p|tei:bibl|tei:anchor|tei:cell|tei:lg|tei:list|tei:sp" 
  mode="generateLink">
  <xsl:variable name="ident">
   <xsl:apply-templates select="." mode="ident"/>
  </xsl:variable>
 <xsl:variable name="file">
 <xsl:apply-templates 
   select="ancestor::tei:*[starts-with(local-name(),'div')][1]"  
   mode="generateLink"/>
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


<xsl:template name="makeAnchor">
  <xsl:if test="@id|@xml:id"><a name="{@id|@xml:id}"/></xsl:if>  
</xsl:template>

<xsl:template name="makeInternalLink">
  <xsl:param name="target"/>
  <xsl:param name="ptr"/>
  <xsl:param name="dest"/>
  <xsl:param name="body"/>
  <xsl:param name="class">link_<xsl:value-of
  select="local-name(.)"/></xsl:param>
  <xsl:variable name="W">
    <xsl:choose>
      <xsl:when test="$target"><xsl:value-of select="$target"/></xsl:when>
      <xsl:when test="contains($dest,'#')">
	<xsl:value-of select="substring-after($dest,'#')"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:value-of select="$dest"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  <a>
    <xsl:attribute name="class">
      <xsl:choose>
	<xsl:when test="@rend"><xsl:value-of select="@rend"/></xsl:when>
	<xsl:otherwise><xsl:value-of select="$class"/></xsl:otherwise>
      </xsl:choose>
    </xsl:attribute>
    <xsl:attribute name="href">
      <xsl:choose>
	<xsl:when test="starts-with($dest,'#') or
			contains($dest,'.html') or contains($dest,'ID=')">
	  <xsl:value-of select="$dest"/>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:apply-templates select="key('IDS',$W)"
			       mode="generateLink"/>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:attribute>
    <xsl:choose>
      <xsl:when test="not($body='')">
	<xsl:value-of select="$body"/>
      </xsl:when>
      <xsl:when test="$ptr='true'">
	<xsl:apply-templates mode="xref" select="key('IDS',$W)">
	  <xsl:with-param name="minimal" select="$minimalCrossRef"/>
	</xsl:apply-templates>
      </xsl:when>
      <xsl:otherwise>
	<xsl:apply-templates/>
      </xsl:otherwise>
    </xsl:choose>
  </a>
</xsl:template>

<xsl:template name="makeExternalLink">
  <xsl:param name="ptr"/>
  <xsl:param name="dest"/>
  <xsl:param name="class">link_<xsl:value-of select="local-name(.)"/></xsl:param>
  <a>
    <xsl:attribute name="class">
      <xsl:choose>
	<xsl:when test="@rend"><xsl:value-of select="@rend"/></xsl:when>
	<xsl:otherwise><xsl:value-of select="$class"/></xsl:otherwise>
      </xsl:choose>
    </xsl:attribute>
    <xsl:attribute name="href">
      <xsl:value-of select="$dest"/>
      <xsl:if test="contains(@from,'id (')">
	<xsl:text>#</xsl:text>
	<xsl:value-of select="substring(@from,5,string-length(normalize-space(@from))-1)"/>
      </xsl:if>
    </xsl:attribute>
    <xsl:choose>
      <xsl:when test="@rend='noframe' or $splitLevel=-1 or substring(@url,string-length(@url),1)='/'">
	<xsl:attribute name="target">_top</xsl:attribute>
      </xsl:when>
      <xsl:when test="@rend='new'">
	<xsl:attribute name="target">_blank</xsl:attribute>
      </xsl:when>
      <xsl:when test="contains($dest,'://') or starts-with($dest,'.') or starts-with($dest,'/')">
	<xsl:attribute name="target">_top</xsl:attribute>
      </xsl:when>
      <xsl:when test="substring($dest,string-length($dest),1)='/'">
	<xsl:attribute name="target">_top</xsl:attribute>
      </xsl:when>
      <xsl:when test="$splitLevel=-1">
	<xsl:attribute name="target">_top</xsl:attribute>
      </xsl:when>
    </xsl:choose>
    <xsl:if test="@n">
      <xsl:attribute name="title">
	<xsl:value-of select="@n"/>
      </xsl:attribute> 
    </xsl:if>
    <xsl:call-template name="xrefHook"/>
    <xsl:choose>
      <xsl:when test="$ptr='true'">
	<xsl:element name="{$fontURL}">
	<xsl:choose>
	  <xsl:when test="starts-with($dest,'mailto:')">
	    <xsl:value-of select="substring-after($dest,'mailto:')"/>
	  </xsl:when>
	  <xsl:when test="starts-with($dest,'file:')">
	    <xsl:value-of select="substring-after($dest,'file:')"/>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:value-of select="$dest"/>
	  </xsl:otherwise>
	</xsl:choose>
	</xsl:element>
      </xsl:when>
      <xsl:otherwise>
	<xsl:apply-templates/>
      </xsl:otherwise>
    </xsl:choose>
  </a>
</xsl:template>

<xsl:template name="generateEndLink">
  <xsl:param name="where"/>
<!--
<xsl:message>find link end for <xsl:value-of select="$where"/>,<xsl:value-of select="name(key('IDS',$where))"/></xsl:message>
-->
  <xsl:apply-templates select="key('IDS',$where)" mode="generateLink"/>
</xsl:template>

</xsl:stylesheet>
