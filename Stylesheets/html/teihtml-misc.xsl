   <!-- 
Text Encoding Initiative Consortium XSLT stylesheet family
$Date$, $Revision$, $Author$

XSL stylesheet to format TEI XML documents to HTML or XSL FO

 
##LICENSE
--> 
<xsl:stylesheet
  xmlns:tei="http://www.tei-c.org/ns/1.0"

  xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
  version="1.0">

<!-- rendering. support for multiple rendition elements added
by Nick Nicholas -->

<xsl:template name="rendering">
<xsl:call-template name="applyRend">
   <xsl:with-param name="parms" select="concat(@rend,$rendSeparator)"/>
</xsl:call-template>
</xsl:template>

<xsl:template name="applyRend">
   <xsl:param name="parms"/>
   <xsl:choose>
   <xsl:when test="not($parms='')">
     <xsl:variable name="thisparm" select="substring-before($parms,
$rendSeparator)"/>
     <xsl:call-template name="renderingInner">
      <xsl:with-param name="rend" select="$thisparm"/>
      <xsl:with-param name="rest" select="substring-after($parms,
$rendSeparator)"/>
     </xsl:call-template>
   </xsl:when>
   <xsl:otherwise>
     <xsl:apply-templates/>
   </xsl:otherwise>
   </xsl:choose>
</xsl:template>

<xsl:template name="renderingInner">
 <xsl:param name="rend"/>
 <xsl:param name="rest"/>
 <xsl:choose>
  <xsl:when test="$rend='bold'">
   <b>
     <xsl:call-template name="applyRend">
      <xsl:with-param name="parms" select="$rest"/>
     </xsl:call-template>
   </b>
  </xsl:when>
  <xsl:when test="$rend='center'">
   <center>
     <xsl:call-template name="applyRend">
      <xsl:with-param name="parms" select="$rest"/>
     </xsl:call-template>
   </center>
  </xsl:when>
  <xsl:when test="$rend='code'">
   <b><tt>
     <xsl:call-template name="applyRend">
      <xsl:with-param name="parms" select="$rest"/>
     </xsl:call-template>
   </tt></b>
  </xsl:when>
  <xsl:when test="$rend='ital'">
   <i>
     <xsl:call-template name="applyRend">
      <xsl:with-param name="parms" select="$rest"/>
     </xsl:call-template>
   </i>
  </xsl:when>
  <xsl:when test="$rend='italic'">
   <i>
     <xsl:call-template name="applyRend">
      <xsl:with-param name="parms" select="$rest"/>
     </xsl:call-template>
   </i>
  </xsl:when>
  <xsl:when test="$rend='it'">
   <i>
     <xsl:call-template name="applyRend">
      <xsl:with-param name="parms" select="$rest"/>
     </xsl:call-template>
   </i>
  </xsl:when>
  <xsl:when test="$rend='i'">
   <i>
     <xsl:call-template name="applyRend">
      <xsl:with-param name="parms" select="$rest"/>
     </xsl:call-template>
   </i>
  </xsl:when>
  <xsl:when test="$rend='sc'">
<!--   <small>
<xsl:value-of
select="translate(.,'abcdefghijklmnopqrstuvwxyz','ABCDEFGHIJKLMNOPQRSTUVWXYZ')"/>
   </small>
-->
   <span style="font-variant: small-caps">
     <xsl:call-template name="applyRend">
      <xsl:with-param name="parms" select="$rest"/>
     </xsl:call-template>
   </span>
  </xsl:when>
  <xsl:when test="$rend='plain'">
     <xsl:call-template name="applyRend">
      <xsl:with-param name="parms" select="$rest"/>
     </xsl:call-template>
  </xsl:when>
  <xsl:when test="$rend='quoted'">
   <xsl:text>&#8216;</xsl:text>
     <xsl:call-template name="applyRend">
      <xsl:with-param name="parms" select="$rest"/>
     </xsl:call-template>
   <xsl:text>&#8217;</xsl:text>
  </xsl:when>
  <xsl:when test="$rend='sub'">
   <sub>
     <xsl:call-template name="applyRend">
      <xsl:with-param name="parms" select="$rest"/>
     </xsl:call-template>
   </sub>
  </xsl:when>
  <xsl:when test="$rend='sup'">
   <sup>
     <xsl:call-template name="applyRend">
      <xsl:with-param name="parms" select="$rest"/>
     </xsl:call-template>
   </sup>
  </xsl:when>
  <xsl:when test="$rend='important'">
   <span class="important">
     <xsl:call-template name="applyRend">
      <xsl:with-param name="parms" select="$rest"/>
     </xsl:call-template>
   </span>
  </xsl:when>
<!-- NN added -->
  <xsl:when test="$rend='ul'">
   <u>
     <xsl:call-template name="applyRend">
      <xsl:with-param name="parms" select="$rest"/>
     </xsl:call-template>
   </u>
  </xsl:when>
<!-- NN added -->
  <xsl:when test="$rend='interlinMarks'">
   <xsl:text>`</xsl:text>
     <xsl:call-template name="applyRend">
      <xsl:with-param name="parms" select="$rest"/>
     </xsl:call-template>
   <xsl:text>&#180;</xsl:text>
  </xsl:when>
  <xsl:when test="$rend='overbar'">
   <span style="text-decoration:overline">
     <xsl:call-template name="applyRend">
      <xsl:with-param name="parms" select="$rest"/>
     </xsl:call-template>
   </span>
  </xsl:when>
  <xsl:when test="$rend='expanded'">
   <span style="letter-spacing: 0.15em">
     <xsl:call-template name="applyRend">
      <xsl:with-param name="parms" select="$rest"/>
     </xsl:call-template>
   </span>
  </xsl:when>
  <xsl:when test="$rend='strike'">
   <span style="text-decoration: line-through">
     <xsl:call-template name="applyRend">
      <xsl:with-param name="parms" select="$rest"/>
     </xsl:call-template>
   </span>
  </xsl:when>
  <xsl:when test="$rend='small'">
   <span style="font-size: 75%">
     <xsl:call-template name="applyRend">
      <xsl:with-param name="parms" select="$rest"/>
     </xsl:call-template>
   </span>
  </xsl:when>
  <xsl:when test="$rend='large'">
   <span style="font-size: 150%">
     <xsl:call-template name="applyRend">
      <xsl:with-param name="parms" select="$rest"/>
     </xsl:call-template>
   </span>
  </xsl:when>
  <xsl:when test="$rend='smaller'">
   <span style="font-size: 50%">
     <xsl:call-template name="applyRend">
      <xsl:with-param name="parms" select="$rest"/>
     </xsl:call-template>
   </span>
  </xsl:when>
  <xsl:when test="$rend='larger'">
   <span style="font-size: 200%">
     <xsl:call-template name="applyRend">
      <xsl:with-param name="parms" select="$rest"/>
     </xsl:call-template>
   </span>
  </xsl:when>
  <xsl:when test="$rend='calligraphic'">
   <span style="font-family: cursive">
     <xsl:call-template name="applyRend">
      <xsl:with-param name="parms" select="$rest"/>
     </xsl:call-template>
   </span>
  </xsl:when>
  <xsl:when test="$rend='gothic'">
   <span style="font-family: fantasy">
     <xsl:call-template name="applyRend">
      <xsl:with-param name="parms" select="$rest"/>
     </xsl:call-template>
   </span>
  </xsl:when>
  <xsl:when test="$rend='noindex'">
     <xsl:call-template name="applyRend">
      <xsl:with-param name="parms" select="$rest"/>
     </xsl:call-template>
   </xsl:when>
  <xsl:otherwise>
  <xsl:choose>
   <xsl:when test="local-name(.)='p'">
    <xsl:call-template name="unknownRendBlock">
      <xsl:with-param name="rest" select="$rest"/>
      <xsl:with-param name="rend" select="$rend"/>
    </xsl:call-template>
   </xsl:when>
   <xsl:otherwise>
    <xsl:call-template name="unknownRendInline">
      <xsl:with-param name="rest" select="$rest"/>
      <xsl:with-param name="rend" select="$rend"/>
    </xsl:call-template>
   </xsl:otherwise>
   </xsl:choose>
  </xsl:otherwise>
 </xsl:choose>
</xsl:template>


<!-- special purpose -->
<xsl:template match="tei:gi">
    <tt><xsl:text>&lt;</xsl:text>
        <xsl:apply-templates/>
      <xsl:text>&gt;</xsl:text></tt>
</xsl:template>

<xsl:template match="tei:code">
    <tt><xsl:apply-templates/></tt>
</xsl:template>

<xsl:template match="ident">
  <xsl:choose>
    <xsl:when test='@type'>
      <span class="ident-{@type}"><xsl:apply-templates/></span>
    </xsl:when>
    <xsl:otherwise>
      <strong><xsl:apply-templates/></strong>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template  match="tei:label">
  <xsl:if test="@id|@xml:id">
    <a name="{@id|@xml:id}"/>
  </xsl:if>
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="tei:kw">
    <em><xsl:apply-templates/></em>
</xsl:template>

<xsl:template match="tei:eg">
    <pre>
<xsl:if test="$cssFile"><xsl:attribute name="class">eg</xsl:attribute></xsl:if>
      <xsl:apply-templates/>
    </pre>
</xsl:template>

<xsl:template name="getQuote">
  <xsl:param name="quote"/>
  <xsl:choose>
    <xsl:when test="$quote='laquo'">&#x00AB;</xsl:when>
    <xsl:when test="$quote='ldquo'">&#x201C;</xsl:when>
    <xsl:when test="$quote='ldquor'">&#x201E;</xsl:when>
    <xsl:when test="$quote='lsaquo'">&#x2039;</xsl:when>
    <xsl:when test="$quote='lsquo'">&#x2018;</xsl:when>
    <xsl:when test="$quote='lsquor'">&#x201A;</xsl:when>
    <xsl:when test="$quote='mdash'">&#x2014;</xsl:when>
    <xsl:when test="$quote='raquo'">&#x00BB;</xsl:when>
    <xsl:when test="$quote='rdquo'">&#x201D;</xsl:when>
    <xsl:when test="$quote='rdquor'">&#x201f;</xsl:when>
    <xsl:when test="$quote='rsaquo'">&#x203a;</xsl:when>
    <xsl:when test="$quote='rsquo'">&#x2019;</xsl:when>
    <xsl:when test="$quote='rsquor'">&#x201b;</xsl:when>
    <xsl:otherwise>?</xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template match="tei:q">
  <xsl:variable name="pre">
    <xsl:choose>
      <xsl:when test="contains(@rend,'PRE')">
        <xsl:choose>
          <xsl:when test="contains(@rend,'POST')">
            <xsl:call-template name="getQuote">
              <xsl:with-param name="quote" select="normalize-space(substring-before(substring-after(@rend,'PRE'),'POST'))" />
            </xsl:call-template>
          </xsl:when>
          <xsl:otherwise>
            <xsl:call-template name="getQuote">
              <xsl:with-param name="quote" select="normalize-space(substring-after(@rend,'PRE'))" />
            </xsl:call-template>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$preQuote"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  <xsl:variable name="post">
    <xsl:choose>
      <xsl:when test="contains(@rend,'POST')">
        <xsl:call-template name="getQuote">
          <xsl:with-param name="quote" select="normalize-space(substring-after(@rend,'POST'))"/>
        </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$postQuote"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  <xsl:value-of select="$pre"/>
  <xsl:apply-templates/>
  <xsl:value-of select="$post"/>
</xsl:template>

<xsl:template match="tei:q[@rend='eg']">
  <pre>
<xsl:if test="$cssFile"><xsl:attribute name="class">eg</xsl:attribute></xsl:if>
  <xsl:apply-templates/>
 </pre>
</xsl:template>

<xsl:template match="tei:q[@rend='display']">
 <blockquote>
  <xsl:apply-templates/>
 </blockquote>
</xsl:template>

<xsl:template match="tei:emph">
 <xsl:choose>
  <xsl:when test="@rend">
   <xsl:call-template name="rendering"/>
  </xsl:when>
  <xsl:otherwise>
   <em><xsl:apply-templates/></em>
  </xsl:otherwise>
 </xsl:choose>
</xsl:template>

<xsl:template match="tei:hi">
 <xsl:choose>
  <xsl:when test="@rend">
   <xsl:call-template name="rendering"/>
  </xsl:when>
  <xsl:otherwise>
   <strong><xsl:apply-templates/></strong>
  </xsl:otherwise>
 </xsl:choose>
</xsl:template>

<xsl:template match="tei:epigraph">
 <div class="epigraph">
      <xsl:apply-templates/>
 </div>
</xsl:template>


<xsl:template match="tei:lb">
 <br/>
</xsl:template>

<xsl:template match="tei:p[@rend='box']">
 <p class="box"><xsl:apply-templates/></p>
</xsl:template>

<!-- quoting -->
<xsl:template match="tei:cit[@rend='display']">
<blockquote>
  <xsl:apply-templates select="tei:q|quote"/>
  <xsl:apply-templates select="tei:bibl"/>
</blockquote>
</xsl:template>

<xsl:template match="tei:cit">
  <xsl:apply-templates select="tei:q|quote"/>
  <xsl:apply-templates select="tei:bibl"/>
</xsl:template>

<xsl:template match="tei:cit">
 <p class="cit">
        <xsl:apply-templates/>
</p>
</xsl:template>


<xsl:template match="tei:quote[@rend='display']">
 <blockquote>
  <xsl:apply-templates/>
  <xsl:if test="following-sibling::tei:bibl">
   <div align="right"><font size="-1">(<xsl:apply-templates select="following-sibling::tei:bibl"/>)</font></div>
  </xsl:if>
 </blockquote>
</xsl:template>

<xsl:template match="tei:quote[@rend='quoted']">
 <xsl:text>`</xsl:text><xsl:apply-templates/><xsl:text>' </xsl:text>
 <xsl:if test="following-sibling::tei:bibl">
  <font size="-1">(<xsl:apply-templates select="following-sibling::tei:bibl"/>)</font>
 </xsl:if>
</xsl:template>

<!-- copyOf handling -->
<xsl:template match="tei:l[@copyOf]|lg[@copyOf]">
     <xsl:variable name="W">
       <xsl:choose>
	 <xsl:when test="starts-with(@copyof,'#')">
	   <xsl:value-of select="substring-after(@copyof,'#')"/>
	 </xsl:when>
	 <xsl:otherwise>
	   <xsl:value-of select="@copyof"/>
	 </xsl:otherwise>
       </xsl:choose>
     </xsl:variable>

 <xsl:apply-templates select="key('IDS',$W)" mode="Copying"/>
</xsl:template>

<xsl:template match="tei:lg" mode="Copying">
 <xsl:apply-templates select="tei:l"/>
</xsl:template>

<xsl:template match="tei:l" mode="Copying">
 <xsl:apply-templates/>
</xsl:template>


<!-- change log -->
<xsl:template name="changes">
 <center>
  <table width="95%" border="1" cols="2" cellspacing="0">
   <tr>
    <td width="15%"><xsl:text>Date</xsl:text></td>
    <td width="85%"><xsl:text>Action</xsl:text></td>
   </tr>
   <xsl:apply-templates select="ancestor::tei:TEI/teiHeader/revisionDesc/change"/>
  </table>
 </center>
</xsl:template>

<xsl:template match="tei:change">
 <tr>
  <td width="15%" valign="top">
   <xsl:value-of select="./date"/>
  </td>
  <td width="85%"><xsl:value-of select="./item"/>
  </td>
 </tr>
</xsl:template>


<xsl:template match="tei:name" mode="plain">
 <xsl:variable name="ident">
  <xsl:apply-templates select="." mode="ident"/>
 </xsl:variable>
 <a name="{$ident}"></a><xsl:apply-templates/>
</xsl:template>

<xsl:template match="tei:gap">
	[...]<xsl:apply-templates/>
</xsl:template>

<xsl:template match="tei:mentioned">
  <xsl:choose>
    <xsl:when test="@rend">
      <xsl:call-template name="rendering"/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:apply-templates/>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template match="tei:term|title">
  <xsl:choose>
    <xsl:when test="@rend">
      <xsl:call-template name="rendering"/>
    </xsl:when>
    <xsl:otherwise>
      <em><xsl:apply-templates/></em>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>


<xsl:template match="tei:soCalled">
	<xsl:choose>
		<xsl:when test="@rend">
			<xsl:call-template name="rendering"/>
		</xsl:when>
		<xsl:otherwise>
		  <xsl:text>&#8216;</xsl:text><xsl:apply-templates/><xsl:text>&#8217;</xsl:text>
		</xsl:otherwise>
	</xsl:choose>
</xsl:template>

<xsl:template match="tei:foreign">
 <xsl:choose>
  <xsl:when test="@rend">
   <xsl:call-template name="rendering"/>
  </xsl:when>
  <xsl:otherwise>
     <xsl:apply-templates/>
  </xsl:otherwise>
 </xsl:choose>
</xsl:template>

<xsl:template match="tei:address">
 <blockquote>
     <xsl:apply-templates/>
 </blockquote>
</xsl:template>

<xsl:template match="tei:addrLine">
     <xsl:apply-templates/><br/>
</xsl:template>

<xsl:template match="tei:seg">
     <span class="{@type}"><xsl:apply-templates/></span>
</xsl:template>

<xsl:template match="tei:space">
  <xsl:choose>
   <xsl:when test="@extent">
	<xsl:call-template name="space_loop">
   <xsl:with-param name="extent" select="@extent"/>
	</xsl:call-template>
<xsl:apply-templates/>
   </xsl:when>
   <xsl:otherwise>
		<xsl:text>&#160;</xsl:text>
   </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template name="space_loop">
   <xsl:param name="extent"/>
   <xsl:choose>
	<xsl:when test="$extent &lt; 1">
    </xsl:when>
    <xsl:otherwise>
		<xsl:text>&#160;</xsl:text>
    <xsl:variable name="newextent">
      <xsl:value-of select="$extent - 1"/>
    </xsl:variable>
    <xsl:call-template name="space_loop">
   <xsl:with-param name="extent" select="$newextent"/>
	</xsl:call-template>
    </xsl:otherwise>
   </xsl:choose>
</xsl:template>

<xsl:template name="unknownRendBlock">
 <xsl:param name="rend"/>
 <xsl:param name="rest"/>
   <xsl:message>Unknown rend attribute <xsl:value-of
select="$rend"/></xsl:message>
   <code class="undone">[Unknown rendering: <xsl:value-of
select="$rend"/>]</code>
     <xsl:call-template name="applyRend">
      <xsl:with-param name="parms" select="$rest"/>
     </xsl:call-template>
   <code class="undone">[End rendering]</code>
 </xsl:template>

<xsl:template name="unknownRendInline">
 <xsl:param name="rend"/>
 <xsl:param name="rest"/>
 <xsl:message>Unknown rend attribute <xsl:value-of
 select="$rend"/></xsl:message>
 <code class="undone">[Unknown rendering: <xsl:value-of
 select="$rend"/>]</code>
 <xsl:call-template name="applyRend">
   <xsl:with-param name="parms" select="$rest"/>
 </xsl:call-template>
 <code class="undone">[End rendering]</code>
</xsl:template>

</xsl:stylesheet>
