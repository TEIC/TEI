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
  
  
  <xsl:template name="noteN">
    <xsl:variable name="place" select="@place"/>
    <xsl:choose>
      <xsl:when test="@id|@xml:id">
	<xsl:value-of select="@id|@xml:id"/>
      </xsl:when>
      <xsl:when test="@n">
	<xsl:value-of select="@n"/>
      </xsl:when>
      <xsl:when test="ancestor::tei:front">
	<xsl:number level="any"  count="tei:note[@place=$place]" from="tei:front"/>
      </xsl:when>
      <xsl:when test="ancestor::tei:back">
	<xsl:number level="any"  count="tei:note[@place=$place]" from="tei:back"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:number level="any" count="tei:note[@place=$place]" from="tei:body"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
  <xsl:template match="tei:note">    
    <xsl:choose>
      <xsl:when test="ancestor::tei:bibl">
	(<xsl:apply-templates/>)
      </xsl:when>
      <xsl:when test="@place='inline'">
	<xsl:text> (</xsl:text>
	<xsl:apply-templates/>
	<xsl:text>)</xsl:text>
      </xsl:when>
      <xsl:when test="@place='display'">
	<blockquote>NOTE:
	<xsl:apply-templates/>
	</blockquote>
      </xsl:when>
      <xsl:when test="@place='foot' or @place='end'">
	<xsl:variable name="identifier">
	  <xsl:call-template name="noteN"/>
	</xsl:variable>
	<xsl:choose>
	  <xsl:when test="$footnoteFile">
	    <a class="notelink" href="{$masterFile}-notes.html#{concat('Note',$identifier)}">
	    <sup><xsl:value-of select="$identifier"/></sup></a>
	  </xsl:when>
	  <xsl:otherwise>
	    <a class="notelink" href="#{concat('Note',$identifier)}">
	    <sup><xsl:value-of select="$identifier"/></sup></a>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:when>      
      <xsl:otherwise>
	<xsl:text> [Note: </xsl:text>
	<xsl:apply-templates select="." mode="printnotes"/>
	<xsl:text>]</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
  
  <xsl:template match="tei:note" mode="printnotes">
    <xsl:if test="not(ancestor::tei:bibl)">
      <xsl:variable name="identifier">
	<xsl:call-template name="noteN"/>
      </xsl:variable>
      <xsl:variable name="parent">
	<xsl:call-template name="locateParentdiv"/>
      </xsl:variable>
      <xsl:if test="$verbose">
	<xsl:message>Note <xsl:value-of select="$identifier"/> with parent <xsl:value-of select="$parent"/></xsl:message>
      </xsl:if>
      <div class="note">
	<a name="{concat('Note',$identifier)}"><xsl:value-of select="$identifier"/>. </a>
	<xsl:apply-templates/>
      </div>
    </xsl:if>
  </xsl:template>
  
  
  <xsl:template name="writeNotes">
    <html><xsl:call-template name="addLangAtt"/> 
    <head>
      <title>Notes for
      <xsl:apply-templates select="descendant-or-self::tei:text/tei:front//tei:docTitle//text()"/></title>
      <xsl:call-template name="includeCSS"/>
    </head>
    <body>
      <xsl:call-template name="bodyHook"/>
      <xsl:call-template name="bodyJavaScriptHook"/>
      <xsl:call-template name="stdheader">
	<xsl:with-param name="title">
	  <xsl:text>Notes for </xsl:text>
	  <xsl:apply-templates select="descendant-or-self::tei:text/tei:front//tei:docTitle//text()"/>
	</xsl:with-param>
      </xsl:call-template>
      
      <div class="notes">
	<div class="noteHeading"><xsl:value-of select="$noteHeading"/></div>
	<xsl:apply-templates
	    select="descendant::tei:note[@place]"
	    mode="printnotes"/>
      </div>
      
      <xsl:call-template name="stdfooter">
	<xsl:with-param name="date">
	  <xsl:choose>
	    <xsl:when test="ancestor-or-self::tei:TEI/tei:teiHeader/tei:revisionDesc//tei:date[1]">
	      <xsl:value-of select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:revisionDesc//tei:date[1]"/>
	    </xsl:when>
	    <xsl:otherwise>
	      <xsl:value-of select="ancestor-or-self::tei:TEI//tei:front//tei:docDate"/>
	    </xsl:otherwise>    
	  </xsl:choose>
	</xsl:with-param>
	<xsl:with-param name="author">
	  <xsl:apply-templates select="ancestor-or-self::tei:TEI//tei:front//tei:docAuthor" mode="author"/>
	</xsl:with-param>
      </xsl:call-template>
    </body>
    </html>
  </xsl:template>
  
  
  <xsl:template match="tei:note[@type='action']">
    <div align="right">
      <b>Action <xsl:number level="any" count="tei:note[@type='action']"/></b>:
      <i><xsl:apply-templates/></i>
    </div>
  </xsl:template>
  
  <xsl:template match="tei:divGen[@type='actions']">
    <h3>Actions arising</h3>
    <dl>
      <xsl:for-each select="/tei:TEI/tei:text//tei:note[@type='action']">
	<dt><b><xsl:number level="any" count="tei:note[@type='action']"/></b></dt>
	<dd><xsl:apply-templates/></dd>      
      </xsl:for-each>
    </dl>
  </xsl:template>
  

<xsl:template name="partialFootNotes">
  <xsl:param name="currentID"/>
  <xsl:choose>
    <xsl:when test="$currentID='current'"/>
    <xsl:when test="$currentID='' and $splitLevel=-1">
      <xsl:call-template name="printNotes"/>
    </xsl:when>
    <xsl:when test="$currentID=''">
      <xsl:for-each select=" descendant::tei:text">
	      <xsl:call-template name="printNotes"/>
      </xsl:for-each>
      </xsl:when>
      <xsl:otherwise>
	<xsl:choose>
	  <xsl:when test="count(key('IDS',$currentID))&gt;0">
	    <xsl:for-each select="key('IDS',$currentID)">  
	      <xsl:call-template name="printNotes"/>
	    </xsl:for-each>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:apply-templates
		select="ancestor-or-self::tei:TEI/descendant::tei:text" 
		mode="xpath">
	       <xsl:with-param name="xpath" select="$currentID" />
	       <xsl:with-param name="action">notes</xsl:with-param>
	    </xsl:apply-templates>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:otherwise>
    </xsl:choose>
</xsl:template>

<xsl:template name="printNotes">
  <xsl:if test="descendant::tei:note[@place]">
    <xsl:choose>
      <xsl:when test="not($footnoteFile='')">
	<xsl:variable name="BaseFile">
	  <xsl:value-of select="$masterFile"/>
	  <xsl:call-template name="addCorpusID"/>
	</xsl:variable>
	<xsl:call-template name="outputChunk">
	  <xsl:with-param name="ident">
	    <xsl:value-of select="concat($BaseFile,'-notes')"/>
	  </xsl:with-param>
	  <xsl:with-param name="content">
	    <xsl:call-template name="writeNotes"/>
	  </xsl:with-param>
	</xsl:call-template>
      </xsl:when>
      <xsl:otherwise>	
	<div class="notes">
	  <div class="noteHeading"><xsl:value-of select="$noteHeading"/></div>
	  <xsl:apply-templates
	      select="descendant::tei:note[@place]"
	      mode="printnotes"/>
	</div>
      </xsl:otherwise>
    </xsl:choose>

  </xsl:if>
</xsl:template>

</xsl:stylesheet>
