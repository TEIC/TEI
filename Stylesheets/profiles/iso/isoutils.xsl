<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
    xmlns:edate="http://exslt.org/dates-and-times"
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    exclude-result-prefixes="edate tei" 
    version="2.0">
  <!-- $Id$ -->

  <xsl:variable name="processor">
    <xsl:value-of select="system-property('xsl:vendor')"/>
  </xsl:variable>
<xsl:param name="doclang">en</xsl:param>
 <xsl:template name="whatsTheDate">
    <xsl:choose>
      <xsl:when test="function-available('edate:date-time')">
        <xsl:value-of select="edate:date-time()"/>
      </xsl:when>
      <xsl:when test="contains($processor,'SAXON')">
        <xsl:value-of select="Date:toString(Date:new())" xmlns:Date="/java.util.Date"/>
      </xsl:when>
      <xsl:otherwise> (unknown date) </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="getiso_committee">
    <xsl:value-of
	select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:respStmt/tei:name"/>
  </xsl:template>

  <xsl:template name="getiso_partNumber">
    <xsl:value-of
	select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:publicationStmt/tei:idno[@type='partNumber']"/>
  </xsl:template>
  <xsl:template name="getiso_wgNumber">
    <xsl:value-of
	select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:publicationStmt/tei:idno[@type='wgNumber']"/>
  </xsl:template>
  
  <xsl:template name="getiso_documentNumber">
    <xsl:value-of
	select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:publicationStmt/tei:idno[@type='documentNumber']"/>
  </xsl:template>
  
  <xsl:template name="getiso_draftNumber">
    <xsl:value-of
	select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:publicationStmt/tei:idno[@type='draftNumber']"/>
  </xsl:template>
  
  <xsl:template name="getiso_stage">
    <xsl:for-each
	select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:publicationStmt/tei:idno[@type='stage']">
      <xsl:choose>
	<xsl:when test="string-length(.)&gt;0">
	  <xsl:text>(</xsl:text>
	  <xsl:value-of select="."/>
	  <xsl:text>)</xsl:text>
	  <xsl:choose>
	    <xsl:when test=".='20'">Preparation</xsl:when>
	    <xsl:when test=".='30'">Committee</xsl:when>
	    <xsl:when test=".='40'">Enquiry</xsl:when>
	    <xsl:when test=".='50'">Approval</xsl:when>
	    <xsl:when test=".='60'">Publication</xsl:when>
	  </xsl:choose>
	</xsl:when>
	<xsl:otherwise>
	  [unspecified]
	</xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>
  
  <xsl:template name="getiso_supplNumber">
    <xsl:value-of
	select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:publicationStmt/tei:idno[@type='supplNumber']"/>
  </xsl:template>
  
  
  <xsl:template name="getiso_authority">
    <xsl:value-of
	select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:publicationStmt/tei:authority"/>
  </xsl:template>
  
  <xsl:template name="getiso_publisher">
    <xsl:value-of
	select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:publicationStmt/tei:publisher"/>
  </xsl:template>
  
  <xsl:template name="getiso_year">
    <xsl:value-of
	select="substring(ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:publicationStmt/tei:date,1,4)"/>
  </xsl:template>
  
  <xsl:template name="getiso_date">
    <xsl:value-of
	select="//tei:TEI/tei:teiHeader/tei:fileDesc/tei:publicationStmt/tei:date"/>
  </xsl:template>
  
  
  <xsl:template name="getiso_title_introductory_fr">
    <xsl:value-of
	select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:title[@xml:lang='fr'
		and @type='introductory']"/>
  </xsl:template>
  <xsl:template name="getiso_title_main_fr">
    <xsl:value-of
	select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:title[@xml:lang='fr'
		and @type='main']"/>
  </xsl:template>
  <xsl:template name="getiso_title_complementary_fr">
    <xsl:param name="withpart">true</xsl:param>
    <xsl:for-each
	select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:title[@xml:lang='fr'
		and @type='complementary']">
      <xsl:variable name="isopart">
	<xsl:call-template name="getiso_partNumber"/>
      </xsl:variable>
      <xsl:if test="not($isopart='') and $withpart='true'">
	<xsl:choose>
	  <xsl:when test="ancestor-or-self::tei:TEI/@xml:lang='ru'"
		    >Part </xsl:when>
	  <xsl:when test="ancestor-or-self::tei:TEI/@xml:lang='fr'"
		    >Partie </xsl:when>
	  <xsl:otherwise>Part </xsl:otherwise>
	</xsl:choose>
	<xsl:value-of select="$isopart"/>
	<xsl:text>: </xsl:text>
      </xsl:if>
      <xsl:value-of select="."/>
    </xsl:for-each>
  </xsl:template>
  <xsl:template name="getiso_title_introductory_en">
    <xsl:value-of
	select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:title[@xml:lang='en'
		and @type='introductory']"/>
  </xsl:template>
  <xsl:template name="getiso_title_main_en">
    <xsl:value-of
	select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:title[@xml:lang='en'
		and @type='main']"/>
  </xsl:template>
  <xsl:template name="getiso_title_complementary_en">
    <xsl:param name="withpart">true</xsl:param>
    <xsl:for-each
	select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:title[@xml:lang='en'
		and @type='complementary']">
      <xsl:variable name="isopart">
	<xsl:call-template name="getiso_partNumber"/>
      </xsl:variable>
      <xsl:if test="not($isopart='') and $withpart='true'">
	<xsl:text>Part </xsl:text>
	<xsl:value-of select="$isopart"/>
	<xsl:text>: </xsl:text>
      </xsl:if>
      <xsl:value-of select="."/>
    </xsl:for-each>
  </xsl:template>
  
  
  <xsl:template name="generateTitle">
    <xsl:call-template name="getiso_title_introductory_en"/>
    <xsl:text> &#x2014; </xsl:text>
    <xsl:call-template name="getiso_title_main_en"/>
    <xsl:if test="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:title[@xml:lang='en'
		and @type='complementary']">
      <xsl:text> &#x2014; </xsl:text>
      <xsl:call-template name="getiso_title_complementary_en"/>
    </xsl:if>
  </xsl:template>
  
  <xsl:template name="makeHTMLHeading">
    <xsl:param name="text"/>
    <xsl:param name="class">title</xsl:param>
    <xsl:param name="level">1</xsl:param>
    <xsl:if test="not($text='')">
      <xsl:element name="h{$level}">
	<xsl:attribute name="class">
	  <xsl:value-of select="$class"/>
	</xsl:attribute>
	<xsl:value-of select="$text"/>
      </xsl:element>
    </xsl:if>
  </xsl:template>
   
  
<xsl:template name="getiso_doctype">
  <xsl:value-of
      select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:profileDesc/tei:textClass/tei:classCode[@scheme='#TYPE']"/>
</xsl:template>

<xsl:template name="getiso_docsubtype">
  <xsl:value-of
      select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:profileDesc/tei:textClass/tei:classCode[@scheme='#SUPPLTYPE']"/>
</xsl:template>
  
<xsl:template name="getiso_copyright">
  <xsl:apply-templates
      select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:publicationStmt/tei:availability[1]"
      mode="titlepage">
    <xsl:with-param name="style">zzCopyright</xsl:with-param>
  </xsl:apply-templates>
</xsl:template>

<xsl:template name="getiso_coverWarning">
  <xsl:apply-templates
      select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:publicationStmt/tei:availability[2]"
      mode="titlepage">
    <xsl:with-param name="style">coverwarning</xsl:with-param>
  </xsl:apply-templates>
</xsl:template>
  
<xsl:template name="getiso_header">
  <xsl:call-template name="getiso_publisher"/>
  <xsl:variable name="stage">
    <xsl:value-of
	select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:publicationStmt/tei:idno[@type='stage']"/>
  </xsl:variable>
  <xsl:choose>
    <xsl:when test="$stage='40'">/DIS </xsl:when>
    <xsl:when test="$stage='50'">/FDIS </xsl:when>
  </xsl:choose>
</xsl:template>

<xsl:template name="getiso_serialNumber">
    <xsl:value-of
	select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:publicationStmt/tei:idno[@type='serialNumber']"/>
</xsl:template>
  
<xsl:template name="docID">
  <xsl:variable name="doclang">
    <xsl:value-of select="ancestor-or-self::tei:TEI/@xml:lang"/>
  </xsl:variable>
  <xsl:variable name="stage">
    <xsl:value-of
	select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:publicationStmt/tei:idno[@type='stage']"/>
  </xsl:variable>
  
  <xsl:call-template name="getiso_documentNumber"/>
  <xsl:text>-</xsl:text>
  <xsl:call-template name="getiso_partNumber"/>
  
  <xsl:choose>
    <xsl:when test="$stage='20'"></xsl:when>
    <xsl:when test="$stage='30'"></xsl:when>
    <xsl:when test="$stage='40'"></xsl:when>
    <xsl:otherwise>
      <xsl:text>:</xsl:text>
      <xsl:call-template name="getiso_year"/>
      <xsl:choose>
	<xsl:when test="starts-with($doclang,'en')">(E)</xsl:when>
	<xsl:when test="starts-with($doclang,'fr')">(F)</xsl:when>
      </xsl:choose>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<!--
40 DIS header: ISO/DIS 15143-2
50 FDIS header: ISO/FDIS 15143-2:2008(E)
60 PUB header: ISO 13909-3:2001(E)
-->    

</xsl:stylesheet>