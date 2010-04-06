<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:cals="http://www.oasis-open.org/specs/tm9901"
		xmlns:iso="http://www.iso.org/ns/1.0"
		xmlns:tei="http://www.tei-c.org/ns/1.0"
		xmlns:tbx="http://www.lisa.org/TBX-Specification.33.0.html"
		xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		exclude-result-prefixes="tei iso cals tbx"
		version="2.0">
  <!-- $Id$ -->
  
  <xsl:variable name="processor">
    <xsl:value-of select="system-property('xsl:vendor')"/>
  </xsl:variable>
  
  <xsl:key name="ISOMETA" match="*[@iso:meta]" use="@iso:meta"/>
  <xsl:key name="ALLMETA" match="*[@iso:meta]" use="1"/>
  
  <xsl:param name="doclang">en</xsl:param>
  
  <xsl:template name="whatsTheDate">
    <xsl:value-of select="format-dateTime(current-dateTime(),'[Y]-[M02]-[D02]T[H02]:[M02]:[s02]Z')"/>
  </xsl:template>
  
  
  <xsl:template name="getiso_meta">
    <xsl:param name="meta"/>
    <xsl:value-of select="key('ISOMETA',$meta)"/>
  </xsl:template>
  
  <!--
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
  -->
  
  
  <xsl:template name="getiso_year">
    <xsl:value-of select="substring(key('ISOMETA','docdate'),1,4)"/>
  </xsl:template>
  <!--  
       
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
       
  -->
  <xsl:template name="generateTitle">
    <xsl:choose>
      <xsl:when test="key('ISOMETA','fullTitle')">
	<xsl:value-of select="key('ISOMETA','fullTitle')"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:value-of select="key('ISOMETA','introductoryTitle')"/>
	<xsl:text> — </xsl:text>
	<xsl:value-of select="key('ISOMETA','mainTitle')"/>
	<xsl:if test="key('ISOMETA','complementaryTitle')">
	  <xsl:text> — </xsl:text>
	  <xsl:value-of select="key('ISOMETA','complementaryTitle')"/>
	</xsl:if>
      </xsl:otherwise>
    </xsl:choose>
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
  
  
  <xsl:template name="getiso_copyright">
    <xsl:apply-templates select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:publicationStmt/tei:availability[1]"
			 mode="titlepage">
      <xsl:with-param name="style">zzCopyright</xsl:with-param>
    </xsl:apply-templates>
  </xsl:template>
  
  <xsl:template name="getiso_coverWarning">
    <xsl:apply-templates select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:publicationStmt/tei:availability[2]"
			 mode="titlepage">
      <xsl:with-param name="style">coverwarning</xsl:with-param>
    </xsl:apply-templates>
  </xsl:template>
  
  <xsl:template name="getiso_header">
    <xsl:value-of select="key('ISOMETA','secretariat')"/>
    <xsl:variable name="stage">
      <xsl:value-of select="key('ISOMETA','docStage')"/>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="$stage='40'">/DIS </xsl:when>
      <xsl:when test="$stage='50'">/FDIS </xsl:when>
    </xsl:choose>
  </xsl:template>
  
  <xsl:template name="docID">
    <xsl:variable name="doclang">
      <xsl:value-of select="ancestor-or-self::tei:TEI/@xml:lang"/>
    </xsl:variable>
    <xsl:variable name="stage">
      <xsl:value-of select="key('ISOMETA','docStage')"/>
    </xsl:variable>
    
    <xsl:value-of select="key('ISOMETA','docNumber')"/>
    <xsl:text>-</xsl:text>
    <xsl:value-of select="key('ISOMETA','docPartNumber')"/>
    
    <xsl:choose>
      <xsl:when test="$stage='20'"/>
      <xsl:when test="$stage='30'"/>
      <xsl:when test="$stage='40'"/>
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
  <xsl:template name="getiso_authority">
    <xsl:value-of select="key('ISOMETA','secretariat')"/>
  </xsl:template>
  
  <xsl:template name="getiso_documentNumber">
    <xsl:value-of select="key('ISOMETA','referenceNumber')"/>
  </xsl:template>
  
  <xsl:template name="getiso_partNumber">
    <xsl:value-of select="key('ISOMETA','partNumber')"/>
  </xsl:template>
  
  <xsl:template name="Identify">
    <xsl:for-each select="ancestor::cals:entry">
      <xsl:text> </xsl:text>
      <xsl:call-template name="Identify"/>
      <xsl:text> </xsl:text>
    </xsl:for-each>
    <xsl:choose>
      <xsl:when test="self::tei:p">
	<xsl:call-template name="LabelChange">
	  <xsl:with-param name="What">Paragraph</xsl:with-param>
	  <xsl:with-param
	      name="N"><xsl:number/></xsl:with-param>
	</xsl:call-template>
      </xsl:when>
      <xsl:when test="self::tei:item">
	<xsl:call-template name="LabelChange">
	  <xsl:with-param name="What">List item</xsl:with-param>
	  <xsl:with-param name="N"><xsl:number/></xsl:with-param>
	</xsl:call-template>
      </xsl:when>
      <xsl:when test="self::tei:head">
	<xsl:choose>
	  <xsl:when
	      test="count(ancestor::tei:div)=1">Clause title</xsl:when>
	  <xsl:otherwise>Clause subtitle</xsl:otherwise>
	</xsl:choose>
      </xsl:when>
      <xsl:when test="self::cals:entry">
	<xsl:for-each select="ancestor::cals:table">
	  <xsl:text>Table </xsl:text>
	  <xsl:number level="any"/>
	  <xsl:text>, </xsl:text>
	</xsl:for-each>
	<xsl:call-template name="LabelChange">
	  <xsl:with-param name="What">row</xsl:with-param>
	  <xsl:with-param name="N">
	    <xsl:for-each select="parent::cals:row">
	      <xsl:number/>
	  </xsl:for-each></xsl:with-param>
	</xsl:call-template>
	<xsl:text>, </xsl:text>
	<xsl:call-template name="LabelChange">
	  <xsl:with-param name="What">cell</xsl:with-param>
	  <xsl:with-param name="N"><xsl:number/></xsl:with-param>
	</xsl:call-template>
      </xsl:when>
      <xsl:when test="self::tei:term">
	<xsl:call-template name="LabelChange">
	  <xsl:with-param name="What">Terminology entry</xsl:with-param>
	  <xsl:with-param
	      name="N"><xsl:number/></xsl:with-param>
	</xsl:call-template>
      </xsl:when>
      <xsl:when test="self::tbx:term">
	<xsl:call-template name="LabelChange">
	  <xsl:with-param name="What">Term</xsl:with-param>
	  <xsl:with-param name="N">
	    <xsl:value-of select="substring-after(@id,'user_')"/>
	  </xsl:with-param>
	</xsl:call-template>
      </xsl:when>
      <xsl:when test="self::tei:bibl">
	<xsl:call-template name="LabelChange">
	  <xsl:with-param name="What">Bibliographical entry</xsl:with-param>
	  <xsl:with-param name="N"><xsl:number/></xsl:with-param>
	</xsl:call-template>
      </xsl:when>
      <xsl:when test="self::tei:math">
	<xsl:call-template name="LabelChange">
	  <xsl:with-param name="What">math</xsl:with-param>
	  <xsl:with-param name="N"><xsl:number/></xsl:with-param>
	</xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
	<xsl:call-template name="LabelChange">
	  <xsl:with-param name="What">
	    <xsl:value-of select="local-name()"/>
	  </xsl:with-param>
	  <xsl:with-param name="N"><xsl:number/></xsl:with-param>
	</xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template name="LabelChange">
    <xsl:param name="What"/>
    <xsl:param name="N"/>
    <xsl:choose>
      <xsl:when test="not(number($N))">
	<xsl:value-of select="$What"/>
	<xsl:text> </xsl:text>
	<xsl:value-of select="$N"/>
      </xsl:when>
      <xsl:when test="$N=1">     
	<xsl:text>1st </xsl:text>
	<xsl:value-of select="$What"/>
      </xsl:when>
      <xsl:when test="$N=2">     
	<xsl:text>2nd </xsl:text>
	<xsl:value-of select="$What"/>
      </xsl:when>
      <xsl:when test="$N=3">     
	<xsl:text>3rd </xsl:text>
	<xsl:value-of select="$What"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:value-of select="$N"/>
	<xsl:text>th </xsl:text>
	<xsl:value-of select="$What"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
  <xsl:template name="simpleRun">
    <xsl:param name="prefix"/>
    <xsl:param name="text"/>
    <xsl:param name="italic"/>
    <xsl:value-of select="$prefix"/>
    <xsl:value-of select="$text"/>
  </xsl:template>
  
  <!-- all the special bibl components -->
  <xsl:template
      match="tei:listBibl/tei:bibl/tei:publisher">
    <xsl:call-template name="simpleRun">
      <xsl:with-param name="text">
	<xsl:value-of select="."/>
	<xsl:choose>
	  <xsl:when test="contains(.,'/')">
	    <xsl:text> </xsl:text>
	  </xsl:when>
	  <xsl:when test="../tei:idno[@type='documentType']">
	    <xsl:text>/</xsl:text>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:text> </xsl:text>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  
  <xsl:template
      match="tei:listBibl/tei:bibl/tei:edition">
    <xsl:call-template name="simpleRun">
      <xsl:with-param name="prefix">
	<xsl:text>:</xsl:text>
      </xsl:with-param>
      <xsl:with-param name="text">
	<xsl:value-of select="normalize-space(.)"/>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  
  <xsl:template
      match="tei:listBibl/tei:bibl/tei:title">
    <xsl:call-template name="simpleRun">
      <xsl:with-param name="prefix">
	<xsl:if test="../tei:publisher"><!-- its structured -->
	  <xsl:text>, </xsl:text>
	</xsl:if>
      </xsl:with-param>
      <xsl:with-param name="text">
	<xsl:apply-templates/>
      </xsl:with-param>
      <xsl:with-param name="italic">true</xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  
  <xsl:template
      match="tei:listBibl/tei:bibl/tei:idno[@type='docPartNumber']">
    <xsl:call-template name="simpleRun">
      <xsl:with-param name="prefix">
	<xsl:text>-</xsl:text>
      </xsl:with-param>
      <xsl:with-param name="text">
	<xsl:value-of select="normalize-space(.)"/>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  
  <xsl:template
      match="tei:listBibl/tei:bibl/tei:idno[@type='docNumber']">
    <xsl:call-template name="simpleRun">
      <xsl:with-param name="text">
	<xsl:value-of select="normalize-space(.)"/>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  
  <xsl:template
      match="tei:listBibl/tei:bibl/tei:idno[@type='corrNumber']">
    <xsl:call-template name="simpleRun">
      <xsl:with-param name="prefix">
	<xsl:text>.</xsl:text>
      </xsl:with-param>
      <xsl:with-param name="text">
	<xsl:value-of select="normalize-space(.)"/>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  
  <xsl:template
      match="tei:listBibl/tei:bibl/tei:idno[@type='corrType']">
    <xsl:call-template name="simpleRun">
      <xsl:with-param name="prefix">
	<xsl:text>/</xsl:text>
      </xsl:with-param>
      <xsl:with-param name="text">
	<xsl:value-of select="normalize-space(.)"/>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  
  <xsl:template
      match="tei:listBibl/tei:bibl/tei:idno[@type='corrYear']">
    <xsl:call-template name="simpleRun">
      <xsl:with-param name="prefix">
	<xsl:text>:</xsl:text>
      </xsl:with-param>
      <xsl:with-param name="text">
	<xsl:value-of select="normalize-space(.)"/>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  
  <xsl:template
      match="tei:listBibl/tei:bibl/tei:idno[@type='documentType']">
    <xsl:call-template name="simpleRun">
      <xsl:with-param name="text">
	<xsl:value-of select="normalize-space(.)"/>
	<xsl:text> </xsl:text>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  
  <xsl:template match="tei:listBibl/tei:bibl/tei:idno[@type='parts']">
    <xsl:call-template name="simpleRun">
      <xsl:with-param name="text">
	<xsl:text> </xsl:text>
	<xsl:value-of select="normalize-space(.)"/>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  
  <xsl:template
      match="tei:listBibl/tei:bibl/tei:idno[@type='supplNumber']">
    <xsl:call-template name="simpleRun">
      <xsl:with-param name="prefix">
	<xsl:text>.</xsl:text>
      </xsl:with-param>
      <xsl:with-param name="text">
	<xsl:value-of select="normalize-space(.)"/>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  
  <xsl:template
      match="tei:listBibl/tei:bibl/tei:idno[@type='supplType']">
    <xsl:call-template name="simpleRun">
      <xsl:with-param name="prefix">
	<xsl:text>/</xsl:text>
      </xsl:with-param>
      <xsl:with-param name="text">
	<xsl:value-of select="normalize-space(.)"/>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  
  <xsl:template
      match="tei:listBibl/tei:bibl/tei:idno[@type='supplYear']">
    <xsl:call-template name="simpleRun">
      <xsl:with-param name="prefix">
	<xsl:text>:</xsl:text>
      </xsl:with-param>
      <xsl:with-param name="text">
	<xsl:value-of select="normalize-space(.)"/>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  
  
</xsl:stylesheet>