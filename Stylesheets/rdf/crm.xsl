<?xml version="1.0"?>
<xsl:stylesheet 
    xmlns:xs="http://www.w3.org/2001/XMLSchema" 
    xmlns="http://purl.org/NET/crm-owl#" 
    xmlns:tei="http://www.tei-c.org/ns/1.0" 
    xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" 
    xmlns:rdfs="http://www.w3.org/2000/01/rdf-schema#" 
    xmlns:owl="http://www.w3.org/2002/07/owl#" 
    xmlns:xsd="http://www.w3.org/2001/XMLSchema#" 
    xmlns:crm="http://purl.org/NET/crm-owl#" 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    version="2.0" 
    xpath-default-namespace="http://www.tei-c.org/ns/1.0" 
    exclude-result-prefixes="tei rdf rdfs owl xsd crm xsl xs">
  
  <xsl:output encoding="utf-8" method="xml" indent="yes"/>

  <xsl:key name="persons" match="persName" use="@ref"/>

  <xsl:template match="/">
    <rdf:RDF>
      <xsl:apply-templates/>
    </rdf:RDF>
  </xsl:template>

  <xsl:template name="E31">
    <E31_Document
	rdf:about="{tei:makeID(.,'id')}">
      <xsl:apply-templates/>
    </E31_Document>
  </xsl:template>


  <xsl:template name="teiname">
    <xsl:choose>
      <xsl:when test="@type='place'">
        <xsl:call-template name="E53"/>
      </xsl:when>
      <xsl:when test="@type='person'">
        <xsl:call-template name="E21"/>
      </xsl:when>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="E53">
      <xsl:choose>
	<xsl:when test="ancestor::state"/>	
	<xsl:when test="ancestor::trait"/>
	<xsl:otherwise>
	  <E53_Place>
	  <xsl:attribute name="rdf:about" select="tei:makeID(.,'place')"/>
	    <xsl:apply-templates select="*[not(self::place or self::listPlace)]"/>
	    <xsl:for-each select="parent::place[1]">
	      <P89_falls_within rdf:resource="{tei:makeID(.,'place')}"/>
	    </xsl:for-each>
	  </E53_Place>
	</xsl:otherwise>
      </xsl:choose>
    <xsl:apply-templates select="place|listPlace"/>
  </xsl:template>

  <xsl:template name="E21">
    <xsl:choose>
      <xsl:when test="self::name">
	<P131_is_identified_by
	      rdf:resource="{tei:makeID(.,'persname')}"/>
      </xsl:when>
      <xsl:otherwise>
	<E21_Person rdf:about="{tei:makeID(.,'person')}">
	  <xsl:apply-templates/>
	</E21_Person>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="E74">
    <E74_Group  rdf:about="{tei:makeID(.,'org')}">
      <xsl:apply-templates/>
    </E74_Group>
  </xsl:template>

  <xsl:template name="E5">
    <P11i_participated_in>
      <E5_Event rdf:about="{tei:makeID(.,'event')}">
	<rdf:value>
	  <xsl:value-of select="."/>
	</rdf:value>
	<xsl:apply-templates/>
      </E5_Event>
    </P11i_participated_in>
  </xsl:template>

  <xsl:template name="E47">
    <P87_is_identified_by>
      <E47_Place_Spatial_Coordinates rdf:about="{tei:makeID(.,'placecoords')}">
	<rdf:value>
	  <xsl:value-of select="."/>
	</rdf:value>
      </E47_Place_Spatial_Coordinates>
    </P87_is_identified_by>
  </xsl:template>

  <xsl:template name="E69">
    <P100i_died_in>
      <E69_Death>
	<P4_has_time-span>
	  <E52_Time-Span>
	    <P82_at_some_time_within>
	      <E61_Time_Primitive>
		<xsl:call-template name="calc-date-value"/>
	      </E61_Time_Primitive>
	    </P82_at_some_time_within>
	  </E52_Time-Span>
	</P4_has_time-span>
      </E69_Death>
    </P100i_died_in>
  </xsl:template>

  <xsl:template name="E52">
    <P4_has_time-span>
      <E52_Time-Span>
	<P82_at_some_time_within>
	  <E61_Time_Primitive>
	    <xsl:call-template name="calc-date-value"/>
	  </E61_Time_Primitive>
	</P82_at_some_time_within>
      </E52_Time-Span>
    </P4_has_time-span>
  </xsl:template>

  <xsl:template name="E67">
    <P98i_was_born>
      <E67_Birth>
	<P4_has_time-span>
	  <E52_Time-Span>
	    <P82_at_some_time_within>
	      <E61_Time_Primitive>
		<xsl:call-template name="calc-date-value"/>
	      </E61_Time_Primitive>
	    </P82_at_some_time_within>
	  </E52_Time-Span>
	</P4_has_time-span>
	<xsl:choose>
	  <xsl:when test="placeName">
	    <P7_took_place_at
		rdf:resource="{tei:makeID(.,'place')}"/>
	  </xsl:when>
	</xsl:choose>
      </E67_Birth>
    </P98i_was_born>
  </xsl:template>

  <xsl:template name="E82">
    <xsl:choose>
      <xsl:when test="parent::person">
	<P131_is_identified_by>
	  <xsl:copy-of select="@xml:lang"/>
	  <E82_Actor_Appellation  rdf:about="{tei:makeID(.,'persname')}">
	    <rdf:value>
	      <xsl:value-of select="normalize-space(.)"/>
	    </rdf:value>
	  </E82_Actor_Appellation>
	</P131_is_identified_by>
      </xsl:when>
      <xsl:when test="self::name or ancestor::event">
	<P11_had_participant>
	  <xsl:attribute name="rdf:resource"
			 select="tei:makeID(.,'person')">
	  </xsl:attribute>
	</P11_had_participant>
      </xsl:when>

      <xsl:when test="parent::label"/>
      <xsl:when test="parent::desc"/>
      <xsl:when test="not(@ref)">
	<E21_Person rdf:about="{tei:makeID(.,'person')}">
	  <P131_is_identified_by>
	    <xsl:copy-of select="@xml:lang"/>
	    <E82_Actor_Appellation  rdf:about="{tei:makeID(.,'persname')}">
	      <rdf:value>
		<xsl:value-of select="normalize-space(.)"/>
	      </rdf:value>
	    </E82_Actor_Appellation>
	  </P131_is_identified_by>
	</E21_Person>
      </xsl:when>
      <xsl:when test="generate-id(.) = generate-id(key('persons',@ref)[1])">
	<E21_Person rdf:about="{tei:makeID(.,'person')}">
	  <P131_is_identified_by>
	    <xsl:copy-of select="@xml:lang"/>
	    <E82_Actor_Appellation  rdf:about="{tei:makeID(.,'persname')}">
	      <rdf:value>
		<xsl:value-of select="normalize-space(.)"/>
	      </rdf:value>
	    </E82_Actor_Appellation>
	  </P131_is_identified_by>
	</E21_Person>
      </xsl:when>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="E48">
    <xsl:choose>
      <xsl:when test=".=''"/>
      <xsl:when test="parent::label"/>
      <xsl:when test="parent::desc"/>
      <xsl:when test="parent::tei:location"/>
      <xsl:when test="parent::place">
	<P87_is_identified_by>
	  <xsl:copy-of select="@xml:lang"/>
	  <E48_Place_Name rdf:about="{tei:makeID(.,'placename')}">
	    <rdf:value>
	      <xsl:value-of select="normalize-space(.)"/>
	    </rdf:value>
	  </E48_Place_Name>
	</P87_is_identified_by>
      </xsl:when>
      <xsl:when test="settlement|region|country">
      </xsl:when>
      <xsl:when test="self::name or ancestor::event">
	<P7_took_place_at>
	  <xsl:attribute name="rdf:resource"
			 select="tei:makeID(.,'place')">
	  </xsl:attribute>
	</P7_took_place_at>
      </xsl:when>
      <xsl:otherwise>
	<E53_Place rdf:about="{tei:makeID(.,'place')}">
	  <P87_is_identified_by>
	    <xsl:copy-of select="@xml:lang"/>
	    <E48_Place_Name rdf:about="{tei:makeID(.,'placename')}">
	      <rdf:value>
		<xsl:value-of select="normalize-space(.)"/>
	      </rdf:value>
	    </E48_Place_Name>
	  </P87_is_identified_by>
	</E53_Place>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="P74">
    <P74_has_current_or_former_residence>
      <xsl:choose>
	<xsl:when test="placeName">
	  <xsl:apply-templates/>
	</xsl:when>
	<xsl:otherwise>
	  <E53_Place rdf:about="{tei:makeID(.,'place')}">
	    <P87_is_identified_by>
	      <xsl:copy-of select="@xml:lang"/>
	      <E53_Place_Name rdf:about="{tei:makeID(.,'placename')}">
		<rdf:value>
		  <xsl:value-of select="normalize-space(.)"/>
		</rdf:value>
	      </E53_Place_Name>
	    </P87_is_identified_by>
	  </E53_Place>
	</xsl:otherwise>
      </xsl:choose>
    </P74_has_current_or_former_residence>
  </xsl:template>

  <xsl:template name="E35">
    <xsl:if test="not(ancestor::biblFull)">
    <P102_has_title>
      <E35_Title>
	<rdf:value>
	  <xsl:value-of select="normalize-space(.)"/>
	</rdf:value>
      </E35_Title>
    </P102_has_title>
    </xsl:if>
  </xsl:template>

  <xsl:template name="E65">
    <xsl:if test="not(ancestor::biblFull)">
    <P94i_was_created_by>
      <E65_Creation>
	<P11_had_participant>
	  <E21_Person rdf:about="{tei:makeID(.,'person')}">
	    <P131_is_identified_by>
	      <E82_Actor_Appellation rdf:about="{tei:makeID(.,'persname')}">
		<rdf:value>
		  <xsl:value-of select="normalize-space(.)"/>
		</rdf:value>
	      </E82_Actor_Appellation>
	    </P131_is_identified_by>
	  </E21_Person>
	</P11_had_participant>
      </E65_Creation>
    </P94i_was_created_by>
    </xsl:if>
  </xsl:template>
  
  <!-- general templates -->

  <xsl:template name="calc-date-value">
    <rdf:value>
      <xsl:choose>
        <xsl:when test="@when">
          <xsl:value-of select="@when"/>
        </xsl:when>
        <xsl:when test="@notBefore and @notAfter">
          <xsl:value-of select="@notBefore"/>
          <xsl:text> to  </xsl:text>
          <xsl:value-of select="@notAfter"/>
        </xsl:when>
        <xsl:when test="@notBefore">
          <xsl:value-of select="@notBefore"/>
          <xsl:text> to </xsl:text>
        </xsl:when>
        <xsl:when test="@notAfter">
          <xsl:text> to </xsl:text>
          <xsl:value-of select="@notAfter"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="normalize-space(.)"/>
        </xsl:otherwise>
      </xsl:choose>
    </rdf:value>
  </xsl:template>

  <xsl:function name="tei:makeID" as="xs:string*">
    <xsl:param name="here"/>
    <xsl:param name="type"/>
    <xsl:for-each select="$here">
      <xsl:variable name="baseid">
	<xsl:choose>
	  <xsl:when
	      test="/TEI/teiHeader/fileDesc/publicationStmt/idno[starts-with(.,'http')]">
	    <xsl:value-of 
		select="/TEI/teiHeader/fileDesc/publicationStmt/idno[starts-with(.,'http')][1]"/>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:choose>
	      <xsl:when test="ancestor-or-self::*/@xml:base">
		<xsl:value-of select="ancestor-or-self::*[@xml:base][1]/@xml:base"/>
	      </xsl:when>
	      <xsl:otherwise>
		<xsl:text>http://www.example.com</xsl:text>
	      </xsl:otherwise>
	    </xsl:choose>
	    <xsl:choose>
	      <xsl:when test="@xml:id">
		<xsl:value-of select="@xml:id"/>
	      </xsl:when>
	      <xsl:when
		  test="/TEI/teiHeader/fileDesc/publicationStmt/idno">
		<xsl:value-of select="/TEI/teiHeader/fileDesc/publicationStmt/idno[1]"/>
	      </xsl:when>
	      <xsl:otherwise>
		<xsl:number level="any"/>
	      </xsl:otherwise>
	    </xsl:choose>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:variable>
      <xsl:variable name="baseurl">
	<xsl:choose>
	  <xsl:when
	      test="/TEI/teiHeader/fileDesc/publicationStmt/idno[contains(.,'/id/')]">
	    <xsl:analyze-string
		select="/TEI/teiHeader/fileDesc/publicationStmt/idno[contains(.,'/id/')][1]"
		regex="(.*)/id/[^/]+">
	      <xsl:matching-substring>
		<xsl:value-of select="regex-group(1)"/>
	      </xsl:matching-substring>
	      <xsl:non-matching-substring>
		<xsl:value-of select="regex-group(0)"/>
	      </xsl:non-matching-substring>
	    </xsl:analyze-string>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:choose>
	      <xsl:when test="ancestor-or-self::*/@xml:base">
		<xsl:value-of select="ancestor-or-self::*[@xml:base][1]/@xml:base"/>
	      </xsl:when>
	      <xsl:otherwise>
		<xsl:text>http://www.example.com</xsl:text>
	      </xsl:otherwise>
	    </xsl:choose>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:variable>
      <xsl:choose>
	<xsl:when test="$type='id'">
	  <xsl:value-of select="$baseid"/>
	</xsl:when>	
	<xsl:when test="$type='place' and @ref">
	  <xsl:value-of select="resolve-uri(@ref,base-uri(/tei:TEI))"/>
	</xsl:when>
	<xsl:when test="$type='place' and placeName[@ref]">
	  <xsl:value-of select="placeName/resolve-uri(@ref,base-uri(/tei:TEI))"/>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:variable name="composite">
	    <xsl:value-of select="$baseurl"/>
	    <xsl:text>/</xsl:text>
	    <xsl:value-of select="$type"/>
	    <xsl:text>/</xsl:text>
	    <xsl:choose>
	      <xsl:when test="@xml:id">
		<xsl:value-of select="@xml:id"/>
	      </xsl:when>
	      <xsl:when test="placeName[@key]">
		<xsl:value-of select="placeName/@key"/>
	      </xsl:when>
	      <xsl:when test="self::placeName and
			      parent::place[@xml:id]">
		<xsl:value-of select="parent::place/@xml:id"/>
	      </xsl:when>
	      <xsl:when test="self::persName and
			      parent::person[@xml:id]">
		<xsl:value-of select="parent::person/@xml:id"/>
	      </xsl:when>
	      <xsl:when test="self::placeName|self::persName">
		<xsl:value-of select="replace(lower-case(normalize-space(.)),'[^A-z]','')"/>
	      </xsl:when>
	      <xsl:when test="self::author">
		<xsl:value-of select="replace(lower-case(normalize-space(.)),'[^A-z]','')"/>
	      </xsl:when>
	      <xsl:when test="persName|placeName">
		<xsl:value-of select="replace(lower-case(normalize-space((persName|placeName)[1])),'[^A-z]','')"/>
	      </xsl:when>
	      <xsl:otherwise>
		<xsl:number level="any"/>
	      </xsl:otherwise>
	    </xsl:choose>
	  </xsl:variable>
	  <xsl:value-of select="$composite"/>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:function>


</xsl:stylesheet>
