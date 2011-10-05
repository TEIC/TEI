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
    <E53_Place rdf:about="{tei:makeID(.)}">
      <xsl:apply-templates/>
    </E53_Place>
  </xsl:template>

  <xsl:template name="E21">
    <E21_Person rdf:about="{tei:makeID(.)}">
      <xsl:apply-templates/>
    </E21_Person>
  </xsl:template>

  <xsl:template name="E74">
    <E74_Place>
      <xsl:apply-templates/>
    </E74_Place>
  </xsl:template>

  <xsl:template name="E5">
    <P11i_participated_in>
      <E5_Event>
      </E5_Event>
    </P11i_participated_in>
  </xsl:template>

  <xsl:template name="E47">
    <P87_is_identified_by>
      <E47_Place_Spatial_Coordinates>
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
	<P7_took_place_at rdf:resource="{tei:makeID(.)}/{placeName/@key}"/>
      </E67_Birth>
    </P98i_was_born>
  </xsl:template>

  <xsl:template name="E82">
    <xsl:choose>
      <xsl:when test="parent::person">
	<P131_is_identified_by>
	  <xsl:copy-of select="@xml:lang"/>
	  <E82_Actor_Appellation>
	    <rdf:value>
	      <xsl:value-of select="normalize-space(.)"/>
	    </rdf:value>
	  </E82_Actor_Appellation>
	</P131_is_identified_by>
      </xsl:when>
      <xsl:when test="not(@ref)">
	<E21_Person rdf:about="{tei:makeID(.)}">
	  <P131_is_identified_by>
	    <xsl:copy-of select="@xml:lang"/>
	    <E82_Actor_Appellation>
	      <rdf:value>
		<xsl:value-of select="normalize-space(.)"/>
	      </rdf:value>
	    </E82_Actor_Appellation>
	  </P131_is_identified_by>
	</E21_Person>
      </xsl:when>
      <xsl:when test="generate-id(.) = generate-id(key('persons',@ref)[1])">
	<E21_Person rdf:about="{tei:makeID(.)}">
	  <P131_is_identified_by>
	    <xsl:copy-of select="@xml:lang"/>
	    <E82_Actor_Appellation>
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
      <xsl:when test="parent::place">
	<P87_is_identified_by>
	  <xsl:copy-of select="@xml:lang"/>
	  <E48_Place_Name>
	    <rdf:value>
	      <xsl:value-of select="normalize-space(.)"/>
	    </rdf:value>
	  </E48_Place_Name>
	</P87_is_identified_by>
      </xsl:when>
      <xsl:when test=".=''"/>
      <xsl:otherwise>
	<E53_Place rdf:about="{tei:makeID(.)}">
	  <P87_is_identified_by>
	    <xsl:copy-of select="@xml:lang"/>
	    <E48_Place_Name>
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
	  <E53_Place rdf:about="{tei:makeID(.)}">
	    <P87_is_identified_by>
	      <xsl:copy-of select="@xml:lang"/>
	      <E53_Place_Name>
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

  <xsl:function name="tei:makeID" as="xs:string*">
    <xsl:param name="here"/>
      <xsl:for-each select="$here">
	<xsl:variable name="id">
        <xsl:choose>
          <xsl:when test="ancestor-or-self::*/@xml:base">
            <xsl:value-of select="ancestor-or-self::*[@xml:base][1]/@xml:base"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:text>http://www.example.com/id</xsl:text>
          </xsl:otherwise>
        </xsl:choose>
        <xsl:choose>
	  <xsl:when test="@ref">
	    <xsl:value-of select="@ref"/>
	  </xsl:when>
          <xsl:when test="@xml:id">
            <xsl:value-of select="@xml:id"/>
          </xsl:when>
          <xsl:otherwise>
	    <xsl:text>/</xsl:text>
            <xsl:number level="any"/>
          </xsl:otherwise>
        </xsl:choose>
    </xsl:variable>
      <xsl:value-of select="$id"/>
      </xsl:for-each>
  </xsl:function>

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

</xsl:stylesheet>
