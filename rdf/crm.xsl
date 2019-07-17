<?xml version="1.0"?>
<xsl:stylesheet 
    xmlns:xs="http://www.w3.org/2001/XMLSchema" 
        xmlns="http://purl.org/NET/crm-owl#" 
    xmlns:crm="http://purl.org/NET/crm-owl#" 
    xmlns:tei="http://www.tei-c.org/ns/1.0" 
    xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" 
    xmlns:rdfs="http://www.w3.org/2000/01/rdf-schema#" 
    xmlns:owl="http://www.w3.org/2002/07/owl#" 
    xmlns:xsd="http://www.w3.org/2001/XMLSchema#" 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    version="2.0" 
    xpath-default-namespace="http://www.tei-c.org/ns/1.0" 
    exclude-result-prefixes="tei rdf rdfs owl xsd crm xsl xs">
  
  <xsl:output encoding="utf-8" method="xml" indent="yes"/>

  <xsl:key name="persons" match="persName" use="@ref"/>
  <xsl:key name="Places" match="place[placeName]" use="placeName"/>
  <xsl:key name="Idents" 
	   match="crm:P131_is_identified_by"
	   use="crm:E82_Actor_Appellation/@rdf:about"/>

  <xsl:key name="Idents" 
	   match="crm:P87_is_identified_by"
	   use="crm:E48_Place_Name/@rdf:about"/>

  <xsl:template name="TEI">
    <xsl:choose>
      <xsl:when test="parent::*">
	<xsl:apply-templates/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:call-template name="doit"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="processAll">
    <xsl:call-template name="doit"/>
  </xsl:template>

  <xsl:template name="doit">
      <xsl:variable name="rdf1">
	<rdf:RDF
	    xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" 
	    xmlns:rdfs="http://www.w3.org/2000/01/rdf-schema#" 
	    >
	  <xsl:call-template name="typology"/>
	  <xsl:apply-templates/>
	</rdf:RDF>
      </xsl:variable>
      <xsl:apply-templates select="$rdf1" mode="rdf2"/>
  </xsl:template>


  <!-- clean up pass -->

  <xsl:template match="crm:*[crm:E53_Place]" mode="rdf2">
    <xsl:copy>
      <xsl:attribute name="rdf:resource"
			 select="crm:E53_Place/@rdf:about"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="crm:E53_Place" mode="rdf2"/>

  <xsl:template match="crm:E53_Place" mode="rdf3">
     <xsl:copy>
      <xsl:apply-templates  select="*|@*|processing-instruction()|comment()|text()"
	  mode="rdf2"/>
     </xsl:copy>    
  </xsl:template>

  <xsl:template match="rdf:RDF" mode="rdf2">
     <xsl:copy>
      <xsl:apply-templates  select="*|@*|processing-instruction()|comment()|text()"
	  mode="rdf2"/>
     <xsl:apply-templates select=".//crm:E53_Place" mode="rdf3"/>
     </xsl:copy>
  </xsl:template>

  <xsl:template match="*" mode="rdf2">
     <xsl:copy>
      <xsl:apply-templates
	  select="*|@*|processing-instruction()|comment()|text()"
	  mode="rdf2"/>
     </xsl:copy>
   </xsl:template>
   
   <xsl:template match="text()|comment()|@*|processing-instruction()"
		 mode="rdf2">
     <xsl:copy-of select="."/>
   </xsl:template>

   <!-- normal pass -->
  <xsl:template name="E31">
    <E31_Document xmlns="http://purl.org/NET/crm-owl#" 
	rdf:about="{tei:makeID(.,'id')}">
      <xsl:apply-templates select="parent::TEI/teiHeader/fileDesc"/>
    </E31_Document>
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template name="anonblock">
    <xsl:choose>
      <xsl:when test="@type='dDay'">
	<E5_Event rdf:about="{tei:makeID(.,'event')}">
	  <rdf:value>
	    <xsl:value-of select="."/>
	  </rdf:value>
	  <xsl:apply-templates/>
	</E5_Event>
      </xsl:when>
    </xsl:choose>
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
	<xsl:when test="parent::p"/>
	<xsl:otherwise>
	  <E53_Place  xmlns="http://purl.org/NET/crm-owl#" >
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
	<P131_is_identified_by  xmlns="http://purl.org/NET/crm-owl#"
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
    <E74_Group xmlns="http://purl.org/NET/crm-owl#" rdf:about="{tei:makeID(.,'org')}">
      <xsl:apply-templates/>
    </E74_Group>
  </xsl:template>

  <xsl:template name="E5">
    <P11i_participated_in  xmlns="http://purl.org/NET/crm-owl#" >
      <E5_Event rdf:about="{tei:makeID(.,'event')}">
	<rdf:value>
	  <xsl:value-of select="."/>
	</rdf:value>
	<xsl:apply-templates/>
      </E5_Event>
    </P11i_participated_in>
  </xsl:template>

  <xsl:template name="E47">
    <P87_is_identified_by  xmlns="http://purl.org/NET/crm-owl#" >
      <E47_Place_Spatial_Coordinates rdf:about="{tei:makeID(.,'placecoords')}">
	<rdf:value>
	  <xsl:value-of select="."/>
	</rdf:value>
      </E47_Place_Spatial_Coordinates>
    </P87_is_identified_by>
  </xsl:template>

  <xsl:template name="E69">
    <P100i_died_in  xmlns="http://purl.org/NET/crm-owl#" >
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
    <xsl:choose>
      <xsl:when test="parent::residence"/>

      <xsl:when test="parent::creation"/>

      <xsl:when test="parent::p"/>

      <xsl:otherwise>
	<P4_has_time-span  xmlns="http://purl.org/NET/crm-owl#" >
	  <E52_Time-Span>
	    <P82_at_some_time_within>
	      <E61_Time_Primitive>
		<xsl:call-template name="calc-date-value"/>
	      </E61_Time_Primitive>
	    </P82_at_some_time_within>
	  </E52_Time-Span>
	</P4_has_time-span>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="E67">
    <P98i_was_born  xmlns="http://purl.org/NET/crm-owl#" >
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
	    <P7_took_place_at>
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
	    </P7_took_place_at>
	  </xsl:when>
	</xsl:choose>
      </E67_Birth>
    </P98i_was_born>
  </xsl:template>

  <xsl:template name="E82">
    <xsl:choose>
      <xsl:when test="parent::change"/>
      <xsl:when test="parent::person">
	<P131_is_identified_by  xmlns="http://purl.org/NET/crm-owl#" >
	  <xsl:copy-of select="@xml:lang"/>
	  <E82_Actor_Appellation  rdf:about="{tei:makeID(.,'persname')}">
	    <rdf:value>
	      <xsl:value-of select="normalize-space(.)"/>
	    </rdf:value>
	  </E82_Actor_Appellation>
	</P131_is_identified_by>
      </xsl:when>
      <xsl:when test="self::name or ancestor::event or ancestor::ab[@type='dDay']">
	<P11_had_participant>
	  <xsl:choose>
	    <xsl:when test="@ref">
	      <xsl:attribute name="rdf:resource" select="resolve-uri(@ref,base-uri(ancestor::tei:TEI))"/>
	    </xsl:when>
	    <xsl:otherwise>
	      <E21_Person rdf:about="{tei:makeID(.,'person')}">
		<P131_is_identified_by>
		  <E82_Actor_Appellation rdf:about="{tei:makeID(.,'persname')}">
		    <rdf:value>
		      <xsl:value-of select="normalize-space(.)"/>
		    </rdf:value>
		  </E82_Actor_Appellation>
		</P131_is_identified_by>
	      </E21_Person>
	    </xsl:otherwise>
	  </xsl:choose>
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
	<P87_is_identified_by  xmlns="http://purl.org/NET/crm-owl#" >
	  <xsl:copy-of select="@xml:lang"/>
	  <E48_Place_Name rdf:about="{tei:makeID(.,'placename')}">
	    <rdf:value>
	      <xsl:value-of select="normalize-space(.)"/>
	    </rdf:value>
	  </E48_Place_Name>
	</P87_is_identified_by>
      </xsl:when>
      <xsl:when test="district|settlement|region|country|bloc">
	<xsl:call-template name="placeHierarchy">
	  <xsl:with-param name="next">district</xsl:with-param>
	</xsl:call-template>
      </xsl:when>
      <xsl:when test="self::name or ancestor::event  or ancestor::ab[@type='dDay']">
	<P7_took_place_at  xmlns="http://purl.org/NET/crm-owl#" >
	  <xsl:choose>
	    <xsl:when test="@ref">
	      <xsl:attribute name="rdf:resource" select="resolve-uri(@ref,base-uri(ancestor::tei:TEI))"/>
	    </xsl:when>
	    <xsl:otherwise>
	      <E53_Place rdf:about="{tei:makeID(.,'place')}"  xmlns="http://purl.org/NET/crm-owl#" >
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
	</P7_took_place_at>
      </xsl:when>
      <xsl:otherwise>
	<E53_Place rdf:about="{tei:makeID(.,'place')}"  xmlns="http://purl.org/NET/crm-owl#" >
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

  <xsl:template name="placeHierarchy">
    <xsl:param name="next"/>
    <xsl:if test="*[local-name()=$next]">
      <xsl:for-each select="*[local-name()=$next][1]">
	<E53_Place rdf:about="{tei:makeID(.,'place')}"  xmlns="http://purl.org/NET/crm-owl#" >
	  <P2_has_type rdf:resource="http://www.tei-c.org/type/place/{$next}"/>
	  <P87_is_identified_by>
	    <xsl:copy-of select="@xml:lang"/>
	    <E48_Place_Name rdf:about="{tei:makeID(.,'placename')}">
	      <rdf:value>
		<xsl:value-of select="normalize-space(.)"/>
	      </rdf:value>
	    </E48_Place_Name>
	  </P87_is_identified_by>
	  <xsl:choose>
	    <xsl:when test="$next='district'">
	      <xsl:call-template name="placeParent">
		<xsl:with-param name="next">settlement</xsl:with-param>
	      </xsl:call-template>
	    </xsl:when>
	    <xsl:when test="$next='settlement'">
	      <xsl:call-template name="placeParent">
		<xsl:with-param name="next">region</xsl:with-param>
	      </xsl:call-template>
	    </xsl:when>
	    <xsl:when test="$next='region'">
	      <xsl:call-template name="placeParent">
		<xsl:with-param name="next">country</xsl:with-param>
	      </xsl:call-template>
	    </xsl:when>
	    <xsl:when test="$next='country'">
	      <xsl:call-template name="placeParent">
		<xsl:with-param name="next">bloc</xsl:with-param>
	      </xsl:call-template>
	    </xsl:when>
	  </xsl:choose>
	</E53_Place>
      </xsl:for-each>
    </xsl:if>
    <xsl:choose>
      <xsl:when test="$next='district'">
	<xsl:call-template name="placeHierarchy">
	  <xsl:with-param name="next">settlement</xsl:with-param>
	</xsl:call-template>
      </xsl:when>
      <xsl:when test="$next='settlement'">
	<xsl:call-template name="placeHierarchy">
	  <xsl:with-param name="next">region</xsl:with-param>
	</xsl:call-template>
      </xsl:when>
      <xsl:when test="$next='region'">
	<xsl:call-template name="placeHierarchy">
	  <xsl:with-param name="next">country</xsl:with-param>
	</xsl:call-template>
      </xsl:when>
      <xsl:when test="$next='country'">
	<xsl:call-template name="placeHierarchy">
	  <xsl:with-param name="next">bloc</xsl:with-param>
	</xsl:call-template>
      </xsl:when>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="placeParent">
    <xsl:param name="next"/>
    <xsl:if test="parent::*/*[local-name()=$next]">
      <P89_falls_within
	  rdf:resource="{tei:makeID(parent::*/*[local-name()=$next],'place')}"  xmlns="http://purl.org/NET/crm-owl#" />
    </xsl:if>
</xsl:template>
	      
  <xsl:template name="P74">
    <P74_has_current_or_former_residence  xmlns="http://purl.org/NET/crm-owl#" >
      <xsl:choose>
	<xsl:when test="placeName">
	  <xsl:apply-templates/>
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
    </P74_has_current_or_former_residence>
  </xsl:template>

  <xsl:template name="E35">
    <xsl:choose>
      <xsl:when test="ancestor::biblFull or ancestor::bibl"/>
      <xsl:when test="parent::p"/>
      <xsl:otherwise>
	<P102_has_title  xmlns="http://purl.org/NET/crm-owl#" >
	  <E35_Title>
	    <rdf:value>
	      <xsl:value-of select="normalize-space(.)"/>
	    </rdf:value>
	  </E35_Title>
	</P102_has_title>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="E65">
    <xsl:if test="not(ancestor::biblFull or ancestor::bibl)">
    <P94i_was_created_by  xmlns="http://purl.org/NET/crm-owl#" >
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

  <xsl:template name="F24">
    <F24_Publication_Expression   xmlns="http://purl.org/NET/crm-owl#" >
      <R24i_was_created_through>
	<xsl:apply-templates/>
      </R24i_was_created_through>
    </F24_Publication_Expression>
  </xsl:template>

  <xsl:template name="F30">
    <F30_Publication_Event   xmlns="http://purl.org/NET/crm-owl#" >
      <R24i_was_created_through>
	<xsl:apply-templates/>
      </R24i_was_created_through>
    </F30_Publication_Event>
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
	      test="(ancestor::TEI|ancestor::teiCorpus)/teiHeader/fileDesc/publicationStmt/idno[starts-with(.,'http')]">
	    <xsl:value-of 
		select="(ancestor::TEI|ancestor::teiCorpus)/teiHeader/fileDesc/publicationStmt/idno[starts-with(.,'http')][1]"/>
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
		  test="(ancestor::TEI|ancestor::teiCorpus)/teiHeader/fileDesc/publicationStmt/idno">
		<xsl:value-of select="(ancestor::TEI|ancestor::teiCorpus)/teiHeader/fileDesc/publicationStmt/idno[1]"/>
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
	      test="(ancestor::TEI|ancestor::teiCorpus)/teiHeader/fileDesc/publicationStmt/idno[contains(.,'/id/')]">
	    <xsl:analyze-string
		select="(ancestor::TEI|ancestor::teiCorpus)/teiHeader/fileDesc/publicationStmt/idno[contains(.,'/id/')][1]"
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
	  <xsl:value-of select="resolve-uri(@ref,base-uri(ancestor::tei:TEI))"/>
	</xsl:when>
	<xsl:when test="$type='person' and @ref">
	  <xsl:value-of select="resolve-uri(@ref,base-uri(ancestor::tei:TEI))"/>
	</xsl:when>
	<xsl:when test="$type='place' and placeName[@ref]">
	  <xsl:value-of select="placeName/resolve-uri(@ref,base-uri(ancestor::tei:TEI))"/>
	</xsl:when>
	<xsl:when test="$type='place' and key('Places',.)">
	  <xsl:value-of select="tei:makeID(key('Places',.)[1],'place')"/>
	</xsl:when>
	<xsl:when test="$type='placename' and key('Places',.)">
	  <xsl:value-of select="tei:makeID(key('Places',.)[1],'placename')"/>
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
	      <xsl:when test="self::placeName|self::persName|self::district|self::settlement|self::region|self::country|self::bloc">
		<xsl:value-of select="replace(lower-case(normalize-space(.)),'[^A-z]','')"/>
	      </xsl:when>
	      <xsl:when test="self::author">
		<xsl:value-of select="replace(lower-case(normalize-space(.)),'[^A-z]','')"/>
	      </xsl:when>
	      <xsl:when test="persName|placeName">
		<xsl:value-of select="replace(lower-case(normalize-space((persName|placeName)[1])),'[^A-z]','')"/>
	      </xsl:when>
	      <xsl:otherwise>
		<xsl:value-of select="generate-id()"/>
	      </xsl:otherwise>
	    </xsl:choose>
	  </xsl:variable>
	  <xsl:value-of select="$composite"/>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:function>


  

</xsl:stylesheet>
