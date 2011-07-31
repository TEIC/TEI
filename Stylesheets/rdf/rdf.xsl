<?xml version="1.0"?>
<xsl:stylesheet 
    xmlns:xs="http://www.w3.org/2001/XMLSchema" 
    xmlns:oo="http://purl.org/openorg/"
    xmlns="http://purl.org/NET/crm-owl#" 
    xmlns:tei="http://www.tei-c.org/ns/1.0" 
    xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" 
    xmlns:rdfs="http://www.w3.org/2000/01/rdf-schema#" 
    xmlns:owl="http://www.w3.org/2002/07/owl#" 
    xmlns:xsd="http://www.w3.org/2001/XMLSchema#" 
    xmlns:crm="http://purl.org/NET/crm-owl#" 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:html="http://www.w3.org/1999/xhtml"
    version="1.0" 
    xmlns:foaf="http://xmlns.com/foaf/0.1/"
    xmlns:service="http://purl.org/service/"
    xmlns:ox-service="http://vocab.ox.ac.uk/service/"
    exclude-result-prefixes="tei rdf rdfs owl xsd crm xsl xs oo foaf
    service ox-service">

  <xsl:param name="REQUEST">/</xsl:param>
  <xsl:param name="SERVER">http://www.oucs.ox.ac.uk</xsl:param>

  <xsl:output encoding="utf-8" method="xml" indent="yes"/>

  <xsl:key name="persons" match="persName" use="@ref"/>

  <xsl:template match="/">
    <rdf:RDF>
      <xsl:apply-templates/>
    </rdf:RDF>
  </xsl:template>

  <xsl:template match="html:link[@rev|@rel]">
    <service:Service
	rdf:about="{@href}">
      <xsl:if test="@rev='ox-service:serviceLevelDescription'">
	<rdfs:label>
	  <xsl:value-of
	      select="ancestor::tei:TEI/tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:title|ancestor::TEI.2/teiHeader/fileDesc/titleStmt/title"/>
	</rdfs:label>
	<service:maintainedBy>
	  <foaf:Group>
	    <xsl:attribute name="rdf:about">
	      <xsl:value-of select="substring-before(@href,'service')"/>
	      <xsl:text>group</xsl:text>
	      <xsl:value-of select="substring-after(@href,'service')"/>
	    </xsl:attribute>
	    <foaf:mbox rdf:resource="mailto:{ancestor::tei:TEI/tei:teiHeader/tei:fileDesc/tei:publicationStmt/tei:authority|ancestor::TEI.2/teiHeader/fileDesc/publicationStmt/authority}"/>
	  </foaf:Group>
	</service:maintainedBy>
	<oo:contact>
	    <xsl:attribute name="rdf:resource">
	      <xsl:value-of select="substring-before(@href,'service')"/>
	      <xsl:text>group</xsl:text>
	      <xsl:value-of select="substring-after(@href,'service')"/>
	    </xsl:attribute>
	</oo:contact>

      </xsl:if>
      <xsl:element name="{@rev|@rel}">
	<xsl:attribute name="rdf:resource">
	  <xsl:value-of select="$SERVER"/>
	  <xsl:value-of select="$REQUEST"/>
	</xsl:attribute>
      </xsl:element>


    </service:Service>
  </xsl:template>
<!--  
  <rdf:RDF
    xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
    xmlns:rdfs="http://www.w3.org/2000/01/rdf-schema#"
    xmlns:foaf="http://xmlns.com/foaf/0.1/"
    xmlns:oo="http://purl.org/openorg/"
    xmlns:service="http://purl.org/service/"
    xmlns:ox-service="http://vocab.ox.ac.uk/service/">
  <service:Service rdf:about="http://data.ox.ac.uk/id/service/oucs/nexus">
    <rdfs:label>Nexus</rdfs:label>
    <service:provider rdf:resource="http://oxpoints.oucs.ox.ac.uk/id/23232373"/>
    <service:maintainedBy>
      <foaf:Group rdf:about="http://data.ox.ac.uk/id/group/nexus-team">
        <rdfs:label>Nexus Team</rdfs:label>
        <org:subOrganizationOf rdf:resource="http://oxpoints.oucs.ox.ac.uk/id/23232373"/>
        <foaf:mbox rdf:resource="mailto:nexus@oucs.ox.ac.uk"/>
      </foaf:Group>
    </service:maintainedBy>
    <oo:contact rdf:resource="http://data.ox.ac.uk/id/group/nexus-team"/>

    <service:subService>
      <service:Service rdf:about="http://data.ox.ac.uk/id/service/oucs/nexus/exchange">
        <rdfs:label>Nexus Exchange</rdfs:label>
        <foaf:homepage rdf:resource="http://www.oucs.ox.ac.uk/nexus/email/"/>
        <service:software rdf:resource="http://dbpedia.org/resource/Microsoft_Exchange_Server"/>
        <service:provides rdf:resource="http://dbpedia.org/resource/Email"/>

        <ox-service:serviceLevelDescription rdf:resource="http://www.oucs.ox.ac.uk/internal/sld/nexus-exchange.xml"/>
        <service:interface>
          <service:Interface rdf:about="http://data.ox.ac.uk/id/service-interface/oucs/nexus/exchange/http">
            <service:protocol rdf:resource="http://dbpedia.org/resource/HTTP_Secure"/>
            <service:endpoint rdf:resource="https://nexus.ox.ac.uk/"/>
          </service:Interface>
        </service:interface>
        <service:interface>
          <service:Interface rdf:about="http://data.ox.ac.uk/id/service-interface/oucs/nexus/exchange/imaps">
            <service:protocol rdf:resource="http://dbpedia.org/resource/Internet_Message_Access_Protocol"/>
            <service:endpoint rdf:resource="imaps://imap.nexus.ox.ac.uk/"/>
          </service:Interface>
        </service:interface>
      </service:Service>
    </service:subService>

    <service:subService>
      <service:Service rdf:about="http://data.ox.ac.uk/id/service/oucs/nexus/sharepoint">
        <rdfs:label>Nexus Sharepoint</rdfs:label>
        <foaf:homepage rdf:resource="http://www.oucs.ox.ac.uk/nexus/sharepoint/"/>
        <service:software rdf:resource="http://dbpedia.org/resource/Microsoft_SharePoint_Server"/>
        <service:provides rdf:resource="http://dbpedia.org/resource/Collaborative_software"/>
        <ox-service:serviceLevelDescription rdf:resource="http://www.oucs.ox.ac.uk/internal/sld/nexus-sharepoint.xml"/>
        <service:interface>
          <service:Interface rdf:about="http://data.ox.ac.uk/id/service-interface/oucs/nexus/sharepoint/http">
            <service:protocol rdf:resource="http://dbpedia.org/resource/HTTP_Secure"/>
            <service:endpoint rdf:resource="https://sharepoint.nexus.ox.ac.uk/"/>
          </service:Interface>
        </service:interface>
      </service:Service>
    </service:subService>
  </service:Service>
</rdf:RDF>



<link rel="alternate" type="application/rdf+xml" href="nexus.rdf"/>
<link rel="foaf:primaryTopic" href="http://data.ox.ac.uk/id/service/oucs/nexus"/>


<link rev="foaf:homepage" href="http://data.ox.ac.uk/id/service/oucs/nexus"/>

<http://data.ox.ac.uk/id/service/oucs/nexus> <http://www.xmlns.com/foaf/0.1/homepage> <http://nexus.ox.ac.uk/>

<rdf:RDF>
  <rdf:Description rdf:about="http://www.oucs.ox.ac.uk/internal/sld/nexus.xml">
    <foaf:primaryTopic rdf:resource="http://data.ox.ac.uk/id/service/oucs/nexus"/>
  </rdf:Description>
  <rdf:Description rdf:about="http://data.ox.ac.uk/id/service/oucs/nexus">
    <foaf:homepage rdf:resource="http://www.oucs.ox.ac.uk/nexus/"/>
    <ox-service:serviceLevelDescription rdf:resource="http://www.oucs.ox.ac.uk/internal/sld/nexus.xml"/>
  </rdf:Description>
</rdf:RDF>

-->

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
    <E53_Place>
      <xsl:call-template name="makeID"/>
      <xsl:apply-templates/>
    </E53_Place>
  </xsl:template>

  <xsl:template name="E21">
    <E21_Person>
      <xsl:call-template name="makeID"/>
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
	<E21_Person><xsl:call-template name="makeID"/>
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
	<E21_Person><xsl:call-template name="makeID"/>
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
	<E53_Place><xsl:call-template name="makeID"/>
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
	  <E53_Place><xsl:call-template name="makeID"/>
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

  <xsl:template name="makeID">
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
    <xsl:attribute name="rdf:about">
      <xsl:value-of select="$id"/>
    </xsl:attribute>
  </xsl:template>

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


</xsl:stylesheet>
